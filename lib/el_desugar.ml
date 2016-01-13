open Parsetree
open Ast_helper
open Ppx_core.Std

module AM = Ast_mapper
module AC = Ast_convenience

open El_utils

(**
   Replace shared expression by the equivalent pair.

   [ [%share
       let x = ... %s ... in
       [%client ... %x ... ]
     ] ]
   ≡
   [ let x = ... s ... in
     [%client ... %x ... ]
     ,
     [%client
       let x = ... %s ... in
       ... x ...
     ]
   ]
*)
module Shared = struct

  type 'a t = { client : 'a ; server : 'a }

  let server = object
    inherit Ppx_core.Ast_traverse.map as super
    method! expression expr = match expr with
      | [%expr [%client [%e? _ ]]] -> expr
      | [%expr ~% [%e? injection_expr ]] -> injection_expr
      | _ -> super#expression expr
  end

  let client = object
    inherit [_] Ppx_core.Ast_traverse.map_with_context as super
    method! expression ctx expr = match expr with
      | [%expr [%client [%e? fragment_expr ]]] ->
        super#expression `Fragment fragment_expr
      | [%expr ~% [%e? injection_expr ]] ->
        begin match ctx with
          | `Top -> expr
          | `Fragment -> injection_expr
        end
      | _ -> super#expression ctx expr
  end

  let expression ~loc expr =
    let server_expr = server#expression expr in
    let client_expr = client#expression `Top expr in
    [%expr
      Eliom_lib.create_shared_value
        [%e server_expr]
        [%client [%e client_expr]]
    ] [@metaloc loc]

  let structure_item stri =
    let server = server#structure_item stri in
    let client = client#structure_item `Top stri in
    { client ; server }

  let signature_item sigi =
    let server = server#signature_item sigi in
    let client = client#signature_item `Top sigi in
    { client ; server }

end

let annotate_fragment ?typ exp =
  let loc = exp.pexp_loc in
  let typ = match typ with
    | None -> [%type: _ Eliom_runtime.fragment][@metaloc loc]
    | Some typ -> [%type: [%t typ] Eliom_runtime.fragment][@metaloc loc]
  in
  Exp.constraint_ ~loc exp typ

(** Given a name map, create an expression of the form
    ((fun _ _ _ -> assert false) e_1 .. e_n)

    The resulting expression should be of type [∀ 'a. 'a].
*)
let make_poly ~loc ?id ?typ m =
  let assert_false = [%expr assert false][@metaloc loc] in
  let exp = annotate_fragment ?typ assert_false in
  let arg = Name.Map.tuple ~loc m in
  match id with
  | Some id ->
    let id = Ast_builder.Default.eint64 ~loc id in
    [%expr
      (fun _id _arg -> [%e exp]) [%e id] [%e arg]
    ][@metaloc loc]
  | None ->
    [%expr
      (fun _arg -> [%e exp]) [%e arg]
    ][@metaloc loc]

let mapper = object (self)
  inherit [Context.shared] Ppx_core.Ast_traverse.map_with_context as super

  val mutable fragment_map = Name.Map.empty
  val mutable injection_counter = 0L

  method! expression context expr =
    let loc = expr.pexp_loc in
    let attrs = expr.pexp_attributes in
    match expr, context with
    | {pexp_desc = Pexp_extension ({txt},_)},
      `Client
      when is_annotation txt ["client"; "shared"] ->
      let side = get_extension expr in
      exp_error ~loc
        "The syntax [%%%s ...] is not allowed inside client code."
        side
    | {pexp_desc = Pexp_extension ({txt},_)}
    , (`Fragment | `Escaped_value | `Injection)
      when is_annotation txt ["client"; "shared"] ->
      let side = get_extension expr in
      exp_error ~loc
        "The syntax [%%%s ...] can not be nested."
        side

    (* [%shared ... ] *)
    | {pexp_desc = Pexp_extension ({txt},PStr [{pstr_desc = Pstr_eval (frag_exp,attrs')}])},
      `Server
      when is_annotation txt ["shared"] ->
      let e = Shared.expression ~loc frag_exp in
      self#expression context @@ exp_add_attrs (attrs@attrs') e

    (* [%client e ] with e = ... ~%x ...

       let escp1 = x in
       ((fun _ -> assert false) escp1)[@eliom.fragment a]
    *)
    | {pexp_desc = Pexp_extension ({txt},PStr [{pstr_desc = Pstr_eval (frag_exp,attrs)}])},
      `Server
      when is_annotation txt ["client"] ->
      let frag_exp = self#expression `Client frag_exp in
      let frag_exp, typ = match frag_exp.pexp_desc with
        | Pexp_constraint (e, typ) -> e, Some typ
        | _ -> frag_exp, None
      in
      let frag_exp, m = collect_escaped frag_exp in
      let eliom_attr =
        Location.mkloc eliom_fragment_attr loc, PStr [Str.eval frag_exp]
      in
      let id, new_fragment_map = Name.add_fragment frag_exp fragment_map in
      fragment_map <- new_fragment_map ;
      let poly_exp =
        exp_add_attrs (eliom_attr :: attrs) @@ make_poly ~loc ~id ?typ m
      in
      if Name.Map.is_empty m then poly_exp
      else Exp.let_ ~loc Nonrecursive (Name.Map.value_bindings m) poly_exp

    (* ~%( ... ) ] *)
    | [%expr ~% [%e? inj ]], _ ->
      begin match context with
        | `Client ->
          let context = `Injection in
          make_inj ~loc @@ super#expression context inj
        | `Fragment ->
          let context = `Escaped_value in
          make_inj ~loc @@ super#expression context inj
        | `Server ->
          exp_error ~loc "The syntax ~%% ... is not allowed inside server code."
        | `Escaped_value | `Injection ->
          exp_error ~loc "The syntax ~%% ... can not be nested."
        | `Shared ->
          assert false (* TODO *)
      end
    | _ -> super#expression context expr

  (** Client section translation.

      We collect all injections, hoist them, and hide the client code in
      a ppx attribute. See also client fragments.

      The resulting code is of this shape:

      let x1 = e1 and ...
      let _ = ((fun _ _ -> assert false) x1 ...)
        [@@eliom.section client_str]
  *)
  method private client_section stri =
    let loc = stri.pstr_loc in
    let stri = self#structure_item `Client stri in
    let stri, new_m, new_injection_counter =
      collect_injection stri injection_counter
    in
    injection_counter <- new_injection_counter ;
    let exp = Name.Map.tuple ~loc new_m in
    let bindings =
      Str.value ~loc Nonrecursive @@ Name.Map.value_bindings new_m
    in
    let attrs = [Location.mkloc eliom_section_attr loc, PStr [stri]] in
    let s = Str.value ~loc Nonrecursive
        [ Vb.mk ~loc ~attrs (Pat.any ~loc ()) exp ]
    in
    [ bindings ; s ]


  (** Toplevel translation *)
  method private dispatch_str context x =
    match context with
    | `Shared ->
      let f x =
        let x = Shared.structure_item x in
        self#structure `Client [x.client] @ self#structure `Server [x.server]
      in flatmap f x
    | `Client ->
      flatmap self#client_section x
    | #Context.t as c -> List.map (self#structure_item c) x

  method private dispatch_sig context x =
    match context with
    | `Shared ->
      let f x =
        let x = Shared.signature_item x in
        self#signature `Client [x.client] @ self#signature `Server [x.server]
      in flatmap f x
    | #Context.t as c -> List.map (self#signature_item c) x

  method! structure context structs =
    let f c pstr =
      let loc = pstr.pstr_loc in
      match pstr.pstr_desc with
      | Pstr_extension (({txt}, payload), _)
        when is_annotation txt ["shared.start"; "client.start" ;"server.start"] ->
        begin match payload with
          | PStr [] -> (Context.of_string txt, [])
          | _ ->
            c, [ str_error ~loc "Wrong payload for the %%%%%s extension." txt ]
        end
      | Pstr_extension (({txt}, PStr strs), _)
        when is_annotation txt ["shared"; "client" ;"server"] ->
        (c, self#dispatch_str (Context.of_string txt) strs)
      | Pstr_extension (({txt}, _), _)
        when is_annotation txt ["shared"; "client" ;"server"] ->
          c, [ str_error ~loc "Wrong payload for the %%%%%s extension." txt ]
      | _ ->
        (c, self#dispatch_str c [pstr])
    in
    fold_accum f structs context

  method! signature context sigs =
    let f c psig =
      let loc = psig.psig_loc in
      match psig.psig_desc with
      | Psig_extension (({txt=("shared.start"|"client.start"|"server.start" as txt)}, PStr strs), _) ->
        if strs <> [] then
          c, [ sig_error ~loc
              "The %%%%%s extension doesn't accept arguments." txt ]
        else (Context.of_string txt, [])
      | _ ->
        (c, self#dispatch_sig c [psig])
    in
    fold_accum f sigs context

end



let mapper' _args =
  let c = `Server in
  {AM.default_mapper
   with
    structure = (fun _ -> mapper#structure c) ;
    signature = (fun _ -> mapper#signature c) ;
  }
