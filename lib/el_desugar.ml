open Parsetree
open Ast_helper

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

module Section = struct

  let attribute ~side ~loc =
    let txt = match side with
      | `Fragment
      | `Client -> "eliom.client"
      | `Escaped_value | `Injection
      | `Server -> "eliom.server"
      | `Shared -> "eliom.shared"
    in
    {Location. txt ; loc}

  let structure ~side ~loc str =
    let s = attribute ~side ~loc in
    Str.extension ~loc (s, PStr str)

  let signature ~side ~loc si =
    let s = attribute ~side ~loc in
    Sig.extension ~loc (s, PSig si)

end

let mapper = object (self)
  inherit [Context.shared] Ppx_core.Ast_traverse.map_with_context as super

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
       [%eliom.fragment e]
    *)
    | {pexp_desc = Pexp_extension ({txt},PStr [{pstr_desc = Pstr_eval (frag_exp,attrs)}])},
      `Server
      when is_annotation txt ["client"] ->
      let frag_exp = self#expression `Client frag_exp in
      let s = {Location.loc ; txt = "eliom.fragment"} in
      Exp.extension ~loc ~attrs (s, PStr [Str.eval ~loc frag_exp])

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

  (** Toplevel translation *)
  method private dispatch_str ~loc context x =
    match context with
    | `Shared ->
      let f x =
        let x = Shared.structure_item x in
        self#structure `Client [x.client] @ self#structure `Server [x.server]
      in flatmap f x
    | #Context.t as side ->
      [Section.structure ~side ~loc @@ List.map (self#structure_item side) x]

  method private dispatch_sig ~loc context x =
    match context with
    | `Shared ->
      let f x =
        let x = Shared.signature_item x in
        self#signature `Client [x.client] @ self#signature `Server [x.server]
      in flatmap f x
    | #Context.t as side ->
      [Section.signature ~side ~loc @@ List.map (self#signature_item side) x]

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
        (c, self#dispatch_str ~loc (Context.of_string txt) strs)
      | Pstr_extension (({txt}, _), _)
        when is_annotation txt ["shared"; "client" ;"server"] ->
          c, [ str_error ~loc "Wrong payload for the %%%%%s extension." txt ]
      | _ ->
        (c, self#dispatch_str ~loc c [pstr])
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
      | Psig_extension (({txt}, PSig sigs), _)
        when is_annotation txt ["shared"; "client" ;"server"] ->
        (c, self#dispatch_sig ~loc (Context.of_string txt) sigs)
      | Psig_extension (({txt}, _), _)
        when is_annotation txt ["shared"; "client" ;"server"] ->
          c, [ sig_error ~loc "Wrong payload for the %%%%%s extension." txt ]
      | _ ->
        (c, self#dispatch_sig ~loc c [psig])
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
