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
    inherit [_] Ppx_core.Ast_traverse.map_with_context as super
    method! expression ctx expr = match expr with
      | [%expr [%client [%e? fragment_expr ]]] ->
        [%expr [%client [%e super#expression `Client fragment_expr ]]]
      | [%expr ~% [%e? injection_expr ]] ->
        begin match ctx with
          | `Shared -> injection_expr
          | `Client -> expr
        end
      | _ -> super#expression ctx expr
  end

  let client = object
    inherit [_] Ppx_core.Ast_traverse.map_with_context as super
    method! expression ctx expr = match expr with
      | [%expr [%client [%e? fragment_expr ]]] ->
        super#expression `Client fragment_expr
      | [%expr ~% [%e? injection_expr ]] ->
        begin match ctx with
          | `Shared -> expr
          | `Client -> injection_expr
        end
      | _ -> super#expression ctx expr
  end

  let expression ~loc expr =
    let server_expr = server#expression `Shared expr in
    let client_expr = client#expression `Shared expr in
    [%expr
      Eliom_lib.create_shared_value
        [%e server_expr]
        [%client [%e client_expr]]
    ] [@metaloc loc]

  let structure_item stri =
    let server = server#structure_item `Shared stri in
    let client = client#structure_item `Shared stri in
    { client ; server }

  let signature_item sigi =
    let server = server#signature_item `Shared sigi in
    let client = client#signature_item `Shared sigi in
    { client ; server }

end

module Section = struct

  let attribute ~side ~loc =
    let txt = match side with
      | `Client -> "eliom.client"
      | `Server -> "eliom.server"
      | `Shared -> "eliom.shared"
    in
    {Location. txt ; loc}

  let structure ~side str =
    let loc = str.pstr_loc in
    let s = attribute ~side ~loc in
    Str.extension ~loc (s, PStr [str])

  let signature ~side si =
    let loc = si.psig_loc in
    let s = attribute ~side ~loc in
    Sig.extension ~loc (s, PSig [si])

end

let possible_annotations =
  ["shared.start"; "client.start" ;"server.start";
   "shared"; "client"; "server"]


let expression_mapper = object (self)
  inherit [ [ `Client | `Server | `Noside ] ] Ppx_core.Ast_traverse.map_with_context as super

  method! structure_item context stri =
    let loc = stri.pstr_loc in
    match stri.pstr_desc with
    | Pstr_extension (({txt}, _), _) when is_annotation txt possible_annotations ->
      str_error ~loc
        "The %%%%%s extension is only available for first class modules and functors."
        txt
    | _ -> super#structure_item context stri

  method! signature_item context sigi =
    let loc = sigi.psig_loc in
    match sigi.psig_desc with
    | Psig_extension (({txt}, _), _) when is_annotation txt possible_annotations ->
      sig_error ~loc
        "The %%%%%s extension is only available for first class modules and functors."
        txt
    | _ -> super#signature_item context sigi

  method! expression context expr =
    let loc = expr.pexp_loc in
    let attrs = expr.pexp_attributes in
    match expr with

    (* [%shared ... ] *)
    | {pexp_desc = Pexp_extension ({txt}, payload)}
      when is_annotation txt ["shared"] ->
      begin match context, payload with
        | `Server, PStr [{pstr_desc = Pstr_eval (frag_exp,attrs')}] ->
          let e = Shared.expression ~loc frag_exp in
          self#expression context @@ exp_add_attrs (attrs@attrs') e
        | `Server, _ ->
          exp_error ~loc
            "Wrong content for a shared fragment. It should be an expression."
        | `Client, _ ->
          exp_error ~loc
            "Shared fragments are only in a server context, \
             but it is used in a client context."
        | `Noside, _ ->
          exp_error ~loc
            "Shared fragments are only in a server context, \
             but it is used in an ocaml context."
      end
    | _ -> super#expression context expr

end

let str_classify x = match x.pstr_desc with
  | Pstr_module _ | Pstr_recmodule _ | Pstr_modtype _
  | Pstr_include _ | Pstr_open _
    -> `Module

  | Pstr_type _ | Pstr_typext _
  | Pstr_exception _
  | Pstr_class _ | Pstr_class_type _
    -> `Type

  | Pstr_eval _ | Pstr_value _ | Pstr_primitive _
  | Pstr_attribute _ | Pstr_extension _
    -> `Value


let sig_classify x = match x.psig_desc with
  | Psig_module _ | Psig_recmodule _ | Psig_modtype _
  | Psig_include _ | Psig_open _
    -> `Module

  | Psig_type _ | Psig_typext _
  | Psig_exception _
  | Psig_class _ | Psig_class_type _
    -> `Type

  | Psig_value _
  | Psig_attribute _ | Psig_extension _
    -> `Value

(** Toplevel dispatch mechanisms *)
let dispatch
    (* This three parameters are used to parametrized over str/sig. *)
    annotate (* add %server annotation *)
    selfrec (* self recursion on str/sig *)
    exprrec (* call to expression_mapper *)
    classify (* figure out if something is a module decl *)
    shared_duplication (* duplicate shared expressions *)

    context item =
  match classify item with
  (* We do not duplicate modules, we just recursively walk their content. *)
  | `Module -> begin match context with
      | `Shared | `Client | `Server as side ->
        [ annotate ~side @@ selfrec side item ]
      | `Noside ->
        [ selfrec context item ]
    end
  (* We duplicate shared types and values. *)
  | `Type | `Value -> begin match context with
      | `Shared ->
        let x : _ Shared.t = shared_duplication item in [
          annotate ~side:`Client @@ selfrec `Client x.client ;
          annotate ~side:`Server @@ selfrec `Server x.server
        ]
      | `Client | `Server as side ->
        [ annotate ~side @@ exprrec side item ]
      | `Noside ->
        [ exprrec `Noside item ]
    end

let mapper = object (self)
  inherit [ Context.t ] Ppx_core.Ast_traverse.map_with_context as super

  (* This avoid issues with structure and signature triggering on payloads. *)
  method! payload c = function
    | PStr s -> PStr (super#structure c s)
    | PSig s -> PSig (super#signature c s)
    | PTyp t -> PTyp (super#core_type c t)
    | PPat (p, None) -> PPat (super#pattern c p, None)
    | PPat (p, Some e) -> PPat (super#pattern c p, Some (super#expression c e))

  method! structure =
    let dispatch_str =
      dispatch
        Section.structure
        self#structure_item
        expression_mapper#structure_item
        str_classify
        Shared.structure_item
    in
    let f c pstr =
      let loc = pstr.pstr_loc in
      match pstr.pstr_desc with
      | Pstr_extension (({txt}, payload), _)
        when is_annotation txt ["shared.start"; "client.start" ;"server.start"] ->
        (Context.of_string txt, get_empty_str_payload ~loc txt payload)
      | Pstr_extension (({txt}, payload), _)
        when is_annotation txt ["shared" ; "client" ; "server"] ->
        let item =
          flatmap (dispatch_str (Context.of_string txt)) @@
          get_str_payload ~loc txt payload
        in
        (c, item)
      | _ ->
        (c, dispatch_str c pstr)
    in
    fun ctx item -> fold_accum f item ctx


  method! signature =
    let dispatch_sig =
      dispatch
        Section.signature
        self#signature_item
        expression_mapper#signature_item
        sig_classify
        Shared.signature_item
    in
    let f c psig : (_ * signature) =
      let loc = psig.psig_loc in
      match psig.psig_desc with
      | Psig_extension (({txt}, payload), _)
        when is_annotation txt ["shared.start"; "client.start" ;"server.start"] ->
        (Context.of_string txt, get_empty_sig_payload ~loc txt payload)
      | Psig_extension (({txt}, payload), _)
        when is_annotation txt ["shared" ; "client" ; "server"] ->
        let item =
          flatmap (dispatch_sig (Context.of_string txt)) @@
          get_sig_payload ~loc txt payload
        in
        (c, item)
      | _ ->
        (c, dispatch_sig c psig)
    in
    fun ctx item -> fold_accum f item ctx

end



let mapper' _args =
  let c = `Noside in
  {AM.default_mapper
   with
    structure = (fun _ -> mapper#structure c) ;
    signature = (fun _ -> mapper#signature c) ;
  }
