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
    method! structure c l =
      let f s = match s.pstr_desc with
        | Pstr_extension (({txt}, payload), _)
          when is_annotation txt ["shared" ; "server"] ->
          get_str_payload ~loc:s.pstr_loc txt payload
        | Pstr_extension (({txt}, _), _)
          when is_annotation txt ["client"] -> []
        | _ -> [s]
      in
      super#structure c @@ flatmap f l
    method! signature c l =
      let f s = match s.psig_desc with
        | Psig_extension (({txt}, payload), _)
          when is_annotation txt ["shared" ; "server"] ->
          get_sig_payload ~loc:s.psig_loc txt payload
        | Psig_extension (({txt}, _), _)
          when is_annotation txt ["client"] -> []
        | _ -> [s]
      in
      super#signature c @@ flatmap f l
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
    method! structure c l =
      let f s = match s.pstr_desc with
        | Pstr_extension (({txt}, payload), _)
          when is_annotation txt ["shared" ; "client"] ->
          get_str_payload ~loc:s.pstr_loc txt payload
        | Pstr_extension (({txt}, _), _)
          when is_annotation txt ["server"] -> []
        | _ -> [s]
      in
      super#structure c @@ flatmap f l
    method! signature c l =
      let f s = match s.psig_desc with
        | Psig_extension (({txt}, payload), _)
          when is_annotation txt ["shared" ; "client"] ->
          get_sig_payload ~loc:s.psig_loc txt payload
        | Psig_extension (({txt}, _), _)
          when is_annotation txt ["server"] -> []
        | _ -> [s]
      in
      super#signature c @@ flatmap f l
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

  let attribute side = match (side : Context.t) with
    | Side Client -> Some "eliom.client"
    | Side Server -> Some "eliom.server"
    | Shared -> Some "eliom.shared"
    | Base -> None

  let structure ~side str =
    let loc = str.pstr_loc in
    match attribute side with
    | Some txt -> Str.extension ~loc ({txt;loc}, PStr [str])
    | None -> str

  let signature ~side si =
    let loc = si.psig_loc in
    match attribute side with
    | Some txt -> Sig.extension ~loc ({txt; loc}, PSig [si])
    | None -> si

end

let _possible_annotations =
  ["shared.start"; "client.start" ;"server.start";
   "shared"; "client"; "server"]


let expression_mapper = object (self)
  inherit [ Eliom_base.side ] Ppx_core.Ast_traverse.map_with_context as super

  method! expression context expr =
    let loc = expr.pexp_loc in
    let attrs = expr.pexp_attributes in
    match expr with

    (* [%shared ... ] *)
    | {pexp_desc = Pexp_extension ({txt}, payload)}
      when is_annotation txt ["shared"] ->
      begin match context, payload with
        | Loc Server, PStr [{pstr_desc = Pstr_eval (frag_exp,attrs')}] ->
          let e = Shared.expression ~loc frag_exp in
          self#expression context @@ exp_add_attrs (attrs@attrs') e
        | Loc Server, _ ->
          exp_error ~loc
            "Wrong content for a shared fragment. It should be an expression."
        | Loc Client, _ ->
          exp_error ~loc
            "Shared fragments are only in a server context, \
             but it is used in a client context."
        | Poly, _ ->
          exp_error ~loc
            "Shared fragments are only in a server context, \
             but it is used in an ocaml context."
      end
    | _ -> super#expression context expr

end

let should_dup_str x = match x.pstr_desc with
  (* | Pstr_module _ *)
  (* | Pstr_recmodule _ *)
  (* | Pstr_modtype _ *)
  (*   -> false *)

  | _ -> false


let should_dup_sig x = match x.psig_desc with
  (* | Psig_module _ *)
  (* | Psig_recmodule _ *)
  (* | Psig_modtype _ *)
  (*   -> false *)

  | _ -> false

(** Toplevel dispatch mechanisms *)
let dispatch
    (* This three parameters are used to parametrized over str/sig. *)
    annotate (* add %server annotation *)
    selfrec (* self recursion on str/sig *)
    exprrec (* call to expression_mapper *)
    classify (* figure out if something is a module decl *)
    shared_duplication (* duplicate shared expressions *)

    (context : Context.t) item =
  if classify item
  then begin match context with
    | Shared | Side _ as side ->
      [ annotate ~side @@ selfrec side item ]
    | Base ->
      [ selfrec context item ]
  end
  else begin match context with
    | Shared ->
      let x : _ Shared.t = shared_duplication item in
      [ annotate ~side:(Side Client) @@
        selfrec (Side Client) x.client ;
        annotate ~side:(Side Server) @@
        selfrec (Side Server) x.server ;
      ]
    | Side side ->
      [ annotate ~side:context @@
        exprrec (Eliom_base.Loc side) item ]
    | Base ->
      [ exprrec Poly item ]
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
        should_dup_str
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
        should_dup_sig
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
  let c = Context.Base in
  {AM.default_mapper
   with
    structure = (fun _ -> mapper#structure c) ;
    signature = (fun _ -> mapper#signature c) ;
  }
