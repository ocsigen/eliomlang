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
      | `Client -> "eliom.client"
      | `Server -> "eliom.server"
      | `Shared -> "eliom.shared"
    in
    {Location. txt ; loc}

  let structure ~side ~loc str =
    let s = attribute ~side ~loc in
    Str.extension ~loc (s, PStr [str])

  let signature ~side ~loc si =
    let s = attribute ~side ~loc in
    Sig.extension ~loc (s, PSig [si])

end

let possible_annotations =
  ["shared.start"; "client.start" ;"server.start";
   "shared"; "client"; "server"]


let expression_mapper = object (self)
  inherit [ [ `Client | `Server | `None ] ] Ppx_core.Ast_traverse.map_with_context as super

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
        | `None, _ ->
          exp_error ~loc
            "Shared fragments are only in a server context, \
             but it is used in an ocaml context."
      end
    | _ -> super#expression context expr

end

let mapper = object (self)
  inherit [ Context.t ] Ppx_core.Ast_traverse.map_with_context as super

  (** Toplevel translation *)
  method private dispatch_str context stri =
    let loc = stri.pstr_loc in
    match stri.pstr_desc, context with
    | Pstr_module _ , (`Shared | `Client | `Server as side) ->
      [ Section.structure ~side ~loc @@ self#structure_item side stri ]
    | _, `Shared ->
      let x = Shared.structure_item stri in [
        Section.structure ~side:`Client ~loc @@ self#structure_item `Client x.client ;
        Section.structure ~side:`Server ~loc @@ self#structure_item `Server x.server
      ]
    | _, (`Client | `Server as side) ->
      [ Section.structure ~side ~loc @@ expression_mapper#structure_item side stri ]
    | _, `None ->
      [ expression_mapper#structure_item `None stri ]

  method! structure context structs =
    let f c pstr =
      let loc = pstr.pstr_loc in
      match pstr.pstr_desc with
      | Pstr_extension (({txt}, payload), _)
        when is_annotation txt ["shared.start"; "client.start" ;"server.start"] ->
        (Context.of_string txt, get_empty_payload ~loc txt payload)
      | Pstr_extension (({txt}, payload), _)
        when is_annotation txt ["shared" ; "client" ; "server"] ->
        let str =
          flatmap (self#dispatch_str (Context.of_string txt)) @@
          get_str_payload ~loc txt payload
        in
        (c, str)
      | _ ->
        (c, self#dispatch_str c pstr)
    in
    fold_accum f structs context

  method! signature context sigs =
    let f c psig =
      let loc = psig.psig_loc in
      match psig.psig_desc with
      | Psig_extension (({txt}, payload), _)
        when is_annotation txt ["shared.start"; "client.start"; "server.start"] ->
        (Context.of_string txt, get_empty_sig_payload ~loc txt payload)
      | Psig_extension (({txt}, payload), _)
        when is_annotation txt ["shared"] ->
        let psig = get_sig_payload ~loc txt payload in
        let f x =
          let x = Shared.signature_item x in [
            Section.signature ~loc ~side:`Client x.client ;
            Section.signature ~loc ~side:`Server x.server ;
          ]
        in
        (c, flatmap f psig)
      | Psig_extension (({txt}, payload), _)
        when is_annotation txt ["client"; "server"] ->
        let side = Context.of_string txt in
        let new_sig =
          List.map (Section.signature ~side ~loc) @@ get_sig_payload ~loc txt payload
        in
        (c, new_sig)
      | _ ->
        (c, [super#signature_item c psig])
    in
    fold_accum f sigs context

end



let mapper' _args =
  let c = `None in
  {AM.default_mapper
   with
    structure = (fun _ -> mapper#structure c) ;
    signature = (fun _ -> mapper#signature c) ;
  }
