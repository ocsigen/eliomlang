open Parsetree
open Ast_helper

module AM = Ast_mapper
module AC = Ast_convenience

(** Various misc functions *)

let flatmap f l = List.flatten @@ List.map f l

let get_extension = function
  | {pexp_desc= Pexp_extension ({txt},_)} -> txt
  | _ -> invalid_arg "Eliom ppx: Should be an extension."

let (%) f g x = f (g x)

let exp_add_attrs attr e =
  {e with pexp_attributes = attr}

let id_of_string str =
  Printf.sprintf "%019d" (Hashtbl.hash str)

let file_loc () =
  Location.in_file !Location.input_name

let eid {Location. txt ; loc } =
  Exp.ident ~loc { loc ; txt = Longident.Lident txt }

let format_args = function
  | [] -> AC.unit ()
  | [e] -> e
  | l -> Exp.tuple l

let pat_args = function
  | [] -> AC.punit ()
  | [p] -> p
  | l -> Pat.tuple l

let file_hash loc =
  Hashtbl.hash @@ loc.Location.loc_start.pos_fname

let lexing_position ~loc l =
  [%expr
    { Lexing.pos_fname = [%e AC.str l.Lexing.pos_fname];
      Lexing.pos_lnum = [%e AC.int @@ l.Lexing.pos_lnum];
      Lexing.pos_bol = [%e AC.int @@ l.Lexing.pos_bol];
      Lexing.pos_cnum = [%e AC.int @@ l.Lexing.pos_cnum]; }
  ] [@metaloc loc]

let position loc =
  let start = loc.Location.loc_start in
  let stop = loc.Location.loc_start in
  Exp.tuple ~loc [ lexing_position ~loc start ; lexing_position ~loc stop ]

let is_annotation txt l =
  List.exists (fun s -> txt = s || txt = "eliom."^s) l

(** Identifiers generation. *)
module Name = struct

  module M = Map.Make(struct
      type t = expression
      let compare x y = match x.pexp_desc ,y.pexp_desc with
        | Pexp_ident {txt = s1}, Pexp_ident {txt = s2} -> compare s1 s2
        | _ -> compare x y
    end )

  module Map = struct
    type t = { i : int64 ; map : string M.t }
    let empty = { i = 0L ; map = M.empty }

    let add make expr {i; map} =
      if M.mem expr map
      then M.find expr map, {i ; map}
      else
        let hash = file_hash expr.pexp_loc in
        let s = make hash i in
        let i = Int64.(add one) i in
        s, {i ; map = M.add expr s map }

    let bindings {map} = M.bindings map

  end

  let escaped_ident_fmt : _ format6 =
    "_eliom_escaped_ident_%Ld"

  let fragment_ident_fmt : _ format6 =
    "_eliom_fragment_%Ld"

  let injected_ident_fmt : _ format6 =
    "_eliom_injected_ident_%019d_%Ld"

  let add_escaped_value =
    let make _ i = Printf.sprintf escaped_ident_fmt i in
    Map.add make

  let add_injection =
    let make hash i = Printf.sprintf injected_ident_fmt hash i in
    Map.add make

  let add_fragment =
    let make _ i = Printf.sprintf fragment_ident_fmt i in
    Map.add make

end

(** Context convenience module. *)
module Context = struct

  let to_string = function
    | `Client -> "client"
    | `Shared -> "shared"
    | `Server -> "server"

  let of_string = function
    | "server" | "server.start" -> `Server
    | "shared" | "shared.start" -> `Shared
    | "client" | "client.start" -> `Client
    | _ -> invalid_arg "Eliom ppx: Not a context"

  type escape_inject = [
    | `Escaped_value
    | `Injection
  ]

  type t = [
    | `Server (* [%%server ... ] *)
    | `Client (* [%%client ... ] *)
    | `Fragment (* [%client ... ] *)
    | `Escaped_value (* [%shared ~%( ... ) ] *)
    | `Injection (* [%%client ~%( ... ) ] *)
  ]
end

let open_eliom_pervasives = [%stri open Eliom_pervasives ]

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

  let expression loc expr =
    let server_expr = server#expression expr in
    let client_expr = client#expression `Top expr in
    [%expr
      Eliom_lib.create_shared_value
        [%e server_expr]
        [%client [%e client_expr]]
    ] [@metaloc loc]

  let structure_item stri =
    let server_stri = server#structure_item stri in
    let client_stri = client#structure_item `Top stri in
    [ client_stri ; server_stri ]

  let signature_item sigi =
    let server_sigi = server#signature_item sigi in
    let client_sigi = client#signature_item `Top sigi in
    [ client_sigi ; server_sigi ]

end


let collect_injections = object
  inherit [_] Ppx_core.Ast_traverse.fold_map as super
  method! expression expr acc = match expr with
    | [%expr ~% [%e? inj ]] ->
      let (s, m) = Name.add_injection inj acc in
      let loc = expr.pexp_loc in
      let e = Exp.ident ~loc @@ Location.mkloc (Longident.Lident s) loc in
      e, m
    | _ ->
      super#expression expr acc
end

let prelim = object
  inherit [Context.t] Ppx_core.Ast_traverse.map_with_context as super

  method! expression context expr =
    let loc = expr.pexp_loc in
    let attrs = expr.pexp_attributes in
    match expr, context with
    | {pexp_desc = Pexp_extension ({txt},_)},
      `Client
      when is_annotation txt ["client"; "shared"] ->
      let side = get_extension expr in
      Exp.extension @@ AM.extension_of_error @@ Location.errorf ~loc
        "The syntax [%%%s ...] is not allowed inside client code."
        side
    | {pexp_desc = Pexp_extension ({txt},_)}
    , (`Fragment | `Escaped_value | `Injection)
      when is_annotation txt ["client"; "shared"] ->
      let side = get_extension expr in
      Exp.extension @@ AM.extension_of_error @@ Location.errorf ~loc
        "The syntax [%%%s ...] can not be nested."
        side

    (* [%shared ... ] *)
    | {pexp_desc = Pexp_extension ({txt},PStr [{pstr_desc = Pstr_eval (frag_exp,attrs')}])},
      `Server
      when is_annotation txt ["shared"] ->
      let e = Shared.expression loc frag_exp in
      super#expression context @@ exp_add_attrs (attrs@attrs') e

    (* [%client e ] with e = ... ~%x ...

       let escp1 = x in
       ((fun _ -> assert false) escp1)[@eliom.fragment a]
    *)
    | {pexp_desc = Pexp_extension ({txt},PStr [{pstr_desc = Pstr_eval (frag_exp,attrs)}])},
      `Server
      when is_annotation txt ["client"] ->
      let frag_exp, m = collect_injections#expression frag_exp Name.Map.empty in
      let map = Name.Map.bindings m in
      let poly_exp = [%expr assert false] in (* this expression is of type 'a . 'a *)
      let e =
        let f (e, s) =
          let loc = e.pexp_loc in Vb.mk ~loc (Pat.var ~loc @@ Location.mkloc s loc) e
        in
        Exp.let_ ~loc Nonrecursive (List.map f map) poly_exp
      in
      let eliom_attr = Location.mkloc "eliom.fragment" loc, PStr [Str.eval frag_exp] in
      exp_add_attrs (eliom_attr :: attrs) e

    (* ~%( ... ) ] *)
    | [%expr ~% [%e? inj ]], _ ->
      begin match context with
        | `Client ->
          let context = `Injection in
          super#expression context inj
        | `Fragment ->
          let context = `Escaped_value in
          super#expression context inj
        | `Server ->
          Location.raise_errorf ~loc
            "The syntax ~%% ... is not allowed inside server code."
        | `Escaped_value | `Injection ->
          Location.raise_errorf ~loc
            "The syntax ~%% ... can not be nested."
      end
    | _ -> super#expression context expr

end


module Make (Pass : Pass) = struct

  let structure_item mapper str =
    let loc = str.pstr_loc in
    match str.pstr_desc with
    | Pstr_extension (({txt=("server"|"shared"|"client")}, _), _) ->
      Location.raise_errorf ~loc
        "Sections are only allowed at toplevel."
    | _ -> AM.default_mapper.structure_item mapper str

  let signature_item mapper sig_ =
    let loc = sig_.psig_loc in
    match sig_.psig_desc with
    | Psig_extension (({txt=("server"|"shared"|"client")}, _), _) ->
      Location.raise_errorf ~loc
        "Sections are only allowed at toplevel."
    | _ -> AM.default_mapper.signature_item mapper sig_

  let eliom_mapper context =
    let context = ref (context :> Context.t) in
    { Ast_mapper.default_mapper
      with
        Ast_mapper.

        expr = eliom_expr context ;

        (* Reject sections not at toplevel. *)
        structure_item ;
        signature_item ;
    }


  (** Toplevel translation *)
  (** Switch the current context when encountering [%%server] (resp. shared, client)
      annotations. Call the eliom mapper and [Pass.server_str] (resp ..) on each
      structure item.
  *)

  let dispatch (server, shared, client) field context str =
    let f = match context with
      | `Server -> server | `Shared -> shared | `Client -> client
    in
    let m = eliom_mapper context in
    f @@ (field m) m str

  let dispatch_str c _mapper =
    dispatch Pass.(server_str, shared_str, client_str)
      (fun x -> x.AM.structure_item) c

  let dispatch_sig c _mapper =
    dispatch Pass.(server_sig, shared_sig, client_sig)
      (fun x -> x.AM.signature_item) c

  let toplevel_structure context mapper structs =
    let f pstr =
      let loc = pstr.pstr_loc in
      match pstr.pstr_desc with
      | Pstr_extension (({txt}, PStr strs), _)
        when is_annotation txt ["shared.start"; "client.start" ;"server.start"] ->
        if strs <> [] then
          [ Str.extension ~loc @@ AM.extension_of_error @@ Location.errorf ~loc
              "The %%%%%s extension doesn't accept arguments." txt ]
        else ( context := Context.of_string txt ; [] )
      | Pstr_extension (({txt}, PStr strs), _)
        when is_annotation txt ["shared"; "client" ;"server"] ->
        let c = Context.of_string txt in
        flatmap (dispatch_str c mapper) strs
      | _ ->
        dispatch_str !context mapper pstr
    in
    let loc = file_loc () in
    open_eliom_pervasives :: Pass.prelude loc @ flatmap f structs @ Pass.postlude loc

  let toplevel_signature context mapper sigs =
    let f psig =
      let loc = psig.psig_loc in
      match psig.psig_desc with
      | Psig_extension (({txt=("shared"|"client"|"server" as txt)}, PStr strs), _) ->
        if strs <> [] then
          [ Sig.extension ~loc @@ AM.extension_of_error @@ Location.errorf ~loc
              "The %%%%%s extension doesn't accept arguments." txt ]
        else ( context := Context.of_string txt ; [] )
      | _ ->
        dispatch_sig !context mapper psig
    in
    flatmap f sigs

  let mapper args =
    let () = match_args args in
    let c = ref `Server in
    {AM.default_mapper
     with
      structure = toplevel_structure c ;
      signature = toplevel_signature c ;
    }

end
