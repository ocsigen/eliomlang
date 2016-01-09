open Parsetree
open Ast_helper
open Ppx_core.Std

module AM = Ast_mapper
module AC = Ast_convenience

(** Various misc functions *)

let flatmap f l = List.flatten @@ List.map f l

let rec fold_accum f l acc = match l with
  | [] -> []
  | h :: t ->
    let acc, newl = f acc h in
    newl @ fold_accum f t acc

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

let error f ?sub ?loc =
  Format.ksprintf (fun s -> f ?loc ?attrs:None @@ AM.extension_of_error @@ Location.error ?loc ?sub s)

let exp_error ?sub ~loc = error Exp.extension ?sub ~loc
let str_error ?sub ~loc = error Str.extension ?sub ~loc
let sig_error ?sub ~loc = error Sig.extension ?sub ~loc

let etuple ~loc = function
  | [] ->  Ast_builder.Default.eunit ~loc
  | [e] -> e
  | l -> Exp.tuple ~loc l

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

let rec get_attr s = function
  | ({Location.txt}, stri) :: _ when txt = s -> Some stri
  | _ :: t -> get_attr s t
  | [] -> None

let is_annotation txt l =
  List.exists (fun s -> txt = s || txt = "eliom."^s) l

(** Internal attributes *)
let eliom_section_attr = "eliom.section"
let eliom_fragment_attr = "eliom.fragment"

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

    let is_empty {map} = M.is_empty map

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

  type shared = [
    | `Shared
    | t
  ]
end

let open_eliom_pervasives = [%stri open Eliom_pervasives ]

let make_inj ~loc e =
  [%expr ~% [%e e]][@metaloc loc]

(** Collect all the injection expressions and substitute them by fresh
    variables. Returns a map from variable to injections.

    Can be applied both to client section and fragments.
*)
let collect_injections = object
  inherit [_] Ppx_core.Ast_traverse.fold_map as super
  method! expression expr acc = match expr with
    | [%expr ~% [%e? inj ]] ->
      let (s, m) = Name.add_injection inj acc in
      let loc = expr.pexp_loc in
      let e = Exp.ident ~loc @@ Location.mkloc (Longident.Lident s) loc in
      make_inj ~loc e, m
    | _ ->
      super#expression expr acc
end

let value_binding_of_map m =
  let f (e, s) =
    let loc = e.pexp_loc in
    Vb.mk ~loc (Pat.var ~loc @@ Location.mkloc s loc) e
  in
  List.map f @@ Name.Map.bindings m
