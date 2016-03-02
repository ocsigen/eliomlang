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

let ptuple ~loc = function
  | [] ->  Ast_builder.Default.punit ~loc
  | [e] -> e
  | l -> Pat.tuple ~loc l

let make_sequence ~loc l =
  let f e l = Exp.sequence ~loc:e.pexp_loc e l in
  List.fold_right f l (Ast_builder.Default.eunit ~loc)

(* We use a strong hash (MD5) of the file name.
   We only keep the first 36 bit, which should be well enough: with
   256 files, the likelihood of a collision is about one in two
   millions.
   These bits are encoded using an OCaml-compatible variant of Base
   64, as the hash is used to generate OCaml identifiers. *)
let file_hash loc =
  let s = Digest.string loc.Location.loc_start.pos_fname in
  let e = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_'" in
  let o = Bytes.create 6 in
  let g p = Char.code s.[p] in
  for i = 0 to 5 do
    let p = i * 6 / 8 in
    let d = 10 - (i * 6) mod 8 in
    Bytes.set o i e.[(g p lsl 8 + g (p + 1)) lsr d land 63]
  done;
  Bytes.to_string o

let lexing_position ~loc l =
  Exp.tuple ~loc [
    AC.int @@ l.Lexing.pos_lnum;
    AC.int @@ l.Lexing.pos_bol;
    AC.int @@ l.Lexing.pos_cnum ;
  ]

let position loc =
  let start = loc.Location.loc_start in
  let stop = loc.Location.loc_end in
  (* Hopefully, start and end positions are in the same file. *)
  let file = AC.str start.Lexing.pos_fname in
  [%expr
    Eliom_runtime.pos [%e file]
      [%e lexing_position ~loc start]
      [%e lexing_position ~loc stop]
  ][@metaloc loc]

let rec get_attr s = function
  | ({Location.txt}, stri) :: _ when txt = s -> Some stri
  | _ :: t -> get_attr s t
  | [] -> None

let is_annotation txt l =
  List.exists (Ppx_core.Name.matches ~pattern:("eliom."^txt)) l

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
    type t = { i : int ; map : (int * string) M.t }
    let empty = { i = 0 ; map = M.empty }
    let seeded i = { i ; map = M.empty }

    let bindings {map} = M.bindings map

    let add make expr {i; map} =
      if M.mem expr map
      then snd @@ M.find expr map, {i ; map}
      else
        let hash = file_hash expr.pexp_loc in
        let s = make hash i in
        let v = (i,s) in
        let i = i + 1 in
        s, {i ; map = M.add expr v map }

    let is_empty {map} = M.is_empty map

    let value_bindings {map} =
      let f (e, (_,s)) =
        let loc = e.pexp_loc in
        Vb.mk ~loc (Pat.var ~loc @@ Location.mkloc s loc) e
      in
      List.map f @@ M.bindings map

    let tuple ~loc {map} =
      let l = M.bindings map in
      etuple ~loc @@ List.map (fun (_,(_,v)) -> Ast_builder.Default.evar ~loc v) l

    let kv ~loc {map} =
      let l = M.bindings map in
      let f (_,(i,v)) =
        Exp.tuple ~loc Ast_builder.Default.[eint ~loc i ; evar ~loc v]
      in
      Exp.array ~loc @@ List.map f l

    let union { i ; map } { map = m2 } = { i ; map = M.fold M.add map m2 }

  end

  let escaped_ident_fmt : _ format6 =
    "_eliom_escaped_%d"

  let fragment_ident_fmt : _ format6 =
    "_eliom_fragment_%s"

  let injected_ident_fmt : _ format6 =
    "_eliom_injection_%6s%d"

  let add_escaped =
    let make _ i = Printf.sprintf escaped_ident_fmt i in
    Map.add make

  let add_injection =
    let make hash i = Printf.sprintf injected_ident_fmt hash i in
    Map.add make

  let add_fragment =
    let make hash i = Printf.sprintf "%s%d" hash i in
    Map.add make

  let make_injection = Printf.sprintf fragment_ident_fmt

end

(** Context convenience module. *)
module Context = struct

  let of_string s =
    let f pattern s = Ppx_core.Name.matches ~pattern s in
    if f "eliom.server" s || f "eliom.server.start" s then `Server
    else if f "eliom.shared" s || f "eliom.shared.start" s then `Shared
    else if f "eliom.client" s || f "eliom.client.start" s then `Client
    else invalid_arg "Eliom ppx: Not a context"

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
(* let collect f = object *)
(*   inherit [_] Ppx_core.Ast_traverse.fold_map as super *)
(*   method! expression expr acc = match expr with *)
(*     | [%expr ~% [%e? inj ]] -> *)
(*       let (s, m) = f inj acc in *)
(*       let loc = expr.pexp_loc in *)
(*       let e = Exp.ident ~loc @@ Location.mkloc (Longident.Lident s) loc in *)
(*       make_inj ~loc e, m *)
(*     | _ -> *)
(*       super#expression expr acc *)
(* end *)

(* let collect_escaped e = *)
(*   (collect Name.add_escaped)#expression e Name.Map.empty *)

(* let collect_injection stri counter = *)
(*   let new_map = Name.Map.seeded counter in *)
(*   let stri, m = (collect Name.add_injection)#structure_item stri new_map in *)
(*   stri, m, m.i *)

let collect_obj = object
  inherit [_] Ppx_core.Ast_traverse.fold as super
  method! expression expr acc = match expr with
    | [%expr ~% [%e? inj ]] ->
      inj :: acc
    | _ ->
      super#expression expr acc
end

let collect_escaped e =
  collect_obj#expression e []

let collect_injection stri =
  List.map (fun e -> (0,e)) @@ collect_obj#structure_item stri []
