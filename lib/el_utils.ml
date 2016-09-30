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

let get_empty_str_payload ~loc txt payload =
  let ppf : _ format6 =
    "The %%%%%s extension does not expect any payload."
  in match payload with
  | PStr [] | PSig [] -> []
  | _ -> [ str_error ~loc ppf txt ]

let get_empty_sig_payload ~loc txt payload =
  let ppf : _ format6 =
    "The %%%%%s extension does not expect any payload."
  in match payload with
  | PStr [] | PSig [] -> []
  | _ -> [ sig_error ~loc ppf txt ]

let get_str_payload ~loc txt payload =
  let ppf : _ format6 =
    "Wrong payload for the %%%%%s extension. It should be a structure."
  in match payload with
  | PStr x -> x
  | _ -> [ str_error ~loc ppf txt ]

let get_sig_payload ~loc txt payload =
  let ppf : _ format6 =
    "Wrong payload for the %%%%%s extension. It should be a signature."
  in match payload with
  | PSig x -> x
  | _ -> [ sig_error ~loc ppf txt ]



let is_annotation txt l =
  List.exists (Ppx_core.Name.matches ~pattern:("eliom."^txt)) l

(** Internal attributes *)
let eliom_section_attr = "eliom.section"
let eliom_fragment_attr = "eliom.client"

(** Context convenience module. *)
module Context = struct

  let of_string s =
    let f pattern s = Ppx_core.Name.matches ~pattern s in
    if f "eliom.server" s || f "eliom.server.start" s then `Server
    else if f "eliom.shared" s || f "eliom.shared.start" s then `Shared
    else if f "eliom.client" s || f "eliom.client.start" s then `Client
    else invalid_arg "Eliom ppx: Not a context"

  type t = Eliom_base.shside
end


let make_inj ~loc e =
  [%expr ~% [%e e]][@metaloc loc]
