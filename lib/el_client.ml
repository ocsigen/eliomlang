open Typedtree
open Ast_helper

module U = Untypeast
module AM = Ast_mapper
module AC = Ast_convenience

open El_utils
open El_untype

(** Server sections *)

let collect_escaped e =
  let tbl = Hashtbl.create 8 in
  let f {id = txt; attrs} =
    let loc = e.exp_loc in
    let lid = Location.mkloc (Longident.Lident txt) loc in
    Hashtbl.replace tbl txt {Location.txt;loc} ;
    Exp.ident ~loc ~attrs lid
  in
  let x = CollectMap.escaped f e in
  let l = Hashtbl.fold (fun _k v l -> v::l) tbl [] in
  x, l

let register_client_closure {id; expr} =
  let e, args = collect_escaped expr in
  let loc = e.Parsetree.pexp_loc in
  let id = Exp.constant ~loc @@ Const.string id in
  let f lid = Pat.var ~loc lid in
  let args = ptuple ~loc (List.map f args) in
  [%expr
    Eliom_runtime.register_client_closure [%e id]
      (fun [%p args] -> [%e e])
  ][@metaloc loc]

let client_closures ~loc str =
  let frags = Collect.fragments str in
  let l = List.map register_client_closure frags in
  match l with
  | [] -> []
  | l -> [%str let () = [%e make_sequence ~loc l ]][@metaloc loc]

let server_section ~loc =
  let e_hash = AC.str @@ file_hash loc in
  [%stri
    let () = Eliom_runtime.close_server_section [%e e_hash]
  ][@metaloc loc]


(** Client sections *)

let make_injection { id ; expr } =
  let loc = expr.exp_loc in
  let id = El_untype.Name.make_injection_id ~loc id in
  [%expr Eliom_runtime.get_injection [%e id]][@metaloc loc]

let open_client_section ~loc =
  let e_hash = AC.str @@ file_hash loc in
  [%stri
    let () = Eliom_runtime.open_client_section [%e e_hash]
  ][@metaloc loc]

let client_section ~loc stri = [
  open_client_section ~loc ;
  CollectMap.injections make_injection stri ;
]


(** The mapper *)

let structure_item _mapper stri =
  let loc = stri.str_loc in
  let str_desc, side = get_section_side stri.str_desc in
  let stri = {stri with str_desc} in
  match side with
  | None | Some `Shared ->
    [str_error ~loc "Eliom ICE: Unspecified section."]
  | Some `Server ->
    client_closures ~loc stri @ [ server_section ~loc ]
  | Some `Client ->
    client_section ~loc stri

let structure mapper {str_items} =
  flatmap (structure_item mapper) str_items

let mapper =
  { U.default_mapper with
    structure ;
  }

let structure = mapper.structure mapper
