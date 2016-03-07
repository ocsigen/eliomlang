open Typedtree
open Ast_helper

module U = Untypeast
module AM = Ast_mapper
module AC = Ast_convenience

open El_utils
open El_untype

(** Server sections *)

let get_fragments str =
  let frags = Hashtbl.create 17 in
  let module Iter = TypedtreeIter.MakeIterator(struct
      include TypedtreeIter.DefaultIteratorArgument
      let enter_expression e =
        match unfold_expression e with
          | Expr _ -> ()
          | Injection _ -> ()
          | Fragment {id;expr} -> begin
              Hashtbl.add frags id expr
            end
    end)
  in
  Iter.iter_structure_item str ;
  Hashtbl.fold (fun k v l -> (k,v)::l) frags []

let make_escaped ~attrs (lid : _ Location.loc) =
  let loc = lid.loc in
  let e = Exp.ident ~loc ~attrs lid in
  [%expr Eliom_runtime.get_escaped_value [%e e]][@metaloc loc]

let collect_escaped e =
  let l = ref [] in
  let f txt e attrs =
    let loc = e.exp_loc in
    let lid = Location.mkloc (Longident.Lident txt) loc in
    l := {Location.txt;loc} :: !l ;
    make_escaped ~attrs lid
  in
  CollectMap.escaped f e, !l

let register_client_closure (id, e) =
  let e, args = collect_escaped e in
  let loc = e.Parsetree.pexp_loc in
  let id = Exp.constant ~loc @@ Const.string id in
  let f lid = Pat.var ~loc lid in
  let args = ptuple ~loc (List.map f args) in
  [%expr
    Eliom_runtime.register_client_closure [%e id]
      (fun [%p args] -> [%e e])
  ][@metaloc loc]

let client_closures ~loc str =
  let frags = get_fragments str in
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

let make_injection id e _attrs =
  let loc = e.exp_loc in
  let id = Exp.constant ~loc @@ Const.string id in
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
