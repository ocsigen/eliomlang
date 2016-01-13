open Typedtree
open Ast_helper

module U = Untypeast
module AM = Ast_mapper
module AC = Ast_convenience

open El_utils

let get_client_section stri =
  match stri.str_desc with
  | Tstr_value (_, [ {vb_attributes} ]) ->
    get_attr eliom_section_attr vb_attributes
  | _ -> None

let get_client_fragment e =
  match get_attr eliom_fragment_attr e.exp_attributes with
  | Some PStr [{pstr_desc = Pstr_eval (e,_)}] -> Some e
  | Some _ -> Some (exp_error ~loc:e.exp_loc "Eliom ICE")
  | _ -> None

let get_id e = match e.exp_desc with
  | Texp_ident (_, {Location.txt = Lident txt; loc}, _) -> {Location.txt; loc}
  | _ -> Location.raise_errorf ~loc:e.exp_loc "Eliom ICE: An identifier was expected."

let tuple_id_to_list e = match e.exp_desc with
  | Texp_construct ({txt=Lident "()"},_,[]) -> []
  | Texp_tuple l -> List.map get_id l
  | Texp_ident (_, {txt = Lident txt; loc}, _) -> [{Location.txt; loc}]
  | _ -> Location.raise_errorf ~loc:e.exp_loc
      "Eliom ICE: A tuple of identifiers was expected."


let server_section ~loc =
  let e_hash = AC.str @@ string_of_int @@ file_hash loc in
  [%stri
    let () = Eliom_runtime.close_server_section [%e e_hash]
  ][@metaloc loc]

let fragment ~loc id arg =
  [%expr
    Eliom_runtime.fragment
      ~pos:[%e position loc ]
      [%e id]
      [%e arg]
  ][@metaloc loc]

let client_section ~loc arg =
  let f {Location. txt;loc} e =
    let id = Exp.ident ~loc {txt = Lident txt; loc} in
    [%expr Eliom_runtime.Poly.make [%e id] :: [%e e]][@metaloc loc]
  in
  let l = List.fold_right f (tuple_id_to_list arg) [%expr []][@metaloc loc] in
  let e_hash = AC.str @@ string_of_int @@ file_hash loc in
  [%stri
    let () = Eliom_runtime.close_client_section [%e e_hash] [%e l]
  ][@metaloc loc]


let expr mapper e =
  match get_client_fragment e with
  | None -> U.default_mapper.expr mapper e
  | Some _ -> begin
      let loc = e.exp_loc in
      match e.exp_desc with
      | Texp_apply (_, [Nolabel, Some id ; Nolabel, Some arg]) ->
        let arg = U.default_mapper.expr U.default_mapper arg in
        let id = U.default_mapper.expr U.default_mapper id in
        fragment ~loc id arg
      | _ -> exp_error ~loc "Eliom ICE"
    end

let structure_item mapper stri =
  let loc = stri.str_loc in
  match get_client_section stri with
  | None -> [
      U.default_mapper.structure_item mapper stri  ;
      server_section ~loc
    ]
  | Some _stri ->
    match stri.str_desc with
    | Tstr_value (_,[{vb_expr={exp_desc=Texp_apply (_, [(_,Some arg)])}}]) ->
      [ client_section ~loc arg ]
    | _ -> [str_error ~loc "Eliom ICE"]

let structure mapper {str_items} =
  flatmap (structure_item mapper) str_items

let mapper =
  { U.default_mapper with
    structure ;
    expr
  }


let structure = mapper.structure mapper
