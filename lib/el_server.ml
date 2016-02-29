open Eliom_typing
open Typedtree
open Ast_helper
open Ppx_core.Std

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
  | Texp_tuple [
      {exp_desc = Texp_constant (Const_int i)} ;
      {exp_desc = Texp_ident (_, ident, _)}
    ] -> (i, ident)
  | _ -> Location.raise_errorf ~loc:e.exp_loc "Eliom ICE: An identifier was expected."

let tuple_id_to_list e = match e.exp_desc with
  | Texp_array l -> List.map get_id l
  | _ -> Location.raise_errorf ~loc:e.exp_loc
      "Eliom ICE: An array of identifiers was expected."


let server_section ~loc =
  let e_hash = AC.str @@ file_hash loc in
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
  let f e (i, (var_name : _ Location.loc)) =
    let loc = var_name.loc in
    let id = Ast_builder.Default.eint ~loc i in
    let ident = Exp.ident ~loc var_name in
    let pos = position loc in
    [%expr
      ([%e id], Eliom_runtime.Poly.make [%e ident], [%e pos]) :: [%e e]
    ][@metaloc loc]
  in
  let l = List.fold_left f ([%expr []][@metaloc loc]) (tuple_id_to_list arg) in
  let e_hash = AC.str @@ file_hash loc in
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
    | Tstr_value (_,[{vb_expr}]) ->
      [ client_section ~loc vb_expr ]
    | _ -> [str_error ~loc "Eliom ICE: Malformed client section."]

let structure mapper {str_items} =
  flatmap (structure_item mapper) str_items

let mapper =
  { U.default_mapper with
    structure ;
    expr
  }


let structure = mapper.structure mapper
