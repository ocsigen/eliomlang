open Typedtree
open Ast_helper

module U = Untypeast
module AM = Ast_mapper
module AC = Ast_convenience

open El_utils
open El_untype

let _get_client_fragment e =
  match get_attr eliom_fragment_attr e.exp_attributes with
  | Some PStr [{pstr_desc = Pstr_eval (e,_)}] -> Some e
  | Some _ -> Some (exp_error ~loc:e.exp_loc "Eliom ICE")
  | _ -> None

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

let client_section ~loc injs =
  let f e (id, inj) =
    let loc = inj.Parsetree.pexp_loc in
    let id = Exp.constant ~loc @@ Const.string id in
    let pos = position loc in
    [%expr
      ([%e id], Eliom_runtime.Poly.make [%e inj], [%e pos]) :: [%e e]
    ][@metaloc loc]
  in
  let l = List.fold_left f ([%expr []][@metaloc loc]) injs in
  let e_hash = AC.str @@ file_hash loc in
  [%stri
    let () = Eliom_runtime.close_client_section [%e e_hash] [%e l]
  ][@metaloc loc]


let expr mapper e =
  let loc = e.exp_loc in
  let aux = function
    | Expr e  -> U.default_mapper.expr mapper e
    | Injection _ -> exp_error ~loc "Eliom ICE: Unexpected injection"
    | Fragment {id ; expr} -> begin
        let injs =
          List.map
            (fun (_,_,e) -> U.default_mapper.expr U.default_mapper e)
            (Collect.escaped expr)
        in
        fragment ~loc (Exp.constant ~loc @@ Const.string id) (etuple ~loc injs)
      end
  in
  aux @@ unfold_expression e

let structure_item mapper stri =
  let loc = stri.str_loc in
  let str_desc, side = get_section_side stri.str_desc in
  let stri = {stri with str_desc} in
  match side with
  | None | Some `Shared ->
    [str_error ~loc "Eliom ICE: Unspecified section."]
  | Some `Server -> [
      U.default_mapper.structure_item mapper stri ;
      server_section ~loc
    ]
  | Some `Client ->
    let injs = List.map
        (fun (id,attrs,e) -> id, exp_add_attrs attrs @@ U.default_mapper.expr mapper e)
        (Collect.injections stri)
    in
    [ client_section ~loc injs ]

let structure mapper {str_items} =
  flatmap (structure_item mapper) str_items

let mapper =
  { U.default_mapper with
    structure ;
    expr
  }


let structure = mapper.structure mapper
