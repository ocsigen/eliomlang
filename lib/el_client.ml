open Typedtree
open Ppx_core.Std
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
  | Some _ -> Some (exp_error ~loc:e.exp_loc "Eliom ICE: This fragment annotation is malformed.")
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

(** Server sections *)

module H = Hashtbl.Make(struct include String let hash = Hashtbl.hash end)
let get_fragments str =
  let h = H.create 17 in
  let module Iter = TypedtreeIter.MakeIterator(struct
      include TypedtreeIter.DefaultIteratorArgument
      let enter_expression e = match get_client_fragment e with
        | None -> ()
        | Some frag ->
          let error () =
            Location.raise_errorf ~loc:e.exp_loc
              "Eliom ICE: This fragment annotation is malformed."
          in
          match e.exp_desc with
          | Texp_apply (_, [(_, Some {exp_desc=Texp_constant (Const_string (id,_))});
                            (_, Some args)]) ->
            let args = tuple_id_to_list args in
            H.add h id (args, frag)
          | _ -> error ()
    end)
  in
  Iter.iter_structure_item str ;
  H.fold (fun k v l -> (k,v)::l) h []


let escaped_values = object
  inherit Ppx_core.Ast_traverse.map
  method! expression e =
    let loc = e.pexp_loc in
    match e with
    | [%expr ~% [%e? inj]] ->
      [%expr Eliom_runtime.get_escaped_value [%e inj]][@metaloc loc]
    | _ -> e
end

let register_client_closure ~id args e =
  let loc = e.Parsetree.pexp_loc in
  let id = Ast_builder.Default.estring ~loc id in

  let f (lid : _ Location.loc) = Pat.var ~loc:lid.loc lid in
  let args = ptuple ~loc (List.map f args) in
  let e = escaped_values#expression e in
  [%expr
    Eliom_runtime.register_client_closure [%e id]
      (fun [%p args] -> [%e e])
  ][@metaloc loc]

let client_closures ~loc str =
  let frags = get_fragments str in
  let rec aux = function
    | [] -> []
    | (id, (args, frag)) :: l ->
      let e = register_client_closure ~id args frag in
      e :: aux l
  in
  match aux frags with
  | [] -> []
  | l -> [%str let () = [%e make_sequence ~loc l ]][@metaloc loc]

let server_section ~loc =
  let e_hash = AC.str @@ file_hash loc in
  [%stri
    let () = Eliom_runtime.close_server_section [%e e_hash]
  ][@metaloc loc]


(** Client sections *)

let client_section ~loc =
  let e_hash = AC.str @@ file_hash loc in
  [%stri
    let () = Eliom_runtime.open_client_section [%e e_hash]
  ][@metaloc loc]


(** The mapper *)

let structure_item _mapper stri =
  let loc = stri.str_loc in
  match get_client_section stri with
  | None ->
    client_closures ~loc stri @
    [ server_section ~loc ]
  | Some client_stri ->
    match client_stri, stri.str_desc with
    | PStr [client_stri],
      Tstr_value (_,[{vb_expr=_}]) ->
      [ client_section ~loc ;
        client_stri ;
      ]
    | _ -> [str_error ~loc "Eliom ICE"]

let structure mapper {str_items} =
  flatmap (structure_item mapper) str_items

let mapper =
  { U.default_mapper with
    structure ;
  }

let structure = mapper.structure mapper
