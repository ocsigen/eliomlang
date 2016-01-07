open Typedtree
module P = Parsetree
(* open Ast_helper *)

module AM = Ast_mapper
module AC = Ast_convenience

open El_utils

let get_client_section stri =
  match stri.str_desc with
  | Tstr_value (_, [ {vb_attributes} ]) ->
    get_attr eliom_section_attr vb_attributes
  | _ -> None

let get_client_fragment e =
  get_attr eliom_fragment_attr e.exp_attributes


let structure_item _mapper stri =
  match get_client_section stri with
  | None -> (* TODO *) ()
  | Some _stri -> ()
