
let pos pos_fname (lnum1, bol1, cnum1) (lnum2, bol2, cnum2) =
  Lexing.(
    { pos_fname ;
      pos_lnum = lnum1 ;
      pos_bol = bol1 ;
      pos_cnum = cnum1 ;
    },
    { pos_fname;
      pos_lnum = lnum2 ;
      pos_bol = bol2 ;
      pos_cnum = cnum2 ;
    })

let string_escape s =
  let l = String.length s in
  let b = Buffer.create (16 * 1024) in
  let conv = "0123456789abcdef" in
  for i = 0 to l - 1 do
    let c = s.[i] in
    match c with
      '\000' when i = l - 1 || s.[i + 1] < '0' || s.[i + 1] > '9' ->
        Buffer.add_string b "\\0"
    | '\b' ->
        Buffer.add_string b "\\b"
    | '\t' ->
        Buffer.add_string b "\\t"
    | '\n' ->
        Buffer.add_string b "\\n"
    (*| '\011' -> (* IE<9 doesn't like vertical tab \v *)
        Buffer.add_string b "\\v"*)
    | '\012' ->
        Buffer.add_string b "\\f"
    | '\r' ->
        Buffer.add_string b "\\r"
    | '\'' ->
        Buffer.add_string b "\\'"
    | '\\' ->
        Buffer.add_string b "\\\\"
    | '\000' .. '\031' | '\127' .. '\255' | '&' | '<' | '>' ->
        let c = Char.code c in
        Buffer.add_string b "\\x";
        Buffer.add_char b conv.[c lsr 4];
        Buffer.add_char b conv.[c land 0xf]
    | _ ->
        Buffer.add_char b c
  done;
  Buffer.contents b

module Poly = struct

  type t
  let make x : t = Obj.magic x

  let marshall (poly : t) =
    string_escape (Marshal.to_string (Eliom_wrap.wrap poly) [])
end

module Client_value = struct

  module Repr = struct

    type +'a t = {
      closure_id: int64;
      instance_id: int64;
    }

    let create ~closure_id ~instance_id =
      { closure_id; instance_id }
    let closure_id { closure_id } = closure_id
    let instance_id { instance_id } = instance_id
  end

  type +'a t = 'a Repr.t * Eliom_wrap.unwrapper

  let unwrap_id = 7
  let unwrapper = Eliom_wrap.(create_unwrapper @@ id_of_int unwrap_id)

  let create cv = (cv, client_value_unwrapper)
  let create' ~closure_id ~instance_id =
    create @@ Repr.create ~closure_id ~instance_id

  exception Creation_invalid_context of int64

end


type pos = Lexing.position * Lexing.position

type client_value_datum = {
  closure_id : int64;
  instance_id : int64;
  loc: pos option;
  args : Poly.t;
  value : Poly.t;
}

type 'injection_value injection_datum = {
  injection_id : string;
  injection_value : 'injection_value;
  injection_loc : pos option;
  injection_ident : string option;
}

type 'injection_value compilation_unit_global_data = {
  server_sections_data : (client_value_datum list) Queue.t;
  client_sections_data : ('injection_value injection_datum list) Queue.t;
}

(* type 'injection_value global_data = *)
(*     'injection_value compilation_unit_global_data String_map.t *)

type request_data = client_value_datum list


let fresh_ix () =
  Int64.of_int (Oo.id (object end))

let is_global = ref false
let set_global b = is_global := b

let register_client_value_data ?loc ~closure_id ~instance_id ~args ~value () =
  let client_value_datum = { closure_id; instance_id; args; loc; value } in
  if !is_global then
    if Eliom_common.get_sp_option () = None then
      current_server_section_data :=
        client_value_datum :: !current_server_section_data
    else
      raise (Client_value.Creation_invalid_context closure_id)
  else
    Eliom_reference.Volatile.modify request_data
      (fun sofar -> client_value_datum :: sofar)

let client_value ?pos closure_id args =
  let instance_id = fresh_ix () in
  let v =
    Client_value.create' ~closure_id ~instance_id
  in
  register_client_value_data
    ?loc:pos ~closure_id ~instance_id
    ~args:(Poly.make args) ~value:(Poly.make v) ();
  v
