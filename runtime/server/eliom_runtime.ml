
type pos = Lexing.position * Lexing.position

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



type 'a fragment = 'a Eliom_fragment.t

type client_value_datum = {
  closure_id : int64;
  instance_id : int64;
  loc: pos option;
  args : Poly.t;
  value : Poly.t;
}

(* type 'injection_value injection_datum = { *)
(*   injection_id : string; *)
(*   injection_value : 'injection_value; *)
(*   injection_loc : pos option; *)
(*   injection_ident : string option; *)
(* } *)

(* type request_data = client_value_datum list *)


module Global_data = struct

  module Tbl = Hashtbl.Make(struct
      include String
      let hash = Hashtbl.hash
    end)

  type data = {
    server : (client_value_datum list) Queue.t;
    client : (Poly.t list) Queue.t;
  }

  let tbl = Tbl.create 17

  let get id =
    if Tbl.mem tbl id then
      Tbl.find tbl id
    else
      let data = {
        server = Queue.create ();
        client = Queue.create ()
      } in
      Tbl.add tbl id data ;
      data

end

let current_server_section_data = ref []

let close_server_section compilation_unit_id =
  let server_queue =
    (Global_data.get compilation_unit_id).server
  in
  Queue.push (List.rev !current_server_section_data) server_queue;
  current_server_section_data := []

let close_client_section compilation_unit_id injection_data =
  let client_queue =
    (Global_data.get compilation_unit_id).client
  in
  Queue.push injection_data client_queue
  (* let injection_datum (injection_id, injection_value, loc, ident) = *)
  (*   { injection_id; injection_value ; injection_loc = Some loc; injection_ident = ident } *)
  (* in *)
  (* Queue.push (List.map injection_datum injection_data) *)
  (*   client_sections_data *)


let fresh_ix () =
  Int64.of_int (Oo.id (object end))

let is_global = ref false
let set_global b = is_global := b

(* let register_client_value_data ?loc ~closure_id ~instance_id ~args ~value () = *)
(*   let client_value_datum = { closure_id; instance_id; args; loc; value } in *)
(*   if !is_global then *)
(*     if Eliom_common.get_sp_option () = None then *)
(*       current_server_section_data := *)
(*         client_value_datum :: !current_server_section_data *)
(*     else *)
(*       raise (Client_value.Creation_invalid_context closure_id) *)
(*   else *)
(*     Eliom_reference.Volatile.modify request_data *)
(*       (fun sofar -> client_value_datum :: sofar) *)

let fragment ~pos:_ closure_id args =
  let instance_id = fresh_ix () in
  let v =
    Eliom_fragment.create ~closure_id ~instance_id
  in
  ignore (v,args)
  (* register_client_value_data *)
  (*   ?loc:pos ~closure_id ~instance_id *)
  (*   ~args:(Poly.make args) ~value:(Poly.make v) (); *)
  (* v *)
