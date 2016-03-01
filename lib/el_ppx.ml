open Eliom_typing

let typing sourcefile str =
  let val_dont_write_files = !Clflags.dont_write_files in
  Clflags.dont_write_files := true;

  let ppf = Format.err_formatter in
  let outputprefix =
    Misc.chop_extension_if_any sourcefile
  in
  let modulename =
    Compenv.module_of_filename ppf sourcefile outputprefix
  in
  let env = Compmisc.initial_env() in
  let typedtree =
    Typemod.type_implementation sourcefile outputprefix modulename env str
  in

  Clflags.dont_write_files := val_dont_write_files;
  typedtree


let typing_mapper str =
  let sourcefile = !Location.input_name in
  let str = El_desugar.mapper#structure `Server str in
  let (tstr, _) = typing sourcefile str in
  tstr

let client_structure _mapper str =
  El_client.structure @@ typing_mapper str
let server_structure _mapper str =
  El_server.structure @@ typing_mapper str

let signature side _mapper = El_desugar.mapper#signature side

let client_mapper = {
  Ast_mapper.default_mapper with
  signature = signature `Client ;
  structure = client_structure ;
}
let server_mapper = {
  Ast_mapper.default_mapper with
  signature = signature `Server ;
  structure = server_structure ;
}

let mapper args =
  let is_client = List.mem "--emit-client" args in
  let is_server = List.mem "--emit-server" args in
  if is_client && is_server then
    Location.raise_errorf "Inconsistent ppx options: %s"
      (String.concat " " args)
  else if is_client then
    client_mapper
  else if is_server then
    server_mapper
  else
    Location.raise_errorf "Side not specified."
