

let typing sourcefile =
  let ppf = Format.err_formatter in
  let outputprefix =
    Misc.chop_extension_if_any sourcefile
  in
  let modulename =
    Compenv.module_of_filename ppf sourcefile outputprefix
  in
  let env = Compmisc.initial_env() in
  Typemod.type_implementation sourcefile outputprefix modulename env


let structure _mapper str =
  let sourcefile = !Location.input_name in
  let str = El_desugar.mapper#structure `Server str in
  let (tstr, _) = typing sourcefile str in
  El_server.structure tstr

let signature _mapper = El_desugar.mapper#signature `Server

let mapper _ = {
  Ast_mapper.default_mapper with
  structure ;
  signature ;
}

let () = Ast_mapper.run_main mapper
