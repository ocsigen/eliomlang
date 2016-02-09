

let typing sourcefile =
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
    Typemod.type_implementation sourcefile outputprefix modulename env
  in

  Clflags.dont_write_files := val_dont_write_files;
  typedtree



let structure _mapper str =
  let sourcefile = !Location.input_name in
  let str = El_desugar.mapper#structure `Server str in
  let (tstr, _) = typing sourcefile str in
  El_client.structure tstr

let signature _mapper = El_desugar.mapper#signature `Server

let mapper _ = {
  Ast_mapper.default_mapper with
  structure ;
  signature ;
}

let () = Ast_mapper.run_main mapper
