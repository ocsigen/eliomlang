
let%server x = 2

let%server x' = [%client 3]

let%server y =
  [[%client (3 + ~%x + ~%(x+1) : int)]]

let%client z = ~%(x+1) + ~%x


(*
Local Variables:
compile-command: "ocamlfind ppx_tools/rewriter -ppx ../desugar.byte -impl test.eliom"
End:
*)