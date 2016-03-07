
let%server x = 2

let%server x' = [%client ~%x + ~%x]

let%server y =
  [[%client (3 + ~%x + ~%(x+1) : int)]]

let%client z = ~%(x+1) + ~%x

let%client z2 = ~%(x+2) + ~%x


(*
Local Variables:
compile-command: "ocamlc -dsource -ppx ../eliom_desugar.byte -impl test.eliom"
End:
*)
