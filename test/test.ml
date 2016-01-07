
let%server x = 2

let%server y =
  [ 1 ; [%client 3 + ~%x + ~%(x+1)]]

let%client z = ~%(x+1) + ~%x
