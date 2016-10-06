let%shared x = 2

module%shared M = struct

  let x = 2
  let%client y = 3

end

let%shared x = [%client 3]

let%shared x = [%client ~%x + 1]

let%shared x = [%client ~%(x*2) + 1]


let%shared y = ~%y + 1

let%shared y = ~%(y*2) + 1
