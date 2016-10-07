let%shared x = 2

module%shared M = struct

  let x = 2
  let%client y = 3
  let%server z = 3

  module%client N = struct
    let a = 2
  end

end

let%shared x = [%client 3]

let%shared x = [%client ~%x + 1]

let%shared x = [%client ~%(x*2) + 1]


let%shared y = ~%y + 1

let%shared y = ~%(y*2) + 1

module type%shared T = sig

  module%client M : S
  module%server M : S

  module%shared M : sig

    type%shared t = int

    val%client x : int
    val%shared x : int

  end

end
