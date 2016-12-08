[%%eliom.client let x = 2 ]
[%%eliom.server let x = 2 ]

[%%eliom.client
  module M =
    struct
      [%%eliom.client let x = 2 ]
      [%%eliom.client let y = 3 ]
      [%%eliom.client module N = struct let a = 2 end]
    end]

[%%eliom.server
  module M =
    struct [%%eliom.server let x = 2 ]
           [%%eliom.server let z = 3 ] end]

[%%eliom.client let x = 3 ]
[%%eliom.server let x = [%client 3] ]

[%%eliom.client let x = x + 1 ]
[%%eliom.server let x = [%client (~% x) + 1] ]

[%%eliom.client let x = (x * 2) + 1 ]
[%%eliom.server let x = [%client (~% (x * 2)) + 1] ]

[%%eliom.client let y = (~% y) + 1 ]
[%%eliom.server let y = y + 1 ]


[%%eliom.client let y = (~% (y * 2)) + 1 ]
[%%eliom.server let y = (y * 2) + 1 ]

[%%eliom.client include M]
[%%eliom.server include M]
[%%eliom.server include M]

[%%eliom.client
  module type T  =
    sig
      [%%eliom.client :module M : S]
      [%%eliom.client
        :module M : sig type t = int val x : int val x : int end]
      [%%eliom.client :include M]
end]

[%%eliom.server
  module type T  =
    sig
      [%%eliom.server :module M : S]
      [%%eliom.server :module M : sig type t = int val x : int end]
      [%%eliom.server :include M]

end]
