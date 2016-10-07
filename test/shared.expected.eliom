[%%eliom.client let x = 2 ]
[%%eliom.server let x = 2 ]

[%%eliom.shared
  module M =
    struct
      [%%eliom.client let x = 2 ]
      [%%eliom.server let x = 2 ]

      [%%eliom.client let y = 3 ]

      [%%eliom.server let z = 3 ]


      [%%eliom.client module N = struct [%%eliom.client let a = 2 ] end]
    end]

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

[%%eliom.shared
  module type T  =
    sig
      [%%eliom.client :module M : S]

      [%%eliom.server :module M : S]

      [%%eliom.shared
        :module M :
        sig
          [%%eliom.client: type t = int]
          [%%eliom.server: type t = int]

          [%%eliom.client: val x : int]
          [%%eliom.client: val x : int]
          [%%eliom.server: val x : int]
       end]

      [%%eliom.client :include M]
      [%%eliom.server :include M]

end]
