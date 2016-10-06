[%%eliom.client let x = 2 ]
[%%eliom.server let x = 2 ]

[%%eliom.shared
  module M =
    struct
      [%%eliom.client let x = 2 ]
      [%%eliom.server let x = 2 ]
      [%%eliom.client let y = 3 ]
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
