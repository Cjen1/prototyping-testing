open Foo

type t = int

module Bar = Functor (struct

    type t = int

    let foo i = i 
    end)
