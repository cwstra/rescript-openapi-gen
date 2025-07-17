type t<'a> = ('a, array<'a>)
let fromArray = arr => Array.at(arr, 0)->Option.map(head => (head, Array.sliceToEnd(arr, ~start=1)))
let toArray = ((fst, rst): t<'a>) => [fst, ...rst]
