type t<'l, 'r> =
  | Left('l)
  | Right('r)

type partitionMapResult<'l, 'r> = {
  lefts: array<'l>,
  rights: array<'r>,
}
let partitionMap = (arr, proj) => {
  let lefts = []
  let rights = []
  Array.forEach(arr, e =>
    switch proj(e) {
    | Left(l) => Array.push(lefts, l)
    | Right(l) => Array.push(rights, l)
    }
  )
  {lefts, rights}
}
