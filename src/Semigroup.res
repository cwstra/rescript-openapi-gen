module type T = {
  type t
  let reduce: (t, t) => t
}
module First = (
  Base: {
    type t
  },
) => {
  type t = Base.t
  let reduce = (t, _) => t
}
module Last = (
  Base: {
    type t
  },
) => {
  type t = Base.t
  let reduce = (_, t) => t
}
