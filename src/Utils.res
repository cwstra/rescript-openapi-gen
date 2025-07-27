module Array = {
  @module("./shims.mjs")
  external groupBy: (array<'a>, 'a => string) => Dict.t<NonEmptyArray.t<'a>> = "groupBy"

  let headTail = arr => Array.at(arr, 0)->Option.map(fst => (fst, Array.sliceToEnd(arr, ~start=1)))

  @module("lodash-es")
  external keyBy: (array<'a>, 'a => string) => Dict.t<'a> = "keyBy"

  let validate = (arr, proj) => {
    let result = ref(Ok([]))
    Array.forEach(arr, e => {
      switch (result.contents, proj(e)) {
      | (Ok(oks), Ok(next)) => Array.push(oks, next)
      | (Ok(_), Error(next)) => result := Error((next, []))
      | (Error(_), Ok(_)) => ()
      | (Error((_, errors)), Error(next)) => Array.push(errors, next)
      }
    })
    result.contents
  }
}

module String = {
  @module("lodash-es")
  external camelCase: string => string = "camelCase"

  let pascalCase = str => {
    let cc = camelCase(str)
    `${String.slice(cc, ~start=0, ~end=1)->String.toUpperCase}${String.sliceToEnd(cc, ~start=1)}`
  }
}

let then = (a, f) => f(a)
