type t =
  | PrintLine(string)
  | PrintPath({name: string, value: OpenAPI.pathItem})

let fromPaths = (paths: option<dict<OpenAPI.pathItem>>) =>
  paths
  ->Option.getOr(Dict.make())
  ->Dict.toArray
  ->Array.map(((name, value)) => PrintPath({name, value}))
  ->List.fromArray

let printPath = (path: OpenAPI.pathItem) => {
  let a = path.parameters
}
