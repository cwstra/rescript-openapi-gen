module Op = {
  type t = [
    | #get
    | #put
    | #post
    | #delete
    | #options
    | #head
    | #patch
    | #trace
  ]
  external toString: t => string = "%identity"
}
module ContextOperation = {
  type t = {
    description: option<string>,
    op: Op.t,
    path: string,
    parentParameters: option<array<OpenAPI.WithReference.t<OpenAPI.parameter>>>,
    value: OpenAPI.operation,
  }
  let make = (op, path, parentParameters, value: OpenAPI.operation) => {
    op,
    description: value.description,
    path,
    parentParameters,
    value,
  }
  let getPascalCaseName = t =>
    `${t.op->Op.toString->Utils.String.pascalCase}${Option.getOr(
        t.value.operationId,
        t.path,
      )->Utils.String.pascalCase}`
  let getUglyName = t => {
    let op = t.op->Op.toString
    Option.mapOr(t.value.operationId, `${t.path}/${op}`, id => `${op}-${id}`)
  }
}

module CustomType = {
  type t =
    | OnlyNull
    | HybridDictionary
  type processResultType =
    | BuiltIn(string)
    | Global({asString: string, customType: t})
    | Custom(t)
  let processJSONSchema = {
    let empty = BuiltIn("JSON.t")
    let goSchema = (schema: JSONSchema.t) =>
      switch schema.type_->Option.map(JSONSchema.Arrayable.classify) {
      | Some(Single(#boolean)) => BuiltIn("boolean")
      | Some(Single(#integer)) => BuiltIn("int")
      | Some(Single(#null)) => Global({asString: "Null.t", customType: OnlyNull})
      | Some(Single(#string)) => BuiltIn("string")
      | Some(Single(#number)) => BuiltIn("float")
      | Some(Single(#object)) =>
        switch schema {
        //| {patternProperties} =>
        //| {dependencies} =>
        //| {properties, additionalProperties} => {}
        //| {properties} => {}
        //| {additionalProperties} => {}
        | _ => %todo
        }
      | Some(Single(#array)) => %todo
      | Some(Array(_)) => %todo
      | None => empty
      }->Utils.then(t => (schema.description, t))
    (schema: option<JSONSchema.t>) => Option.mapOr(schema, (None, empty), goSchema)
  }
}

type definedParameter = {
  propertyName: string,
  required: bool,
  stringTypeRepresentation: string,
  in_: OpenAPI.parameterLocation,
}
type item =
  | PrintLine(string)
  | Indent
  | Unindent
  | PrintDescription(option<string>)
  | PrintResponse(OpenAPI.response)
  | PrintCustomType(CustomType.t)
  | PrintOperation({operation: ContextOperation.t, definedParameters: dict<definedParameter>})
  | PrintComponents(OpenAPI.components)
type t = {
  items: list<item>,
  globalTypes: Dict.t<CustomType.t>,
}

type vertexBookkeeping = {
  index: int,
  mutable lowlink: int,
  mutable onStack: bool,
}
type vertex<'item> = {
  mutable bookkeeping: option<vertexBookkeeping>,
  value: 'item,
  outgoingEdges: array<string>,
}
// Given a list of items to treat as vertices, along with functions to define their names and relationships,
// split them into a topologically sorted list of strongly connected components
let tarjan = (
  items: array<'item>,
  getName: 'item => string,
  getOutgoingEdges: 'item => array<string>,
) => {
  let vertices = Array.map(items, value => {
    bookkeeping: None,
    value,
    outgoingEdges: getOutgoingEdges(value),
  })
  let vertexMap = Array.map(vertices, v => (getName(v.value), v))->Dict.fromArray
  let result = []
  let index = ref(0)
  let stack = []
  let rec stackPop = (component, currentIndex) => {
    let (vertex, bookkeeping) = Array.pop(stack)->Option.getUnsafe
    bookkeeping.onStack = false
    Array.push(component, vertex.value)
    if bookkeeping.index != currentIndex {
      stackPop(component, currentIndex)
    }
  }
  let rec strongConnect = vertex => {
    let bookkeeping = {
      index: index.contents,
      lowlink: index.contents,
      onStack: true,
    }
    vertex.bookkeeping = Some(bookkeeping)
    Array.push(stack, (vertex, bookkeeping))
    index := index.contents + 1
    Array.forEach(vertex.outgoingEdges, name => {
      let otherVertex = Dict.getUnsafe(vertexMap, name)
      switch otherVertex.bookkeeping {
      | None => {
          let otherBookkeeping = strongConnect(otherVertex)
          bookkeeping.lowlink = Math.Int.min(bookkeeping.lowlink, otherBookkeeping.lowlink)
        }
      | Some({onStack: true, lowlink}) =>
        bookkeeping.lowlink = Math.Int.min(bookkeeping.lowlink, lowlink)
      | _ => ()
      }
    })
    if bookkeeping.lowlink == bookkeeping.index {
      let component = []
      stackPop(component, bookkeeping.index)
      Array.push(result, component)
    }
    bookkeeping
  }
  Array.forEach(vertices, v =>
    switch v.bookkeeping {
    | None => strongConnect(v)->ignore
    | Some(_) => ()
    }
  )
  result
}

exception Invalid_Parameter_URI(string)
exception Missing_Parameter(string)

let rec getSafeName = (desired, reserved) =>
  if Array.includes(reserved, desired) {
    getSafeName(`${desired}_`, reserved)
  } else {
    desired
  }

module ComponentReferencePrinters: {
  type t<'a> = (string, OpenAPI.WithReference.reference<'a>) => array<item>
  let schemas: t<'a>
  let responses: t<'a>
  //let parameters: t
  //let requestBodies: t
  let headers: t<'a>
  //let pathItems: t
} = {
  type t<'a> = (string, OpenAPI.WithReference.reference<'a>) => array<item>
  let makeExtractReference = part => {
    let regexp = RegExp.fromString(`#/components/${part}/(\\w+)`)
    (reference: OpenAPI.WithReference.reference<_>) =>
      RegExp.exec(regexp, reference.ref)
      ->Option.getUnsafe
      ->Array.at(1)
      ->Option.getUnsafe
      ->Option.getUnsafe
  }
  let makeModulePrinter = (part): t<_> => {
    let extractReference = makeExtractReference(part)
    (name, reference) =>
      extractReference(reference)->Utils.then(sourceName => [
        PrintLine(
          `module ${name} = Components.${Utils.String.pascalCase(part)}.${Utils.String.pascalCase(
              sourceName,
            )}`,
        ),
        PrintDescription(reference.description),
      ])
  }
  let makePropPrinter = (part): t<_> => {
    let extractReference = makeExtractReference(part)
    (name, reference) =>
      extractReference(reference)->Utils.then(sourceName => [
        PrintLine(
          `${name}: Components.${Utils.String.pascalCase(part)}.${Utils.String.pascalCase(
              sourceName,
            )}.t`,
        ),
        PrintDescription(reference.description),
      ])
  }
  let schemas = makeModulePrinter("schemas")
  let responses = makeModulePrinter("responses")
  //let parameters = makePrinter("parameters")
  //let requestBodies = makePrinter("requestBodies")
  let headers = makePropPrinter("headers")
  //let pathItems = makePrinter("pathItems")
}

let fromOpenAPISchema = (schema: OpenAPI.t) => {
  let definedParameters = {
    let raw = schema.components->Option.flatMap(c => c.parameters)->Option.getOr(Dict.make())
    let byReference =
      Dict.toArray(raw)
      ->Array.map(((key, value)) => (`#/components/parameters/${key}`, value))
      ->Dict.fromArray
    let rec resolveInformation = (parameter: OpenAPI.WithReference.t<OpenAPI.parameter>) =>
      switch OpenAPI.WithReference.classify(parameter) {
      | Reference(r) =>
        switch Dict.get(byReference, r.ref) {
        | Some(parameter) => resolveInformation(parameter)
        | None => raise(Missing_Parameter(r.ref))
        }
      | Object(o) => (Option.getOr(o.required, false), o.in_, o.name)
      }
    schema.components
    ->Option.flatMap(c => c.parameters)
    ->Option.mapOr(Dict.make(), parameters =>
      Dict.toArray(parameters)
      ->Array.map(((key, value)) => {
        let (required, in_, propertyName) = resolveInformation(value)
        (
          `#/components/parameters/${key}`,
          {
            propertyName,
            required,
            stringTypeRepresentation: `Components.Parameters.${Utils.String.pascalCase(key)}`,
            in_,
          },
        )
      })
      ->Dict.fromArray
    )
  }
  let fromPaths = (paths: option<dict<OpenAPI.pathItem>>) => {
    let contextOperations =
      paths
      ->Option.getOr(Dict.make())
      ->Dict.toArray
      ->Array.flatMap(((path, item)) => {
        open ContextOperation
        Array.keepSome([
          item.get->Option.map(make(#get, path, item.parameters, _)),
          item.put->Option.map(make(#put, path, item.parameters, _)),
          item.post->Option.map(make(#post, path, item.parameters, _)),
          item.delete->Option.map(make(#delete, path, item.parameters, _)),
          item.options->Option.map(make(#options, path, item.parameters, _)),
          item.head->Option.map(make(#head, path, item.parameters, _)),
          item.patch->Option.map(make(#patch, path, item.parameters, _)),
          item.trace->Option.map(make(#trace, path, item.parameters, _)),
        ])
      })
    let byName = switch Utils.Array.groupBy(contextOperations, ContextOperation.getPascalCaseName)
    ->Dict.toArray
    ->Utils.Array.validate(((name, values)) =>
      switch values {
      | (fst, []) => Ok((name, fst))
      | ne => Error((name, ne))
      }
    ) {
    | Ok(nonRepeated) => nonRepeated
    | Error(fst, rst) => {
        Console.warn(`Duplicate pascal case names detected. Using ugly names instead.`)
        let (name, values) = fst
        Console.warn(
          `"${name}" was generated for ${NonEmptyArray.toArray(values)
            ->Array.map(v => `${v.path}.${v.op->Op.toString}`)
            ->Array.join(",")}`,
        )
        if Array.length(rst) > 0 {
          Console.warn(
            `Other duplicate names were: ${rst->Array.map(((name, _)) => name)->Array.join(", ")}`,
          )
        }
        Utils.Array.keyBy(contextOperations, ContextOperation.getUglyName)->Dict.toArray
      }
    }
    Array.flatMap(byName, ((name, operation)) => [
      PrintLine("}"),
      Indent,
      PrintOperation({operation, definedParameters}),
      Unindent,
      PrintLine(`module ${name} = {`),
      PrintDescription(operation.description),
    ])
  }
  {
    items: [
      ...fromPaths(schema.paths),
      ...Option.mapOr(schema.components, [], components => [PrintComponents(components)]),
    ]->List.fromArray,
    globalTypes: Dict.make(),
  }
}

module GroupedParameters = {
  type entry = {
    description: option<string>,
    asString: string,
    required: bool,
  }
  type t = {
    query: Dict.t<entry>,
    header: Dict.t<entry>,
    path: Dict.t<entry>,
    cookie: Dict.t<entry>,
  }
  let make = () => {
    query: Dict.make(),
    header: Dict.make(),
    path: Dict.make(),
    cookie: Dict.make(),
  }
  let getLocationDict = (grouped: t, location: OpenAPI.parameterLocation) =>
    switch location {
    | Query => grouped.query
    | Header => grouped.header
    | Path => grouped.path
    | Cookie => grouped.cookie
    }
}

let printParameters = (
  parentParameters: array<OpenAPI.WithReference.t<OpenAPI.parameter>>,
  myParameters: array<OpenAPI.WithReference.t<OpenAPI.parameter>>,
  definedParameters: dict<definedParameter>,
) => {
  let result = GroupedParameters.make()
  let setParameter = (parameter: OpenAPI.WithReference.t<OpenAPI.parameter>) =>
    switch OpenAPI.WithReference.classify(parameter) {
    | Reference(r) =>
      switch Dict.get(definedParameters, r.ref) {
      | Some({in_, propertyName, stringTypeRepresentation, required}) =>
        Dict.set(
          GroupedParameters.getLocationDict(result, in_),
          propertyName,
          {description: r.description, asString: stringTypeRepresentation, required},
        )
      | None => raise(Missing_Parameter(r.ref))
      }
    | Object({name, in_, ?required, ?schema}) =>
      switch CustomType.processJSONSchema(schema) {
      | (description, BuiltIn(asString)) =>
        Dict.set(
          GroupedParameters.getLocationDict(result, in_),
          name,
          {description, asString, required: Option.getOr(required, false)},
        )
      | (description, Global({asString, customType})) =>
        Dict.set(
          GroupedParameters.getLocationDict(result, in_),
          name,
          {description, asString, required: Option.getOr(required, false)},
        )
      }
    }
  Array.forEach(parentParameters, setParameter)
  Array.forEach(myParameters, setParameter)
  let printOne = (key, entry: GroupedParameters.entry) => [
    if entry.required {
      PrintLine(`${key}: ${entry.asString}`)
    } else {
      PrintLine(`${key}?: ${entry.asString}`)
    },
    PrintDescription(entry.description),
  ]
  let printDict = (name: string, dict: dict<GroupedParameters.entry>) =>
    switch Dict.toArray(dict) {
    | [] => []
    | properties =>
      [
        PrintLine("}"),
        Indent,
        ...Array.flatMap(properties, ((key, value)) => printOne(key, value)),
        Unindent,
        PrintLine(`type ${name} = {`),
      ]
    }
  switch [
    ...printDict("query", result.query),
    ...printDict("path", result.path),
    ...printDict("header", result.header),
    ...printDict("cookie", result.cookie),
  ] {
  | [] => []
  | parts => [PrintLine("}"), Indent, ...parts, Unindent, PrintLine("module Parameters = {")]
  }
}
let printResponse = (response: OpenAPI.response) => {
  [
    ...Option.mapOr(response.headers, [], headers =>
      Dict.toArray(headers)->Array.flatMap(((name, value)) =>
        switch OpenAPI.WithReference.classify(value) {
        | Reference(r) => ComponentReferencePrinters.headers(name, r)
        }
      )
    ),
    PrintDescription(Some(response.description)),
  ]->List.fromArray
}
let printResponses = (responses: dict<OpenAPI.WithReference.t<OpenAPI.response>>) =>
  Dict.toArray(responses)->Array.flatMap(((response, value)) => {
    switch OpenAPI.WithReference.classify(value) {
    | Reference(r) => ComponentReferencePrinters.responses(`Code${response}`, r)
    | Object(o) => [PrintResponse(o)]
    }
  })

let printOperation = (
  operation: ContextOperation.t,
  definedParameters: dict<definedParameter>,
): list<item> => {
  [
    ...Option.mapOr(operation.value.responses, [], printResponses),
    ...printParameters(
      Option.getOr(operation.parentParameters, []),
      Option.getOr(operation.value.parameters, []),
      definedParameters,
    ),
    PrintLine(`let operation = #${operation.op->Op.toString}`),
    PrintLine(`let path = "${operation.path}"`),
  ]->List.fromArray
}
/*
let printComponents = (components: OpenAPI.components): list<t> => {
  list{
    PrintLine(`let operation = #${operation.op->Op.toString}`),
    PrintLine(`let operation = #${operation.op->Op.toString}`),
    PrintLine(`let path = "${operation.path}"`),
  }
}
*/

exception Missing_Command(item)

let printItem = (item: item, indent: int): (list<item>, array<string>, int) =>
  switch item {
  | PrintLine(str) => (list{}, [`${String.repeat(" ", indent)}${str}`], indent)
  | Indent => (list{}, [], indent + 2)
  | Unindent => (list{}, [], indent - 2)
  | PrintOperation({operation, definedParameters}) => (
      printOperation(operation, definedParameters),
      [],
      indent,
    )
  | PrintComponents(components) => (list{}, [], indent) //printComponents(components, definedParameters),
  | PrintResponse(response) => (printResponse(response), [], indent)
  | _ => raise(Missing_Command(item))
  }
