let arrayable = (base: S.t<'a>): S.t<JSONSchema.Arrayable.t<'a>> =>
  S.union([
    base->S.transform(s => {
      parser: base => base->JSONSchema.Arrayable.single,
      serializer: obj =>
        switch JSONSchema.Arrayable.classify(obj) {
        | Single(base) => base
        | _ => s.fail("Can't convert multiple to single")
        },
    }),
    base
    ->S.array
    ->S.transform(s => {
      parser: base => base->JSONSchema.Arrayable.array,
      serializer: obj =>
        switch JSONSchema.Arrayable.classify(obj) {
        | Array(base) => base
        | _ => s.fail("Can't convert single to multiple")
        },
    }),
  ])

let typeName = S.union([
  S.literal(#string),
  S.literal(#number),
  S.literal(#integer),
  S.literal(#boolean),
  S.literal(#object),
  S.literal(#array),
  S.literal(#null),
])

let t = S.recursive((t: S.t<JSONSchema.t>) => {
  let definition = S.union([
    t->S.transform(s => {
      parser: base => base->JSONSchema.Definition.schema,
      serializer: obj =>
        switch JSONSchema.Definition.classify(obj) {
        | Schema(t) => t
        | _ => s.fail("Can't convert bool to schema")
        },
    }),
    S.bool->S.transform(s => {
      parser: base => base->JSONSchema.Definition.boolean,
      serializer: obj =>
        switch JSONSchema.Definition.classify(obj) {
        | Boolean(t) => t
        | _ => s.fail("Can't convert schema to bool")
        },
    }),
  ])
  let dependency = S.union([
    t->S.transform(s => {
      parser: base => base->JSONSchema.Dependency.schema,
      serializer: obj =>
        switch JSONSchema.Dependency.classify(obj) {
        | Schema(t) => t
        | _ => s.fail("Can't convert required to schema")
        },
    }),
    S.string
    ->S.array
    ->S.transform(s => {
      parser: base => base->JSONSchema.Dependency.required,
      serializer: obj =>
        switch JSONSchema.Dependency.classify(obj) {
        | Required(t) => t
        | _ => s.fail("Can't convert schema to required")
        },
    }),
  ])
  S.object((s): JSONSchema.t => {
    id: ?s.field("$id", S.string->S.option),
    ref: ?s.field("$ref", S.string->S.option),
    schema: ?s.field("$schema", S.string->S.option),
    defs: ?s.field("$defs", definition->S.dict->S.option),
    type_: ?s.field(
      "type",
      S.union([
        typeName->arrayable->S.option,
        S.literal(#"")->S.transform(
          _ => {
            parser: _ => None,
            serializer: _ => #"",
          },
        ),
      ]),
    ),
    enum: ?s.field("enum", S.json(~validate=true)->S.array->S.option),
    const: ?s.field("const", S.json(~validate=true)->S.option),
    multipleOf: ?s.field("multipleOf", S.float->S.option),
    maximum: ?s.field("maximum", S.float->S.option),
    exclusiveMaximum: ?s.field("exclusiveMaximum", S.float->S.option),
    minimum: ?s.field("minimum", S.float->S.option),
    exclusiveMinimum: ?s.field("exclusiveMinimum", S.float->S.option),
    maxLength: ?s.field("maxLength", S.int->S.option),
    minLength: ?s.field("minLength", S.int->S.option),
    pattern: ?s.field("pattern", S.string->S.option),
    items: ?s.field("items", definition->arrayable->S.option),
    additionalItems: ?s.field("additionalItems", definition->S.option),
    maxItems: ?s.field("maxItems", S.int->S.option),
    minItems: ?s.field("minItems", S.int->S.option),
    uniqueItems: ?s.field("uniqueItems", S.bool->S.option),
    contains: ?s.field("contains", t->S.option),
    maxProperties: ?s.field("maxProperties", S.int->S.option),
    minProperties: ?s.field("minProperties", S.int->S.option),
    required: ?s.field("required", S.string->S.array->S.option),
    properties: ?s.field("properties", definition->S.dict->S.option),
    patternProperties: ?s.field("patternProperties", definition->S.dict->S.option),
    additionalProperties: ?s.field("additionalProperties", definition->S.option),
    dependencies: ?s.field("dependencies", dependency->S.dict->S.option),
    propertyNames: ?s.field("propertyNames", definition->S.option),
    if_: ?s.field("if", definition->S.option),
    then: ?s.field("then", definition->S.option),
    else_: ?s.field("else", definition->S.option),
    allOf: ?s.field("allOf", definition->S.array->S.option),
    anyOf: ?s.field("anyOf", definition->S.array->S.option),
    oneOf: ?s.field("oneOf", definition->S.array->S.option),
    not: ?s.field("not", definition->S.option),
    format: ?s.field("format", S.string->S.option),
    contentMediaType: ?s.field("contentMediaType", S.string->S.option),
    contentEncoding: ?s.field("contentEncoding", S.string->S.option),
    definitions: ?s.field("definitions", definition->S.dict->S.option),
    title: ?s.field("title", S.string->S.option),
    description: ?s.field("description", S.string->S.option),
    default: ?s.field("default", S.json(~validate=true)->S.option),
    readOnly: ?s.field("readOnly", S.bool->S.option),
    writeOnly: ?s.field("writeOnly", S.bool->S.option),
    examples: ?s.field("examples", S.json(~validate=true)->S.array->S.option),
  })
})
