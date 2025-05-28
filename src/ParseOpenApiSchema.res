let withReference = (base: S.t<'a>): S.t<OpenAPI.WithReference.t<'a>> => {
  Console.log(base)
  S.union([
    base->S.transform(s => {
      parser: base => base->OpenAPI.WithReference.object,
      serializer: obj =>
        switch OpenAPI.WithReference.classify(obj) {
        | Object(base) => base
        | _ => s.fail("Can't convert reference to object")
        },
    }),
    S.object((s): OpenAPI.WithReference.reference<'a> => {
      ref: s.field("$ref", S.string),
      summary: ?s.field("summary", S.string->S.option),
      description: ?s.field("description", S.string->S.option),
    })->S.transform(s => {
      parser: reference => reference->OpenAPI.WithReference.reference,
      serializer: obj =>
        switch OpenAPI.WithReference.classify(obj) {
        | Reference(base) => base
        | _ => s.fail("Can't convert object to reference")
        },
    }),
  ])
}

let info = S.object((s): OpenAPI.info => {
  title: s.field("title", S.string),
  summary: ?s.field("summary", S.string->S.option),
  description: ?s.field("description", S.string->S.option),
  termsOfService: ?s.field("termsOfService", S.string->S.url->S.option),
  contact: ?s.field(
    "contact",
    S.object((s): OpenAPI.contact => {
      name: ?s.field("name", S.string->S.option),
      url: ?s.field("url", S.string->S.url->S.option),
      email: ?s.field("email", S.string->S.email->S.option),
    })->S.option,
  ),
  license: ?s.field(
    "license",
    S.object((s): OpenAPI.license => {
      name: s.field("name", S.string),
      identifier: ?s.field("identifier", S.string->S.option),
      url: ?s.field("url", S.string->S.url->S.option),
    })->S.option,
  ),
  version: s.field("version", S.string),
})
let serverVariable = S.object((s): OpenAPI.serverVariable => {
  default: s.field("default", S.string),
  enum: ?s.field("enum", S.string->S.array->S.option),
  description: s.field("description", S.string),
})
let server = S.object((s): OpenAPI.server => {
  url: s.field("url", S.string),
  description: ?s.field("description", S.string->S.option),
  variables: ?s.field("variables", serverVariable->S.dict->S.option),
})
let externalDocumentation = S.object((s): OpenAPI.externalDocumentation => {
  description: ?s.field("description", S.string->S.option),
  url: s.field("url", S.string->S.url),
})
let parameterLocation = S.union([
  S.literal(OpenAPI.Query),
  S.literal(OpenAPI.Header),
  S.literal(OpenAPI.Path),
  S.literal(OpenAPI.Cookie),
])
let parameterStyle: S.t<OpenAPI.parameterStyle> = S.union([
  S.literal(#matrix),
  S.literal(#label),
  S.literal(#form),
  S.literal(#simple),
  S.literal(#spaceDelimited),
  S.literal(#pipeDelimited),
  S.literal(#deepObject),
])
let example = S.object((s): OpenAPI.example => {
  summary: ?s.field("summary", S.string->S.option),
  description: ?s.field("description", S.string->S.option),
  value: ?s.field("value", S.unknown->S.option),
  externalValue: ?s.field("externalValue", S.string->S.option),
})
let makeMediaType = (baseParameter: S.t<OpenAPI.baseParameter>) => {
  let encoding = S.object((s): OpenAPI.encoding => {
    contentType: ?s.field("contentType", S.string->S.option),
    headers: ?s.field("headers", baseParameter->withReference->S.dict->S.option),
    style: ?s.field("style", S.string->S.option),
    explode: ?s.field("explode", S.bool->S.option),
    allowReserved: ?s.field("allowReserved", S.bool->S.option),
  })
  S.object((s): OpenAPI.mediaType => {
    schema: ?s.field("schema", ParseJSONSchema.t->S.option),
    example: ?s.field("example", S.unknown->S.option),
    examples: ?s.field("examples", example->withReference->S.dict->S.option),
    encoding: ?s.field("encoding", encoding->S.dict->S.option),
  })
}
let makeBaseParameterFields = (
  s: S.Object.s,
  mediaType: S.t<OpenAPI.mediaType>,
): OpenAPI.baseParameter => {
  description: ?s.field("description", S.string->S.option),
  required: ?s.field("required", S.bool->S.option),
  deprecated: ?s.field("deprecated", S.bool->S.option),
  allowEmptyValue: ?s.field("allowEmptyValue", S.bool->S.option),
  style: ?s.field("style", parameterStyle->S.option),
  explode: ?s.field("explode", S.bool->S.option),
  allowReserved: ?s.field("allowReserved", S.bool->S.option),
  schema: ?s.field("schema", ParseJSONSchema.t->S.option),
  examples: ?s.field("examples", example->withReference->S.dict->S.option),
  example: ?s.field("example", S.json(~validate=true)->S.option),
  content: ?s.field("content", mediaType->withReference->S.dict->S.option),
}
let baseParameter = S.recursive((baseParameter: S.t<OpenAPI.baseParameter>) => {
  let mediaType = makeMediaType(baseParameter)
  S.object((s): OpenAPI.baseParameter => makeBaseParameterFields(s, mediaType))
})
let header = baseParameter
let mediaType = makeMediaType(baseParameter)
let parameter = S.object((s): OpenAPI.parameter => {
  let {
    ?description,
    ?required,
    ?deprecated,
    ?allowEmptyValue,
    ?style,
    ?explode,
    ?allowReserved,
    ?schema,
    ?examples,
    ?example,
    ?content,
  } = makeBaseParameterFields(s, mediaType)
  {
    name: s.field("name", S.string),
    in_: s.field("in", parameterLocation),
    ?description,
    ?required,
    ?deprecated,
    ?allowEmptyValue,
    ?style,
    ?explode,
    ?allowReserved,
    ?schema,
    ?examples,
    ?example,
    ?content,
  }
})
let requestBody = S.object((s): OpenAPI.requestBody => {
  description: ?s.field("description", S.string->S.option),
  content: s.field("content", mediaType->S.dict),
  required: ?s.field("required", S.bool->S.option),
})
let link = S.object((s): OpenAPI.link => {
  operationRef: ?s.field("operationRef", S.string->S.option),
  operationId: ?s.field("operationId", S.string->S.option),
  parameters: ?s.field("parameters", S.unknown->S.dict->S.option),
  requestBody: ?s.field("requestBody", S.unknown->S.option),
  description: ?s.field("description", S.string->S.option),
  server: ?s.field("server", server->S.option),
})
let response = S.object((s): OpenAPI.response => {
  description: s.field("description", S.string),
  headers: ?s.field("headers", header->withReference->S.dict->S.option),
  content: ?s.field("content", mediaType->S.dict->S.option),
  links: ?s.field("links", link->withReference->S.dict->S.option),
})
let securityRequirement = S.string->S.array->S.dict
let pathItem = S.recursive((pathItem: S.t<OpenAPI.pathItem>) => {
  let callback = pathItem->withReference->S.dict
  let operation = S.object((s): OpenAPI.operation => {
    tags: ?s.field("tags", S.string->S.array->S.option),
    summary: ?s.field("summary", S.string->S.option),
    description: ?s.field("description", S.string->S.option),
    externalDocs: ?s.field("externalDocs", externalDocumentation->S.option),
    operationId: ?s.field("operationId", S.string->S.option),
    parameters: ?s.field("parameters", parameter->withReference->S.array->S.option),
    requestBody: ?s.field("requestBody", requestBody->withReference->S.option),
    responses: ?s.field("responses", response->withReference->S.dict->S.option),
    callbacks: ?s.field("callbacks", callback->withReference->S.dict->S.option),
    deprecated: ?s.field("deprecated", S.bool->S.option),
    security: ?s.field("security", securityRequirement->S.array->S.option),
    servers: ?s.field("servers", server->S.array->S.option),
  })
  S.object((s): OpenAPI.pathItem => {
    ref: ?s.field("$ref", S.string->S.option),
    summary: ?s.field("summary", S.string->S.option),
    description: ?s.field("description", S.string->S.option),
    get: ?s.field("get", operation->S.option),
    put: ?s.field("put", operation->S.option),
    post: ?s.field("post", operation->S.option),
    delete: ?s.field("delete", operation->S.option),
    options: ?s.field("options", operation->S.option),
    head: ?s.field("head", operation->S.option),
    patch: ?s.field("patch", operation->S.option),
    trace: ?s.field("trace", operation->S.option),
    servers: ?s.field("servers", server->S.array->S.option),
    parameters: ?s.field("parameters", parameter->withReference->S.array->S.option),
  })
})
let oauthflow = S.object((s): OpenAPI.oauthFlow => {
  authorizationUrl: s.field("authorizationUrl", S.string),
  tokenUrl: s.field("tokenUrl", S.string),
  refreshUrl: s.field("refreshUrl", S.string),
  scopes: s.field("scopes", S.string->S.dict),
})
let oauthflows = S.object((s): OpenAPI.oauthFlows => {
  implicit: ?s.field("implicit", oauthflow->S.option),
  password: ?s.field("password", oauthflow->S.option),
  clientCredentials: ?s.field("clientCredentials", oauthflow->S.option),
  authorizationCode: ?s.field("authorizationCode", oauthflow->S.option),
})
let securityScheme = S.object((s): OpenAPI.securityScheme => {
  type_: s.field("type", S.string),
  description: ?s.field("description", S.string->S.option),
  name: s.field("name", S.string),
  in_: s.field("in", S.string),
  scheme: s.field("scheme", S.string),
  bearerFormat: ?s.field("bearerFormat", S.string->S.option),
  flows: s.field("flows", oauthflows),
  openIdConnectUrl: s.field("openIdConnectUrl", S.string),
})
let components = S.object((s): OpenAPI.components => {
  schemas: ?s.field("schemas", ParseJSONSchema.t->S.dict->S.option),
  responses: ?s.field("responses", response->withReference->S.dict->S.option),
  parameters: ?s.field("parameters", parameter->withReference->S.dict->S.option),
  examples: ?s.field("examples", example->withReference->S.dict->S.option),
  requestBodies: ?s.field("requestBodies", requestBody->withReference->S.dict->S.option),
  headers: ?s.field("headers", header->withReference->S.dict->S.option),
  securitySchemes: ?s.field("securitySchemes", securityScheme->withReference->S.dict->S.option),
  links: ?s.field("links", link->withReference->S.dict->S.option),
  callbacks: ?s.field(
    "callbacks",
    pathItem->withReference->S.dict->withReference->S.dict->S.option,
  ),
  pathItems: ?s.field("pathItems", pathItem->withReference->S.dict->S.option),
})
let tag = S.object((s): OpenAPI.tag => {
  name: s.field("name", S.string),
  description: ?s.field("description", S.string->S.option),
  externalDocs: ?s.field("externalDocs", externalDocumentation->S.option),
})
let t = S.object((s): OpenAPI.t => {
  openapi: s.field("openapi", S.string),
  info: s.field("info", info),
  jsonSchemaDialect: ?s.field("jsonSchemaDialect", S.string->S.option),
  servers: ?s.field("servers", server->S.array->S.option),
  paths: ?s.field("paths", pathItem->S.dict->S.option),
  webhooks: ?s.field("webhooks", pathItem->withReference->S.dict->S.option),
  components: ?s.field("components", components->S.option),
  security: ?s.field("security", S.string->S.array->S.dict->S.array->S.option),
  tags: ?s.field("tags", tag->S.array->S.option),
  externalDocs: ?s.field("externalDocs", externalDocumentation->S.option),
})->S.strict
