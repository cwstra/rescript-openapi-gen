type t
@module("yaml")
external parseExn: string => JSON.t = "parse"
@module("yaml")
external stringify: 'a => string = "stringify"
