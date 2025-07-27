exception Invalid_args

let (inputFile, outputFile) = switch NodeJs.Process.process.argv {
| [_, _, input, output] => (input, output)
| _ => raise(Invalid_args)
}

let rawApiSchema = {
  open NodeJs.Fs
  let inputFileHandle = await PromiseAPI.open_(~path=#Str(inputFile), ~flags=Flag.read)
  let inputFileContents = await FileHandle.readFileWith(inputFileHandle, {encoding: "utf8"})
  let result = try {
    inputFileContents->JSON.parseExn
  } catch {
  | _ => inputFileContents->Yaml.parseExn
  }
  result
}

let res = rawApiSchema->S.parseOrThrow(ParseOpenApiSchema.t)

let processSchema = (schema: OpenAPI.t): string => {
  let rec step = (queue: WorkQueue.t, lines: array<string>, indent: int) =>
    switch queue.items {
    | list{} => Array.join(lines, "\n")
    | list{head, ...tail} => {
        let (newItems, newLines, newIndent) = WorkQueue.printItem(head, indent)
        Array.unshiftMany(lines, newLines)
        step({...queue, items: List.concat(newItems, tail)}, lines, newIndent)
      }
    }
  step(WorkQueue.fromOpenAPISchema(schema), [], 0)
}

Console.log(processSchema(res))
