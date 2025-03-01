@new
external _unsafeCreateUninitializedArray: int => array<'a> = "Array"

@val external _stringify: Js.Json.t => string = "JSON.stringify"

let _isInteger = value => Js.Float.isFinite(value) && Js.Math.floor_float(value) === value

type decoder<'a> = Js.Json.t => 'a

exception DecodeError(string)

let id = json => json

let bool = json =>
  if Js.typeof(json) == "boolean" {
    (Obj.magic((json: Js.Json.t)): bool)
  } else {
    \"@@"(raise, DecodeError("Expected boolean, got " ++ _stringify(json)))
  }

let float = json =>
  if Js.typeof(json) == "number" {
    (Obj.magic((json: Js.Json.t)): float)
  } else {
    \"@@"(raise, DecodeError("Expected number, got " ++ _stringify(json)))
  }

let int = json => {
  let f = float(json)
  if _isInteger(f) {
    (Obj.magic((f: float)): int)
  } else {
    \"@@"(raise, DecodeError("Expected integer, got " ++ _stringify(json)))
  }
}

let string = json =>
  if Js.typeof(json) == "string" {
    (Obj.magic((json: Js.Json.t)): string)
  } else {
    \"@@"(raise, DecodeError("Expected string, got " ++ _stringify(json)))
  }

let char = json => {
  let s = string(json)
  if String.length(s) == 1 {
    String.get(s, 0)
  } else {
    \"@@"(raise, DecodeError("Expected single-character string, got " ++ _stringify(json)))
  }
}

let date = json => json |> string |> Js.Date.fromString

let nullable = (decode, json) =>
  if (Obj.magic(json): Js.null<'a>) === Js.null {
    Js.null
  } else {
    Js.Null.return(decode(json))
  }

/* TODO: remove this? */
let nullAs = (value, json) =>
  if (Obj.magic(json): Js.null<'a>) === Js.null {
    value
  } else {
    \"@@"(raise, DecodeError("Expected null, got " ++ _stringify(json)))
  }

let array = (decode, json) =>
  if Js.Array.isArray(json) {
    let source: array<Js.Json.t> = Obj.magic((json: Js.Json.t))
    let length = Js.Array.length(source)
    let target = _unsafeCreateUninitializedArray(length)
    for i in 0 to length - 1 {
      let value = try decode(Array.getUnsafe(source, i)) catch {
      | DecodeError(msg) =>
        \"@@"(raise, DecodeError(msg ++ ("\n\tin array at index " ++ string_of_int(i))))
      }

      Array.setUnsafe(target, i, value)
    }
    target
  } else {
    \"@@"(raise, DecodeError("Expected array, got " ++ _stringify(json)))
  }

let list = (decode, json) => json |> array(decode) |> List.fromArray

let pair = (decodeA, decodeB, json) =>
  if Js.Array.isArray(json) {
    let source: array<Js.Json.t> = Obj.magic((json: Js.Json.t))
    let length = Js.Array.length(source)
    if length == 2 {
      try (decodeA(Array.getUnsafe(source, 0)), decodeB(Array.getUnsafe(source, 1))) catch {
      | DecodeError(msg) => \"@@"(raise, DecodeError(msg ++ "\n\tin pair/tuple2"))
      }
    } else {
      \"@@"(raise, DecodeError(j`Expected array of length 2, got array of length $length`))
    }
  } else {
    \"@@"(raise, DecodeError("Expected array, got " ++ _stringify(json)))
  }

let tuple2 = pair

let tuple3 = (decodeA, decodeB, decodeC, json) =>
  if Js.Array.isArray(json) {
    let source: array<Js.Json.t> = Obj.magic((json: Js.Json.t))
    let length = Js.Array.length(source)
    if length == 3 {
      try (
        decodeA(Array.getUnsafe(source, 0)),
        decodeB(Array.getUnsafe(source, 1)),
        decodeC(Array.getUnsafe(source, 2)),
      ) catch {
      | DecodeError(msg) => \"@@"(raise, DecodeError(msg ++ "\n\tin tuple3"))
      }
    } else {
      \"@@"(raise, DecodeError(j`Expected array of length 3, got array of length $length`))
    }
  } else {
    \"@@"(raise, DecodeError("Expected array, got " ++ _stringify(json)))
  }

let tuple4 = (decodeA, decodeB, decodeC, decodeD, json) =>
  if Js.Array.isArray(json) {
    let source: array<Js.Json.t> = Obj.magic((json: Js.Json.t))
    let length = Js.Array.length(source)
    if length == 4 {
      try (
        decodeA(Array.getUnsafe(source, 0)),
        decodeB(Array.getUnsafe(source, 1)),
        decodeC(Array.getUnsafe(source, 2)),
        decodeD(Array.getUnsafe(source, 3)),
      ) catch {
      | DecodeError(msg) => \"@@"(raise, DecodeError(msg ++ "\n\tin tuple4"))
      }
    } else {
      \"@@"(raise, DecodeError(j`Expected array of length 4, got array of length $length`))
    }
  } else {
    \"@@"(raise, DecodeError("Expected array, got " ++ _stringify(json)))
  }

let dict = (decode, json) =>
  if (
    Js.typeof(json) == "object" &&
      (!Js.Array.isArray(json) &&
      !((Obj.magic(json): Js.null<'a>) === Js.null))
  ) {
    let source: Js.Dict.t<Js.Json.t> = Obj.magic((json: Js.Json.t))
    let keys = Js.Dict.keys(source)
    let l = Js.Array.length(keys)
    let target = Js.Dict.empty()
    for i in 0 to l - 1 {
      let key = Array.getUnsafe(keys, i)
      let value = try decode(Js.Dict.unsafeGet(source, key)) catch {
      | DecodeError(msg) => \"@@"(raise, DecodeError(msg ++ "\n\tin dict"))
      }

      Js.Dict.set(target, key, value)
    }
    target
  } else {
    \"@@"(raise, DecodeError("Expected object, got " ++ _stringify(json)))
  }

let field = (key, decode, json) =>
  if (
    Js.typeof(json) == "object" &&
      (!Js.Array.isArray(json) &&
      !((Obj.magic(json): Js.null<'a>) === Js.null))
  ) {
    let dict: Js.Dict.t<Js.Json.t> = Obj.magic((json: Js.Json.t))
    switch Js.Dict.get(dict, key) {
    | Some(value) =>
      try decode(value) catch {
      | DecodeError(msg) => \"@@"(raise, DecodeError(msg ++ ("\n\tat field '" ++ (key ++ "'"))))
      }
    | None => \"@@"(raise, DecodeError(j`Expected field '$(key)'`))
    }
  } else {
    \"@@"(raise, DecodeError("Expected object, got " ++ _stringify(json)))
  }

let rec at = (key_path, decoder) =>
  switch key_path {
  | list{key} => field(key, decoder)
  | list{first, ...rest} => field(first, at(rest, decoder))
  | list{} => \"@@"(raise, Invalid_argument("Expected key_path to contain at least one element"))
  }

let optional = (decode, json) =>
  try Some(decode(json)) catch {
  | DecodeError(_) => None
  }

let oneOf = (decoders, json) => {
  let rec inner = (decoders, errors) =>
    switch decoders {
    | list{} =>
      let formattedErrors = "\n- " ++ Js.Array.joinWith("\n- ", List.toArray(List.reverse(errors)))
      \"@@"(
        raise,
        DecodeError(
          j`All decoders given to oneOf failed. Here are all the errors: $formattedErrors\\nAnd the JSON being decoded: ` ++
          _stringify(json),
        ),
      )
    | list{decode, ...rest} =>
      try decode(json) catch {
      | DecodeError(e) => inner(rest, list{e, ...errors})
      }
    }
  inner(decoders, list{})
}

let either = (a, b) => oneOf(list{a, b})

let withDefault = (default, decode, json) =>
  try decode(json) catch {
  | DecodeError(_) => default
  }

let map = (f, decode, json) => f(decode(json))

let andThen = (b, a, json) => b(a(json), json)
