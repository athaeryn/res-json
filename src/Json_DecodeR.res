@new
external _unsafeCreateUninitializedArray: int => array<'a> = "Array"

@val external _stringify: Js.Json.t => string = "JSON.stringify"

let _isInteger = value => Js.Float.isFinite(value) && Js.Math.floor_float(value) === value

type decoder<'a> = Js.Json.t => result<'a, string>

let id = json => Ok(json)

let bool = json =>
  if Js.typeof(json) == "boolean" {
    Ok((Obj.magic((json: Js.Json.t)): bool))
  } else {
    Error("Expected boolean, got " ++ _stringify(json))
  }

let float = json =>
  if Js.typeof(json) == "number" {
    Ok((Obj.magic((json: Js.Json.t)): float))
  } else {
    Error("Expected number, got " ++ _stringify(json))
  }

let int = json => {
  json
  ->float
  ->Result.flatMap(f =>
    if _isInteger(f) {
      Ok((Obj.magic((f: float)): int))
    } else {
      Error("Expected integer, got " ++ _stringify(json))
    }
  )
}

let string = json =>
  if Js.typeof(json) == "string" {
    Ok((Obj.magic((json: Js.Json.t)): string))
  } else {
    Error("Expected string, got " ++ _stringify(json))
  }

let char = json => {
  json
  ->string
  ->Result.flatMap(s =>
    if String.length(s) == 1 {
      Ok(String.get(s, 0))
    } else {
      Error("Expected single-character string, got " ++ _stringify(json))
    }
  )
}

let date = json => json->string->Result.map(Js.Date.fromString)

let nullable = (decode, json) =>
  if (Obj.magic(json): Js.null<'a>) === Js.null {
    Ok(Js.null)
  } else {
    decode(json)->Result.map(Js.Null.return)
  }

let array = (decode, json) =>
  if Js.Array.isArray(json) {
    let source: array<Js.Json.t> = Obj.magic((json: Js.Json.t))

    source
    ->Array.keepMap(item => {
      switch decode(item) {
      | Ok(value) => Some(value)
      | Error(_) => None
      }
    })
    ->Ok
  } else {
    Error("Expected array, got " ++ _stringify(json))
  }

let list = (decode, json) => json->array(decode, _)->Result.map(List.fromArray)

let pair = (decodeA, decodeB, json) =>
  if Js.Array.isArray(json) {
    let source: array<Js.Json.t> = Obj.magic((json: Js.Json.t))
    switch source {
    | [a, b] =>
      switch (decodeA(a), decodeB(b)) {
      | (Ok(a), Ok(b)) => Ok(a, b)
      | (Error(a), Ok(_b)) => Error(a)
      | (Ok(_a), Error(b)) => Error(b)
      | (Error(a), Error(b)) => Error(a ++ "\n" ++ b)
      }
    | _ =>
      let length = Js.Array.length(source)
      Error(`Expected array of length 2, got array of length ${length->Int.toString}`)
    }
  } else {
    Error("Expected array, got " ++ _stringify(json))
  }

let tuple2 = pair

let tuple3 = (decodeA, decodeB, decodeC, json) =>
  if Js.Array.isArray(json) {
    let source: array<Js.Json.t> = Obj.magic((json: Js.Json.t))
    switch source {
    | [a, b, c] =>
      switch (decodeA(a), decodeB(b), decodeC(c)) {
      | (Ok(a), Ok(b), Ok(c)) => Ok(a, b, c)
      | (Error(a), Ok(_b), Ok(_c)) => Error(a)
      | (Ok(_a), Error(b), Ok(_c)) => Error(b)
      | (Error(a), Error(b), Ok(_c)) => Error(a ++ "\n" ++ b)
      | (Ok(_a), Ok(_b), Error(c)) => Error(c)
      | (Error(a), Ok(_b), Error(c)) => Error(a ++ "\n" ++ c)
      | (Ok(_a), Error(b), Error(c)) => Error(b ++ "\n" ++ c)
      | (Error(a), Error(b), Error(c)) => Error(a ++ "\n" ++ b ++ "\n" ++ c)
      }
    | _ =>
      let length = Js.Array.length(source)
      Error(`Expected array of length 3, got array of length ${length->Int.toString}`)
    }
  } else {
    Error("Expected array, got " ++ _stringify(json))
  }

let tuple4 = (decodeA, decodeB, decodeC, decodeD, json) =>
  if Js.Array.isArray(json) {
    let source: array<Js.Json.t> = Obj.magic((json: Js.Json.t))
    switch source {
    | [a, b, c, d] =>
      switch (decodeA(a), decodeB(b), decodeC(c), decodeD(d)) {
      | (Ok(a), Ok(b), Ok(c), Ok(d)) => Ok(a, b, c, d)
      | (Error(a), Ok(_b), Ok(_c), Ok(_d)) => Error(a)
      | (Ok(_a), Error(b), Ok(_c), Ok(_d)) => Error(b)
      | (Error(a), Error(b), Ok(_c), Ok(_d)) => Error(a ++ "\n" ++ b)
      | (Ok(_a), Ok(_b), Error(c), Ok(_d)) => Error(c)
      | (Error(a), Ok(_b), Error(c), Ok(_d)) => Error(a ++ "\n" ++ c)
      | (Ok(_a), Error(b), Error(c), Ok(_d)) => Error(b ++ "\n" ++ c)
      | (Error(a), Error(b), Error(c), Ok(_d)) => Error(a ++ "\n" ++ b ++ "\n" ++ c)
      | (Ok(_a), Ok(_b), Ok(_c), Error(d)) => Error(d)
      | (Error(a), Ok(_b), Ok(_c), Error(d)) => Error(a ++ "\n" ++ d)
      | (Ok(_a), Error(b), Ok(_c), Error(d)) => Error(b ++ "\n" ++ d)
      | (Error(a), Error(b), Ok(_c), Error(d)) => Error(a ++ "\n" ++ b ++ "\n" ++ d)
      | (Ok(_a), Ok(_b), Error(c), Error(d)) => Error(c ++ "\n" ++ d)
      | (Error(a), Ok(_b), Error(c), Error(d)) => Error(a ++ "\n" ++ c ++ "\n" ++ d)
      | (Ok(_a), Error(b), Error(c), Error(d)) => Error(b ++ "\n" ++ c ++ "\n" ++ d)
      | (Error(a), Error(b), Error(c), Error(d)) => Error(a ++ "\n" ++ b ++ "\n" ++ c ++ "\n" ++ d)
      }
    | _ =>
      let length = Js.Array.length(source)
      Error(`Expected array of length 4, got array of length ${length->Int.toString}`)
    }
  } else {
    Error("Expected array, got " ++ _stringify(json))
  }

let dict = (decode, json) =>
  if (
    Js.typeof(json) == "object" &&
      (!Js.Array.isArray(json) &&
      !((Obj.magic(json): Js.null<'a>) === Js.null))
  ) {
    let source: Js.Dict.t<Js.Json.t> = Obj.magic((json: Js.Json.t))

    let decoded = source->Js.Dict.entries->Array.map(((key, json)) => (key, decode(json)))
    let oks = decoded->Array.keepMap(r =>
      switch r {
      | (key, Ok(value)) => Some(key, value)
      | (_, Error(_)) => None
      }
    )
    let errors = decoded->Array.keepMap(r =>
      switch r {
      | (key, Error(e)) => Some(`${key}: ${e}`)
      | (_, Ok(_)) => None
      }
    )
    switch errors {
    | [] => Ok(oks->Js.Dict.fromArray)
    | messages => messages->Js.Array2.joinWith("\n")->Error
    }
  } else {
    Error("Expected object, got " ++ _stringify(json))
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
      switch decode(value) {
      | Ok(_) as ok => ok
      | Error(msg) => Error(msg ++ ("\n\tat field '" ++ (key ++ "'")))
      }
    | None => Error(j`Expected field '$(key)'`)
    }
  } else {
    Error("Expected object, got " ++ _stringify(json))
  }

let rec at = (key_path, decoder) =>
  switch key_path {
  | list{key} => field(key, decoder)
  | list{first, ...rest} => field(first, at(rest, decoder))
  | list{} => decoder
  }

let optional = (decode, json) =>
  switch json->decode {
  | Ok(v) => Ok(Some(v))
  | Error(_) => Ok(None)
  }

let oneOf = (decoders, json) => {
  let rec inner = (decoders, errors) =>
    switch decoders {
    | list{} =>
      let formattedErrors = "\n- " ++ Js.Array.joinWith("\n- ", List.toArray(List.reverse(errors)))
      Error(
        j`All decoders given to oneOf failed. Here are all the errors: $formattedErrors\\nAnd the JSON being decoded: ` ++
        _stringify(json),
      )
    | list{decode, ...rest} =>
      switch json->decode {
      | Ok(_) as ok => ok
      | Error(e) => inner(rest, list{e, ...errors})
      }
    }
  inner(decoders, list{})
}

let either = (a, b) => oneOf(list{a, b})

let withDefault = (default, decode, json) =>
  switch json->decode {
  | Ok(_) as ok => ok
  | Error(_) => Ok(default)
  }

let map = (f, decode, json) => decode(json)->Result.map(f)

let andThen = (b, a, json) => json->a->Result.flatMap(a => b(a, json))
