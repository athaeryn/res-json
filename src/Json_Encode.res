type encoder<'a> = 'a => Js.Json.t

@val external null: Js.Json.t = "null"
external string: string => Js.Json.t = "%identity"
external float: float => Js.Json.t = "%identity"
external int: int => Js.Json.t = "%identity"
external bool: bool => Js.Json.t = "%identity"

let char = c => c->String.make(1, _)->string

let date = d => d->Js.Date.toJSONUnsafe->string

let nullable = (encode, x) =>
  switch x {
  | None => null
  | Some(v) => encode(v)
  }

let withDefault = (d, encode, x) =>
  switch x {
  | None => d
  | Some(v) => encode(v)
  }

external jsonDict: Js_dict.t<Js.Json.t> => Js.Json.t = "%identity"
let dict = (encode, d) => {
  let pairs = Js.Dict.entries(d)
  let encodedPairs = pairs->Array.map(((k, v)) => (k, encode(v)))
  jsonDict(Js.Dict.fromArray(encodedPairs))
}

let object_ = (props): Js.Json.t => props->Js.Dict.fromList->jsonDict

external jsonArray: array<Js.Json.t> => Js.Json.t = "%identity"
let array = (encode, l) => l->Array.map(encode)->jsonArray
let list = (encode, x) =>
  switch x {
  | list{} => jsonArray([])
  | list{hd, ...tl} as l =>
    let a = Array.make(List.length(l), encode(hd))
    let rec fill = (i, x) =>
      switch x {
      | list{} => a
      | list{hd, ...tl} =>
        Array.setUnsafe(a, i, encode(hd))
        fill(i + 1, tl)
      }

    jsonArray(fill(1, tl))
  }

let pair = (encodeA, encodeB, (a, b)) => jsonArray([encodeA(a), encodeB(b)])
let tuple2 = pair
let tuple3 = (encodeA, encodeB, encodeC, (a, b, c)) =>
  jsonArray([encodeA(a), encodeB(b), encodeC(c)])
let tuple4 = (encodeA, encodeB, encodeC, encodeD, (a, b, c, d)) =>
  jsonArray([encodeA(a), encodeB(b), encodeC(c), encodeD(d)])

external stringArray: array<string> => Js.Json.t = "%identity"
external numberArray: array<float> => Js.Json.t = "%identity"
external boolArray: array<bool> => Js.Json.t = "%identity"
