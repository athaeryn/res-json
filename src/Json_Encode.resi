type encoder<'a> = 'a => Js.Json.t

let null: Js.Json.t

let string: string => Js.Json.t

let float: float => Js.Json.t

let int: int => Js.Json.t

let bool: bool => Js.Json.t

let char: char => Js.Json.t

let date: Js.Date.t => Js.Json.t

let nullable: (encoder<'a>, option<'a>) => Js.Json.t

let withDefault: (Js.Json.t, encoder<'a>, option<'a>) => Js.Json.t

let pair: (encoder<'a>, encoder<'b>, ('a, 'b)) => Js.Json.t

let tuple2: (encoder<'a>, encoder<'b>, ('a, 'b)) => Js.Json.t
let tuple3: (encoder<'a>, encoder<'b>, encoder<'c>, ('a, 'b, 'c)) => Js.Json.t
let tuple4: (encoder<'a>, encoder<'b>, encoder<'c>, encoder<'d>, ('a, 'b, 'c, 'd)) => Js.Json.t

let jsonDict: Js.Dict.t<Js.Json.t> => Js.Json.t

let dict: (encoder<'a>, Js.Dict.t<'a>) => Js.Json.t

let object_: list<(string, Js.Json.t)> => Js.Json.t

let array: encoder<'a> => encoder<array<'a>>

let list: encoder<'a> => encoder<list<'a>>

let jsonArray: array<Js.Json.t> => Js.Json.t
let stringArray: array<string> => Js.Json.t
let numberArray: array<float> => Js.Json.t
let boolArray: array<bool> => Js.Json.t
