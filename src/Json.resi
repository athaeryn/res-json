module Decode = Json_Decode
module DecodeR = Json_DecodeR
module Encode = Json_Encode

let parse: string => option<Js.Json.t>

let stringify: Js.Json.t => string
