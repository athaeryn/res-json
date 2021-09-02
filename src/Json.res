module Decode = Json_Decode
module Encode = Json_Encode

exception ParseError(string)

let parse = s =>
  try Some(Js.Json.parseExn(s)) catch {
  | _ => None
  }

@val external stringify: Js.Json.t => string = "JSON.stringify"
