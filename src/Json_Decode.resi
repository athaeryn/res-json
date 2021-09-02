type decoder<'a> = Js.Json.t => 'a

let id: decoder<Js.Json.t>

let bool: decoder<bool>

let float: decoder<float>

let int: decoder<int>

let string: decoder<string>

let char: decoder<char>

let date: decoder<Js.Date.t>

let nullable: decoder<'a> => decoder<Js.null<'a>>

let nullAs: 'a => decoder<'a>

let array: decoder<'a> => decoder<array<'a>>

let list: decoder<'a> => decoder<list<'a>>

let pair: (decoder<'a>, decoder<'b>) => decoder<('a, 'b)>

let tuple2: (decoder<'a>, decoder<'b>) => decoder<('a, 'b)>
let tuple3: (decoder<'a>, decoder<'b>, decoder<'c>) => decoder<('a, 'b, 'c)>
let tuple4: (decoder<'a>, decoder<'b>, decoder<'c>, decoder<'d>) => decoder<('a, 'b, 'c, 'd)>

let dict: decoder<'a> => decoder<Js.Dict.t<'a>>

let field: (string, decoder<'a>) => decoder<'a>

let at: (list<string>, decoder<'a>) => decoder<'a>

let optional: decoder<'a> => decoder<option<'a>>

let oneOf: list<decoder<'a>> => decoder<'a>

let either: (decoder<'a>, decoder<'a>) => decoder<'a>

let withDefault: ('a, decoder<'a>) => decoder<'a>

let map: ('a => 'b, decoder<'a>) => decoder<'b>

let andThen: ('a => decoder<'b>, decoder<'a>) => decoder<'b>
