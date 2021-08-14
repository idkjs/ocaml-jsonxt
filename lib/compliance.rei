module type S = {
  type json;
  type json_stream;

  let lex_string: string => string;
  let lex_number: Tokens.token => Tokens.token;
  let lex_integer: Tokens.token => Tokens.token;
  let lex_largeint: Tokens.token => Tokens.token;
  let lex_tuple: Tokens.token => bool;
  let lex_variant: Tokens.token => bool;

  let comment_check: unit => result(unit, string);

  let number_to_string: float => string;

  let number:
    [
      | `Float(float)
      | `Infinity
      | `Neginfinity
      | `Nan
      | `Floatlit(string)
    ] =>
    json;
  let integer: int => json;
  let largeint: string => json;
  let null: json;
  let string: string => json;
  let bool: bool => json;
  let assoc: list((string, json)) => json;
  let list: list(json) => json;
  let tuple: list(json) => json;
  let variant: (string, option(json)) => json;

  /* streaming functions */

  module Stream: {
    let number:
      [
        | `Float(float)
        | `Infinity
        | `Neginfinity
        | `Nan
        | `Floatlit(string)
      ] =>
      json_stream;
    let integer: int => json_stream;
    let largeint: string => json_stream;
    let null: json_stream;
    let string: string => json_stream;
    let bool: bool => json_stream;

    let array_start: unit => json_stream;
    let array_end: unit => json_stream;
    let object_start: unit => json_stream;
    let object_end: unit => json_stream;
    let tuple_start: unit => json_stream;
    let tuple_end: unit => json_stream;
    let variant_start: unit => json_stream;
    let variant_end: unit => json_stream;
    let name: string => json_stream;
  };
};
