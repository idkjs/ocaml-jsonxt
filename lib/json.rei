/** Json types for the various compliance levels */;

/** The following is a complete list of the polymorphic variants supported by Jsonxt
    - `Null: JSON null
    - `Bool of bool: JSON boolean
    - `Int of int: JSON number without decimal point or exponent.
    - `Intlit of string: JSON number without decimal point or exponent, preserved as a string.
    - `Float of float: JSON number, inf, -inf, Infinity, -Infinity, nan, -nan, NaN or -NaN
    - `Floatlit of string: JSON number, inf, -inf, Infinity, -Infinity, nan, -nan, NaN or -NaN, preserved as a string.
    - `String of string: JSON string. Bytes in the range 128-255 are preserved when reading and writing.
    - `Stringlit of string: JSON string literal including the double quotes.
    - `Assoc of (string * json) list: JSON object.
    - `List of json list: JSON array.
    - `Tuple of json list: Tuple (non-standard extension of JSON). Syntax: ("abc", 123).
    - `Variant of (string * json option): Variant (non-standard extension of JSON). Syntax: <"Foo"> or <"Bar":123>.
*/

type json = [
  | `Null
  | `Bool(bool)
  | `Int(int)
  | `Intlit(string)
  | `Float(float)
  | `Floatlit(string)
  | `String(string)
  | `Stringlit(string)
  | `Assoc(list((string, json)))
  | `List(list(json))
  | `Tuple(list(json))
  | `Variant(string, option(json))
];

type t = json;

module Extended: {
  /** [Extended] supports all Json types including the non-standard
      tuple and variant introduced by [Yojson] */;

  type nonrec json = json;
  type t = json;
};

module Basic: {
  /** [Basic] supports standard Json types that are supported by the
      JSON standard but also supports integers rather than just floats */;

  /** The following polymorphic variants supported
      - `Null: JSON null
      - `Bool of bool: JSON boolean
      - `Int of int: JSON number without decimal point or exponent.
      - `Float of float: JSON number. Infinity, NaN etc are not supported
      - `String of string: JSON string. Bytes in the range 128-255 are preserved when reading and writing.
      - `Assoc of (string * json) list: JSON object.
      - `List of json list: JSON array.
  */

  type json = [
    | `Null
    | `Bool(bool)
    | `Int(int)
    | `Float(float)
    | `String(string)
    | `Assoc(list((string, json)))
    | `List(list(json))
  ];
  type t = json;
};

module Strict: {
  /** [Strict] supports only types that are supported by the JSON standard.
      Integers are not supported */;

  /** The following polymorphic variants supported
      - `Null: JSON null
      - `Bool of bool: JSON boolean
      - `Float of float: JSON number. Infinity, NaN etc are not supported
      - `String of string: JSON string. Bytes in the range 128-255 are preserved when reading and writing.
      - `Assoc of (string * json) list: JSON object.
      - `List of json list: JSON array.
  */

  type json = [
    | `Null
    | `Bool(bool)
    | `Float(float)
    | `String(string)
    | `Assoc(list((string, json)))
    | `List(list(json))
  ];
  type t = json;
};
