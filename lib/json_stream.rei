/** Json stream types for the various compliance levels */;

/** The following polymorphic variants are supported by Jsonxt
    - `Null: JSON null
    - `Bool of bool: JSON boolean
    - `Int of int: JSON number without decimal point or exponent
    - `Intlit of string: JSON number without decimal point or exponent preserved as a string
    - `Float of float: JSON number, inf, -inf, Infinity, -Infinity, nan, -nan, NaN or -NaN
    - `Floatlit of string: JSON number preserved as a string
    - `String of string: JSON string with characters in the range 128-255 preserved
    - `Stringlit of string: JSON string including the double quotes
    - `As - Array start marker
    - `Ae - Array end marker
    - `Os - Object start marker
    - `Oe - Object end marker
    - `Ts - Tuple start marker
    - `Te - Tuple end marger
    - `Vs - Variant start marger
    - `Ve - Variant end marger
    - `Name of string - JSON object key as a JSON string
    - `Infinity - Infinity
    - `Neg_infinity - -Infinity
    - `Nan - NaN
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
  | `As
  | `Ae
  | `Os
  | `Oe
  | `Ts
  | `Te
  | `Vs
  | `Ve
  | `Name(string)
  | `Infinity
  | `Neg_infinity
  | `Nan
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

  /** The following polymorphic variants are supported
    - `Null: JSON null
    - `Bool of bool: JSON boolean
    - `Int of int: JSON number without decimal point or exponent
    - `Float of float: JSON number
    - `String of string: JSON string with characters in the range 128-255 preserved
    - `As - Array start marker
    - `Ae - Array end marker
    - `Os - Object start marker
    - `Oe - Object end marker
    - `Name of string - JSON object key as a JSON string
  */

  type json = [
    | `Null
    | `Bool(bool)
    | `Int(int)
    | `Float(float)
    | `String(string)
    | `As
    | `Ae
    | `Os
    | `Oe
    | `Name(string)
  ];
  type t = json;
};

module Strict: {
  /** [Strict] supports only types that are supported by the JSON standard.
      Integers are not supported */;

  /** The following polymorphic variants are supported
    - `Null: JSON null
    - `Bool of bool: JSON boolean
    - `Float of float: JSON number
    - `String of string: JSON string with characters in the range 128-255 preserved
    - `As - Array start marker
    - `Ae - Array end marker
    - `Os - Object start marker
    - `Oe - Object end marker
    - `Name of string - JSON object key as a JSON string
  */

  type json = [
    | `Null
    | `Bool(bool)
    | `Float(float)
    | `String(string)
    | `As
    | `Ae
    | `Os
    | `Oe
    | `Name(string)
  ];
  type t = json;
};
