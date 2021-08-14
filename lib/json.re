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

module Extended = {
  type nonrec json = json;
  type t = json;
};

module Basic = {
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

module Strict = {
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
