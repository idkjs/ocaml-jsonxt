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
    | `As
    | `Ae
    | `Os
    | `Oe
    | `Name(string)
  ];
  type t = json;
};

module Strict = {
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
