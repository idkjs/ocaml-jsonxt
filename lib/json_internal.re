type constrained('a) =
  [<
    | `Assoc(list((string, 'a)))
    | `Bool(bool)
    | `Float(float)
    | `Floatlit(string)
    | `Int(int)
    | `Intlit(string)
    | `List(list('a))
    | `Null
    | `String(string)
    | `Stringlit(string)
    | `Tuple(list('a))
    | `Variant(string, option('a))
  ] as 'a;

type constrained_stream('a) = 'a
constraint 'a =
  [<
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
  ] as 'a;
