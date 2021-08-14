let token_error = tok => {
  open Tokens;
  let err =
    switch (tok) {
    | STRING(s) => "unexpected string '" ++ s ++ "'"
    | OS => "unexpected '{'"
    | OE => "unexpected '}'"
    | NULL => "unexpected null value"
    | NEGINFINITY => "unexpected negative infinity"
    | NAN => "unexpected Not-a-Number"
    | LEX_ERROR(s) => s
    | LARGEINT(s) => "unexpected large integer '" ++ s ++ "'"
    | INT(i) => "unexpected integer '" ++ string_of_int(i) ++ "'"
    | INFINITY => "unexpected infinity"
    | FLOAT(f) => "unexpected float '" ++ string_of_float(f) ++ "'"
    | EOF => "unexpected end-of-input"
    | COMPLIANCE_ERROR(s) => "compliance error '" ++ s ++ "'"
    | COMMA => "unexpected ','"
    | COLON => "unexpected ':'"
    | BOOL(b) =>
      "unexpected boolean '" ++ (if (b) {"true"} else {"false"}) ++ "'"
    | AS => "unexpected '['"
    | AE => "unexpected ']'"
    | TS => "unexpected '('"
    | TE => "unexpected ')'"
    | VS => "unexpected '<'"
    | VE => "unexpected '>'"
    };

  `Syntax_error(err);
};
