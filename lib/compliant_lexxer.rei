module type Lex = {let read: Lexing.lexbuf => Tokens.token;};

module Make: (Compliance: Compliance.S) => Lex;
