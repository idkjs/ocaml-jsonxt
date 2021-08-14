module type IO = Io.IO;

module type LexIO = {
  module IO: IO;

  let read: (Bytes.t, int) => IO.t(int);
};

module type Lex = {
  module IO: IO;

  let read: Lexing.lexbuf => IO.t(result(Tokens.token, string));
};

module Make:
  (Compliance: Compliance.S, LexIO: LexIO) => Lex with module IO := LexIO.IO;
