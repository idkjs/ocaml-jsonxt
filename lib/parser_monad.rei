module type IO = Io.IO;

module type Parser = {
  module IO: IO;
  module Compliance: Compliance.S;

  let decode:
    (~reader: unit => IO.t(Tokens.token)) =>
    IO.t(result(option(Compliance.json), string));
};

module Make:
  (Compliance: Compliance.S, IO: IO) =>
   Parser with module IO = IO and module Compliance = Compliance;
