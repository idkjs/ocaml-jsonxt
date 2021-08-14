module type Parser = {
  module Compliance: Compliance.S;
  type t;

  let create: (~reader: unit => Tokens.token) => t;
  let decode: t => result(option(Compliance.json_stream), string);
};

module Make:
  (Compliance: Compliance.S) => Parser with module Compliance = Compliance;
