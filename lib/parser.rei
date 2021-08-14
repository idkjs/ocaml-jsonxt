module type Parser = {
  module Compliance: Compliance.S;

  let decode:
    (~reader: unit => Tokens.token) =>
    result(option(Compliance.json), string);
};

module Make:
  (Compliance: Compliance.S) => Parser with module Compliance = Compliance;
