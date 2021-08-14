module type Parser = {
  module Compliance: Compliance.S;

  let decode:
    (~reader: unit => Tokens.token) =>
    result(option(Compliance.json), string);
};

module Make =
       (Compliance: Compliance.S)
       : (Parser with module Compliance = Compliance) => {
  module Compliance = Compliance;

  exception Parse_error([ | `Eof | `Syntax_error(string)]);

  let json_value = (~reader) => {
    open Tokens;
    open Parser_tools;
    let rec token_value = tok =>
      switch (tok) {
      | INT(i) => Compliance.integer(i)
      | STRING(s) => Compliance.string(s)
      | BOOL(b) => Compliance.bool(b)
      | FLOAT(f) => Compliance.number(`Float(f))
      | INFINITY => Compliance.number(`Infinity)
      | NEGINFINITY => Compliance.number(`Neginfinity)
      | NAN => Compliance.number(`Nan)
      | NULL => Compliance.null
      | LARGEINT(s) => Compliance.largeint(s)
      | EOF => raise(Parse_error(`Eof))
      | COMMA
      | COLON
      | AE
      | OE
      | TE
      | VE
      | LEX_ERROR(_)
      | COMPLIANCE_ERROR(_) => raise(Parse_error(token_error(tok)))
      | AS => array_value_start()
      | OS => object_value_start()
      | TS => tuple_value_start()
      | VS => variant_value_start()
      }
    and value = () => token_value(reader())
    and array_value_start = () => {
      let tok = reader();
      switch (tok) {
      | AE => Compliance.list([])
      | _ => array_values_start(tok, [])
      };
    }
    and array_values_start = (tok, acc) => {
      let v = token_value(tok);
      switch (reader()) {
      | AE => Compliance.list(List.rev([v, ...acc]))
      | COMMA => array_values([v, ...acc])
      | tok => raise(Parse_error(token_error(tok)))
      };
    }
    and array_values = acc => {
      let v = value();
      switch (reader()) {
      | AE => Compliance.list(List.rev([v, ...acc]))
      | COMMA => array_values([v, ...acc])
      | tok => raise(Parse_error(token_error(tok)))
      };
    }
    and object_value_start = () => {
      let tok = reader();
      switch (tok) {
      | OE => Compliance.assoc([])
      | _ => object_values_start(tok, [])
      };
    }
    and object_values_start = (tok, acc) => {
      let v = colon_value(tok, ());
      switch (reader()) {
      | OE => Compliance.assoc(List.rev([v, ...acc]))
      | COMMA => object_values([v, ...acc])
      | tok => raise(Parse_error(token_error(tok)))
      };
    }
    and object_values = acc => {
      let v = key_colon_value();
      switch (reader()) {
      | OE => Compliance.assoc(List.rev([v, ...acc]))
      | COMMA => object_values([v, ...acc])
      | tok => raise(Parse_error(token_error(tok)))
      };
    }
    and colon_value = (v, ()) =>
      switch (v) {
      | STRING(k) =>
        switch (reader()) {
        | COLON => (k, value())
        | tok => raise(Parse_error(token_error(tok)))
        }
      | tok => raise(Parse_error(token_error(tok)))
      }
    and key_colon_value = () =>
      switch (reader()) {
      | STRING(k) =>
        switch (reader()) {
        | COLON => (k, value())
        | tok => raise(Parse_error(token_error(tok)))
        }
      | tok => raise(Parse_error(token_error(tok)))
      }
    and tuple_value_start = () => {
      let v1 = value();
      switch (reader()) {
      | COMMA =>
        let v2 = value();
        switch (reader()) {
        | TE => Compliance.tuple([v1, v2])
        | COMMA => tuple_values([v2, v1])
        | tok => raise(Parse_error(token_error(tok)))
        };
      | TE =>
        raise(
          Parse_error(`Syntax_error("tuple must have at least 2 elements")),
        )
      | tok => raise(Parse_error(token_error(tok)))
      };
    }
    and tuple_values = acc => {
      let v = value();
      switch (reader()) {
      | TE => Compliance.tuple(List.rev([v, ...acc]))
      | COMMA => tuple_values([v, ...acc])
      | tok => raise(Parse_error(token_error(tok)))
      };
    }
    and variant_value_start = () =>
      switch (reader()) {
      | STRING(k) =>
        switch (reader()) {
        | VE => Compliance.variant(k, None)
        | COLON => variant_end(k, Some(value()))
        | tok => raise(Parse_error(token_error(tok)))
        }
      | VE =>
        raise(
          Parse_error(`Syntax_error("variant must have at least a string")),
        )
      | tok => raise(Parse_error(token_error(tok)))
      }
    and variant_end = (k, v) =>
      switch (reader()) {
      | VE => Compliance.variant(k, v)
      | tok => raise(Parse_error(token_error(tok)))
      };

    switch (reader()) {
    | exception (Parse_error(`Eof)) => None
    | exception exn_ => raise(exn_)
    | EOF => None
    | tok => Some(token_value(tok))
    };
  };

  let decode = (~reader) =>
    switch (json_value(~reader)) {
    | exception (Parse_error(`Eof)) => Error("Unexpected end-of-input")
    | exception (Parse_error(`Syntax_error(err))) => Error(err)
    | exception (Lexxer_utils.Lex_error(err)) => Error(err)
    | res => Ok(res)
    };
};
