module type Parser = {
  module Compliance: Compliance.S;
  type t;

  let create: (~reader: unit => Tokens.token) => t;
  let decode: t => result(option(Compliance.json_stream), string);
};

module Make =
       (Compliance: Compliance.S)
       : (Parser with module Compliance = Compliance) => {
  module Compliance = Compliance;

  exception Parse_error([ | `Eof | `Syntax_error(string)]);

  type t = {
    reader: unit => Tokens.token,
    continuation: Stack.t(unit => Compliance.json_stream),
    state: ref([ | `Start | `Process | `End]),
  };

  let create = (~reader) => {
    reader,
    continuation: Stack.create(),
    state: ref(`Start),
  };

  let json_stream = t => {
    open Tokens;
    open Parser_tools;
    let rec token_value = tok =>
      switch (tok) {
      | INT(i) => Compliance.Stream.integer(i)
      | STRING(s) => Compliance.Stream.string(s)
      | BOOL(b) => Compliance.Stream.bool(b)
      | FLOAT(f) => Compliance.Stream.number(`Float(f))
      | INFINITY => Compliance.Stream.number(`Infinity)
      | NEGINFINITY => Compliance.Stream.number(`Neginfinity)
      | NAN => Compliance.Stream.number(`Nan)
      | NULL => Compliance.Stream.null
      | LARGEINT(s) => Compliance.Stream.largeint(s)
      | EOF => raise(Parse_error(`Eof))
      | COMMA
      | COLON
      | AE
      | OE
      | TE
      | VE
      | LEX_ERROR(_)
      | COMPLIANCE_ERROR(_) => raise(Parse_error(token_error(tok)))
      | AS =>
        Stack.push(array_value, t.continuation);
        Compliance.Stream.array_start();
      | OS =>
        Stack.push(object_value, t.continuation);
        Compliance.Stream.object_start();
      | TS =>
        Stack.push(tuple_value, t.continuation);
        Compliance.Stream.tuple_start();
      | VS =>
        Stack.push(variant_value, t.continuation);
        Compliance.Stream.variant_start();
      }
    and array_value = () => {
      let tok = t.reader();
      switch (tok) {
      | AE => Compliance.Stream.array_end()
      | tok =>
        Stack.push(array_value_next, t.continuation);
        token_value(tok);
      };
    }
    and array_value_next = () =>
      switch (t.reader()) {
      | AE => Compliance.Stream.array_end()
      | COMMA =>
        let tok = t.reader();
        Stack.push(array_value_next, t.continuation);
        token_value(tok);
      | tok => raise(Parse_error(token_error(tok)))
      }
    and object_value = () => {
      let tok = t.reader();
      switch (tok) {
      | OE => Compliance.Stream.object_end()
      | STRING(s) =>
        Stack.push(object_colon_value, t.continuation);
        Compliance.Stream.name(s);
      | tok => raise(Parse_error(token_error(tok)))
      };
    }
    and object_colon_value = () =>
      switch (t.reader()) {
      | COLON =>
        let tok = t.reader();
        Stack.push(object_value_next, t.continuation);
        token_value(tok);
      | tok => raise(Parse_error(token_error(tok)))
      }
    and object_value_next = () =>
      switch (t.reader()) {
      | OE => Compliance.Stream.object_end()
      | COMMA =>
        switch (t.reader()) {
        | STRING(s) =>
          Stack.push(object_colon_value, t.continuation);
          Compliance.Stream.name(s);
        | tok => raise(Parse_error(token_error(tok)))
        }
      | tok => raise(Parse_error(token_error(tok)))
      }
    and tuple_value = () => {
      let tok = t.reader();
      switch (tok) {
      | TE =>
        raise(
          Parse_error(`Syntax_error("tuple must have at least 2 elements")),
        )
      | tok =>
        Stack.push(tuple_value_1, t.continuation);
        token_value(tok);
      };
    }
    and tuple_value_1 = () =>
      switch (t.reader()) {
      | TE =>
        raise(
          Parse_error(`Syntax_error("tuple must have at least 2 elements")),
        )
      | COMMA =>
        let tok = t.reader();
        Stack.push(tuple_value_2, t.continuation);
        token_value(tok);
      | tok => raise(Parse_error(token_error(tok)))
      }
    and tuple_value_2 = () =>
      switch (t.reader()) {
      | TE => Compliance.Stream.tuple_end()
      | COMMA =>
        let tok = t.reader();
        Stack.push(tuple_value_2, t.continuation);
        token_value(tok);
      | tok => raise(Parse_error(token_error(tok)))
      }
    and variant_value = () =>
      switch (t.reader()) {
      | STRING(s) =>
        Stack.push(variant_colon_or_end, t.continuation);
        Compliance.Stream.name(s);
      | tok => raise(Parse_error(token_error(tok)))
      }
    and variant_colon_or_end = () =>
      switch (t.reader()) {
      | VE => Compliance.Stream.variant_end()
      | COLON =>
        let tok = t.reader();
        Stack.push(variant_end, t.continuation);
        token_value(tok);
      | tok => raise(Parse_error(token_error(tok)))
      }
    and variant_end = () =>
      switch (t.reader()) {
      | VE => Compliance.Stream.variant_end()
      | tok => raise(Parse_error(token_error(tok)))
      };

    if (Stack.is_empty(t.continuation)) {
      switch (t.reader()) {
      | exception (Parse_error(`Eof)) => None
      | exception exn_ => raise(exn_)
      | tok =>
        switch (tok) {
        | EOF => None
        | tok => Some(token_value(tok))
        }
      };
    } else {
      Some((Stack.pop(t.continuation))());
    };
  };

  let decode = t => {
    let handle_eof = () =>
      if (Stack.length(t.continuation) > 0) {
        Error("unexpected end-of-input");
      } else {
        switch (t.state^) {
        | `Start => Error("empty input")
        | `Process
        | `End => Ok(None)
        };
      };

    switch (json_stream(t)) {
    | exception (Parse_error(`Syntax_error(err))) => Error(err)
    | exception (Lexxer_utils.Lex_error(err)) => Error(err)
    | exception (Parse_error(`Eof)) => handle_eof()
    | None => handle_eof()
    | res =>
      switch (t.state^) {
      | `Start =>
        t.state := `Process;
        Ok(res);
      | `Process =>
        if (Stack.length(t.continuation) == 0) {
          t.state := `End;
        };
        Ok(res);
      | `End => Error("Junk following JSON value")
      }
    };
  };
};
