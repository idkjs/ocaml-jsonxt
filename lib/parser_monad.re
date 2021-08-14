module type IO = Io.IO;

module type Parser = {
  module IO: IO;
  module Compliance: Compliance.S;

  let decode:
    (~reader: unit => IO.t(Tokens.token)) =>
    IO.t(result(option(Compliance.json), string));
};

module Make =
       (Compliance: Compliance.S, IO: IO)
       : (Parser with module IO = IO and module Compliance = Compliance) => {
  module IO = IO;
  module Compliance = Compliance;

  open IO;
  module Error_or = Error_or.Make(IO);
  open Error_or;

  let json_value = (~reader) => {
    open Tokens;
    open Parser_tools;

    let rec token_value = tok =>
      switch (tok) {
      | INT(i) => return(Compliance.integer(i))
      | STRING(s) => return(Compliance.string(s))
      | BOOL(b) => return(Compliance.bool(b))
      | FLOAT(f) => return(Compliance.number(`Float(f)))
      | INFINITY => return(Compliance.number(`Infinity))
      | NEGINFINITY => return(Compliance.number(`Neginfinity))
      | NAN => return(Compliance.number(`Nan))
      | NULL => return(Compliance.null)
      | LARGEINT(s) => return(Compliance.largeint(s))
      | EOF => fail(`Eof)
      | COMMA
      | COLON
      | AE
      | OE
      | TE
      | VE
      | LEX_ERROR(_)
      | COMPLIANCE_ERROR(_) => fail(token_error(tok))
      | AS => array_value_start()
      | OS => object_value_start()
      | TS => tuple_value_start()
      | VS => variant_value_start()
      }
    and value = () => reader() >>= (tok => token_value(tok))
    and array_value_start = () =>
      reader()
      >>= (
        tok =>
          switch (tok) {
          | AE => return(Compliance.list([]))
          | _ => array_values_start(tok, [])
          }
      )
    and array_values_start = (tok, acc) =>
      token_value(tok)
      >>=? (
        v =>
          reader()
          >>= (
            tok =>
              switch (tok) {
              | AE => return(Compliance.list(List.rev([v, ...acc])))
              | COMMA => array_values([v, ...acc])
              | tok => fail(token_error(tok))
              }
          )
      )
    and array_values = acc =>
      value()
      >>=? (
        v =>
          reader()
          >>= (
            tok =>
              switch (tok) {
              | AE => return(Compliance.list(List.rev([v, ...acc])))
              | COMMA => array_values([v, ...acc])
              | tok => fail(token_error(tok))
              }
          )
      )
    and object_value_start = () =>
      reader()
      >>= (
        tok =>
          switch (tok) {
          | OE => return(Compliance.assoc([]))
          | _ => object_values_start(tok, [])
          }
      )
    and object_values_start = (tok, acc) =>
      colon_value(tok, ())
      >>=? (
        v =>
          reader()
          >>= (
            tok =>
              switch (tok) {
              | OE => return(Compliance.assoc(List.rev([v, ...acc])))
              | COMMA => object_values([v, ...acc])
              | tok => fail(token_error(tok))
              }
          )
      )
    and object_values = acc =>
      key_colon_value()
      >>=? (
        v =>
          reader()
          >>= (
            tok =>
              switch (tok) {
              | OE => return(Compliance.assoc(List.rev([v, ...acc])))
              | COMMA => object_values([v, ...acc])
              | tok => fail(token_error(tok))
              }
          )
      )
    and colon_value = (v, ()) =>
      switch (v) {
      | STRING(k) =>
        reader()
        >>= (
          tok =>
            switch (tok) {
            | COLON => value() >>=? (v => return((k, v)))
            | tok => fail(token_error(tok))
            }
        )
      | tok => fail(token_error(tok))
      }
    and key_colon_value = () =>
      reader()
      >>= (
        tok =>
          switch (tok) {
          | STRING(k) =>
            reader()
            >>= (
              tok =>
                switch (tok) {
                | COLON => value() >>=? (v => return((k, v)))
                | tok => fail(token_error(tok))
                }
            )
          | tok => fail(token_error(tok))
          }
      )
    and tuple_value_start = () =>
      value()
      >>=? (
        v1 =>
          reader()
          >>= (
            tok =>
              switch (tok) {
              | COMMA =>
                value()
                >>=? (
                  v2 =>
                    reader()
                    >>= (
                      tok =>
                        switch (tok) {
                        | TE => return(Compliance.tuple([v1, v2]))
                        | COMMA => tuple_values([v2, v1])
                        | tok => fail(token_error(tok))
                        }
                    )
                )
              | TE =>
                fail(`Syntax_error("tuple must have at least 2 elements"))
              | tok => fail(token_error(tok))
              }
          )
      )
    and tuple_values = acc =>
      value()
      >>=? (
        v =>
          reader()
          >>= (
            tok =>
              switch (tok) {
              | TE => return(Compliance.tuple(List.rev([v, ...acc])))
              | COMMA => tuple_values([v, ...acc])
              | tok => fail(token_error(tok))
              }
          )
      )
    and variant_value_start = () =>
      reader()
      >>= (
        tok =>
          switch (tok) {
          | STRING(k) =>
            reader()
            >>= (
              tok =>
                switch (tok) {
                | VE => return(Compliance.variant(k, None))
                | COLON => value() >>=? (v => variant_end(k, Some(v)))
                | tok => fail(token_error(tok))
                }
            )
          | VE => fail(`Syntax_error("variant must have at least a string"))
          | tok => fail(token_error(tok))
          }
      )
    and variant_end = (k, v) =>
      reader()
      >>= (
        tok =>
          switch (tok) {
          | VE => return(Compliance.variant(k, v))
          | tok => fail(token_error(tok))
          }
      );

    reader()
    >>= (
      tok =>
        switch (tok) {
        | EOF => return(None)
        | tok => token_value(tok) >>=? (res => return(Some(res)))
        }
    );
  };

  let decode = (~reader) =>
    json_value(~reader)
    >>= (
      fun
      | Ok(res) => return(res)
      | Error(`Syntax_error(err)) => fail(err)
      | Error(`Eof) => fail("unexpected end-of-input")
    );
};
