module type Reader_monad = {
  module IO: Io.IO;
  type json;

  let read_json:
    (~stream: bool=?, ~reader: (Bytes.t, int) => IO.t(int), unit) =>
    IO.t(result(json, string));
};

module Make =
       (Parser: Parser_monad.Parser)

         : (
           Reader_monad with
             type json = Parser.Compliance.json and module IO := Parser.IO
       ) => {
  type json = Parser.Compliance.json;

  open Parser.IO;

  let create_lex_reader = reader => {
    module Lexxer =
      Compliant_lexxer_monad.Make(
        Parser.Compliance,
        {
          module IO = Parser.IO;
          include IO;
          let read = (buf, len) => reader(buf, len);
        },
      );

    lexbuf => Lexxer.read(lexbuf);
  };

  let read_json = (~stream=false, ~reader, ()) => {
    let lexbuf = Lexutils.create_lexbuf();
    let lex_reader = create_lex_reader(reader);
    let reader = () =>
      lex_reader(lexbuf)
      >>= (
        fun
        | Ok(tok) => return(tok)
        | Error(err) => return(Tokens.LEX_ERROR(err))
      );

    Parser.decode(~reader)
    >>= (
      fun
      | Ok(None) => return(Error("empty string"))
      | Ok(Some(res)) =>
        stream
          ? return(Ok(res))
          : reader()
            >>= (
              fun
              | EOF => return(Ok(res))
              | tok =>
                return(
                  Error(
                    "junk after end of JSON value: "
                    ++ Token_utils.token_to_string(tok),
                  ),
                )
            )
      | Error(s) => return(Error(s))
    )
    >>= (
      fun
      | Ok(_) as res => return(res)
      | Error(err) => {
          let err_info = Error_info.create_from_lexbuf(lexbuf, err);
          return(Error(Error_info.to_string(err_info)));
        }
    );
  };
};
