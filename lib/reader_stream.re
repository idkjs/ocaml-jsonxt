module type Reader_stream = {
  type json_stream;
  type stream;

  let json_stream_of_string: string => stream;
  let json_stream_of_channel: in_channel => stream;
  let json_stream_of_function: ((bytes, int) => int) => stream;
  let decode_stream: stream => result(option(json_stream), string);
  let stream_from_string: string => Stream.t(json_stream);
  let stream_from_channel: in_channel => Stream.t(json_stream);
  let stream_from_function: ((bytes, int) => int) => Stream.t(json_stream);
};

module Make =
       (Lexxer: Compliant_lexxer.Lex, Parser: Parser_stream.Parser)
       : (Reader_stream with type json_stream = Parser.Compliance.json_stream) => {
  type json_stream = Parser.Compliance.json_stream;
  type stream = {
    parser_t: Parser.t,
    lexbuf: Lexing.lexbuf,
  };

  let create_parser = (~lexbuf) => {
    let reader = () => Lexxer.read(lexbuf);
    {parser_t: Parser.create(~reader), lexbuf};
  };

  let json_stream_of_string = s => {
    let lexbuf = Lexing.from_string(s);
    create_parser(~lexbuf);
  };

  let json_stream_of_channel = inc => {
    let lexbuf = Lexing.from_channel(inc);
    create_parser(~lexbuf);
  };

  let json_stream_of_function = f => {
    let lexbuf = Lexing.from_function(f);
    create_parser(~lexbuf);
  };

  let decode_stream = t =>
    switch (Parser.decode(t.parser_t)) {
    | Error(err) =>
      let err_info = Error_info.create_from_lexbuf(t.lexbuf, err);
      Error(Error_info.to_string(err_info));
    | v => v
    };

  let stream_from_lexbuf = lexbuf => {
    let decoder = create_parser(~lexbuf);
    let f = _i =>
      switch (decode_stream(decoder)) {
      | Ok(None) => None
      | Ok(Some(v)) => Some(v)
      | Error(err) => raise(Failure(err))
      };

    Stream.from(f);
  };

  let stream_from_string = s => {
    let lexbuf = Lexing.from_string(s);
    stream_from_lexbuf(lexbuf);
  };

  let stream_from_channel = inc => {
    let lexbuf = Lexing.from_channel(inc);
    stream_from_lexbuf(lexbuf);
  };

  let stream_from_function = f => {
    let lexbuf = Lexing.from_function(f);
    stream_from_lexbuf(lexbuf);
  };
};
