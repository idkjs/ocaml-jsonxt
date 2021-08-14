module type Reader_string_file = {
  type json;

  let json_of_string: string => result(json, string);
  let json_of_string_exn: string => json;
  let json_of_file: string => result(json, string);
  let json_of_file_exn: string => json;
  let json_of_channel: in_channel => result(json, string);
  let json_of_channel_exn: in_channel => json;
  let json_of_function: ((bytes, int) => int) => result(json, string);
  let json_of_function_exn: ((bytes, int) => int) => json;
  let json_of_lexbuf: Lexing.lexbuf => result(json, string);
  let json_of_lexbuf_exn: Lexing.lexbuf => json;
  let of_string: string => json;
  let of_file: string => json;
  let of_channel: in_channel => json;
  let of_function: ((bytes, int) => int) => json;

  let json_of_string_error_info: string => result(json, Error_info.t);
  let json_of_file_error_info: string => result(json, Error_info.t);
  let json_of_channel_error_info: in_channel => result(json, Error_info.t);
  let json_of_function_error_info:
    ((bytes, int) => int) => result(json, Error_info.t);
  let json_of_lexbuf_error_info: Lexing.lexbuf => result(json, Error_info.t);
  let json_of_lexbuf_error_info_compat:
    (~stream: bool=?, Lexing.lexbuf) => result(option(json), Error_info.t);

  let stream_from_string: string => Stream.t(json);
  let stream_from_channel:
    (~fin: unit => unit=?, in_channel) => Stream.t(json);
  let stream_from_file: string => Stream.t(json);
  let stream_from_function: ((bytes, int) => int) => Stream.t(json);
  let stream_from_lexbuf: Lexing.lexbuf => Stream.t(json);

  let stream_from_string_error_info: string => Stream.t(json);
  let stream_from_channel_error_info:
    (~fin: unit => unit=?, in_channel) => Stream.t(json);
  let stream_from_file_error_info: string => Stream.t(json);
  let stream_from_function_error_info:
    ((bytes, int) => int) => Stream.t(json);
  let stream_from_lexbuf_error_info: Lexing.lexbuf => Stream.t(json);
};

module Make =
       (Lexxer: Compliant_lexxer.Lex, Parser: Parser.Parser)
       : (Reader_string_file with type json = Parser.Compliance.json) => {
  type json = Parser.Compliance.json;

  let read_json' = (~lexbuf) => {
    let reader = () => Lexxer.read(lexbuf);
    switch (Parser.decode(~reader)) {
    | Ok(None) => Error("empty input")
    | Ok(Some(res)) =>
      switch (reader()) {
      | EOF => Ok(res)
      | exception (Lexxer_utils.Lex_error(err)) => Error(err)
      | tok =>
        Error(
          "junk after end of JSON value: " ++ Token_utils.token_to_string(tok),
        )
      }
    | Error(s) => Error(s)
    };
  };

  let read_json = (~lexbuf) =>
    switch (read_json'(~lexbuf)) {
    | Ok(_) as res => res
    | Error(s) =>
      let err_info = Error_info.create_from_lexbuf(lexbuf, s);
      Error(Error_info.to_string(err_info));
    };

  let json_of_string = s => {
    let lexbuf = Lexing.from_string(s);
    read_json(~lexbuf);
  };

  let json_of_string_exn = s =>
    switch (json_of_string(s)) {
    | Ok(res) => res
    | Error(s) => raise(Failure(s))
    };

  let of_string = s => json_of_string_exn(s);

  let json_of_file = filename =>
    try({
      let inc = open_in(filename);
      let lexbuf = Lexing.from_channel(inc);
      let res = read_json(~lexbuf);
      close_in(inc);
      res;
    }) {
    | Sys_error(err) => Error(err)
    };

  let json_of_file_exn = filename =>
    switch (json_of_file(filename)) {
    | Ok(res) => res
    | Error(s) => raise(Failure(s))
    };

  let json_of_channel = inc => {
    let lexbuf = Lexing.from_channel(inc);
    read_json(~lexbuf);
  };

  let json_of_channel_exn = inc =>
    switch (json_of_channel(inc)) {
    | Ok(res) => res
    | Error(s) => raise(Failure(s))
    };

  let json_of_function = f => {
    let lexbuf = Lexing.from_function(f);
    read_json(~lexbuf);
  };

  let json_of_lexbuf = lexbuf => read_json(~lexbuf);

  let json_of_lexbuf_exn = lexbuf =>
    switch (json_of_lexbuf(lexbuf)) {
    | Ok(res) => res
    | Error(s) => raise(Failure(s))
    };

  let json_of_function_exn = f =>
    switch (json_of_function(f)) {
    | Ok(res) => res
    | Error(s) => raise(Failure(s))
    };

  let of_file = json_of_file_exn;
  let of_channel = json_of_channel_exn;
  let of_function = json_of_function_exn;

  /* Error_info.t returning functions */

  let read_json_error_info = (~lexbuf) =>
    switch (read_json'(~lexbuf)) {
    | Ok(_) as res => res
    | Error(err) =>
      let err_info = Error_info.create_from_lexbuf(lexbuf, err);
      Error(err_info);
    };

  let json_of_string_error_info = s => {
    let lexbuf = Lexing.from_string(s);
    read_json_error_info(~lexbuf);
  };

  let json_of_channel_error_info = inc => {
    let lexbuf = Lexing.from_channel(inc);
    read_json_error_info(~lexbuf);
  };

  let json_of_file_error_info = filename =>
    try({
      let inc = open_in(filename);
      let res = json_of_channel_error_info(inc);
      close_in(inc);
      res;
    }) {
    | Sys_error(err) =>
      Error({Error_info.line: 0, start_char: 0, end_char: 0, msg: err})
    };

  let json_of_function_error_info = f => {
    let lexbuf = Lexing.from_function(f);
    read_json_error_info(~lexbuf);
  };

  let json_of_lexbuf_error_info = lexbuf => read_json_error_info(~lexbuf);

  /* Internal compatibility function supporting the stream flag */

  let json_of_lexbuf_error_info_compat = (~stream=false, lexbuf) => {
    let reader = () => Lexxer.read(lexbuf);
    let res =
      switch (Parser.decode(~reader)) {
      | Ok(None) => if (stream) {Ok(None)} else {Error("empty input")}
      | Ok(Some(res)) =>
        stream
          ? Ok(Some(res))
          : (
            switch (reader()) {
            | EOF => Ok(Some(res))
            | exception (Lexxer_utils.Lex_error(err)) => Error(err)
            | tok =>
              Error(
                "junk after end of JSON value: "
                ++ Token_utils.token_to_string(tok),
              )
            }
          )
      | Error(s) => Error(s)
      };

    switch (res) {
    | Ok(res) => Ok(res)
    | Error(s) =>
      let err_info = Error_info.create_from_lexbuf(lexbuf, s);
      Error(err_info);
    };
  };

  /* Stream.t returning functions */

  let read_json_stream = (~fin, ~lexbuf) => {
    let reader = () => Lexxer.read(lexbuf);
    let f = _i =>
      switch (Parser.decode(~reader)) {
      | Ok(None) =>
        fin();
        None;
      | Ok(Some(res)) => Some(res)
      | Error(err) =>
        let () = fin();
        let err_info = Error_info.create_from_lexbuf(lexbuf, err);
        let msg = Error_info.to_string(err_info);
        raise(Failure(msg));
      };

    Stream.from(f);
  };

  let stream_from_string = s => {
    let lexbuf = Lexing.from_string(s);
    read_json_stream(~fin=() => (), ~lexbuf);
  };

  let stream_from_channel = (~fin=() => (), inc) => {
    let lexbuf = Lexing.from_channel(inc);
    read_json_stream(~fin, ~lexbuf);
  };

  let stream_from_function = f => {
    let lexbuf = Lexing.from_function(f);
    read_json_stream(~fin=() => (), ~lexbuf);
  };

  let stream_from_file = filename => {
    let inc = open_in(filename);
    stream_from_channel(~fin=() => close_in(inc), inc);
  };

  let stream_from_lexbuf = lexbuf => read_json_stream(~fin=() => (), ~lexbuf);

  /* Stream.t Json_error_info raising functions */

  let read_json_stream_error_info = (~fin, ~lexbuf) => {
    let reader = () => Lexxer.read(lexbuf);
    let f = _i =>
      switch (Parser.decode(~reader)) {
      | Ok(None) =>
        fin();
        None;
      | Ok(Some(res)) => Some(res)
      | Error(err) =>
        let () = fin();
        let err_info = Error_info.create_from_lexbuf(lexbuf, err);
        raise(Error_info.Json_error_info(err_info));
      };

    Stream.from(f);
  };

  let stream_from_string_error_info = s => {
    let lexbuf = Lexing.from_string(s);
    read_json_stream_error_info(~fin=() => (), ~lexbuf);
  };

  let stream_from_channel_error_info = (~fin=() => (), inc) => {
    let lexbuf = Lexing.from_channel(inc);
    read_json_stream_error_info(~fin, ~lexbuf);
  };

  let stream_from_function_error_info = f => {
    let lexbuf = Lexing.from_function(f);
    read_json_stream_error_info(~fin=() => (), ~lexbuf);
  };

  let stream_from_file_error_info = filename => {
    let inc = open_in(filename);
    stream_from_channel_error_info(~fin=() => close_in(inc), inc);
  };

  let stream_from_lexbuf_error_info = lexbuf =>
    read_json_stream_error_info(~fin=() => (), ~lexbuf);
};
