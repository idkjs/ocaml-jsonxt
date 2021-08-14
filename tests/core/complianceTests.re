let read_test_data = inc => {
  let l = input_line(inc);
  let p =
    Utils.split_string(' ', l) |> List.filter(v => !String.equal("", v));
  let (level, passfail, filename, bits) =
    switch (p) {
    | [lv, pf, fn, "32"] => (lv, pf, fn, "32")
    | [lv, pf, fn, "64"] => (lv, pf, fn, "64")
    | [lv, pf, fn, "all"] => (lv, pf, fn, "all")
    | [lv, pf, fn] => (lv, pf, fn, "all")
    | _ => Utils.die("invalid test line: " ++ l)
    };

  let level =
    switch (level) {
    | "strict" => `Strict
    | "basic" => `Basic
    | "extended" => `Extended
    | "yjsafe" => `Yojson_safe
    | "yjbasic" => `Yojson_basic
    | _ => Utils.die("invalid test line, first column invalid level: " ++ l)
    };

  let passfail =
    switch (passfail) {
    | ("pass" | "fail") as v => v
    | _ =>
      Utils.die(
        "invalid test line, second column must be pass or fail: " ++ l,
      )
    };

  (level, passfail, filename, bits);
};

let get_json_stream = (of_string, decode, jsons) => {
  let stream = of_string(jsons);
  let rec loop = res =>
    switch (decode(stream)) {
    | Error(err) => Error(err)
    | Ok(None) => Ok(List.rev(res))
    | Ok(Some(tok)) => loop([tok, ...res])
    };

  loop([]);
};

let of_error = (f, a) =>
  switch (f(a)) {
  | Ok(_) => "pass"
  | Error(_) => "fail"
  };

let level_to_string =
  fun
  | `Strict => "strict"
  | `Basic => "basic"
  | `Extended => "extended"
  | `Yojson_safe => "yjsafe"
  | `Yojson_basic => "yjbasic";

let string_parse_test = (level, filename, _passfail) => {
  let yojson_basic_json_of_string = s =>
    try(Ok(Jsonxt.Yojson.Basic.from_string(s))) {
    | Failure(err) => Error(err)
    | Jsonxt.Yojson.Json_error(err) => Error(err)
    };

  let yojson_safe_json_of_string = s =>
    try(Ok(Jsonxt.Yojson.Safe.from_string(s))) {
    | Failure(err) => Error(err)
    | Jsonxt.Yojson.Json_error(err) => Error(err)
    };

  let txt =
    try(Utils.load_file(filename ++ ".json")) {
    | Sys_error(err) => Utils.die(err)
    };
  let string_parser =
    switch (level) {
    | `Strict => of_error(Jsonxt.Strict.json_of_string)
    | `Basic => of_error(Jsonxt.Basic.json_of_string)
    | `Extended => of_error(Jsonxt.Extended.json_of_string)
    | `Yojson_basic => of_error(yojson_basic_json_of_string)
    | `Yojson_safe => of_error(yojson_safe_json_of_string)
    };

  string_parser(txt);
};

let file_parse_test = (level, filename, _passfail) => {
  let yojson_basic_json_of_file = filename =>
    try(Ok(Jsonxt.Yojson.Basic.from_file(filename))) {
    | Failure(err) => Error(err)
    | Jsonxt.Yojson.Json_error(err) => Error(err)
    };

  let yojson_safe_json_of_file = filename =>
    try(Ok(Jsonxt.Yojson.Safe.from_file(filename))) {
    | Failure(err) => Error(err)
    | Jsonxt.Yojson.Json_error(err) => Error(err)
    };

  let file_parser =
    switch (level) {
    | `Strict => of_error(Jsonxt.Strict.json_of_file)
    | `Basic => of_error(Jsonxt.Basic.json_of_file)
    | `Extended => of_error(Jsonxt.Extended.json_of_file)
    | `Yojson_basic => of_error(yojson_basic_json_of_file)
    | `Yojson_safe => of_error(yojson_safe_json_of_file)
    };

  file_parser(filename ++ ".json");
};

let stream_parse_test = (level, filename, passfail) => {
  let txt =
    try(Utils.load_file(filename ++ ".json")) {
    | Sys_error(err) => Utils.die(err)
    };
  let stream_parser =
    switch (level) {
    | `Strict =>
      of_error(
        get_json_stream(
          Jsonxt.Strict_stream.json_stream_of_string,
          Jsonxt.Strict_stream.decode_stream,
        ),
      )
    | `Basic =>
      of_error(
        get_json_stream(
          Jsonxt.Basic_stream.json_stream_of_string,
          Jsonxt.Basic_stream.decode_stream,
        ),
      )
    | `Extended =>
      of_error(
        get_json_stream(
          Jsonxt.Extended_stream.json_stream_of_string,
          Jsonxt.Extended_stream.decode_stream,
        ),
      )
    | `Yojson_basic => (_ => passfail)
    | `Yojson_safe => (_ => passfail)
    };

  stream_parser(txt);
};

let monad_parse_test = (level, filename, passfail) => {
  open Utils.IO;
  let txt =
    try(Utils.load_file(filename ++ ".json")) {
    | Sys_error(err) => Utils.die(err)
    };
  let iobuf = Utils.StringIO.create(txt);
  let reader = (buf, len) =>
    Utils.StringIO.read(iobuf, buf, len) |> Utils.IO.return;
  let of_error = res =>
    switch (res) {
    | Ok(_) => return("pass")
    | Error(_) => return("fail")
    };
  let result =
    switch (level) {
    | `Strict =>
      module JsonIO = Jsonxt.Strict_monad.Make(Utils.IO);
      JsonIO.read_json(~reader, ()) >>= of_error;
    | `Basic =>
      module JsonIO = Jsonxt.Basic_monad.Make(Utils.IO);
      JsonIO.read_json(~reader, ()) >>= of_error;
    | `Extended =>
      module JsonIO = Jsonxt.Extended_monad.Make(Utils.IO);
      JsonIO.read_json(~reader, ()) >>= of_error;
    | `Yojson_basic => return(passfail) /* monad isn't supported by Yojson */
    | `Yojson_safe => return(passfail)
    };

  Utils.IO.result(result);
};

let tester = (f, level, filename, passfail, ()) => {
  let slevel = level_to_string(level);
  let msg = slevel ++ " " ++ filename;
  Alcotest.(check(string))(msg, passfail, f(level, filename, passfail));
};

let gen_tests = filename => {
  let inc =
    try(open_in(filename)) {
    | Sys_error(err) => Utils.die(err)
    };
  let rec loop = (str, file, monad, stream) =>
    switch (read_test_data(inc)) {
    | (level, passfail, filename, bits) =>
      let msg = filename ++ " " ++ level_to_string(level);
      let stest =
        Alcotest.test_case(
          msg,
          `Quick,
          tester(string_parse_test, level, filename, passfail),
        );
      let ftest =
        Alcotest.test_case(
          msg,
          `Quick,
          tester(file_parse_test, level, filename, passfail),
        );
      let mtest =
        Alcotest.test_case(
          msg,
          `Quick,
          tester(monad_parse_test, level, filename, passfail),
        );
      let ttest =
        Alcotest.test_case(
          msg,
          `Quick,
          tester(stream_parse_test, level, filename, passfail),
        );
      switch (bits) {
      | "64" when Utils.int_bits == 32 => loop(str, file, monad, stream)
      | "32" when Utils.int_bits == 64 => loop(str, file, monad, stream)
      | _ =>
        loop(
          [stest, ...str],
          [ftest, ...file],
          [mtest, ...monad],
          [ttest, ...stream],
        )
      };
    | exception End_of_file => (str, file, monad, stream)
    };

  let (str_t, file_t, monad_t, stream_t) = loop([], [], [], []);
  [
    ("string", List.rev(str_t)),
    ("file", List.rev(file_t)),
    ("monad", List.rev(monad_t)),
    ("stream", List.rev(stream_t)),
  ];
};

let run_tests = (filename, alco_opts) => {
  let argv = Array.of_list(["compliance", ...alco_opts]);
  Alcotest.run(~argv, "Compliance", gen_tests(filename));
};
