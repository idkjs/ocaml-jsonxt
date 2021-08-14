let string_parse_std = jsons =>
  switch (Jsonxt.Strict.json_of_string(jsons)) {
  | Ok(_) => `Pass
  | Error(_) => `Fail
  };

let string_parse_stream = jsons => {
  let stream = Jsonxt.Strict_stream.json_stream_of_string(jsons);
  let rec loop = () =>
    switch (Jsonxt.Strict_stream.decode_stream(stream)) {
    | Error(_) => `Fail
    | Ok(None) => `Pass
    | Ok(Some(_)) => loop()
    };

  loop();
};

let string_parse_monad = jsons => {
  open Utils.IO;
  let iobuf = Utils.StringIO.create(jsons);
  let reader = (buf, len) =>
    Utils.StringIO.read(iobuf, buf, len) |> Utils.IO.return;
  module JsonIO = Jsonxt.Strict_monad.Make(Utils.IO);
  switch (result(JsonIO.read_json(~reader, ()))) {
  | Ok(_) => `Pass
  | Error(_) => `Fail
  };
};

let filename_to_success = filename => {
  let name = Filename.basename(filename);
  switch (name.[0]) {
  | 'y'
  | 'Y' => `Pass
  | 'n'
  | 'N' => `Fail
  | 'i'
  | 'I' => `Undefined
  | _ => `Undefined
  };
};

let result_to_string =
  fun
  | `Pass => "pass"
  | `Fail => "fail"
  | `Undefined => "undef";

let pass_fail = {
  let pp = (ppf, v) => Fmt.pf(ppf, "%s", result_to_string(v));
  let pass_fail_eq = (expected, result) =>
    switch (expected, result) {
    | (`Pass, `Pass) => true
    | (`Fail, `Fail) => true
    | (`Undefined, _) => true
    | _ => false
    };

  Alcotest.testable(pp, pass_fail_eq);
};

let test_parse_file = (filename, parser_f, ()) => {
  let jsons = Utils.load_file(filename);
  let expected = filename_to_success(filename);
  let result = parser_f(jsons);
  Alcotest.(check(pass_fail))(filename, expected, result);
};

let gen_tests = (parser_name, parser_f, files) => {
  let create_test = file => {
    let msg = Filename.basename(file);
    Alcotest.test_case(msg, `Quick, test_parse_file(file, parser_f));
  };

  let tests = List.map(create_test, files);
  [(parser_name, tests)];
};

let test_suite_std = files => {
  let alco_opts = [];
  let argv = Array.of_list(["suite", ...alco_opts]);
  let alco_tests = gen_tests("standard", string_parse_std, files);
  Alcotest.run(~argv, "Suite", alco_tests);
};

let test_suite_stream = files => {
  let alco_opts = [];
  let argv = Array.of_list(["suite", ...alco_opts]);
  let alco_tests = gen_tests("stream", string_parse_stream, files);
  Alcotest.run(~argv, "Suite", alco_tests);
};

let test_suite_monad = files => {
  let alco_opts = [];
  let argv = Array.of_list(["suite", ...alco_opts]);
  let alco_tests = gen_tests("monad", string_parse_monad, files);
  Alcotest.run(~argv, "Suite", alco_tests);
};

let test_suite_all = files => {
  let alco_opts = [];
  let argv = Array.of_list(["suite", ...alco_opts]);
  let alco_tests =
    gen_tests("standard", string_parse_std, files)
    @ gen_tests("stream", string_parse_monad, files)
    @ gen_tests("monad", string_parse_monad, files);

  Alcotest.run(~argv, "Suite", alco_tests);
};
