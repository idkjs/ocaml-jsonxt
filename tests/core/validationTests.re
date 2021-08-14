module JsonSexp = {
  open Core_kernel;
  [@deriving sexp]
  type json = [
    | `Null
    | `Bool(bool)
    | `Int(int)
    | `Intlit(string)
    | `Float(float)
    | `Floatlit(string)
    | `String(string)
    | `Stringlit(string)
    | `Assoc(list((string, json)))
    | `List(list(json))
    | `Tuple(list(json))
    | `Variant(string, option(json))
  ];
};

module JsonStreamSexp = {
  open Core_kernel;
  [@deriving sexp]
  type json_stream =
    list(
      [
        | `Null
        | `Bool(bool)
        | `Int(int)
        | `Intlit(string)
        | `Float(float)
        | `Floatlit(string)
        | `String(string)
        | `Stringlit(string)
        | `As
        | `Ae
        | `Os
        | `Oe
        | `Ts
        | `Te
        | `Vs
        | `Ve
        | `Name(string)
        | `Infinity
        | `Neg_infinity
        | `Nan
      ],
    );
};

let regexp_nan = Str.regexp_string("__NAN__");
let string_nan = String.uppercase_ascii(string_of_float(0.0 /. 0.0));

let read_json_sexp = inc => {
  let l = input_line(inc);
  let p = Utils.split_string('\t', l);
  let (bits, jsons, sexps, sexps_json_stream) =
    switch (p) {
    | [bt, jv, sv, ssv] => (bt, jv, sv, ssv)
    | [bt, jv] => (bt, jv, "", "")
    | _ => Utils.die("invalid test line: " ++ l)
    };

  /* Replace __NAN__ with the architecture specific version */
  let sexps = Str.global_replace(regexp_nan, string_nan, sexps);
  let sexps_json_stream =
    Str.global_replace(regexp_nan, string_nan, sexps_json_stream);
  (bits, jsons, sexps, sexps_json_stream);
};

let output_validation_config = (bits, jsons, json, json_stream) =>
  Printf.printf(
    "%s\t%s\t%s\t%s\n",
    bits,
    jsons,
    JsonSexp.sexp_of_json(json) |> Core_kernel.Sexp.to_string,
    JsonStreamSexp.sexp_of_json_stream(json_stream)
    |> Core_kernel.Sexp.to_string,
  );

let get_json_stream = jsons => {
  let stream = Jsonxt.Extended_stream.json_stream_of_string(jsons);
  let rec loop = res =>
    switch (Jsonxt.Extended_stream.decode_stream(stream)) {
    | Error(err) => Error(err)
    | Ok(None) => Ok(List.rev(res))
    | Ok(Some(tok)) => loop([tok, ...res])
    };

  loop([]);
};

let parse_json = jsons => {
  let json_result = Jsonxt.Extended.json_of_string(jsons);
  let json_stream_result = get_json_stream(jsons);
  let json =
    switch (json_result) {
    | Ok(json) => json
    | Error(err) =>
      Printf.sprintf("failed to parse \"%s\": %s", jsons, err) |> Utils.die
    };

  let json_stream =
    switch (json_stream_result) {
    | Ok(json_stream) => json_stream
    | Error(err) =>
      Printf.sprintf("failed to parse stream \"%s\": %s", jsons, err)
      |> Utils.die
    };

  (json, json_stream);
};

let gen_config = (filename, _alco_opts) => {
  let inc =
    try(open_in(filename)) {
    | Sys_error(err) => Utils.die(err)
    };
  let rec loop = () =>
    switch (read_json_sexp(inc)) {
    | (bits, jsons, sexps, sexps_json_stream) =>
      switch (bits) {
      | "64" when Utils.int_bits == 32 =>
        Printf.printf(
          "%s\t%s\t%s\t%s\n",
          bits,
          jsons,
          sexps,
          sexps_json_stream,
        )
      | "32" when Utils.int_bits == 64 =>
        Printf.printf(
          "%s\t%s\t%s\t%s\n",
          bits,
          jsons,
          sexps,
          sexps_json_stream,
        )
      | _ =>
        let (json, json_stream) = parse_json(jsons);
        output_validation_config(bits, jsons, json, json_stream);
      };
      loop();
    | exception End_of_file => ()
    };
  loop();
};

let get_json_stream = jsons => {
  let stream = Jsonxt.Extended_stream.json_stream_of_string(jsons);
  let rec loop = res =>
    switch (Jsonxt.Extended_stream.decode_stream(stream)) {
    | Error(err) => Error(err)
    | Ok(None) => Ok(List.rev(res))
    | Ok(Some(tok)) => loop([tok, ...res])
    };

  loop([]);
};

let json_stream_to_string = stream => {
  let buf = Buffer.create(100);
  let add_char = c => Buffer.add_char(buf, c);
  let add_string = s => Buffer.add_string(buf, s);
  let encoder = Jsonxt.Extended_stream.create_encoder(~add_char, ~add_string);
  let encode = () => {
    List.iter(
      v => Jsonxt.Extended_stream.encode_stream_exn(encoder, v),
      stream,
    );
    Buffer.contents(buf);
  };

  try(Ok(encode())) {
  | Failure(err) => Error(err)
  };
};

let sexp = {
  let pp = (ppf, v) => Fmt.pf(ppf, "%s", Core_kernel.Sexp.to_string(v));
  let sexp_eq = (a, b) =>
    switch (Core_kernel.Sexp.compare(a, b)) {
    | 0 => true
    | _ => false
    };
  Alcotest.testable(pp, sexp_eq);
};

let string_parse_test = (jsons, sexps, ()) => {
  let jsonsexp =
    switch (Jsonxt.Extended.json_of_string(jsons)) {
    | Ok(json) => JsonSexp.sexp_of_json(json)
    | Error(err) =>
      Core_kernel.Sexp.Atom(
        Printf.sprintf("Failed to parse '%s': %s", jsons, err),
      )
    };

  let sexpv = Core_kernel.Sexp.of_string(sexps);
  Alcotest.(check(sexp))(jsons, sexpv, jsonsexp);
};

let stream_t_parse_test = (jsons, sexps, ()) => {
  let jsons = String.concat(" ", [jsons, jsons, jsons]);
  let sexps = String.concat("", ["(", sexps, sexps, sexps, ")"]);
  let stream = Jsonxt.Extended.stream_from_string(jsons);
  let rec loop = res =>
    switch (Stream.next(stream)) {
    | exception Stream.Failure => Core_kernel.Sexp.List(List.rev(res))
    | v => loop([JsonSexp.sexp_of_json(v), ...res])
    };

  let jsonsexp = loop([]);
  let sexpv = Core_kernel.Sexp.of_string(sexps);
  Alcotest.(check(sexp))(jsons, sexpv, jsonsexp);
};

let file_write_test = (jsons, sexps, tmpfile, ()) => {
  let jsonsexp =
    switch (Jsonxt.Extended.json_of_string(jsons)) {
    | Ok(json) =>
      switch (Jsonxt.Extended.json_to_file(tmpfile, json)) {
      | Ok () =>
        switch (Jsonxt.Extended.json_of_file(tmpfile)) {
        | Ok(json) => JsonSexp.sexp_of_json(json)
        | Error(err) =>
          let str = Utils.load_file(tmpfile);
          Core_kernel.Sexp.Atom(
            Printf.sprintf(
              "Failed to re-parse written json '%s': %s",
              str,
              err,
            ),
          );
        }
      | Error(err) =>
        Core_kernel.Sexp.Atom(
          Printf.sprintf("Failed to write parsed json '%s': %s", jsons, err),
        )
      }
    | Error(err) =>
      Core_kernel.Sexp.Atom(
        Printf.sprintf("Failed to parse '%s': %s", jsons, err),
      )
    };

  let sexpv = Core_kernel.Sexp.of_string(sexps);
  Alcotest.(check(sexp))(jsons, sexpv, jsonsexp);
};

let string_write_test' = (f, jsons, sexps, ()) => {
  let jsonsexp =
    switch (Jsonxt.Extended.json_of_string(jsons)) {
    | Ok(json) =>
      switch (f(json)) {
      | Ok(str) =>
        switch (Jsonxt.Extended.json_of_string(str)) {
        | Ok(json) => JsonSexp.sexp_of_json(json)
        | Error(err) =>
          Core_kernel.Sexp.Atom(
            Printf.sprintf(
              "Failed to re-parse written json '%s': %s",
              str,
              err,
            ),
          )
        }
      | Error(err) =>
        Core_kernel.Sexp.Atom(
          Printf.sprintf("Failed to write parsed json '%s': %s", jsons, err),
        )
      }
    | Error(err) =>
      Core_kernel.Sexp.Atom(
        Printf.sprintf("Failed to parse '%s': %s", jsons, err),
      )
    };

  let sexpv = Core_kernel.Sexp.of_string(sexps);
  Alcotest.(check(sexp))(jsons, sexpv, jsonsexp);
};

let string_write_test = (jsons, sexp) =>
  string_write_test'(Jsonxt.Extended.json_to_string, jsons, sexp);
let string_write_hum_test = (jsons, sexp) =>
  string_write_test'(Jsonxt.Extended.json_to_string_hum, jsons, sexp);

let stream_write_test = (jsons, sexp_streams, ()) => {
  let json_stream_sexp =
    switch (get_json_stream(jsons)) {
    | Ok(json_stream) =>
      switch (json_stream_to_string(json_stream)) {
      | Ok(str) =>
        switch (Jsonxt.Extended.json_of_string(str)) {
        | Ok(json) => JsonSexp.sexp_of_json(json)
        | Error(err) =>
          Core_kernel.Sexp.Atom(
            Printf.sprintf(
              "Failed to re-parse written json stream '%s': %s",
              str,
              err,
            ),
          )
        }
      | Error(err) =>
        Core_kernel.Sexp.Atom(
          Printf.sprintf(
            "Failed to write parsed json_stream '%s': %s",
            jsons,
            err,
          ),
        )
      }
    | Error(err) =>
      Core_kernel.Sexp.Atom(
        Printf.sprintf("Failed to stream parse '%s': %s", jsons, err),
      )
    };

  let sexpv = Core_kernel.Sexp.of_string(sexp_streams);
  Alcotest.(check(sexp))(jsons, sexpv, json_stream_sexp);
};

let stream_parse_test = (jsons, sexp_streams, ()) => {
  let json_stream_sexp =
    switch (get_json_stream(jsons)) {
    | Ok(json_stream) => JsonStreamSexp.sexp_of_json_stream(json_stream)
    | Error(err) =>
      Core_kernel.Sexp.Atom(
        Printf.sprintf("Failed to parse '%s': %s", jsons, err),
      )
    };

  let sexp_stream = Core_kernel.Sexp.of_string(sexp_streams);
  Alcotest.(check(sexp))(jsons, sexp_stream, json_stream_sexp);
};

let monad_read = jsons => {
  open Utils.IO;
  let iobuf = Utils.StringIO.create(jsons);
  let reader = (buf, len) =>
    Utils.StringIO.read(iobuf, buf, len) |> Utils.IO.return;
  module JsonIO = Jsonxt.Extended_monad.Make(Utils.IO);
  result(JsonIO.read_json(~reader, ()));
};

let monad_write = json => {
  open Utils.IO;
  let iobuf = Utils.StringIO.create("");
  let writer = s => Utils.StringIO.write(iobuf, s) |> Utils.IO.return;
  module JsonIO = Jsonxt.Extended_monad.Make(Utils.IO);
  let () = result(JsonIO.write_json(~writer, json));
  Utils.StringIO.contents(iobuf);
};

let monad_parse_test = (jsons, sexps, ()) => {
  let jsonsexp =
    switch (monad_read(jsons)) {
    | Ok(json) => JsonSexp.sexp_of_json(json)
    | Error(err) =>
      Core_kernel.Sexp.Atom(
        Printf.sprintf("Failed to parse '%s': %s", jsons, err),
      )
    };

  let sexpv = Core_kernel.Sexp.of_string(sexps);
  Alcotest.(check(sexp))(jsons, sexpv, jsonsexp);
};

let monad_write_test = (jsons, sexps, ()) => {
  let jsonsexp =
    switch (monad_read(jsons)) {
    | Ok(json) =>
      let str = monad_write(json);
      switch (Jsonxt.Extended.json_of_string(str)) {
      | Ok(json) => JsonSexp.sexp_of_json(json)
      | Error(err) =>
        Core_kernel.Sexp.Atom(
          Printf.sprintf(
            "Failed to re-parse written json '%s': %s",
            str,
            err,
          ),
        )
      };
    | Error(err) =>
      Core_kernel.Sexp.Atom(
        Printf.sprintf("Failed to parse '%s': %s", jsons, err),
      )
    };

  let sexpv = Core_kernel.Sexp.of_string(sexps);
  Alcotest.(check(sexp))(jsons, sexpv, jsonsexp);
};

let gen_tests = (filename, tmpfile) => {
  let filter = (a, l) =>
    List.filter(((k, _)) => k == a, l) |> List.map(((_, v)) => v);
  let inc =
    try(open_in(filename)) {
    | Sys_error(err) => Utils.die(err)
    };
  let rec loop = tests =>
    switch (read_json_sexp(inc)) {
    | (bits, jsons, sexps, sexps_json_stream) =>
      let msg = jsons;
      switch (bits) {
      | "64" when Utils.int_bits == 32 => loop(tests)
      | "32" when Utils.int_bits == 64 => loop(tests)
      | _ =>
        let tests = [
          (
            `Std,
            Alcotest.test_case(msg, `Quick, string_parse_test(jsons, sexps)),
          ),
          ...tests,
        ];
        let tests = [
          (
            `Stream,
            Alcotest.test_case(
              msg,
              `Quick,
              stream_parse_test(jsons, sexps_json_stream),
            ),
          ),
          ...tests,
        ];
        let tests = [
          (
            `Std_write,
            Alcotest.test_case(msg, `Quick, string_write_test(jsons, sexps)),
          ),
          ...tests,
        ];
        let tests = [
          (
            `Std_write_hum,
            Alcotest.test_case(
              msg,
              `Quick,
              string_write_hum_test(jsons, sexps),
            ),
          ),
          ...tests,
        ];
        let tests = [
          (
            `File_write,
            Alcotest.test_case(
              msg,
              `Quick,
              file_write_test(jsons, sexps, tmpfile),
            ),
          ),
          ...tests,
        ];
        let tests = [
          (
            `Stream_write,
            Alcotest.test_case(msg, `Quick, stream_write_test(jsons, sexps)),
          ),
          ...tests,
        ];
        let tests = [
          (
            `Monad,
            Alcotest.test_case(msg, `Quick, monad_parse_test(jsons, sexps)),
          ),
          ...tests,
        ];
        let tests = [
          (
            `Monad_write,
            Alcotest.test_case(msg, `Quick, monad_write_test(jsons, sexps)),
          ),
          ...tests,
        ];
        let tests = [
          (
            `Stream_t,
            Alcotest.test_case(
              msg,
              `Quick,
              stream_t_parse_test(jsons, sexps),
            ),
          ),
          ...tests,
        ];
        loop(tests);
      };
    | exception End_of_file => List.rev(tests)
    };

  let tests = loop([]);
  [
    ("standard", filter(`Std, tests)),
    ("standard-write-string", filter(`Std_write, tests)),
    ("standard-write-string-hum", filter(`Std_write_hum, tests)),
    ("standard-write-file", filter(`File_write, tests)),
    ("standard-streamt", filter(`Stream_t, tests)),
    ("stream", filter(`Stream, tests)),
    ("stream-write", filter(`Stream_write, tests)),
    ("monad", filter(`Monad, tests)),
    ("monad-write", filter(`Monad_write, tests)),
  ];
};

let run_tests = (filename, alco_opts) => {
  let argv = Array.of_list(["compliance", ...alco_opts]);
  let tmpfile = Filename.temp_file("jxtester", ".json");
  Alcotest.run(
    ~and_exit=false,
    ~argv,
    "Validation",
    gen_tests(filename, tmpfile),
  );
  try(Sys.remove(tmpfile)) {
  | Sys_error(_) => ()
  };
};
