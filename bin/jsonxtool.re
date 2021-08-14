open Printf;

let die = msg => {
  printf("ERROR: %s\n", msg);
  exit(255);
};

let help_error = err =>
  switch (err) {
  | Some(err) => printf("ERROR: %s\n\n", err)
  | None => ()
  };

let help_msg = (msg, err) => {
  help_error(err);
  printf("%s", msg);
  exit(0);
};

let load_file = f => {
  let ic =
    if (String.equal(f, "-")) {
      stdin;
    } else {
      open_in(f);
    };
  let buf = Buffer.create(100);
  let rec loop = () =>
    try(
      {
        Buffer.add_channel(buf, ic, 4096);
        loop();
      }
    ) {
    | End_of_file => ()
    };

  loop();
  close_in(ic);
  Buffer.contents(buf);
};

let command_dump = (idx, sys_argv_len) => {
  let usage = "jsonxtool dump [-help|-h] [-compliance [strict|basic|extended|yjbasic|yjsafe]] file\n\n   Load the specified file or stdin of file is minus (-) and dump out in internal tree format.\n   The compliance level can be selected, defaulting to extended.\n";

  let rdstdin = ref(false);
  let file = ref("");
  let compliance = ref(`Extended);
  let set_compliance =
    fun
    | "strict" => compliance := `Strict
    | "basic" => compliance := `Basic
    | "extended" => compliance := `Extended
    | "yjbasic" => compliance := `Yojson_basic
    | "yjsafe" => compliance := `Yojson_safe
    | level => die("unknown compliance level: " ++ level);

  let anon = arg =>
    if (String.equal(file^, "") && ! rdstdin^) {
      file := arg;
    } else {
      die("only one file maybe dumped");
    };

  let set_rdstdin = () =>
    if (String.equal(file^, "") && ! rdstdin^) {
      rdstdin := true;
    } else {
      die("only one file maybe dumped");
    };

  let argspec = [
    (
      "-compliance",
      [@implicit_arity]
      Arg.Symbol(
        ["strict", "basic", "extended", "yjbasic", "yjsafe"],
        set_compliance,
      ),
      "compliance level",
    ),
    ("-", Unit(set_rdstdin), "read from stdin"),
  ];

  let current = ref(idx - 1);
  if (sys_argv_len < 3) {
    help_msg(usage, Some("dump expected at least one option"));
  };
  try(Arg.parse_argv(~current, Sys.argv, argspec, anon, usage)) {
  | Arg.Bad(err) => die(err)
  | Arg.Help(msg) => help_msg(msg, None)
  };
  if (rdstdin^) {
    file := "-";
  };
  if (String.equal(file^, "")) {
    die("expected file to dump");
  };
  let contents = load_file(file^);
  let dump =
    try(
      switch (compliance^) {
      | `Strict =>
        let json = Jsonxt.Strict.of_string(contents);
        Jsonxt.Utilities.json_to_string_repr(json);
      | `Basic =>
        let json = Jsonxt.Basic.of_string(contents);
        Jsonxt.Utilities.json_to_string_repr(json);
      | `Extended =>
        let json = Jsonxt.Extended.of_string(contents);
        Jsonxt.Utilities.json_to_string_repr(json);
      | `Yojson_basic =>
        let json = Jsonxt.Yojson.Basic.from_string(contents);
        Jsonxt.Utilities.json_to_string_repr(json);
      | `Yojson_safe =>
        let json = Jsonxt.Yojson.Safe.from_string(contents);
        Jsonxt.Utilities.json_to_string_repr(json);
      }
    ) {
    | Failure(err) => sprintf("Parse failed with: %s", err)
    };

  printf("%s\n", dump);
};

let () = {
  let usage = "jsonxtool [help|dump]\n";
  let sys_argv_len = Array.length(Sys.argv);
  if (sys_argv_len < 2) {
    help_msg(usage, Some("expected command"));
  };
  switch (Sys.argv[1]) {
  | "help" => help_msg(usage, None)
  | "dump" => command_dump(2, sys_argv_len)
  | _ => help_msg(usage, Some("unknown command " ++ Sys.argv[1]))
  };
};
