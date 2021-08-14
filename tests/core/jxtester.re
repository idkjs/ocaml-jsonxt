module CmdlineOptions = {
  let tester = (command, file, alco_opts) =>
    Printf.printf(
      "command: %s\nFile: %s\nAlcoTest: %s\n",
      command,
      file,
      String.concat(", ", alco_opts),
    );

  open Cmdliner;
  let alco_opts =
    Arg.(
      value & pos_all(string, []) & info([], ~docv="-- [AlcoTest options]")
    );
  let tfile =
    Arg.(
      required
      & opt(some(file), None)
      & info(["t", "tests"], ~docv="FILE", ~doc="file to get test data from")
    );

  let compliance_cmd = f => {
    let doc = "perform compliance checks with the various supported levels";
    let man = [
      `S(Manpage.s_description),
      `P(
        "run internal compliance testing as defined in -t FILE. This has the format:",
      ),
      `Pre("  <level> [pass|fail] <filename> [32|64|all]"),
      `P("Where the fields are defined as follows"),
      `I((
        "level",
        "one of strict, basic, extended, yjbasic and yjsafe. These correspond)\n           to the various compliance levels (yjbasic maps to Yojson.Basic etc)",
      )),
      `I(("[pass|fail]", "indicates the expected outcome")),
      `I((
        "<filename>",
        "the file containing the test, it is assumed to be in the current directory,\n           .json is automatically appended to the filename.",
      )),
      `I((
        "[32|64|all]",
        "is optional and defines the int size to run the test on, defaults to all.",
      )),
    ];

    (
      Term.(const(f) $ tfile $ alco_opts),
      Term.info("compliance", ~exits=Term.default_exits, ~doc, ~man),
    );
  };

  let validation_cmd = (gen, validate) => {
    let man = [
      `S(Manpage.s_description),
      `P(
        "run the parser test suite, parsing and verifying each of the\n          json strings defined in -t FILE. This is a tab seperated list\n          of json and expected sexp in the format:",
      ),
      `Pre("  bits <tab> json <tab> sexp <tab> sexp_strem"),
      `P("Where the fields are defined as follows"),
      `I((
        "bits",
        "bit size of platform to run test on. Either 32, 64 or all",
      )),
      `I(("json", "json to parse")),
      `I(("sexp", "is the expected sexp")),
      `I(("sexp_stream", "is the expected sexp from the stream parser")),
      `P(
        "When in gen mode the sexps are ignored and a file suitable for\n          using with run is output to stdout\n",
      ),
    ];

    let gen_run = (subcmd, tfile, alco_opts) =>
      switch (subcmd) {
      | "gen" =>
        gen(tfile, alco_opts);
        `Ok();
      | "run" =>
        validate(tfile, alco_opts);
        `Ok();
      | _ => `Error((true, "expected run or gen"))
      };

    let alco_opts =
      Arg.(
        value
        & pos_right(0, string, [])
        & info([], ~docv="-- [AlcoTest options]")
      );
    let doc = "run to run encode/decode, gen to generated validation data";
    let subcmd =
      Arg.(
        required
        & pos(0, some(string), None)
        & info([], ~docv="[gen|run]", ~doc)
      );
    let doc = "perform decode and encode validation";
    (
      Term.(ret(const(gen_run) $ subcmd $ tfile $ alco_opts)),
      Term.info("validation", ~exits=Term.default_exits, ~doc, ~man),
    );
  };

  let suite_cmd = (std_f, stream_f, monad_f, all_f) => {
    let suite_run = (subcmd, files) => {
      let files = List.filter(n => !Filename.check_suffix(n, ".exe"), files);
      switch (subcmd) {
      | "std" =>
        std_f(files);
        `Ok();
      | "stream" =>
        stream_f(files);
        `Ok();
      | "monad" =>
        monad_f(files);
        `Ok();
      | "all" =>
        all_f(files);
        `Ok();
      | _ => `Error((true, "expected std, stream or monad"))
      };
    };

    let doc = "Process the JsonSuite files available from https://github.com/nst/JSONTestSuite";
    let subcmd =
      Arg.(
        required
        & pos(0, some(string), None)
        & info([], ~docv="[std|stream|monad]", ~doc)
      );
    let files =
      Arg.(value & pos_right(0, string, []) & info([], ~docv="FILES"));
    (
      Term.(ret(const(suite_run) $ subcmd $ files)),
      Term.info("suite", ~exits=Term.default_exits, ~doc),
    );
  };

  let default_cmd = {
    let exits = Term.default_exits;
    (
      Term.(ret(const(_ => `Help((`Pager, None))) $ alco_opts)),
      Term.info("jxtester", ~version="%%VERSION%%", ~exits),
    );
  };

  let command =
    Arg.(
      required
      & pos(0, some(string), None)
      & info([], ~docv="[compliance|validation]")
    );

  let cmd = (
    Term.(const(tester) $ command $ tfile $ alco_opts),
    Term.info("jxtester", ~version="%%VERSION%%", ~exits=Term.default_exits),
  );
};

let cmds = [
  CmdlineOptions.compliance_cmd(ComplianceTests.run_tests),
  CmdlineOptions.validation_cmd(
    ValidationTests.gen_config,
    ValidationTests.run_tests,
  ),
  CmdlineOptions.suite_cmd(
    JsonTestSuite.test_suite_std,
    JsonTestSuite.test_suite_stream,
    JsonTestSuite.test_suite_monad,
    JsonTestSuite.test_suite_all,
  ),
];
let () =
  Cmdliner.Term.(exit @@ eval_choice(CmdlineOptions.default_cmd, cmds));
