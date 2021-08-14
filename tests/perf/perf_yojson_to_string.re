let gen_data = () => {
  let record = "  {\n    \"foo\" : \"bar\",\n    \"foo1\" : \"bar1\",\n    \"float\" : 1.1,\n    \"int\" : 10,\n    \"int_is_float\" : 72057594037927935,\n    \"int_is_int\" : 1111111111\n  }";

  let buf = Buffer.create(8192);
  let add_string = s => Buffer.add_string(buf, s);
  let () = add_string("[\n");
  let rec loop = i =>
    if (i <= 0) {
      ();
    } else {
      add_string(record);
      add_string(",\n");
      loop(i - 1);
    };
  loop(99);
  add_string(record);
  add_string("\n]\n");
  Buffer.contents(buf);
};

open Core;
open Core_bench;

module Yj = {
  open Yojson;

  let read = contents => Basic.from_string(contents);
  let write = json => Basic.to_string(json);
};

let jsonxt_data = gen_data() |> Jsonxt.Basic.of_string;
let yojson_data = gen_data() |> Yj.read;

let () =
  Command.run(
    Bench.make_command([
      Bench.Test.create(~name="jsonxtwr", () =>
        Jsonxt.Basic.to_string(jsonxt_data)
      ),
      Bench.Test.create(~name="yjsonwr", () => Yj.write(yojson_data)),
    ]),
  );
