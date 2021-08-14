open Printf;

let parse_stream_string = s => {
  let stream = Jsonxt.Basic_stream.stream_from_string(s);
  Stream.iter(
    el => {
      let s = Jsonxt.Utilities.json_stream_to_string_repr(el);
      printf("%s ", s);
    },
    stream,
  );
  printf("\n");
};

let () = {
  let json_s = {| [ { "id":10, "str":"foo" }, { "id":11, "str":"bar" } ] |};
  parse_stream_string(json_s);
};
