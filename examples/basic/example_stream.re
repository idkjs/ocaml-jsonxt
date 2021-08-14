open Printf;

let parse_stream_file = filename => {
  let stream = Jsonxt.Basic.stream_from_file(filename);
  Stream.iter(
    json => {
      let s = Jsonxt.Basic.to_string(json);
      printf("%s\n", s);
    },
    stream,
  );
};

let () =
  if (Array.length(Sys.argv) < 2) {
    printf("expected filename\n");
  } else {
    let filename = Sys.argv[1];
    parse_stream_file(filename);
  };
