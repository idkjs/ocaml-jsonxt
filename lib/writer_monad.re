module type IO = Io.IO;

module type Writer_monad = {
  module IO: IO;

  let json_writer:
    (
      ~writer: string => IO.t(unit),
      ~eol: string,
      ~incr: int,
      ~psep: string,
      Json_internal.constrained('a)
    ) =>
    IO.t(unit);
  let write_json:
    (~writer: string => IO.t(unit), Json_internal.constrained('a)) =>
    IO.t(unit);
  let write_json_hum:
    (~writer: string => IO.t(unit), Json_internal.constrained('a)) =>
    IO.t(unit);
};

module Make =
       (Compliance: Compliance.S, IO: IO)
       : (Writer_monad with module IO := IO) => {
  open IO;

  let nibble_to_hex = i =>
    char_of_int(
      if (i > 9) {
        65 + i - 10;
      } else {
        48 + i;
      },
    );

  let add_hex_byte = (buf, i) => {
    Buffer.add_char(buf, nibble_to_hex(i lsr 4 land 0x0f));
    Buffer.add_char(buf, nibble_to_hex(i land 0x0f));
  };

  let escape = s => {
    let buf = Buffer.create(100);
    let add_char = Buffer.add_char(buf);
    let add_string = Buffer.add_string(buf);
    let l = String.length(s);
    for (i in 0 to l - 1) {
      switch (s.[i]) {
      | '"' => add_string("\\\"")
      | '\\' => add_string("\\\\")
      | '\b' => add_string("\\b")
      | '\012' => add_string("\\f")
      | '\n' => add_string("\\n")
      | '\r' => add_string("\\r")
      | '\t' => add_string("\\t")
      | ('\000' .. '\031' | '\127') as c =>
        add_string("\\u00");
        add_hex_byte(buf, int_of_char(c));
      | _ => add_char(s.[i])
      };
    };
    Buffer.contents(buf);
  };

  let json_writer = (~writer, ~eol, ~incr, ~psep, json) => {
    let psep = ":" ++ psep;
    let string_of_char = c => String.make(1, c);
    let write_char = c => writer(string_of_char(c));
    let write_string = writer;
    let write_quote_string = s =>
      write_char('"')
      >>= (() => writer(escape(s)) >>= (() => write_char('"')));

    let write_int = i => write_string(string_of_int(i));
    let write_float = f => write_string(Compliance.number_to_string(f));
    let write_list = (off, f, l) => {
      let ldr = String.make(off, ' ');
      let rec loop =
        fun
        | [] => return()
        | [hd, ...tl] =>
          write_string("," ++ eol ++ ldr)
          >>= (() => f(hd) >>= (() => loop(tl)));

      let first =
        fun
        | [] => return()
        | [hd, ...tl] =>
          write_string(eol ++ ldr) >>= (() => f(hd) >>= (() => loop(tl)));

      first(l);
    };

    let rec fmt = (off, value) =>
      switch (value) {
      | `Assoc(o) =>
        let ldr = String.make(off, ' ');
        write_string("{")
        >>= (
          () =>
            json_assoc(off + incr, o)
            >>= (() => write_string(eol ++ ldr ++ "}"))
        );
      | `List(l) =>
        let ldr = String.make(off, ' ');
        write_string("[")
        >>= (() => json_list(off + incr, l))
        >>= (() => write_string(eol ++ ldr ++ "]"));
      | `Null => write_string("null")
      | `Bool(b) => write_string(string_of_bool(b))
      | `Int(i) => write_int(i)
      | `Intlit(s) => write_string(s)
      | `Float(f) => write_float(f)
      | `Floatlit(s) => write_string(s)
      | `String(s) => write_quote_string(s)
      | `Stringlit(s) => write_string(s)
      | `Tuple(t) =>
        let ldr = String.make(off, ' ');
        write_string("(" ++ eol)
        >>= (
          () =>
            json_list(off + incr, t)
            >>= (() => write_string(eol ++ ldr ++ ")"))
        );
      | `Variant(v) =>
        let ldr = String.make(off, ' ');
        write_string("<" ++ eol)
        >>= (
          () =>
            variant(off + incr, v)
            >>= (() => write_string(eol ++ ldr ++ ">"))
        );
      }
    and json_assoc = (off, o) => write_list(off, v => pair(off, v), o)
    and pair = (off, (k, v)) =>
      write_quote_string(k)
      >>= (() => write_string(psep) >>= (() => fmt(off, v)))
    and json_list = (off, l) => write_list(off, v => fmt(off, v), l)
    and variant = (off, (k, j)) =>
      write_quote_string(k)
      >>= (
        () =>
          switch (j) {
          | Some(j) => write_string(psep) >>= (() => fmt(off + incr, j))
          | None => return()
          }
      );

    fmt(0, json) >>= (() => write_string(eol));
  };

  let write_json = (~writer, json) =>
    json_writer(~writer, ~eol="", ~incr=0, ~psep="", json);
  let write_json_hum = (~writer, json) =>
    json_writer(~writer, ~eol="\n", ~incr=2, ~psep=" ", json);
};
