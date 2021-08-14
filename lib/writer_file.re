module type Intf = {
  let json_to_file:
    (string, Json_internal.constrained('a)) => result(unit, string);
  let json_to_file_exn: (string, Json_internal.constrained('a)) => unit;
  let json_to_channel:
    (out_channel, Json_internal.constrained('a)) => result(unit, string);
  let json_to_channel_exn:
    (out_channel, Json_internal.constrained('a)) => unit;
  let json_to_file_hum:
    (string, Json_internal.constrained('a)) => result(unit, string);
  let json_to_file_hum_exn: (string, Json_internal.constrained('a)) => unit;
  let json_to_channel_hum:
    (out_channel, Json_internal.constrained('a)) => result(unit, string);
  let json_to_channel_hum_exn:
    (out_channel, Json_internal.constrained('a)) => unit;
  let to_file: (string, Json_internal.constrained('a)) => unit;
  let to_file_hum: (string, Json_internal.constrained('a)) => unit;
  let to_channel: (out_channel, Json_internal.constrained('a)) => unit;
  let to_channel_hum: (out_channel, Json_internal.constrained('a)) => unit;
  let stream_to_channel:
    (out_channel, Stream.t(Json_internal.constrained('a))) => unit;
  let stream_to_file:
    (string, Stream.t(Json_internal.constrained('a))) => unit;
};

module Make = (Compliance: Compliance.S) : Intf => {
  let nibble_to_hex = i =>
    char_of_int(
      if (i > 9) {
        65 + i - 10;
      } else {
        48 + i;
      },
    );

  let add_hex_byte = (oc, i) => {
    output_char(oc, nibble_to_hex(i lsr 4 land 0x0f));
    output_char(oc, nibble_to_hex(i land 0x0f));
  };

  let escape = (oc, s) => {
    let add_char = output_char(oc);
    let add_string = output_string(oc);
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
        add_hex_byte(oc, int_of_char(c));
      | _ => add_char(s.[i])
      };
    };
  };

  let json_to_channel_fmt = (oc, json, ~eol, ~incr, ~psep) => {
    let add_char = output_char(oc);
    let add_string = output_string(oc);
    let add_quote_string = s => {
      add_char('"');
      escape(oc, s);
      add_char('"');
    };
    let add_int = i => add_string(string_of_int(i));
    let add_float = f => add_string(Compliance.number_to_string(f));
    let psep = ":" ++ psep;
    let rec fmt = (off, value) =>
      switch (value) {
      | `Assoc(o) =>
        let ldr = String.make(off, ' ');
        add_char('{');
        add_string(eol);
        json_assoc(off + incr, o);
        add_string(eol);
        add_string(ldr);
        add_char('}');
      | `List(l) =>
        let ldr = String.make(off, ' ');
        add_char('[');
        add_string(eol);
        json_list(off + incr, l);
        add_string(eol);
        add_string(ldr);
        add_char(']');
      | `Null => add_string("null")
      | `Bool(b) => add_string(string_of_bool(b))
      | `Int(i) => add_int(i)
      | `Intlit(s) => add_string(s)
      | `Float(f) => add_float(f)
      | `Floatlit(s) => add_string(s)
      | `String(s) => add_quote_string(s)
      | `Stringlit(s) => add_string(s)
      | `Tuple(t) =>
        let ldr = String.make(off, ' ');
        add_char('(');
        add_string(eol);
        json_list(off + incr, t);
        add_string(eol);
        add_string(ldr);
        add_char(')');
      | `Variant(v) =>
        let ldr = String.make(off, ' ');
        add_char('<');
        add_string(eol);
        variant(off + incr, v);
        add_string(eol);
        add_string(ldr);
        add_char('>');
      }
    and json_assoc = (off, o) => {
      let ldr = String.make(off, ' ');
      let sep = ref(ldr);
      let newsep = "," ++ eol ++ ldr;
      List.iter(
        v => {
          add_string(sep^);
          sep := newsep;
          pair(off, v);
        },
        o,
      );
    }
    and pair = (off, (k, v)) => {
      add_quote_string(k);
      add_string(psep);
      fmt(off, v);
    }
    and json_list = (off, l) => {
      let ldr = String.make(off, ' ');
      let sep = ref(ldr);
      let newsep = "," ++ eol ++ ldr;
      List.iter(
        v => {
          add_string(sep^);
          sep := newsep;
          fmt(off, v);
        },
        l,
      );
    }
    and variant = (off, (k, j)) => {
      add_quote_string(k);
      switch (j) {
      | Some(j) =>
        add_string(psep);
        fmt(off + incr, j);
      | None => ()
      };
    };

    fmt(0, json);
    add_string(eol);
  };

  let json_to_channel' = json_to_channel_fmt(~eol="", ~incr=0, ~psep="");
  let json_to_channel_hum' =
    json_to_channel_fmt(~eol="\n", ~incr=2, ~psep=" ");

  let json_to_channel = (oc, json) =>
    try(Ok(json_to_channel'(oc, json))) {
    | Failure(err) => Error(err)
    };

  let json_to_channel_hum = (oc, json) =>
    try(Ok(json_to_channel_hum'(oc, json))) {
    | Failure(err) => Error(err)
    };

  let json_to_channel_exn = json_to_channel';
  let json_to_channel_hum_exn = json_to_channel_hum';

  let json_to_file = (file, json) => {
    let oc = open_out(file);
    let res = json_to_channel(oc, json);
    close_out(oc);
    res;
  };

  let json_to_file_hum = (file, json) => {
    let oc = open_out(file);
    let res = json_to_channel_hum(oc, json);
    close_out(oc);
    res;
  };

  let json_to_file_exn = (file, json) => {
    let oc = open_out(file);
    try(
      {
        json_to_channel'(oc, json);
        close_out(oc);
      }
    ) {
    | exn =>
      close_out(oc);
      raise(exn);
    };
  };

  let json_to_file_hum_exn = (file, json) => {
    let oc = open_out(file);
    try(
      {
        json_to_channel_hum'(oc, json);
        close_out(oc);
      }
    ) {
    | exn =>
      close_out(oc);
      raise(exn);
    };
  };

  let to_file = json_to_file_exn;
  let to_file_hum = json_to_file_hum_exn;
  let to_channel = json_to_channel_exn;
  let to_channel_hum = json_to_channel_hum_exn;

  let stream_to_channel = (oc, stream) =>
    Stream.iter(
      json => {
        json_to_channel_exn(oc, json);
        output_char(oc, '\n');
      },
      stream,
    );

  let stream_to_file = (file, stream) => {
    let oc = open_out(file);
    try(
      {
        stream_to_channel(oc, stream);
        close_out(oc);
      }
    ) {
    | exn =>
      close_out(oc);
      raise(exn);
    };
  };
};
