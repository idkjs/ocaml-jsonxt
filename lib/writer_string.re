module type Intf = {
  let json_to_string:
    Json_internal.constrained('a) => result(string, string);
  let json_to_string_exn: Json_internal.constrained('a) => string;
  let to_string: Json_internal.constrained('a) => string;
  let json_to_string_hum:
    Json_internal.constrained('a) => result(string, string);
  let json_to_string_hum_exn: Json_internal.constrained('a) => string;
  let to_string_hum: Json_internal.constrained('a) => string;
  let json_to_buffer:
    (Buffer.t, Json_internal.constrained('a)) => result(unit, string);
  let json_to_buffer_exn: (Buffer.t, Json_internal.constrained('a)) => unit;
  let json_to_buffer_hum:
    (Buffer.t, Json_internal.constrained('a)) => result(unit, string);
  let json_to_buffer_hum_exn:
    (Buffer.t, Json_internal.constrained('a)) => unit;
  let to_buffer: (Buffer.t, Json_internal.constrained('a)) => unit;
  let to_buffer_hum: (Buffer.t, Json_internal.constrained('a)) => unit;
  let stream_to_string: Stream.t(Json_internal.constrained('a)) => string;
  let stream_to_buffer:
    (Buffer.t, Stream.t(Json_internal.constrained('a))) => unit;
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

  let add_hex_byte = (buf, i) => {
    Buffer.add_char(buf, nibble_to_hex(i lsr 4 land 0x0f));
    Buffer.add_char(buf, nibble_to_hex(i land 0x0f));
  };

  let escape = (buf, s) => {
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
  };

  let json_to_buffer' = (buf, json) => {
    let add_char = Buffer.add_char(buf);
    let add_string = Buffer.add_string(buf);
    let add_quote_string = s => {
      add_char('"');
      escape(buf, s);
      add_char('"');
    };
    let add_int = i => add_string(string_of_int(i));
    let add_float = f => add_string(Compliance.number_to_string(f));
    let rec fmt = value =>
      switch (value) {
      | `Assoc(o) =>
        add_char('{');
        json_assoc(o);
        add_char('}');
      | `List(l) =>
        add_char('[');
        json_list(l);
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
        add_char('(');
        json_list(t);
        add_char(')');
      | `Variant(v) =>
        add_char('<');
        variant(v);
        add_char('>');
      }
    and json_assoc = o => {
      let sep = ref("");
      List.iter(
        v => {
          add_string(sep^);
          sep := ",";
          pair(v);
        },
        o,
      );
    }
    and pair = ((k, v)) => {
      add_quote_string(k);
      add_char(':');
      fmt(v);
    }
    and json_list = l => {
      let sep = ref("");
      List.iter(
        v => {
          add_string(sep^);
          sep := ",";
          fmt(v);
        },
        l,
      );
    }
    and variant = ((k, j)) => {
      add_quote_string(k);
      switch (j) {
      | Some(j) =>
        add_char(':');
        fmt(j);
      | None => ()
      };
    };

    fmt(json);
  };

  let json_to_buffer_hum' = (buf, json) => {
    let add_char = Buffer.add_char(buf);
    let add_string = Buffer.add_string(buf);
    let add_quote_string = s => {
      add_char('"');
      escape(buf, s);
      add_char('"');
    };
    let add_int = i => add_string(string_of_int(i));
    let add_float = f => add_string(Compliance.number_to_string(f));
    let rec fmt = (ldr, value) =>
      switch (value) {
      | `Assoc(o) =>
        add_string("{\n");
        json_assoc(ldr ++ "  ", o);
        add_char('\n');
        add_string(ldr);
        add_char('}');
      | `List(l) =>
        add_string("[\n");
        json_list(ldr ++ "  ", l);
        add_char('\n');
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
        add_string("(\n");
        json_list(ldr ++ "  ", t);
        add_char('\n');
        add_string(ldr);
        add_char(')');
      | `Variant(v) =>
        add_string("<");
        variant(ldr ++ "  ", v);
        add_char('\n');
        add_string(ldr);
        add_char('>');
      }
    and json_assoc = (ldr, o) => {
      let sep = ref(ldr);
      let newsep = ",\n" ++ ldr;
      List.iter(
        v => {
          add_string(sep^);
          sep := newsep;
          pair(ldr, v);
        },
        o,
      );
    }
    and pair = (ldr, (k, v)) => {
      add_quote_string(k);
      add_string(": ");
      fmt(ldr, v);
    }
    and json_list = (ldr, l) => {
      let sep = ref(ldr);
      let newsep = ",\n" ++ ldr;
      List.iter(
        v => {
          add_string(sep^);
          sep := newsep;
          fmt(ldr, v);
        },
        l,
      );
    }
    and variant = (ldr, (k, j)) => {
      add_quote_string(k);
      switch (j) {
      | Some(j) =>
        add_string(": ");
        fmt(ldr ++ "  ", j);
      | None => ()
      };
    };

    fmt("", json);
    add_char('\n');
  };

  let json_to_string' = json => {
    let buf = Buffer.create(100);
    json_to_buffer'(buf, json);
    Buffer.contents(buf);
  };

  let json_to_string = json =>
    try(Ok(json_to_string'(json))) {
    | Failure(err) => Error(err)
    };

  let json_to_buffer = (buf, json) =>
    try(Ok(json_to_buffer'(buf, json))) {
    | Failure(err) => Error(err)
    };

  let json_to_string_exn = json_to_string';
  let to_string = json_to_string';
  let json_to_buffer_exn = json_to_buffer';
  let to_buffer = json_to_buffer';

  let json_to_string_hum' = json => {
    let buf = Buffer.create(100);
    json_to_buffer_hum'(buf, json);
    Buffer.contents(buf);
  };

  let json_to_string_hum = json =>
    try(Ok(json_to_string_hum'(json))) {
    | Failure(err) => Error(err)
    };

  let json_to_buffer_hum = (buf, json) =>
    try(Ok(json_to_buffer'(buf, json))) {
    | Failure(err) => Error(err)
    };

  let json_to_string_hum_exn = json_to_string_hum';
  let to_string_hum = json_to_string_hum';
  let json_to_buffer_hum_exn = json_to_buffer_hum';
  let to_buffer_hum = json_to_buffer_hum';

  let stream_to_string = stream => {
    let buf = Buffer.create(100);
    let () =
      Stream.iter(
        json => {
          to_buffer(buf, json);
          Buffer.add_char(buf, '\n');
        },
        stream,
      );
    Buffer.contents(buf);
  };

  let stream_to_buffer = (buf, stream) =>
    Stream.iter(
      json => {
        to_buffer(buf, json);
        Buffer.add_char(buf, '\n');
      },
      stream,
    );
};
