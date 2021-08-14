module type Intf = {
  type t;

  let create_encoder':
    (
      ~add_char: char => unit,
      ~add_string: string => unit,
      ~incr: int,
      ~eol: string
    ) =>
    t;

  let create_encoder:
    (~add_char: char => unit, ~add_string: string => unit) => t;

  let create_encoder_hum:
    (~add_char: char => unit, ~add_string: string => unit) => t;

  let create_encoder_channel: out_channel => t;
  let create_encoder_channel_hum: out_channel => t;

  let encode_stream_exn: (t, Json_internal.constrained_stream('a)) => unit;
  let encode_stream:
    (t, Json_internal.constrained_stream('a)) => result(unit, string);
};

module Make = (Compliance: Compliance.S) : Intf => {
  type state =
    | Token
    | List_start
    | List_next
    | Object_name
    | Object_name_next
    | Object_value
    | Tuple_start
    | Tuple_next
    | Variant_start
    | Variant_value
    | Variant_end;
  type t = {
    stack: Stack.t((state, int)),
    add_char: char => unit,
    add_string: string => unit,
    incr: int,
    eol: string,
  };

  let create_encoder' = (~add_char, ~add_string, ~incr, ~eol) => {
    stack: Stack.create(),
    add_char,
    add_string,
    incr,
    eol,
  };

  let create_encoder = (~add_char, ~add_string) =>
    create_encoder'(~add_char, ~add_string, ~incr=0, ~eol="");
  let create_encoder_hum = (~add_char, ~add_string) =>
    create_encoder'(~add_char, ~add_string, ~incr=2, ~eol="\n");

  let create_encoder_channel = oc => {
    let add_char = output_char(oc);
    let add_string = output_string(oc);
    create_encoder(~add_char, ~add_string);
  };

  let create_encoder_channel_hum = oc => {
    let add_char = output_char(oc);
    let add_string = output_string(oc);
    create_encoder_hum(~add_char, ~add_string);
  };

  let nibble_to_hex = i =>
    char_of_int(
      if (i > 9) {
        65 + i - 10;
      } else {
        48 + i;
      },
    );

  let add_hex_byte = (add_char, i) => {
    add_char(nibble_to_hex(i lsr 4 land 0x0f));
    add_char(nibble_to_hex(i land 0x0f));
  };

  let escape = (~add_char, ~add_string, s) => {
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
        add_hex_byte(add_char, int_of_char(c));
      | _ => add_char(s.[i])
      };
    };
  };

  let encode_stream_exn = (t, tok: Json_internal.constrained_stream('a)) => {
    let add_char = t.add_char;
    let add_string = t.add_string;
    let add_quote_string = s => {
      add_char('"');
      escape(~add_char, ~add_string, s);
      add_char('"');
    };
    let add_leader = off => add_string(String.make(off, ' '));
    let add_eol = () => add_string(t.eol);
    let add_comma_eol_ldr = off => {
      add_char(',');
      add_eol();
      add_leader(off);
    };
    let add_int = i => add_string(string_of_int(i));
    let add_float = f => add_string(Compliance.number_to_string(f));
    let end_of_obj_list = (off, c) => {
      add_leader(off - t.incr);
      add_char(c);
    };
    let fmt = (t, off, tok: Json_internal.constrained_stream('a)) =>
      switch (tok) {
      | `Null => add_string("null")
      | `Bool(b) => add_string(string_of_bool(b))
      | `Int(i) => add_int(i)
      | `Intlit(s) => add_string(s)
      | `Float(f) => add_float(f)
      | `Floatlit(s) => add_string(s)
      | `String(s) => add_quote_string(s)
      | `Stringlit(s) => add_string(s)
      | `Name(s) =>
        add_quote_string(s);
        add_char(':');
      | `Infinity => add_string("inf")
      | `Neg_infinity => add_string("-inf")
      | `Nan => add_string("nan")
      | `As =>
        add_char('[');
        add_eol();
        Stack.push((List_start, off + t.incr), t.stack);
      | `Ae => ()
      | `Os =>
        add_char('{');
        add_eol();
        Stack.push((Object_name, off + t.incr), t.stack);
      | `Oe => ()
      | `Ts =>
        add_char('(');
        add_eol();
        Stack.push((Tuple_start, off + t.incr), t.stack);
      | `Te => ()
      | `Vs =>
        add_char('<');
        add_eol();
        Stack.push((Variant_start, off + t.incr), t.stack);
      | `Ve => ()
      };

    let fmt_list_start = (t, off, tok: Json_internal.constrained_stream('a)) =>
      switch (tok) {
      | `Ae => end_of_obj_list(off, ']')
      | _ =>
        let () = Stack.push((List_next, off), t.stack);
        add_leader(off);
        fmt(t, off, tok);
      };

    let fmt_list_next = (t, off, tok: Json_internal.constrained_stream('a)) =>
      switch (tok) {
      | `Ae =>
        add_eol();
        end_of_obj_list(off, ']');
      | _ =>
        let () = Stack.push((List_next, off), t.stack);
        add_comma_eol_ldr(off);
        fmt(t, off, tok);
      };

    let fmt_object_name = (t, off, tok: Json_internal.constrained_stream('a)) =>
      switch (tok) {
      | `Oe => end_of_obj_list(off, '}')
      | `Name(s) =>
        add_leader(off);
        add_quote_string(s);
        add_char(':');
        Stack.push((Object_value, off), t.stack);
      | _ => raise(Failure("Unexpected token, expected object key"))
      };

    let fmt_object_name_next =
        (t, off, tok: Json_internal.constrained_stream('a)) =>
      switch (tok) {
      | `Oe =>
        add_eol();
        end_of_obj_list(off, '}');
      | `Name(s) =>
        add_comma_eol_ldr(off);
        add_quote_string(s);
        add_char(':');
        Stack.push((Object_value, off), t.stack);
      | _ => raise(Failure("Unexpected token, expected object key"))
      };

    let fmt_object_value =
        (t, off, tok: Json_internal.constrained_stream('a)) =>
      switch (tok) {
      | `Oe => raise(Failure("Unexpected '`Oe', expected object value"))
      | _ =>
        Stack.push((Object_name_next, off), t.stack);
        fmt(t, off, tok);
      };

    let fmt_tuple_start = (t, off, tok: Json_internal.constrained_stream('a)) =>
      switch (tok) {
      | `Te => end_of_obj_list(off, ')')
      | _ =>
        let () = Stack.push((Tuple_next, off), t.stack);
        add_leader(off);
        fmt(t, off, tok);
      };

    let fmt_tuple_next = (t, off, tok: Json_internal.constrained_stream('a)) =>
      switch (tok) {
      | `Te =>
        add_eol();
        end_of_obj_list(off, ')');
      | _ =>
        let () = Stack.push((Tuple_next, off), t.stack);
        add_comma_eol_ldr(off);
        fmt(t, off, tok);
      };

    let fmt_variant_start =
        (t, off, tok: Json_internal.constrained_stream('a)) =>
      switch (tok) {
      | `Name(s) =>
        add_leader(off);
        add_quote_string(s);
        Stack.push((Variant_value, off), t.stack);
      | _ => raise(Failure("Unexpected token, expected varient name"))
      };

    let fmt_variant_value =
        (t, off, tok: Json_internal.constrained_stream('a)) =>
      switch (tok) {
      | `Ve =>
        add_eol();
        end_of_obj_list(off, '>');
      | _ =>
        Stack.push((Variant_end, off), t.stack);
        add_char(':');
        fmt(t, off, tok);
      };

    let fmt_variant_end =
        (_t, off, tok: Json_internal.constrained_stream('a)) =>
      switch (tok) {
      | `Ve =>
        add_eol();
        end_of_obj_list(off, '>');
      | _ => raise(Failure("Unexpected token, expected varient end (`Ve)"))
      };

    let (state, off) =
      if (Stack.is_empty(t.stack)) {
        (Token, 0);
      } else {
        Stack.pop(t.stack);
      };
    switch (state) {
    | Token => fmt(t, off, tok)
    | List_start => fmt_list_start(t, off, tok)
    | List_next => fmt_list_next(t, off, tok)
    | Object_name => fmt_object_name(t, off, tok)
    | Object_name_next => fmt_object_name_next(t, off, tok)
    | Object_value => fmt_object_value(t, off, tok)
    | Tuple_start => fmt_tuple_start(t, off, tok)
    | Tuple_next => fmt_tuple_next(t, off, tok)
    | Variant_start => fmt_variant_start(t, off, tok)
    | Variant_value => fmt_variant_value(t, off, tok)
    | Variant_end => fmt_variant_end(t, off, tok)
    };
  };

  let encode_stream = (t, tok: Json_internal.constrained_stream('a)) =>
    try(Ok(encode_stream_exn(t, tok))) {
    | Failure(err) => Error(err)
    };
};
