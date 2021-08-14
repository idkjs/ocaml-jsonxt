let nibble_to_hex = i =>
  char_of_int(
    if (i > 9) {
      65 + i - 10;
    } else {
      48 + i;
    },
  );

let add_hex_byte = (~add_char, i) => {
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
      add_hex_byte(~add_char, int_of_char(c));
    | _ => add_char(s.[i])
    };
  };
};
