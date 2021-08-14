module type Intf = {
  let pretty_print: (Format.formatter, Json_internal.constrained('a)) => unit;
  let pretty_print_to_string: Json_internal.constrained('a) => string;
  let pretty_print_to_channel:
    (out_channel, Json_internal.constrained('a)) => unit;
};

module Make: (Compliance: Compliance.S) => Intf;
