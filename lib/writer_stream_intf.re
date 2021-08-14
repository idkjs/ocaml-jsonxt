module type Intf = {
  type json_stream;
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

  let encode_stream_exn: (t, json_stream) => unit;
  let encode_stream: (t, json_stream) => result(unit, string);
};
