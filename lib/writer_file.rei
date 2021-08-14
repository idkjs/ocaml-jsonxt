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

module Make: (Compliance: Compliance.S) => Intf;
