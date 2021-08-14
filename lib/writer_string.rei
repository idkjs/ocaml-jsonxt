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

module Make: (Compliance: Compliance.S) => Intf;
