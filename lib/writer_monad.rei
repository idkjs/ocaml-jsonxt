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

module Make:
  (Compliance: Compliance.S, IO: IO) => Writer_monad with module IO := IO;
