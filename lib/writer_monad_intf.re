module type Intf = {
  module IO: Io.IO;
  type json;

  /** [json_writer ~writer ~eol ~incr ~psep json] converts [json] to a string [s]
      and writes it out using the [writer string] function.  [incr], [eol] and [psep]
      work together to output human readable output. [incr] defines the increase
      in indentation, [eol] the end of line sequence and [psep] the string to
      seperate the : from the value in objects

      The [writer string] function takes a string and returns a [unit IO.t]
  */

  let json_writer:
    (
      ~writer: string => IO.t(unit),
      ~eol: string,
      ~incr: int,
      ~psep: string,
      json
    ) =>
    IO.t(unit);

  /** [create_encoder ~writer] creates a compact encoder using [json_writer].  */

  let write_json: (~writer: string => IO.t(unit), json) => IO.t(unit);

  /** [create_encoder ~writer] creates a human readable encoder using [json_writer]
      with [incr] set to 2 and eol to '\n'. */

  let write_json_hum: (~writer: string => IO.t(unit), json) => IO.t(unit);
};
