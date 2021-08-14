/** Yojson compatibility module

    To use Jsonxt's Yojson compatibility module create a [yojson.ml] file in
    the projects source directory with the following contents:
    {[
    include Jsonxt.Yojson
    ]}
    Note that compatibility is mostly a thin layer on top of Jsonxt.
    In particular the error reporting by the utils module uses
    the [Failure] exception rather than Yojson's specialist exceptions

    {2 JSON compatibility differences}

    The underlying parser used by the Yojson compatibility modules
    are RFC 8259 compliant except for specific extensions. In
    particular:
        - Control characters must be escaped as defined by RFC 8259
        - Comment are supported with /* */ and // at the end of the line
        - Variants and tuples are supported but with syntax changes noted
          later
        - Object keys must be quoted. So \{ab:10\} is not valid and must be
          encoded as \{"ab":10\}
        - Variant names must be quoted. eg. <"ab"> and <"ab":10>

    {2 Behavioural differences}
        - The [lexer_state] data structure is used to report errors but
          not updated during the parsing of the input
        - The optional [buf] parameter is ignored
        - Error messages are likely to be different
        - The utils module module uses the [Failure] exception rather
          than Yojson's specialist exceptions
 */;

exception Json_error(string);

type lexer_state = {
  buf: Buffer.t,
  mutable lnum: int,
  mutable bol: int,
  mutable fname: option(string),
};

let init_lexer:
  (~buf: Buffer.t=?, ~fname: string=?, ~lnum: int=?, unit) => lexer_state;

module Basic: {
  type json = Json.Basic.json;
  type t = json;
  type json_line = [ | `Json(t) | `Exn(exn)];

  /* Readers */

  let from_string:
    (~buf: Buffer.t=?, ~fname: string=?, ~lnum: int=?, string) => t;
  let from_channel:
    (~buf: Buffer.t=?, ~fname: string=?, ~lnum: int=?, in_channel) => t;
  let from_file:
    (~buf: Buffer.t=?, ~fname: string=?, ~lnum: int=?, string) => t;
  let from_lexbuf: (lexer_state, ~stream: bool=?, Lexing.lexbuf) => t;
  let stream_from_string:
    (~buf: Buffer.t=?, ~fname: string=?, ~lnum: int=?, string) => Stream.t(t);
  let stream_from_channel:
    (
      ~buf: Buffer.t=?,
      ~fin: unit => unit=?,
      ~fname: string=?,
      ~lnum: int=?,
      in_channel
    ) =>
    Stream.t(t);
  let stream_from_file:
    (~buf: Buffer.t=?, ~fname: string=?, ~lnum: int=?, string) => Stream.t(t);
  let stream_from_lexbuf:
    (lexer_state, ~fin: unit => unit=?, Lexing.lexbuf) => Stream.t(t);

  let linestream_from_channel:
    (
      ~buf: Buffer.t=?,
      ~fin: unit => unit=?,
      ~fname: string=?,
      ~lnum: int=?,
      in_channel
    ) =>
    Stream.t(json_line);

  let linestream_from_file:
    (~buf: Buffer.t=?, ~fname: string=?, ~lnum: int=?, string) =>
    Stream.t(json_line);

  let read_t: (lexer_state, Lexing.lexbuf) => t;

  /* Writers */

  let to_string: (~buf: Buffer.t=?, ~len: int=?, ~std: bool=?, t) => string;
  let to_channel:
    (~buf: Buffer.t=?, ~len: int=?, ~std: bool=?, out_channel, t) => unit;
  let to_file: (~len: int=?, ~std: bool=?, string, t) => unit;
  let to_outbuf: (~std: bool=?, Buffer.t, t) => unit;
  let to_output:
    (
      ~buf: Buffer.t=?,
      ~len: int=?,
      ~std: bool=?,
      {.. output: (string, int, int) => 'a},
      t
    ) =>
    'a;
  let stream_to_string:
    (~buf: Buffer.t=?, ~len: int=?, ~std: bool=?, Stream.t(t)) => string;
  let stream_to_channel:
    (~buf: Buffer.t=?, ~len: int=?, ~std: bool=?, out_channel, Stream.t(t)) =>
    unit;
  let stream_to_file:
    (~len: int=?, ~std: bool=?, string, Stream.t(t)) => unit;
  let stream_to_outbuf: (~std: bool=?, Buffer.t, Stream.t(t)) => unit;
  let write_t: (Buffer.t, t) => unit;

  /* Pretty printers */
  let pretty_print: (~std: bool=?, Format.formatter, t) => unit;
  let pretty_to_string: (~std: bool=?, t) => string;
  let pretty_to_channel: (~std: bool=?, out_channel, t) => unit;
  let prettify: (~std: bool=?, string) => string;
  let compact: (~std: bool=?, string) => string;

  /* Tools */
  let equal: (t, t) => bool;
  let sort: t => t;
  let show: t => string;
  let pp: (Format.formatter, t) => unit;

  module Util: {
    include Process_intf.Shared with type json := json;
    include Process_intf.Basic with type json := json;
  };
};

module Safe: {
  type json = [
    | `Null
    | `Bool(bool)
    | `Int(int)
    | `Intlit(string)
    | `Float(float)
    | `String(string)
    | `Assoc(list((string, json)))
    | `List(list(json))
    | `Tuple(list(json))
    | `Variant(string, option(json))
  ];

  type t = json;
  type json_line = [ | `Json(t) | `Exn(exn)];

  /* Readers */

  let from_string:
    (~buf: Buffer.t=?, ~fname: string=?, ~lnum: int=?, string) => t;
  let from_channel:
    (~buf: Buffer.t=?, ~fname: string=?, ~lnum: int=?, in_channel) => t;
  let from_file:
    (~buf: Buffer.t=?, ~fname: string=?, ~lnum: int=?, string) => t;
  let from_lexbuf: (lexer_state, ~stream: bool=?, Lexing.lexbuf) => t;
  let stream_from_string:
    (~buf: Buffer.t=?, ~fname: string=?, ~lnum: int=?, string) => Stream.t(t);
  let stream_from_channel:
    (
      ~buf: Buffer.t=?,
      ~fin: unit => unit=?,
      ~fname: string=?,
      ~lnum: int=?,
      in_channel
    ) =>
    Stream.t(t);
  let stream_from_file:
    (~buf: Buffer.t=?, ~fname: string=?, ~lnum: int=?, string) => Stream.t(t);
  let stream_from_lexbuf:
    (lexer_state, ~fin: unit => unit=?, Lexing.lexbuf) => Stream.t(t);

  let linestream_from_channel:
    (
      ~buf: Buffer.t=?,
      ~fin: unit => unit=?,
      ~fname: string=?,
      ~lnum: int=?,
      in_channel
    ) =>
    Stream.t(json_line);

  let linestream_from_file:
    (~buf: Buffer.t=?, ~fname: string=?, ~lnum: int=?, string) =>
    Stream.t(json_line);

  let read_t: (lexer_state, Lexing.lexbuf) => t;

  /* Writers */

  let to_string: (~buf: Buffer.t=?, ~len: int=?, ~std: bool=?, t) => string;
  let to_channel:
    (~buf: Buffer.t=?, ~len: int=?, ~std: bool=?, out_channel, t) => unit;
  let to_file: (~len: int=?, ~std: bool=?, string, t) => unit;
  let to_outbuf: (~std: bool=?, Buffer.t, t) => unit;
  let to_output:
    (
      ~buf: Buffer.t=?,
      ~len: int=?,
      ~std: bool=?,
      {.. output: (string, int, int) => 'a},
      t
    ) =>
    'a;
  let stream_to_string:
    (~buf: Buffer.t=?, ~len: int=?, ~std: bool=?, Stream.t(t)) => string;
  let stream_to_channel:
    (~buf: Buffer.t=?, ~len: int=?, ~std: bool=?, out_channel, Stream.t(t)) =>
    unit;
  let stream_to_file:
    (~len: int=?, ~std: bool=?, string, Stream.t(t)) => unit;
  let stream_to_outbuf: (~std: bool=?, Buffer.t, Stream.t(t)) => unit;
  let write_t: (Buffer.t, t) => unit;

  /* Pretty printers */
  let pretty_print: (~std: bool=?, Format.formatter, t) => unit;
  let pretty_to_string: (~std: bool=?, t) => string;
  let pretty_to_channel: (~std: bool=?, out_channel, t) => unit;
  let prettify: (~std: bool=?, string) => string;
  let compact: (~std: bool=?, string) => string;

  /* Tools */
  let sort: t => t;
  let equal: (t, t) => bool;
  let show: t => string;
  let pp: (Format.formatter, t) => unit;
  let to_basic: t => Basic.json;

  module Util: {
    include Process_intf.Shared with type json := json;
    include Process_intf.Basic with type json := json;
    include Process_intf.Extended with type json := json;
  };
};

module Raw: {
  type json = [
    | `Null
    | `Bool(bool)
    | `Intlit(string)
    | `Floatlit(string)
    | `Stringlit(string)
    | `Assoc(list((string, json)))
    | `List(list(json))
    | `Tuple(list(json))
    | `Variant(string, option(json))
  ];

  type t = json;
  type json_line = [ | `Json(t) | `Exn(exn)];

  /* Readers */

  let from_string:
    (~buf: Buffer.t=?, ~fname: string=?, ~lnum: int=?, string) => t;
  let from_channel:
    (~buf: Buffer.t=?, ~fname: string=?, ~lnum: int=?, in_channel) => t;
  let from_file:
    (~buf: Buffer.t=?, ~fname: string=?, ~lnum: int=?, string) => t;
  let from_lexbuf: (lexer_state, ~stream: bool=?, Lexing.lexbuf) => t;
  let stream_from_string:
    (~buf: Buffer.t=?, ~fname: string=?, ~lnum: int=?, string) => Stream.t(t);
  let stream_from_channel:
    (
      ~buf: Buffer.t=?,
      ~fin: unit => unit=?,
      ~fname: string=?,
      ~lnum: int=?,
      in_channel
    ) =>
    Stream.t(t);
  let stream_from_file:
    (~buf: Buffer.t=?, ~fname: string=?, ~lnum: int=?, string) => Stream.t(t);
  let stream_from_lexbuf:
    (lexer_state, ~fin: unit => unit=?, Lexing.lexbuf) => Stream.t(t);

  let linestream_from_channel:
    (
      ~buf: Buffer.t=?,
      ~fin: unit => unit=?,
      ~fname: string=?,
      ~lnum: int=?,
      in_channel
    ) =>
    Stream.t(json_line);

  let linestream_from_file:
    (~buf: Buffer.t=?, ~fname: string=?, ~lnum: int=?, string) =>
    Stream.t(json_line);

  let read_t: (lexer_state, Lexing.lexbuf) => t;

  /* Writers */

  let to_string: (~buf: Buffer.t=?, ~len: int=?, ~std: bool=?, t) => string;
  let to_channel:
    (~buf: Buffer.t=?, ~len: int=?, ~std: bool=?, out_channel, t) => unit;
  let to_file: (~len: int=?, ~std: bool=?, string, t) => unit;
  let to_outbuf: (~std: bool=?, Buffer.t, t) => unit;
  let to_output:
    (
      ~buf: Buffer.t=?,
      ~len: int=?,
      ~std: bool=?,
      {.. output: (string, int, int) => 'a},
      t
    ) =>
    'a;
  let stream_to_string:
    (~buf: Buffer.t=?, ~len: int=?, ~std: bool=?, Stream.t(t)) => string;
  let stream_to_channel:
    (~buf: Buffer.t=?, ~len: int=?, ~std: bool=?, out_channel, Stream.t(t)) =>
    unit;
  let stream_to_file:
    (~len: int=?, ~std: bool=?, string, Stream.t(t)) => unit;
  let stream_to_outbuf: (~std: bool=?, Buffer.t, Stream.t(t)) => unit;
  let write_t: (Buffer.t, t) => unit;

  /* Pretty printers */
  let pretty_print: (~std: bool=?, Format.formatter, t) => unit;
  let pretty_to_string: (~std: bool=?, t) => string;
  let pretty_to_channel: (~std: bool=?, out_channel, t) => unit;
  let prettify: (~std: bool=?, string) => string;
  let compact: (~std: bool=?, string) => string;

  /* Tools */
  let sort: t => t;
  let equal: (t, t) => bool;
  let show: t => string;
  let pp: (Format.formatter, t) => unit;
};
