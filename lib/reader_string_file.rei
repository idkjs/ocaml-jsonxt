module type Reader_string_file = {
  type json;

  /** [Reader_string_file] supports functions to parse JSON data from various sources.
      The interface includes two type of parser
      - Single value parsers that return a single Json tree
      - [json Stream.t] functions that process a stream of JSON data, possibly seperated by whitespace.
        eg. [{"a": 1}  {"b":2}] would result in 2 Json values being returned by the stream
  */;

  /** [json_of_string string] converts [string] to a [json] value returing an error
      if the string has syntax, grammar or compliance errors */

  let json_of_string: string => result(json, string);

  /** [json_of_string_exn string] converts [string] to a [json] value raising a
      [Failure] exception if the string has syntax, grammar or compliance errors */

  let json_of_string_exn: string => json;

  /** [json_of_file file] converts the text from [file] to a [json] value returing an error
      if the file contents have syntax, grammar or compliance errors. The file is closed on error */

  let json_of_file: string => result(json, string);

  /** [json_of_file file] converts the text from [file] to a [json] value raising
      a [Failure] exception if the file contents have syntax, grammar or compliance errors.
      The file is closed on error */

  let json_of_file_exn: string => json;

  /** [json_of_channel channel] converts the text from [channel] to a [json] value
      returing an error if the channel contents have syntax, grammar or compliance errors.
      The channel is not closed */

  let json_of_channel: in_channel => result(json, string);

  /** [json_of_channel channel] converts the text from [channel] to a [json] value raising
      a [Failure] exception if the channel contents have syntax, grammar or compliance errors.
      The file is not closed */

  let json_of_channel_exn: in_channel => json;

  /** [json_of_function f] converts text provided by [f] to a [json] value
      returing an error if the supplied text has syntax, grammar or
      compliance errors.  The function [f buf len] takes a [bytes] [buf] buffer, the
      maximum number of bytes to read [len] and returns the number of bytes read.
      Returning 0 indicates end-of-file */

  let json_of_function: ((bytes, int) => int) => result(json, string);

  /** [json_of_function_exn f] converts text provided by [f] to a [json] value
      raising a [Failure] exception if the channel contents have syntax, grammar or
      compliance errors.  See [json_of_function] for detail of function [f] */

  let json_of_function_exn: ((bytes, int) => int) => json;

  /** [json_of_lexbuf lexbuf] converts text in the supplied [lexbuf] to a [json]
      value returning an error if the supplied text has syntax, grammar or compliance
      errors.  This is a low level function and json_of_function should be used
      in preference */

  let json_of_lexbuf: Lexing.lexbuf => result(json, string);

  /** [json_of_lexbuf_exn lexbuf] converts text in the supplied [lexbuf] to a [json]
      value raising a [Failure] exception if the supplied text has syntax, grammar or
      compliance errors.  This is a low level function and json_of_function_exn should
      be used in preference */

  let json_of_lexbuf_exn: Lexing.lexbuf => json;

  /** [of_string] is an alias for json_of_string_exn */

  let of_string: string => json;

  /** [of_file] is an alias for json_of_file_exn */

  let of_file: string => json;

  /** [of_channel] is an alias for json_of_channel_exn */

  let of_channel: in_channel => json;

  /** [of_function] is an alias for json_of_function_exn */

  let of_function: ((bytes, int) => int) => json;

  /** {2 Error_info.t returning functions}

      The following functions are identical to the functions without
      the _error_info extension except they return an [(json, Error_info.t) result]
      instead of a [(json, string) result].  See {!module:Jsonxt.Error_info} for details
      of of [Error_info.t]
   */;

  let json_of_string_error_info: string => result(json, Error_info.t);
  let json_of_file_error_info: string => result(json, Error_info.t);
  let json_of_channel_error_info: in_channel => result(json, Error_info.t);
  let json_of_function_error_info:
    ((bytes, int) => int) => result(json, Error_info.t);
  let json_of_lexbuf_error_info: Lexing.lexbuf => result(json, Error_info.t);

  /** {3 compatablity functions for internal use} */;

  let json_of_lexbuf_error_info_compat:
    (~stream: bool=?, Lexing.lexbuf) => result(option(json), Error_info.t);

  /** {2 [Stream.t] readers}

      [Stream.t] readers provide a mechanism to read a stream of JSON values. eg
      {[
        {"datapoint": 1, "value": 2}
        {"datapoint": 2, "value": 5}
      ]}
      */;

  /** [stream_from_string string] converts [string] containing zero or more json
      object to a [json Stream.t] value raising a [Failure] exception if the
      string has syntax, grammar or compliance errors */

  let stream_from_string: string => Stream.t(json);

  /** [stream_from_channel in_channel] converts the text from [in_channel], containing
      zero or more json objects, to a [json Stream.t] value raising a [Failure] exception
      if the file has syntax, grammar or compliance errors. The optional parameter
      [fin] specifies a function to call when all json objects have been returned or a
      failure occurs */

  let stream_from_channel:
    (~fin: unit => unit=?, in_channel) => Stream.t(json);

  /** [stream_from_file filename] converts the text from file [filename], containing
      zero or more json objects, to a [json Stream.t] value raising a [Failure] exception
      if the file has syntax, grammar or compliance errors */

  let stream_from_file: string => Stream.t(json);

  /** [stream_from_function f] converts text provided by [f], containing zero
      of more JSON objects, to a [json Stream.t] value raising a [Failure] exception
      if the file has syntax, grammar or compliance errors.  The function [f buf len]
      takes a [buf] buffer to fill, the maximum number of bytes to read [len] and
      returns the number of bytes read.  Returning 0 indicates end-of-file */

  let stream_from_function: ((bytes, int) => int) => Stream.t(json);

  /** [stream_from_file lexbuf] converts the text from [lexbuf], containing
      zero or more json objects, to a [json Stream.t] value raising a [Failure] exception
      if the file has syntax, grammar or compliance errors. This is a low level function
      and stream_from_function should be used in preference */

  let stream_from_lexbuf: Lexing.lexbuf => Stream.t(json);

  /** {2 Error_info.Json_error_info raising Stream.t functions}

      The following functions are identical to the functions without
      the _error_info extension except they raise an [Error_info.Json_error_info Error_info.t]
      exception instead of a [Failure string].  See {!module:Jsonxt.Error_info} for details
      of the exception */;

  let stream_from_string_error_info: string => Stream.t(json);
  let stream_from_channel_error_info:
    (~fin: unit => unit=?, in_channel) => Stream.t(json);
  let stream_from_file_error_info: string => Stream.t(json);
  let stream_from_function_error_info:
    ((bytes, int) => int) => Stream.t(json);
  let stream_from_lexbuf_error_info: Lexing.lexbuf => Stream.t(json);
};

module Make:
  (Lexxer: Compliant_lexxer.Lex, Parser: Parser.Parser) =>
   Reader_string_file with type json = Parser.Compliance.json;
