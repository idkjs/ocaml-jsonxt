/** Parsing error handling */;

/** [t] encapsulates a parsing error and has the following fields
    - line: Line where the error occurred
    - start_char: first character of error in line
    - end_char: last character of error in line
    - msg - error message
 */

type t = {
  line: int,
  start_char: int,
  end_char: int,
  msg: string,
};

exception Json_error_info(t);

let create_from_lexbuf: (Lexing.lexbuf, string) => t;
let to_string: t => string;
