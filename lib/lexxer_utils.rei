exception Lex_error(string);

let error_pos: Lexing.lexbuf => (int, int, int);
let error_pos_msg: Lexing.lexbuf => string;
let lex_error: string => 'a;
let string2num: string => Tokens.token;
let update_pos: Lexing.lexbuf => unit;
let unescape_string: string => string;
