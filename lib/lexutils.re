open Lexing;

let fill_lexbuf = (buf, read_len, lexbuf) => {
  let n =
    if (read_len > 0) {
      read_len;
    } else {
      lexbuf.lex_eof_reached = true;
      0;
    };
  /* Current state of the buffer:
           <-------|---------------------|----------->
           |  junk |      valid data     |   junk    |
           ^       ^                     ^           ^
           0    start_pos             buffer_end    Bytes.length buffer
     */
  if (lexbuf.lex_buffer_len + n > Bytes.length(lexbuf.lex_buffer)) {
    /* There is not enough space at the end of the buffer */
    if (lexbuf.lex_buffer_len
        - lexbuf.lex_start_pos
        + n <= Bytes.length(lexbuf.lex_buffer)) {
      /* But there is enough space if we reclaim the junk at the beginning
         of the buffer */
      Bytes.blit(
        lexbuf.lex_buffer,
        lexbuf.lex_start_pos,
        lexbuf.lex_buffer,
        0,
        lexbuf.lex_buffer_len - lexbuf.lex_start_pos,
      );
    } else {
      /* We must grow the buffer.  */
      let newlen =
        min(
          max(2 * Bytes.length(lexbuf.lex_buffer), n),
          Sys.max_string_length,
        );

      if (lexbuf.lex_buffer_len - lexbuf.lex_start_pos + n > newlen) {
        failwith("Lexing.lex_refill: cannot grow buffer");
      };
      let newbuf = Bytes.create(newlen);
      /* Copy the valid data to the beginning of the new buffer */
      Bytes.blit(
        lexbuf.lex_buffer,
        lexbuf.lex_start_pos,
        newbuf,
        0,
        lexbuf.lex_buffer_len - lexbuf.lex_start_pos,
      );
      lexbuf.lex_buffer = newbuf;
    };
    /* Reallocation or not, we have shifted the data left by
       start_pos characters; update the positions */
    let s = lexbuf.lex_start_pos;
    lexbuf.lex_abs_pos = lexbuf.lex_abs_pos + s;
    lexbuf.lex_curr_pos = lexbuf.lex_curr_pos - s;
    lexbuf.lex_start_pos = 0;
    lexbuf.lex_last_pos = lexbuf.lex_last_pos - s;
    lexbuf.lex_buffer_len = lexbuf.lex_buffer_len - s;
    let t = lexbuf.lex_mem;
    for (i in 0 to Array.length(t) - 1) {
      let v = t[i];
      if (v >= 0) {
        t[i] = v - s;
      };
    };
  };
  /* There is now enough space at the end of the buffer */
  Bytes.blit(buf, 0, lexbuf.lex_buffer, lexbuf.lex_buffer_len, n);
  lexbuf.lex_buffer_len = lexbuf.lex_buffer_len + n;
};

let zero_pos = {pos_fname: "", pos_lnum: 1, pos_bol: 0, pos_cnum: 0};

let create_lexbuf = () => {
  refill_buff: _lexbuf => (),
  lex_buffer: Bytes.create(1024),
  lex_buffer_len: 0,
  lex_abs_pos: 0,
  lex_start_pos: 0,
  lex_curr_pos: 0,
  lex_last_pos: 0,
  lex_last_action: 0,
  lex_mem: [||],
  lex_eof_reached: false,
  lex_start_p: zero_pos,
  lex_curr_p: zero_pos,
};
