module IO: {
  type t('a);

  let return: 'a => t('a);
  let (>>=): (t('a), 'a => t('b)) => t('b);
  let result: t('a) => 'a;
} = {
  type t('a) = 'a;

  let return = v => v;
  let (>>=) = (v, f) => f(v);
  let result = t => t;
};

module StringIO = {
  type t = {
    buf: Buffer.t,
    off: ref(int),
  };

  let create = s => {
    let iobuf = {buf: Buffer.create(32), off: ref(0)};
    Buffer.add_string(iobuf.buf, s);
    iobuf;
  };

  let read = (t, buf, len) => {
    let buflen = Buffer.length(t.buf);
    let noff = t.off^ + len;
    let noff =
      if (noff > buflen) {
        buflen;
      } else {
        noff;
      };
    let clen = noff - t.off^;
    if (clen <= 0) {
      Buffer.reset(t.buf);
      t.off := 0;
      0;
    } else {
      Buffer.blit(t.buf, t.off^, buf, 0, clen);
      t.off := noff;
      clen;
    };
  };

  let write = (t, s) => Buffer.add_string(t.buf, s);

  let contents = t =>
    Buffer.sub(t.buf, t.off^, Buffer.length(t.buf) - t.off^);
};

/* Determine the size of an integer, handles 31bit, 63bit and Jsoo using 32bit ints */
let int_bits = {
  let rec log2 = n =>
    if (n <= 1) {
      0;
    } else {
      1 + log2(n asr 1);
    };
  let bits = n => log2(n) + 1;
  switch (bits(max_int)) {
  | 30
  | 31
  | 32 => 32
  | _ => 64
  };
};

let die = msg => {
  Printf.fprintf(stderr, "\nERROR: %s\n", msg);
  exit(255);
};

let load_file = f => {
  let ic = open_in(f);
  let n = in_channel_length(ic);
  let s = Bytes.create(n);
  really_input(ic, s, 0, n);
  close_in(ic);
  Bytes.to_string(s);
};

/* String.split_on_char was introduced at 4.04 */
let split_string = (c, str) => {
  let len = String.length(str);
  let rec loop = (acc, last_pos, pos) =>
    if (pos == (-1)) {
      [String.sub(str, 0, last_pos), ...acc];
    } else if (Char.equal(c, str.[pos])) {
      let pos1 = pos + 1;
      let sub_str = String.sub(str, pos1, last_pos - pos1);
      loop([sub_str, ...acc], pos, pos - 1);
    } else {
      loop(acc, last_pos, pos - 1);
    };

  loop([], len, len - 1);
};
