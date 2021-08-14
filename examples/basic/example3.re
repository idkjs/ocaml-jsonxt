module IO = {
  type t('a) = 'a;

  let return = v => v;
  let (>>=) = (v, f) => f(v);
};

module JsonIO = Jsonxt.Basic_monad.Make(IO);
open IO;

let _ = {
  let ic = open_in("test.json");
  let reader = (buf, len) => return(input(ic, buf, 0, len));
  let writer = s => return(output_string(stdout, s));
  JsonIO.read_json(~reader, ())
  >>= (
    fun
    | Error(err) => {
        print_endline("ERROR: " ++ err);
        return();
      }
    | Ok(json) => JsonIO.write_json_hum(~writer, json)
  );
};
