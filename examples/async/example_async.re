open Core;
open Async;

module Json_async = {
  module Json_of_async =
    Jsonxt.Basic_monad.Make({
      type t('a) = Deferred.t('a);

      let return = Deferred.return;
      let (>>=) = Deferred.Monad_infix.(>>=);
    });

  let reader = (inc, buf, size) =>
    Reader.read(inc, ~len=size, buf)
    >>= (
      fun
      | `Eof => return(0)
      | `Ok(len) => return(len)
    );

  let read = inc => {
    let reader = reader(inc);
    Json_of_async.read_json(~reader, ());
  };

  let write = outc => {
    let writer = buf => Writer.write(outc, buf) |> return;
    Json_of_async.write_json(~writer);
  };
};

let run = () =>
  Reader.open_file("./asyncdata.json")
  >>= (
    inc =>
      Json_async.read(inc)
      >>= (
        fun
        | Error(err) => raise(Failure(err))
        | Ok(json) =>
          Json_async.write(force(Writer.stdout), json)
          >>= (
            () => {
              printf("\n");
              shutdown(0) |> return;
            }
          )
      )
  );

let () = {
  ignore(run());
  never_returns(Scheduler.go());
};
