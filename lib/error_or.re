module type IO = Io.IO;

module type Intf = {
  module IO: IO;

  let return: 'a => IO.t(result('a, 'b));
  let fail: 'b => IO.t(result('a, 'b));
  let (>>=?):
    (IO.t(result('a, 'b)), 'a => IO.t(result('c, 'b))) =>
    IO.t(result('c, 'b));
};

module Make = (IO: IO) : (Intf with module IO := IO) => {
  let return = v => IO.return(Ok(v));
  let fail = err => IO.return(Error(err));

  let (>>=?) = (a, f) =>
    IO.(
      a
      >>= (
        a =>
          switch (a) {
          | Ok(a) => f(a)
          | Error(err) => IO.return(Error(err))
          }
      )
    );
};
