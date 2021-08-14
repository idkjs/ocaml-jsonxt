module type IO = Io.IO;

module Make = (IO: IO) => {
  module Parser = Parser_monad.Make(Strict.Compliance, IO);
  include Reader_monad.Make(Parser);
  include Writer_monad.Make(Strict.Compliance, IO);
};
