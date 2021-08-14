/** [Extended_monad] supports parsing and writing JSON data that conforms to the
    {!type:Json.Extended.json} json type using reader and writer funtions that
    implement the IO monad. This supports non-standard JSON types including integer
    as well as tuples and variants introduced by [Yojson]*/;

module type IO = Io.IO;

/** The [Make] functor is used to create a module with reader and writer
    functions using the IO monad.
 */

module Make:
  (IO: IO) =>
   {
    /** The [Make] functor is used to create a module with reader and writer
      functions using the IO monad. This has the signature
      {[
        module type IO = sig
          type 'a t

          val return : 'a -> 'a t
          val (>>=)  : 'a t -> ('a -> 'b t) -> 'b t
        end
      ]}
  */

    include
      Reader_monad.Reader_monad with
        type json := Extended.Compliance.json and module IO := IO;
    include
      Writer_monad_intf.Intf with
        type json := Extended.Compliance.json and module IO := IO;
  };
