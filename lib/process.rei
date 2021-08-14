/** [Process] provides functions for processing Jsonxt json trees.
    This includes functions to map, flatten and extract data */;

/** [Strict] supports processing JSON data that conforms to the
    {!type:Json.Strict.json} json type.  */

module Strict: {include Process_intf.Shared with type json := Json.Strict.json;
};

/** [Basic] supports processing JSON data that conforms to the
    {!type:Json.Basic.json} json type.  */

module Basic: {
  include Process_intf.Shared with type json := Json.Basic.json;
  include Process_intf.Basic with type json := Json.Basic.json;
};

/** [Extended] supports processing JSON data that conforms to the
    {!type:Json.Extended.json} json type.  */

module Extended: {
  include Process_intf.Shared with type json := Json.Extended.json;
  include Process_intf.Basic with type json := Json.Extended.json;
  include Process_intf.Extended with type json := Json.Extended.json;
};

/** [Yojson_safe] supports processing JSON data that conforms to
    Yojson's Safe json type. */

module Yojson_safe: {
  type json = [
    | `Null
    | `Bool(bool)
    | `Int(int)
    | `Intlit(string)
    | `Float(float)
    | `String(string)
    | `Assoc(list((string, json)))
    | `List(list(json))
    | `Tuple(list(json))
    | `Variant(string, option(json))
  ];
  include Process_intf.Shared with type json := json;
  include Process_intf.Basic with type json := json;
  include Process_intf.Extended with type json := json;
};
