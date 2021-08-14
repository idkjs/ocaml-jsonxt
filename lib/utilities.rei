/** json tree conversion functions

    Support various converstions of a json tree
    - To strict and basic types
    - To strings, intended mostly for debugging
*/;

/** {2 Tree type conversion} */;

/** [to_basic json] converts [json] into the [Basic.json] type
    transforming variants that do not comply to that type.
    All json types can be converted */

let to_basic: Json_internal.constrained('a) => Basic.json;

/** [to_basic json] converts [json] into the [Strict.json] type
    transforming variants that do not comply to that type.
    In particular ints are converted to floats. All json types can
    be converted */

let to_strict: Json_internal.constrained('a) => Strict.json;

/** {2 Tree conversion to strings} */;

/** [json_to_string_repr json] converts [json] into a string representation
    of the tree. This is not JSON but a textual represenation of the json
    tree. eg
    {[
      `Assoc [
        "Boo1": `List []
      ]
    ]}
*/

let json_to_string_repr: Json_internal.constrained('a) => string;

/** [json_to_string json] converts the json tree to standard JSON
    in compact format. The function does not apply any type
    constraints to the json tree
*/

let json_to_string: Json_internal.constrained('a) => string;

/** [json_to_string_repr json] converts [json] into a string representation
    of the json_stream element. This is not JSON but a textual represenation
    of the json_stream_type. eg `Ae
    */

let json_stream_to_string_repr:
  Json_internal.constrained_stream('a) => string;

/** [equal json1 json2] recursively compares the two trees returning true
    if they are equal.  Object key/value pairs do not need to be in the same
    order. Duplicate keys are equal if and only if the key/value pairs are
    in the same order in each tree. */

let equal: (Json_internal.constrained('a), 'a) => bool;
