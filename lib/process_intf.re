module type Shared = {
  type json;

  /** [member key json] searches the JSON object [json], which
      must be an [`Assoc] element, for [key] and returns the
      value or [`Null] if the [key] is missing */

  let member: (string, [> | `Assoc(list((string, json)))]) => json;

  /** [index idx json] returns the [idx]-th JSON object in the [json] array,
      which must be an [`List] element. A negative [idx] value starts from
      the end with -1 being the last element.  A [Failure] exception is raise
      if the idx is out of bounds */

  let index: (int, [> | `List(list(json))]) => json;

  /** [map f json] applies the function [f] to each element of the JSON
      array [json], which must be an [`List] element, and returns a
      [`List] element */

  let map:
    (json => json, [> | `List(list(json))]) => [> | `List(list(json))];

  /** [to_assoc json] converts the JSON object [`Assoc a] to [a] */

  let to_assoc:
    [> | `Assoc(list((string, json)))] => list((string, json));

  /** [to_bool json] converts [`Bool b] to [b] */

  let to_bool: [> | `Bool(bool)] => bool;

  /** [to_bool_option json] converts [`Bool b] to [Some b] and [`Null] to [None] */

  let to_bool_option: [> | `Bool(bool) | `Null] => option(bool);

  /** [to_float json] converts [`Float f] to [f] */

  let to_float: [> | `Float(float)] => float;

  /** [to_float_option json] converts [`Float f] to [Some f] and [`Null] to [None] */

  let to_float_option: [> | `Float(float) | `Null] => option(float);

  /** [to_option f json] returns [None] if [json] is [`Null] otherwise [Some (f json)].  */

  let to_option: (([> | `Null] as 'a) => json, 'a) => option(json);

  /** [to_list json] converts [`List l] to [l] */

  let to_list: [> | `List(list(json))] => list(json);

  /** [to_number json] converts [`Float f] to [f] */

  let to_number: [> | `Float(float)] => float;

  /** [to_number_option json] converts [`Float f] to [Some f] and [`Null] to [None] */

  let to_number_option: [> | `Float(float) | `Null] => option(float);

  /** [to_string json] converts [`String s] to [s] */

  let to_string: [> | `String(string)] => string;

  /** [to_string_option json] converts [`String s] to [Some s] and [`Null] to [None] */

  let to_string_option: [> | `String(string) | `Null] => option(string);

  /** [convert_each f json] applies the function [f] to each element of the
      JSON array [json], which must be an [`List] element, and returns a
      list of the returned values. */

  let convert_each: (json => json, [> | `List(list(json))]) => list(json);

  /** [filter_map f l] applies [f] to each element of the list [l] and returns
      a new list with the values [v] for which [f] returned [Some v].  */

  let filter_map: ('a => option('a), list('a)) => list('a);

  /** [rev_filter_map f acc l] applies [f] to each element of the list [l] and
      prepends the values for which [f] returned [Some v] to list [acc]. [acc]
      is returned as the result and is in reverse order to the input.  This is
      a tail call optimised version of [filter_map] */

  let rev_filter_map: ('a => option('a), list('a), list('a)) => list('a);

  /** [flatten l] given a list of [json] elements filters the [`List] elements
      and flattens them into a single list. This is the same as
      [filter_list |> List.flatten] */

  let flatten: list([> | `List(list('a))]) => list('a);

  /** [rev_flatten acc l] is the tail recursive version of [flatten] with
      the result accumulated in [acc]. The result is in reverse order.  */

  let rev_flatten: (list('a), list([> | `List(list('a))])) => list('a);

  /** [filter_index i l] returns the [i]'th element from each [`List l1] in [l].
      Thus,
      {[
        [[`List [`Int 2; `Int 3]; `List [`Int 4; `Int 5]] |> filter_index 1]
      ]}
      returns [[`Int 3; `Int 5]]
      */

  let filter_index: (int, list([> | `List(list(json))])) => list(json);

  /** [filter_list l] returns a list of all the values of [`List value] elements in l */

  let filter_list: list([> | `List('a)]) => list('a);

  /** [filter_assoc l] returns a list of all the values of [`Assoc value] elements in l */

  let filter_assoc: list([> | `Assoc('a)]) => list('a);

  /** [filter_bool l] returns a list of all the values of [`Bool value] elements in l */

  let filter_bool: list([> | `Bool(bool)]) => list(bool);

  /** [filter_float l] returns a list of all the values of [`Float value] elements in l */

  let filter_float: list([> | `Float(float)]) => list(float);

  /** [filter_string l] returns a list of all the values of [`String value] elements in l */

  let filter_string: list([> | `String(string)]) => list(string);

  /** [filter_member key js] given a [key] and a list of json [`Assoc]-s, [js], returns
      the list of values extracted from each of the [`Assoc]-s. Thus,
      {[
        [[`Assoc [("id", `Int 1)]; `Assoc [("id", `Int 2)]]] |> filter_member "id"]
      ]}
      returns [[`Int 1; `Int 2]] */

  let filter_member:
    (string, list([> | `Assoc(list((string, json)))])) => list(json);

  /**[filter_number l] returns a list of all the values of [`Float value] elements in l */

  let filter_number: list([> | `Float(float)]) => list(float);

  /** [keys assoc] returns all the keys from the [`Assoc] element */

  let keys: [> | `Assoc(list((string, 'a)))] => list(string);

  /** [values assoc] returns all the values from the [`Assoc] element */

  let values: [> | `Assoc(list((string, 'a)))] => list('a);

  /** [combine assoc1 assoc2] appends the associative lists of two [`Assoc] elements returning
      an [`Assoc] element */

  let combine:
    ([> | `Assoc(list('a))], [> | `Assoc(list('a))]) =>
    [> | `Assoc(list('a))];

  /** [sort json] sorts the [json] tree based on field names.  Objects and lists are sorted
      recursively. Note that the function only sorts field names and not the values.
      The sort is stable */

  let sort:
    ([> | `Assoc(list((string, 'a))) | `List(list('a))] as 'a) => 'a;
};

module type Basic = {
  type json;

  /** [to_number json] converts [`Float f] to [f] and [Int i] to [float i] */

  let to_number: [> | `Int(int) | `Float(float)] => float;

  /** [to_number_option json] converts [`Float f] to [Some f], [`Int i] to [Some (float i)]
      and [`Null] to [None] */

  let to_number_option:
    [> | `Int(int) | `Float(float) | `Null] => option(float);

  /** [to_int json] converts [`Int i] to [i] */

  let to_int: [> | `Int(int)] => int;

  /** [to_int_option json] converts [`Int i] to [Some i] and [`Null] to [None] */

  let to_int_option: [> | `Int(int) | `Null] => option(int);

  /** [filter_int l] returns a list of all the values of [`Int value] elements in l */

  let filter_int: list([> | `Int(int)]) => list(int);

  let filter_number: list([> | `Int(int) | `Float(float)]) => list(float);
};

module type Extended = {
  type json;

  /** [sort json] sorts the [json] tree based on field names.  Objects and lists are sorted
      recursively. Note that the function only sorts field names and not the values.
      The sort is stable */

  let sort:
    (
      [>
        | `Assoc(list((string, 'a)))
        | `List(list('a))
        | `Tuple(list('a))
        | `Variant('b, option('a))
      ] as 'a
    ) =>
    'a;
};
