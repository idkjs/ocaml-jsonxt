let error = (msg, _json) => raise(Failure(msg));

module Internal = {
  module type S = {
    type json;
    let null: unit => json;
  };

  module type Internal_strict_intf = {
    type json;

    let member: (string, [> | `Assoc(list((string, json)))]) => json;
    let index: (int, [> | `List(list(json))]) => json;
    let map:
      (json => json, [> | `List(list(json))]) => [> | `List(list(json))];
    let to_assoc:
      [> | `Assoc(list((string, json)))] => list((string, json));
    let to_bool: [> | `Bool(bool)] => bool;
    let to_float: [> | `Float(float)] => float;
    let to_string: [> | `String(string)] => string;
    let to_string_option: [> | `String(string) | `Null] => option(string);
    let to_option: (([> | `Null] as 'a) => json, 'a) => option(json);
    let to_list: [> | `List(list(json))] => list(json);
    let to_bool_option: [> | `Bool(bool) | `Null] => option(bool);
    let to_float_option: [> | `Float(float) | `Null] => option(float);
    let to_number: [> | `Float(float)] => float;
    let to_number_option: [> | `Float(float) | `Null] => option(float);
    let convert_each: (json => json, [> | `List(list(json))]) => list(json);
    let rev_filter_map: ('a => option('a), list('a), list('a)) => list('a);
    let filter_map: ('a => option('a), list('a)) => list('a);
    let rev_flatten: (list('a), list([> | `List(list('a))])) => list('a);
    let flatten: list([> | `List(list('a))]) => list('a);
    let filter_index: (int, list([> | `List(list(json))])) => list(json);
    let filter_list: list([> | `List('a)]) => list('a);
    let filter_assoc: list([> | `Assoc('a)]) => list('a);
    let filter_bool: list([> | `Bool(bool)]) => list(bool);
    let filter_float: list([> | `Float(float)]) => list(float);
    let filter_string: list([> | `String(string)]) => list(string);
    let filter_member:
      (string, list([> | `Assoc(list((string, json)))])) => list(json);
    let filter_number: list([> | `Float(float)]) => list(float);
    let keys: [> | `Assoc(list((string, 'a)))] => list(string);
    let values: [> | `Assoc(list((string, 'a)))] => list('a);
    let combine:
      ([> | `Assoc(list('a))], [> | `Assoc(list('a))]) =>
      [> | `Assoc(list('a))];
    let sort:
      ([> | `Assoc(list((string, 'a))) | `List(list('a))] as 'a) => 'a;
  };

  module Shared = {
    let rec rev_filter_map = (f, acc, l) =>
      switch (l) {
      | [] => acc
      | [hd, ...tl] =>
        switch (f(hd)) {
        | None => rev_filter_map(f, acc, tl)
        | Some(v) => rev_filter_map(f, [v, ...acc], tl)
        }
      };

    let filter_map = (f, l) => List.rev(rev_filter_map(f, [], l));

    let rec rev_flatten = (acc, l) =>
      switch (l) {
      | [] => acc
      | [hd, ...tl] =>
        switch (hd) {
        | `List(l2) => rev_flatten(List.rev_append(l2, acc), tl)
        | _ => rev_flatten(acc, tl)
        }
      };

    let flatten = l => List.rev(rev_flatten([], l));
  };

  module Strict = (M: S) : (Internal_strict_intf with type json = M.json) => {
    type json = M.json;

    let assoc = (name, obj): json =>
      try(List.assoc(name, obj)) {
      | Not_found => M.null()
      };

    let member = (name, v): json =>
      switch (v) {
      | `Assoc(obj) => assoc(name, obj)
      | json =>
        error("Expected `Assoc to find name '" ++ name ++ "' in", json)
      };

    let index = (i, v): json =>
      switch (v) {
      | `List(l) =>
        let len = List.length(l);
        let i' =
          if (i < 0) {
            len + i;
          } else {
            i;
          };
        if (i' < 0 || i' >= len) {
          raise(Invalid_argument(string_of_int(i) ++ " out of bounds"));
        } else {
          List.nth(l, i');
        };
      | json => error("Can't index none `List type ", json)
      };

    let map = (f, v) =>
      switch (v) {
      | `List(l) => `List(List.map(f, l))
      | json => error("Can't map over none `List type ", json)
      };

    let to_assoc =
      fun
      | `Assoc(obj) => obj
      | json => error("Expected `Assoc", json);
    let to_bool =
      fun
      | `Bool(b) => b
      | json => error("Expected `Bool", json);
    let to_float =
      fun
      | `Float(f) => f
      | json => error("Expected `Float", json);

    let to_string =
      fun
      | `String(s) => s
      | json => error("Expected `String", json);

    let to_string_option =
      fun
      | `String(s) => Some(s)
      | `Null => None
      | json => error("Expected `String or `Null", json);

    let to_option = (f, v): option(json) =>
      switch (v) {
      | `Null => None
      | v => Some(f(v))
      };

    let to_list = (v): list(json) =>
      switch (v) {
      | `List(l) => l
      | json => error("Expected `List", json)
      };

    let to_float_option =
      fun
      | `Float(f) => Some(f)
      | `Null => None
      | json => error("Expected `Float or `Null", json);

    let to_bool_option =
      fun
      | `Bool(b) => Some(b)
      | `Null => None
      | json => error("Expected `Bool or `Null", json);

    let to_number =
      fun
      | `Float(f) => f
      | json => error("Expected `Float", json);

    let to_number_option =
      fun
      | `Float(f) => Some(f)
      | `Null => None
      | json => error("Expected `Float or `Null", json);

    let convert_each = f =>
      fun
      | `List(l) => List.map(f, l)
      | json => error("Expected `List", json);

    let rev_filter_map = Shared.rev_filter_map;
    let filter_map = Shared.filter_map;

    let rev_flatten = Shared.rev_flatten;
    let flatten = Shared.flatten;

    let filter_index = (i, l) =>
      filter_map(
        fun
        | `List(l) =>
          try(Some(List.nth(l, i))) {
          | _ => None
          }
        | _ => None,
        l,
      );

    let filter_list = l =>
      filter_map(
        fun
        | `List(l) => Some(l)
        | _ => None,
        l,
      );
    let filter_assoc = l =>
      filter_map(
        fun
        | `Assoc(l) => Some(l)
        | _ => None,
        l,
      );
    let filter_bool = l =>
      filter_map(
        fun
        | `Bool(b) => Some(b)
        | _ => None,
        l,
      );
    let filter_float = l =>
      filter_map(
        fun
        | `Float(f) => Some(f)
        | _ => None,
        l,
      );
    let filter_string = l =>
      filter_map(
        fun
        | `String(s) => Some(s)
        | _ => None,
        l,
      );

    let filter_member = (k, l) =>
      filter_map(
        fun
        | `Assoc(l) =>
          try(Some(List.assoc(k, l))) {
          | _ => None
          }
        | _ => None,
        l,
      );

    let filter_number = l =>
      filter_map(
        fun
        | `Float(f) => Some(f)
        | _ => None,
        l,
      );

    let keys = o => to_assoc(o) |> List.map(((key, _)) => key);

    let values = o => to_assoc(o) |> List.map(((_, value)) => value);

    let combine = (first, second) =>
      switch (first, second) {
      | (`Assoc(a), `Assoc(b)) => `Assoc(a @ b)
      | (_, _) => raise(Invalid_argument("Expected two objects"))
      };

    let rec sort = json =>
      switch (json) {
      | `Assoc(o) =>
        let o = List.rev(List.rev_map(((k, v)) => (k, sort(v)), o));
        `Assoc(
          (List.stable_sort(((k1, _), (k2, _)) => String.compare(k1, k2)))(
            o,
          ),
        );
      | `List(l) => `List(List.rev(List.rev_map(sort, l)))
      | el => el
      };
  };

  module type Internal_basic_intf = {
    let to_number: [> | `Int(int) | `Float(float)] => float;
    let to_number_option:
      [> | `Int(int) | `Float(float) | `Null] => option(float);
    let to_int: [> | `Int(int)] => int;
    let to_int_option: [> | `Int(int) | `Null] => option(int);
    let filter_int: list([> | `Int(int)]) => list(int);
    let filter_number:
      list([> | `Int(int) | `Float(float)]) => list(float);
  };

  module Basic = (M: S) : Internal_basic_intf => {
    let to_number =
      fun
      | `Int(i) => float(i)
      | `Float(f) => f
      | json => error("Expected `Int or `Float", json);

    let to_number_option =
      fun
      | `Int(i) => Some(float(i))
      | `Float(f) => Some(f)
      | `Null => None
      | json => error("Expected `Int, `Float or `Null", json);

    let to_int =
      fun
      | `Int(i) => i
      | json => error("Expected `Int", json);

    let to_int_option =
      fun
      | `Int(i) => Some(i)
      | `Null => None
      | json => error("Expected `Int or `Null", json);

    let filter_int = l =>
      Shared.filter_map(
        fun
        | `Int(i) => Some(i)
        | _ => None,
        l,
      );
    let filter_number = l =>
      Shared.filter_map(
        fun
        | `Int(i) => Some(float(i))
        | `Float(f) => Some(f)
        | _ => None,
        l,
      );
  };

  module type Internal_extended_intf = {
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

  module Extended = (M: S) : Internal_extended_intf => {
    let rec sort = json =>
      switch (json) {
      | `Assoc(o) =>
        let o = List.rev(List.rev_map(((k, v)) => (k, sort(v)), o));
        `Assoc(
          (List.stable_sort(((k1, _), (k2, _)) => String.compare(k1, k2)))(
            o,
          ),
        );
      | `Tuple(l)
      | `List(l) => `List(List.rev(List.rev_map(sort, l)))
      | `Variant(k, Some(v)) as v1 =>
        let v' = sort(v);
        if (v' === v) {
          v1;
        } else {
          `Variant((k, Some(v')));
        };
      | el => el
      };
  };
};

module Strict = {
  module M = {
    type json = Json.Strict.json;
    let null = () => `Null;
  };
  include Internal.Strict(M);
};

module Basic = {
  module M = {
    type json = Json.Basic.json;
    let null = () => `Null;
  };
  include Internal.Strict(M);
  include Internal.Basic(M);
};

module Extended = {
  module M = {
    type json = Json.Extended.json;
    let null = () => `Null;
  };
  include Internal.Strict(M);
  include Internal.Basic(M);
  include Internal.Extended(M);
};

module Yojson_safe = {
  module M = {
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

    let null = () => `Null;
  };
  include Internal.Strict(M);
  include Internal.Basic(M);
  include Internal.Extended(M);
};
