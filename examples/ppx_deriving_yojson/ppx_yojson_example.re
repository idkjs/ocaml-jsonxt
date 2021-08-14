module Item = {
  [@deriving yojson]
  type t = {
    str: string,
    cost: float,
  };
};

module Stock = {
  [@deriving yojson]
  type t = {
    desc: string,
    inventory: int,
    backorder: option(int),
    items: list(Item.t),
  };
};

let () = {
  let item1 = {Item.str: "Store Baked Beans", cost: 1.22};
  let item2 = {Item.str: "Branded Baked Beans", cost: 1.47};
  let stock = {
    Stock.desc: "Beans",
    inventory: 2,
    backorder: Some(3),
    items: [item1, item2],
  };
  let json = Stock.to_yojson(stock);
  print_endline(Yojson.Safe.show(json));
  print_endline(Yojson.Safe.pretty_to_string(json));
};
