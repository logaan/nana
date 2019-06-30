open CoreTypes;

let rec string_of_expression = expr =>
  switch (expr) {
  | Number(n) => "Number(" ++ string_of_int(n) ++ ")"
  | Symbol(s) => "Symbol(" ++ s ++ ")"
  | List(l) =>
    let children = List.map(string_of_expression, l);
    "List(" ++ String.concat(", ", children) ++ ")";
  | Function(_) => "Function()"
  | Lambda(_env, args, body) =>
    "Lambda("
    ++ "environment"
    ++ ", ["
    ++ String.concat(", ", args)
    ++ "], ["
    ++ String.concat(", ", List.map(string_of_expression, body))
    ++ "])"
  };

let string_of_expressions = exprs =>
  String.concat("\n", List.map(string_of_expression, exprs));
