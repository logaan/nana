open CoreTypes;

let string_of_builtin_function = fn =>
  switch (fn) {
  | Equals => "Equals"
  | Plus => "Plus"
  | Minus => "Minus"
  | Times => "Times"
  | First => "First"
  | Println => "Println"
  };

let rec string_of_expression = expr =>
  switch (expr) {
  | True => "True"
  | False => "False"
  | Number(n) => "Number(" ++ string_of_int(n) ++ ")"
  | Symbol(s) => "Symbol(" ++ s ++ ")"
  | List(l) =>
    let children = List.map(string_of_expression, l);
    "List(" ++ String.concat(", ", children) ++ ")";
  | Function(fn) => "Function(" ++ string_of_builtin_function(fn) ++ ")"
  | Lambda(_env, args, body) =>
    "Lambda("
    ++ "environment"
    ++ ", ["
    ++ String.concat(", ", args)
    ++ "], ["
    ++ string_of_expression(body)
    ++ "])"
  };

let string_of_expressions = exprs =>
  String.concat("\n", List.map(string_of_expression, exprs));

let print_expression = expr => expr |> string_of_expression |> print_endline;

let print_expressions = exprs =>
  exprs |> string_of_expressions |> print_endline;

let print_environment = environment =>
  StringMap.iter(
    (x, y) => Printf.printf("%s -> %s\n", x, string_of_expression(y)),
    environment,
  );

let string_of_frame = frame =>
  switch (frame) {
  | Stop(_) => raise(ArgumentError("Should never be passed to ePE"))
  | Start(_environment, expr) =>
    "Start(environment, " ++ string_of_expression(expr) ++ ")"
  | AddToEnv(_environment, name) => "AddToEnv(environment, " ++ name ++ ")"
  | EvalFn(_environment, args) =>
    "EvalFn(environment, " ++ string_of_expressions(args) ++ ")"
  | PushBranch(_environment, thenExpr, elseExpr) =>
    "PushBranch(environment, "
    ++ string_of_expression(thenExpr)
    ++ ", "
    ++ string_of_expression(elseExpr)
    ++ ")"
  | EvalArgs(_environment, fn, left, right) =>
    "EvalArgs(environment, "
    ++ string_of_expression(fn)
    ++ ", "
    ++ string_of_expressions(left)
    ++ ", "
    ++ string_of_expressions(right)
    ++ ")"
  };
