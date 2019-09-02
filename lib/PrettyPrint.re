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

let printEvalStep = evalStep =>
  switch (evalStep) {
  | Stop(_) => raise(ArgumentError("Should never be passed to ePE"))
  | Start(environment, x) =>
    print_endline("Start");
    x |> string_of_expression |> print_endline;
    print_environment(environment);
  | AddToEnv(_environment, name) =>
    print_endline("AddToEnv");
    print_endline(name);
  | EvalFn(_environment, args) =>
    print_endline("EvalFn");
    print_expressions(args);
  | EvalArgs(_environment, fn, left, right) =>
    print_endline("EvalArgs");
    print_expressions(left);
    print_endline("------");
    print_expressions(right);
    print_endline("------");
    fn |> string_of_expression |> print_endline;
  };
