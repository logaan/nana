open CoreTypes;

let rec string_of_expression = expr =>
  switch (expr) {
  | Number(n) => "Number(" ++ string_of_int(n) ++ ")"
  | Symbol(s) => "Symbol(" ++ s ++ ")"
  | List(l) =>
    let children = List.map(string_of_expression, l);
    "List(" ++ String.concat(", ", children) ++ ")";
  | Function(_) => "Function()"
  };

let string_of_expressions = exprs =>
  String.concat("\n", List.map(string_of_expression, exprs));

let tokenize = str => {
  let expanded = Str.global_replace(Str.regexp("[()]"), " \\0 ", str);
  Str.split(Str.regexp("[ \n]+"), expanded);
};

exception UnbalancedParens;

let isNumber = str => Str.string_match(Str.regexp("^[0-9]*$"), str, 0);

let rec read = (expressions, tokens) =>
  switch (tokens) {
  | [] => EndOfTokens(List.rev(expressions))

  | ["(", ...tail] =>
    switch (read([], tail)) {
    | EndOfTokens(_) => raise(UnbalancedParens)
    | EndOfExpression(nestedExpressions, newTail) =>
      read([List(nestedExpressions), ...expressions], newTail)
    }

  | [")", ...tail] => EndOfExpression(List.rev(expressions), tail)

  | [head, ...tail] when isNumber(head) =>
    read([Number(int_of_string(head)), ...expressions], tail)

  | [head, ...tail] => read([Symbol(head), ...expressions], tail)
  };

let read_tokens = tokens =>
  switch (read([], tokens)) {
  | EndOfTokens(results) => results
  | EndOfExpression(_, _) => raise(UnbalancedParens)
  };

let parse = str => str |> tokenize |> read_tokens;

let apply = (fn, args) =>
  switch (fn) {
  | Function(fn) => fn(args);
  | _ => raise(ArgumentError("Lists must start with functions"))
  };

let rec eval = (expression, environment) =>
  switch (expression) {
  | Number(i) => Number(i)
  | Symbol(s) => StringMap.find(s, environment)
  | List([Symbol(functionName), ...argExprs]) =>
    let result = StringMap.find(functionName, environment);
    let args = List.map(expr => eval(expr, environment), argExprs);
    apply(result, args);
  | List(_) => raise(ArgumentError("Lists must start with symbols"))
  | Function(_) => raise(ArgumentError("There's no syntax for functions"))
  };
