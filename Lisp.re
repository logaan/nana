type lispFn = list(expression) => expression

and expression =
  | Number(int)
  | Symbol(string)
  | List(list(expression))
  | Function(lispFn);

exception ArgumentError(string);

let rec string_of_expression = expr =>
  switch (expr) {
  | Number(n) => "Number(" ++ string_of_int(n) ++ ")"
  | Symbol(s) => "Symbol(" ++ s ++ ")"
  | List(l) => "List(" ++ String.concat(", ", List.map(string_of_expression, l)) ++ ")"
  | Function(_) => "Function()"
  };

let string_of_expressions = exprs =>
  String.concat("\n", List.map(string_of_expression, exprs));

let tokenize = str => {
  let expanded = Str.global_replace(Str.regexp("[()]"), " \\0 ", str);
  Str.split(Str.regexp("[ \n]+"), expanded);
};

type readResult =
  | EndOfTokens(list(expression))
  | EndOfExpression(list(expression), list(string));

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

module StringMap = Map.Make(String);

let lispPlus = args =>
  switch (args) {
  | [Number(a), Number(b)] => Number(a + b)
  | _ => raise(ArgumentError("+ takes two numbers"))
  };

let lispMinus = args =>
  switch (args) {
  | [Number(a), Number(b)] => Number(a - b)
  | _ => raise(ArgumentError("- takes two numbers"))
  };

let lispFirst = args =>
  switch (args) {
  | [first, ..._rest] => first
  | _ => raise(ArgumentError("first a list with at least one value"))
  };

let environment =
  StringMap.empty
  |> StringMap.add("+", Function(lispPlus))
  |> StringMap.add("-", Function(lispMinus))
  |> StringMap.add("first", Function(lispFirst));

let rec eval = (expression, environment) =>
  switch (expression) {
  | Number(i) => Number(i)
  | Symbol(s) => StringMap.find(s, environment)
  | List([Symbol(functionName), ...argExprs]) =>
    let result: expression = StringMap.find(functionName, environment)
    switch (result) {
    | Function(fn) => fn(List.map(expression => eval(expression, environment), argExprs))
    | _ => raise(ArgumentError("Lists must start with functions"))
    }
  | List(_) => raise(ArgumentError("Lists must start with symbols"))
  | Function(_) => raise(ArgumentError("There's no syntax for functions"))
  };
