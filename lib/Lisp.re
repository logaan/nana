open CoreTypes;
open PrettyPrint;

let tokenize = str => {
  let expanded = Str.global_replace(Str.regexp("[()]"), " \\0 ", str);
  Str.split(Str.regexp("[ \n]+"), expanded);
};

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

let argsToEnv = (env, names, values) => {
  let add = (env, name, value) => StringMap.add(name, value, env);
  List.fold_left2(add, env, names, values);
};

let argsToStrings = exp =>
  switch (exp) {
  | Symbol(name) => name
  | _ => raise(ArgumentError("All arguments must be symbols"))
  };

let rec apply = (fn, args) =>
  switch (fn) {
  | Function(fn) => fn(args)
  | Lambda(environment, argNames, body) =>
    List.map(evalPureExpression(_, argsToEnv(environment, argNames, args)), body)
    |> List.rev
    |> List.hd
  | _ => raise(ArgumentError("Lists must start with functions"))
  }

and evalPureExpression = (expression, environment) =>
  switch (expression) {
  | Number(i) => Number(i)

  | Symbol(s) => StringMap.find(s, environment)

  | List([Symbol("quote"), quotedValue]) => quotedValue
  | List([Symbol("quote"), ..._tooManyArgs]) =>
    raise(ArgumentError("Quote only takes one argument"))

  | List([Symbol("let"), List([Symbol(name), valueExpr]), bodyExpr]) =>
    let newEnv =
      StringMap.add(name, evalPureExpression(valueExpr, environment), environment);
    evalPureExpression(bodyExpr, newEnv);
  | List([Symbol("let"), ..._wrongArgs]) =>
    raise(
      ArgumentError(
        "Let takes a pair of name, value and a single body expression",
      ),
    )

  | List([Symbol("lambda"), List(argsExprs), ...body]) =>
    Lambda(environment, List.map(argsToStrings, argsExprs), body)
  | List([Symbol("lambda")]) =>
    raise(ArgumentError("Lambda needs args and body"))

  | List([func, ...argExprs]) =>
    let result = evalPureExpression(func, environment);
    let args = List.map(expr => evalPureExpression(expr, environment), argExprs);
    apply(result, args);

  | List(_) => raise(ArgumentError("Lists must start with symbols"))
  | Function(_) => raise(ArgumentError("There's no syntax for functions"))
  | Lambda(_, _, _) => raise(ArgumentError("There's no syntax for lambda"))
  }

and evalExpression = (environment, expression) =>
  switch(expression) {
  | List([Symbol("def"), Symbol(name), valueExpr]) =>
    let result = evalPureExpression(valueExpr, environment);
    let newEnv = StringMap.add(name, result, environment);
    (newEnv, result)
  | expression => (environment, evalPureExpression(expression, environment))
  };

let evalAndPrint = code => {
  let parsed = parse(code);
  let (_lastEnvironment, lastResult) =
    List.fold_left(
    ((environment, _lastResult), expression) => evalExpression(environment, expression),
    (StandardLibrary.environment, Symbol("start")),
    parsed
  );
  print_endline(string_of_expression(lastResult));
};
