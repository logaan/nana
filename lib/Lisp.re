open CoreTypes;

let tokenize = str =>
  str
  |> Str.global_replace(Str.regexp("[()]"), " \\0 ")
  |> Str.split(Str.regexp("[ \n]+"));

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

let rec apply = (fn, args): evalStep =>
  switch (fn) {
  | Function(fn) => Final(StandardLibrary.builtinApply(fn, args))
  | Lambda(environment, argNames, body) =>
    List.map(
      evalPureExpression(_, argsToEnv(environment, argNames, args)),
      body,
    )
    |> List.rev
    |> List.hd
  | _ => raise(ArgumentError("Lists must start with functions"))
  }

and evalPureExpression = (expression, environment): evalStep =>
  switch (expression) {
  | Number(i) => Final(Number(i))

  | Symbol(s) => Final(StringMap.find(s, environment))

  | List([Symbol("quote"), quotedValue]) => Final(quotedValue)
  | List([Symbol("quote"), ..._tooManyArgs]) =>
    raise(ArgumentError("Quote only takes one argument"))

  | List([Symbol("lambda"), List(argsExprs), ...body]) =>
    Final(Lambda(environment, List.map(argsToStrings, argsExprs), body))
  | List([Symbol("lambda")]) =>
    raise(ArgumentError("Lambda needs args and body"))

  | List([func, ...argExprs]) =>
    switch (evalPureExpression(func, environment)) {
    | Final(result) =>
      let args =
        List.map(
          expr =>
            switch (evalPureExpression(expr, environment)) {
            | Final(expr) => expr
            },
          argExprs,
        );
      apply(result, args);
    }

  | List(_) => raise(ArgumentError("Lists must start with symbols"))
  | Function(_) => raise(ArgumentError("There's no syntax for functions"))
  | Lambda(_, _, _) => raise(ArgumentError("There's no syntax for lambda"))
  }

and evalExpression = (environment, expression) =>
  switch (expression) {
  | List([Symbol("def"), Symbol(name), valueExpr]) =>
    switch (evalPureExpression(valueExpr, environment)) {
    | Final(result) =>
      let newEnv = StringMap.add(name, result, environment);
      (newEnv, result);
    }
  | expression =>
    switch (evalPureExpression(expression, environment)) {
    | Final(result) => (environment, result)
    }
  };

let eval = (environment, code) => {
  List.fold_left(
    ((environment, _lastResult), expression) =>
      evalExpression(environment, expression),
    (environment, Symbol("start")),
    parse(code),
  );
};

let evalOnceOff = code => {
  let (_, result) = eval(StandardLibrary.environment, code);
  result;
};
