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

let rec apply = (fn, args) =>
  switch (fn) {
  /* Done */
  | Function(fn) => Stop(StandardLibrary.builtinApply(fn, args))

  | Lambda(environment, argNames, body) =>
    List.map(
      expr =>
        evalPureExpression'(
          Start(expr),
          argsToEnv(environment, argNames, args),
        ),
      body,
    )
    |> List.rev
    |> List.hd
  | _ => raise(ArgumentError("Lists must start with functions"))
  }

and evalPureExpression' = (expression, environment) =>
  switch (expression) {
  | Stop(_) => raise(ArgumentError("Should never be passed to ePE"))

  /* Done */
  | Start(Number(i)) => Stop(Number(i))

  /* Done */
  | Start(Symbol(s)) => Stop(StringMap.find(s, environment))

  /* Done */
  | Start(List([Symbol("quote"), quotedValue])) => Stop(quotedValue)
  | Start(List([Symbol("quote"), ..._tooManyArgs])) =>
    raise(ArgumentError("Quote only takes one argument"))

  /* Done */
  | Start(List([Symbol("lambda"), List(argsExprs), ...body])) =>
    Stop(Lambda(environment, List.map(argsToStrings, argsExprs), body))
  | Start(List([Symbol("lambda")])) =>
    raise(ArgumentError("Lambda needs args and body"))

  | Start(List([func, ...argExprs])) =>
    switch (evalPureExpression'(Start(func), environment)) {
    | Stop(fn) => EvalArgs(fn, [], argExprs)
    | EvalArgs(_, _, _) => raise(ArgumentError("Unconsidered"))
    | Start(_) => raise(ArgumentError("Won't be returned by ePE"))
    }

  | EvalArgs(fn, evaluated, []) => apply(fn, List.rev(evaluated))

  | EvalArgs(fn, evaluated, [next, ...unevaluated]) =>
    switch (evalPureExpression'(Start(next), environment)) {
    | Stop(result) => EvalArgs(fn, [result, ...evaluated], unevaluated)
    | EvalArgs(_, _, _) => raise(ArgumentError("Unconsidered"))
    | Start(_) => raise(ArgumentError("Won't be returned by ePE"))
    }

  /* Done */
  | Start(List(_)) => raise(ArgumentError("Lists must start with symbols."))
  | Start(Function(_)) =>
    raise(ArgumentError("There's no syntax for functions."))
  | Start(Lambda(_, _, _)) =>
    raise(ArgumentError("There's no syntax for lambda."))
  }

and evalPureExpression = (step, environment) =>
  switch (step) {
  | Stop(result) => result
  | EvalArgs(fn, evaluated, unevaluated) =>
    evalPureExpression(
      evalPureExpression'(EvalArgs(fn, evaluated, unevaluated), environment),
      environment,
    )
  | Start(_) => raise(ArgumentError("Won't be returned by ePE"))
  }

and evalPureExpressionKickoff = (expression, environment) =>
  evalPureExpression(
    evalPureExpression'(Start(expression), environment),
    environment,
  )

and evalExpression = (environment, expression) =>
  switch (expression) {
  | List([Symbol("def"), Symbol(name), valueExpr]) =>
    let result = evalPureExpressionKickoff(valueExpr, environment);
    let newEnv = StringMap.add(name, result, environment);
    (newEnv, result);
  | expression => (
      environment,
      evalPureExpressionKickoff(expression, environment),
    )
  };

/*
   Takes unparsed code, evaluates it in the standard env and returns the result
   and the (possibly modified) environment.
 */
let eval = (environment, code) => {
  List.fold_left(
    ((environment, _lastResult), expression) =>
      evalExpression(environment, expression),
    (environment, Symbol("start")),
    parse(code),
  );
};

/*
   Takes unparsed code, evaluates it in the standard env and returns the result
 */
let evalOnceOff = code => {
  let (_, result) = eval(StandardLibrary.environment, code);
  result;
};
