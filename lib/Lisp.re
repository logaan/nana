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
  | Function(fn) => Stop(StandardLibrary.builtinApply(fn, args))

  | Lambda(environment, argNames, body) =>
    Stop(eval(body, argsToEnv(environment, argNames, args)))

  | _ => raise(ArgumentError("Lists must start with functions"))
  }

and evalStep = (expression, environment) =>
  switch (expression) {
  | Stop(_) => raise(ArgumentError("Should never be passed to ePE"))

  | Start(Number(i)) => Stop(Number(i))

  | Start(Symbol(s)) => Stop(StringMap.find(s, environment))

  | Start(List([Symbol("quote"), quotedValue])) => Stop(quotedValue)
  | Start(List([Symbol("quote"), ..._tooManyArgs])) =>
    raise(ArgumentError("Quote only takes one argument"))

  | Start(List([Symbol("lambda"), List(argsExprs), body])) =>
    Stop(Lambda(environment, List.map(argsToStrings, argsExprs), body))
  | Start(List([Symbol("lambda")])) =>
    raise(ArgumentError("Lambda needs args and body"))
  | Start(List([Symbol("lambda"), ..._])) =>
    raise(ArgumentError("Lambda needs args and a single body expression"))

  | Start(List([func, ...argExprs])) =>
    EvalArgs(eval(func, environment), [], argExprs)

  | EvalArgs(fn, evaluated, []) => apply(fn, List.rev(evaluated))

  | EvalArgs(fn, evaluated, [next, ...unevaluated]) =>
    EvalArgs(fn, [eval(next, environment), ...evaluated], unevaluated)

  | Start(List(_)) => raise(ArgumentError("Lists must start with symbols."))
  | Start(Function(_)) =>
    raise(ArgumentError("There's no syntax for functions."))
  | Start(Lambda(_, _, _)) =>
    raise(ArgumentError("There's no syntax for lambda."))
  }

and evalStepper = (step, environment) =>
  switch (step) {
  | Stop(result) => result
  | EvalArgs(fn, evaluated, unevaluated) =>
    evalStepper(
      evalStep(EvalArgs(fn, evaluated, unevaluated), environment),
      environment,
    )
  | Start(_) => raise(ArgumentError("Won't be returned by ePE"))
  }

and eval = (expression, environment) =>
  evalStepper(evalStep(Start(expression), environment), environment)

/* Should maybe be evalTopLevel */
and evalExpression = (environment, expression) =>
  switch (expression) {
  | List([Symbol("def"), Symbol(name), valueExpr]) =>
    let result = eval(valueExpr, environment);
    let newEnv = StringMap.add(name, result, environment);
    (newEnv, result);
  | expression => (environment, eval(expression, environment))
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
