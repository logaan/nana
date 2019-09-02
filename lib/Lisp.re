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

  | ["true", ...tail] => read([True, ...expressions], tail)

  | ["false", ...tail] => read([False, ...expressions], tail)

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

let notSpecialForm = word =>
  word != Symbol("def")
  && word != Symbol("if")
  && word != Symbol("quote")
  && word != Symbol("lambda");

let rec apply = (env, fn, args) =>
  switch (fn) {
  | Function(fn) =>
    let result = StandardLibrary.builtinApply(fn, args);
    Stop(env, result);

  | Lambda(environment, argNames, body) =>
    let merged = argsToEnv(environment, argNames, args);
    Start(merged, body);

  | _ => raise(ArgumentError("Lists must start with functions"))
  }

and evalStart = (env, expr) =>
  switch (expr) {
  | True => Stop(env, True)
  | False => Stop(env, False)

  | Number(i) => Stop(env, Number(i))

  | Symbol(s) =>
    try (Stop(env, StringMap.find(s, env))) {
    | Not_found => raise(ArgumentError(s ++ " not found"))
    }

  | List([Symbol("def"), Symbol(name), valueExpr]) =>
    let result = eval(valueExpr, env);
    let newEnv = StringMap.add(name, result, env);
    Stop(newEnv, result);
  | List([Symbol("if"), conditionalExpr, thenExpr, elseExpr]) =>
    let next = eval(conditionalExpr, env) == True ? thenExpr : elseExpr;
    Stop(env, eval(next, env));
  | List([Symbol("quote"), quotedValue]) => Stop(env, quotedValue)
  | List([Symbol("lambda"), List(argsExprs), body]) =>
    Stop(env, Lambda(env, List.map(argsToStrings, argsExprs), body))

  | List(_) => raise(ArgumentError("Lists must start with a fn."))
  | Function(_) => raise(ArgumentError("You can't eval a function."))
  | Lambda(_, _, _) => raise(ArgumentError("You can't eval a lambda."))
  }

and evalFrame = stack =>
  switch (stack) {
  | [Start(env, List([func, ...argExprs])), ...stack]
      when notSpecialForm(func) => [
      Start(env, func),
      EvalFn(env, argExprs),
      ...stack,
    ]
  // This one has to be this way or a can't be found in the y combinator test
  | [Stop(_, result), EvalFn(env, argExprs), ...stack] => [
      EvalArgs(env, result, [], argExprs),
      ...stack,
    ]
  | [Start(env, expr), ...stack] => [evalStart(env, expr)] @ stack
  | [EvalArgs(env, fn, evaluated, [next, ...unevaluated]), ...stack] => [
      Start(env, next),
      EvalArgs(env, fn, evaluated, unevaluated),
      ...stack,
    ]
  // This one has to be this way or n can't be found somewhere before the
  // y-combinator one
  | [Stop(env, result), EvalArgs(_, fn, evaluated, unevaluated), ...stack] => [
      EvalArgs(env, fn, [result, ...evaluated], unevaluated),
      ...stack,
    ]

  | [EvalArgs(env, fn, evaluated, []), ...stack] => [
      apply(env, fn, List.rev(evaluated)),
      ...stack,
    ]
  | [EvalFn(_, _), ..._] =>
    raise(
      ArgumentError("This should never appear in the head of the stack."),
    )
  | [Stop(_, _), ..._] =>
    raise(ArgumentError("Don't know how to handle this stop."))
  | [] => raise(ArgumentError("Nothing on the stack."))
  }

and evalStepper = stack =>
  switch (stack) {
  | [] => raise(ArgumentError("Nothing on the stack."))
  | [Stop(env, result)] => (env, result)
  | stack => evalStepper(evalFrame(stack))
  }

and eval = (expression, env): expression => {
  print_endline("eval");
  let (_, result) = evalStepper([Start(env, expression)]);
  result;
};

let evalExpressions = (environment, code) => {
  List.fold_left(
    ((env, _lastResult), expression) =>
      evalStepper([Start(env, expression)]),
    (environment, Symbol("start")),
    parse(code),
  );
};

let evalOnceOff = code => {
  let (_, result) = evalExpressions(StandardLibrary.environment, code);
  result;
};
