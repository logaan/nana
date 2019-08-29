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

let rec apply = (env, fn, args) =>
  switch (fn) {
  | Function(fn) => Stop(env, StandardLibrary.builtinApply(fn, args))

  | Lambda(environment, argNames, body) =>
    // I think it's actually fine to ignore the stack here. That maybe gets us
    // tco? :S
    let merged = argsToEnv(environment, argNames, args);
    evalStep(Start(merged, body));

  | _ => raise(ArgumentError("Lists must start with functions"))
  }

and evalStep = evalStep => {
  switch (evalStep) {
  | Stop(_, _) => raise(ArgumentError("Should never be passed to ePE"))

  | Start(e, True) => Stop(e, True)
  | Start(e, False) => Stop(e, False)
  | Start(e, Number(i)) => Stop(e, Number(i))

  | Start(env, Symbol(s)) => Stop(env, StringMap.find(s, env))

  | Start(env, List([Symbol("if"), conditionalExpr, thenExpr, elseExpr])) =>
    Stop(
      env,
      eval(conditionalExpr, env) == True
        ? eval(thenExpr, env) : eval(elseExpr, env),
    )

  | Start(e, List([Symbol("quote"), quotedValue])) => Stop(e, quotedValue)
  | Start(_, List([Symbol("quote"), ..._tooManyArgs])) =>
    raise(ArgumentError("Quote only takes one argument"))

  | Start(env, List([Symbol("lambda"), List(argsExprs), body])) =>
    Stop(env, Lambda(env, List.map(argsToStrings, argsExprs), body))
  | Start(_, List([Symbol("lambda"), ..._])) =>
    raise(ArgumentError("Lambda needs args and a single body expression"))

  | Start(env, List([func, ...argExprs])) =>
    EvalArgs(env, eval(func, env), [], argExprs)

  | EvalArgs(env, fn, evaluated, []) => apply(env, fn, List.rev(evaluated))

  | EvalArgs(env, fn, evaluated, [next, ...unevaluated]) =>
    EvalArgs(env, fn, [eval(next, env), ...evaluated], unevaluated)

  | Start(_, List(_)) => raise(ArgumentError("Lists must start with a fn."))
  | Start(_, Function(_)) =>
    raise(ArgumentError("You can't eval a function."))
  | Start(_, Lambda(_, _, _)) =>
    raise(ArgumentError("You can't eval a lambda."))
  };
}

and evalStepper = step => {
  // print_endline("evalStepper");
  switch (step) {
  | Stop(_, result) => result
  | EvalArgs(env, fn, evaluated, unevaluated) =>
    evalStepper(evalStep(EvalArgs(env, fn, evaluated, unevaluated)))
  | Start(_) => raise(ArgumentError("Won't be returned by ePE"))
  };
}

and eval = (expression, env): expression => {
  // print_endline("eval");
  evalStepper(evalStep(Start(env, expression)));
}

and evalTopLevel = (environment, expression) =>
  switch (expression) {
  | List([Symbol("def"), Symbol(name), valueExpr]) =>
    let result = eval(valueExpr, environment);
    let newEnv = StringMap.add(name, result, environment);
    (newEnv, result);
  | expression => (environment, eval(expression, environment))
  };

let evalExpressions = (environment, code) => {
  List.fold_left(
    ((environment, _lastResult), expression) =>
      evalTopLevel(environment, expression),
    (environment, Symbol("start")),
    parse(code),
  );
};

let evalOnceOff = code => {
  let (_, result) = evalExpressions(StandardLibrary.environment, code);
  result;
};
