open CoreTypes;
// open PrettyPrint;

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

let rec apply = (fn, args) =>
  switch (fn) {
  | Function(fn) => Stop(StandardLibrary.builtinApply(fn, args))

  | Lambda(environment, argNames, body) =>
    // This should be calling evalStep but... hmm. Putting that result at the
    // top of a stack, with apply below it?
    // body |> string_of_expression |> print_endline;
    let merged = argsToEnv(environment, argNames, args);
    // print_environment(merged);
    evalStep(Start(body), merged);

  | _ => raise(ArgumentError("Lists must start with functions"))
  }

and evalStep = (expression, environment) => {
  // print_endline("evalStep");
  // switch (expression) {
  // | Stop(_) => raise(ArgumentError("Should never be passed to ePE"))
  // | Start(x) =>
  //   print_endline("Start");
  //   x |> string_of_expression |> print_endline;
  //   print_environment(environment);
  // | EvalArgs(_environment, fn, left, right) =>
  //   print_endline("EvalArgs");
  //   print_expressions(left);
  //   print_endline("------");
  //   print_expressions(right);
  //   print_endline("------");
  //   fn |> string_of_expression |> print_endline;
  // };
  // print_endline("");
  switch (expression) {
  | Stop(_) => raise(ArgumentError("Should never be passed to ePE"))

  | Start(True) => Stop(True)
  | Start(False) => Stop(False)
  | Start(Number(i)) => Stop(Number(i))

  | Start(Symbol(s)) => Stop(StringMap.find(s, environment))

  | Start(List([Symbol("if"), conditionalExpr, thenExpr, elseExpr])) =>
    Stop(eval(conditionalExpr, environment) == True ?
         eval(thenExpr, environment) :
         eval(elseExpr, environment))

  | Start(List([Symbol("quote"), quotedValue])) => Stop(quotedValue)
  | Start(List([Symbol("quote"), ..._tooManyArgs])) =>
    raise(ArgumentError("Quote only takes one argument"))

  | Start(List([Symbol("lambda"), List(argsExprs), body])) =>
    Stop(Lambda(environment, List.map(argsToStrings, argsExprs), body))
  | Start(List([Symbol("lambda"), ..._])) =>
    raise(ArgumentError("Lambda needs args and a single body expression"))

  | Start(List([func, ...argExprs])) =>
    EvalArgs(environment, eval(func, environment), [], argExprs)

  | EvalArgs(_environment, fn, evaluated, []) =>
    apply(fn, List.rev(evaluated))

  | EvalArgs(environment, fn, evaluated, [next, ...unevaluated]) =>
    EvalArgs(
      environment,
      fn,
      [eval(next, environment), ...evaluated],
      unevaluated,
    )

  | Start(List(_)) => raise(ArgumentError("Lists must start with a fn."))
  | Start(Function(_)) => raise(ArgumentError("You can't eval a function."))
  | Start(Lambda(_, _, _)) =>
    raise(ArgumentError("You can't eval a lambda."))
  };
}

/* I think what's happening is that eval is calling evalStep with a new
     environment but that environment is getting lost. I think maybe EvalArgs
     should contain an environment that gets used when calling back.

     Probably this is where a stack is important. Otherwise each method call will
     just add to the environment on and on.
   */
and evalStepper = (step, _environment) =>
  switch (step) {
  | Stop(result) => result
  | EvalArgs(environment, fn, evaluated, unevaluated) =>
    evalStepper(
      evalStep(
        EvalArgs(environment, fn, evaluated, unevaluated),
        environment,
      ),
      environment,
    )
  | Start(_) => raise(ArgumentError("Won't be returned by ePE"))
  }

and eval = (expression, environment):expression =>
  evalStepper(evalStep(Start(expression), environment), environment)

and evalTopLevel = (environment, expression) =>
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
      evalTopLevel(environment, expression),
    (environment, Symbol("start")),
    parse(code),
  );
};

let evalOnceOff = code => {
  let (_, result) = eval(StandardLibrary.environment, code);
  result;
};
