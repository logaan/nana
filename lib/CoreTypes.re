module StringMap = Map.Make(String);

type builtinFunction =
  | Plus
  | Minus
  | Times
  | First
  | Println;

type expression =
  | Number(int)
  | Symbol(string)
  | List(list(expression))
  | Function(builtinFunction)
  | Lambda(environment, list(string), list(expression))

and environment = StringMap.t(expression);

type readResult =
  | EndOfTokens(list(expression))
  | EndOfExpression(list(expression), list(string));

exception ArgumentError(string);

exception UnbalancedParens;

// Eval may be called with start but never with Stop. It may return Stop but
// never Start.
type evalStepIn =
  | Start(expression);

type evalStepOut =
  | Stop(expression);
