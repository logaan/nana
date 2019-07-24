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
  | Lambda(environment, list(string), expression)

and environment = StringMap.t(expression);

type readResult =
  | EndOfTokens(list(expression))
  | EndOfExpression(list(expression), list(string));

exception ArgumentError(string);

exception UnbalancedParens;

type evalStep =
  | Start(expression)
  | EvalArgs(expression, list(expression), list(expression))
  | Stop(expression);
