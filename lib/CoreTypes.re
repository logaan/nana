module StringMap = Map.Make(String);
type lispFn = list(expression) => expression

and expression =
  | Number(int)
  | Symbol(string)
  | List(list(expression))
  | Function(lispFn)
  | Lambda(environment, list(string), list(expression))

and environment = StringMap.t(expression);

type readResult =
  | EndOfTokens(list(expression))
  | EndOfExpression(list(expression), list(string));

exception ArgumentError(string);

exception UnbalancedParens;
