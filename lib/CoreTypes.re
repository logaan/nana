module StringMap = Map.Make(String);

type expression =
  | Number(int)
  | Symbol(string)
  | List(list(expression))
  | Function(list(expression) => expression)
  | Lambda(environment, list(string), list(expression))

and environment = StringMap.t(expression);

type readResult =
  | EndOfTokens(list(expression))
  | EndOfExpression(list(expression), list(string));

exception ArgumentError(string);

exception UnbalancedParens;
