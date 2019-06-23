type lispFn = list(expression) => expression

and expression =
| Number(int)
| Symbol(string)
| List(list(expression))
| Function(lispFn);

type readResult =
  | EndOfTokens(list(expression))
  | EndOfExpression(list(expression), list(string));

module StringMap = Map.Make(String);

exception ArgumentError(string);
