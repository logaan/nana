module StringMap = Map.Make(String);

type builtinFunction =
  | Equals
  | Plus
  | Minus
  | Times
  | First
  | Println;

type expression =
  | True
  | False
  | Number(int)
  | Symbol(string)
  | List(list(expression))
  | Function(builtinFunction)
  | Lambda(ref(environment), list(string), expression)
  | Continuation(stack)

and environment = StringMap.t(expression)

and frame =
  | Start(environment, expression)
  | AddToEnv(environment, string)
  | PushBranch(environment, expression, expression)
  | EvalFn(environment, list(expression))
  | EvalArgs(environment, expression, list(expression), list(expression))
  | Stop(environment, expression)

and stack = list(frame);

type readResult =
  | EndOfTokens(list(expression))
  | EndOfExpression(list(expression), list(string));

exception ArgumentError(string);

exception UnbalancedParens;
