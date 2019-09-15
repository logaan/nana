open CoreTypes;
open PrettyPrint;
open StringMap;

let builtinApply = (func, args) =>
  switch (func, args) {
  | (Equals, [a, b]) => a == b ? True : False
  | (Plus, [Number(a), Number(b)]) => Number(a + b)
  | (Minus, [Number(a), Number(b)]) => Number(a - b)
  | (Times, [Number(a), Number(b)]) => Number(a * b)
  | (First, [List([first, ..._rest])]) => first
  | (Println, [value]) =>
    value |> string_of_expression |> Js.Console.log;
    value;
  | _ => raise(ArgumentError("ArgumentError on a builtin function."))
  };

let environment =
  StringMap.empty
  |> add("true", True)
  |> add("false", False)
  |> add("=", Function(Equals))
  |> add("+", Function(Plus))
  |> add("-", Function(Minus))
  |> add("*", Function(Times))
  |> add("first", Function(First))
  |> add("println", Function(Println));
