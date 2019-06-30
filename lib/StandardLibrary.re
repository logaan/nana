open CoreTypes;
open PrettyPrint;
open StringMap;

let builtinPlus = args =>
  switch (args) {
  | [Number(a), Number(b)] => Number(a + b)
  | _ => raise(ArgumentError("+ takes two numbers"))
  };

let builtinMinus = args =>
  switch (args) {
  | [Number(a), Number(b)] => Number(a - b)
  | _ => raise(ArgumentError("- takes two numbers"))
  };

let builtinTimes = args =>
  switch (args) {
  | [Number(a), Number(b)] => Number(a * b)
  | _ => raise(ArgumentError("* takes two numbers"))
  };

let builtinFirst = args =>
  switch (args) {
  | [List([first, ..._rest])] => first
  | _ => raise(ArgumentError("first a list with at least one value"))
  };

let builtinPrintln = args =>
  switch (args) {
  | [value] =>
    value |> string_of_expression |> print_endline;
    value;
  | _ => raise(ArgumentError("println only takes one argument"))
  };

let environment =
  StringMap.empty
  |> add("+", Function(builtinPlus))
  |> add("-", Function(builtinMinus))
  |> add("*", Function(builtinTimes))
  |> add("first", Function(builtinFirst))
  |> add("println", Function(builtinPrintln));
