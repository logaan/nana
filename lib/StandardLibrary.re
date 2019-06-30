open CoreTypes;
open PrettyPrint;

let lispPlus = args =>
  switch (args) {
  | [Number(a), Number(b)] => Number(a + b)
  | _ => raise(ArgumentError("+ takes two numbers"))
  };

let lispMinus = args =>
  switch (args) {
  | [Number(a), Number(b)] => Number(a - b)
  | _ => raise(ArgumentError("- takes two numbers"))
  };

let lispTimes = args =>
  switch (args) {
  | [Number(a), Number(b)] => Number(a * b)
  | _ => raise(ArgumentError("* takes two numbers"))
  };

let lispFirst = args =>
  switch (args) {
  | [List([first, ..._rest])] => first
  | _ => raise(ArgumentError("first a list with at least one value"))
  };

let lispPrintln = args =>
  switch (args) {
  | [value] =>
    value |> string_of_expression |> print_endline;
    value;
  | _ => raise(ArgumentError("println only takes one argument"))
  };

let environment: environment =
  StringMap.empty
  |> StringMap.add("+", Function(lispPlus))
  |> StringMap.add("-", Function(lispMinus))
  |> StringMap.add("*", Function(lispTimes))
  |> StringMap.add("first", Function(lispFirst))
  |> StringMap.add("println", Function(lispPrintln));
