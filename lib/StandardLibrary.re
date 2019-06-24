open CoreTypes;

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

let lispFirst = args =>
  switch (args) {
  | [List([first, ..._rest])] => first
  | _ => raise(ArgumentError("first a list with at least one value"))
  };

let environment =
  StringMap.empty
  |> StringMap.add("+", Function(lispPlus))
  |> StringMap.add("-", Function(lispMinus))
  |> StringMap.add("first", Function(lispFirst));
