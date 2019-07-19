open Lib;
open CoreTypes;
open Continuations;

let printBool = value => value |> string_of_bool |> print_endline;

// 1
let expressions = [Number(1)];

let step0 = {left: [], right: [Number(1)], up: None};

let step1 = {left: [Number(1)], right: [], up: None};

let run = () => {
  printBool(step(step0) == step1);
};
