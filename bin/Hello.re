open Lib;
open Lisp;
open PrettyPrint;

let rec run = (env) =>
  switch (read_line()) {
  | "exit" => print_endline("Done.")
  | input =>
    print_endline("Result:");
    let (newEnv, result) = eval(env, input);
    result |> string_of_expression |> print_endline;
    run(newEnv);
  };

let () = {
  print_endline("HTWALIIR REPL.");
  run(StandardLibrary.environment);
};
