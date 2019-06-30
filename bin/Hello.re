open Lib;
open Lisp;
open PrettyPrint;

let rec run = (env) =>
  switch (read_line()) {
  | "exit" => print_endline("Done.")
  | input =>
    let (newEnv, result) = eval(env, input);
    print_endline("Result: " ++ string_of_expression(result));
    run(newEnv);
  };

let () = {
  print_endline("HTWALIIR REPL.");
  run(StandardLibrary.environment);
};
