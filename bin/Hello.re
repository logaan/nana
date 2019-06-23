open Lib;
open Lisp;

let rec run = () =>
  switch(read_line()) {
    | "exit" => print_endline("Done.");
  | input => {
      print_endline("Result:")
      evalAndPrint(input);
      run();
    }
  }

let () = {
  print_endline("HTWALIIR REPL.");
  run();
}
