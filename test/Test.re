open Lib;
open Lisp;
open CoreTypes;
open PrettyPrint;

let square = "
(defn square (n)
  (* n n))

(println (square 2))
   ";

let squareParsed = [
  List([
    Symbol("defn"),
    Symbol("square"),
    List([Symbol("n")]),
    List([Symbol("*"), Symbol("n"), Symbol("n")]),
  ]),
  List([Symbol("println"), List([Symbol("square"), Number(2)])]),
];

let run = () => {
  List.length(tokenize(square)) == 19 |> string_of_bool |> print_endline;

  let some_atoms = "42 foo    bar 99      ";

  List.length(parse(some_atoms)) == 4 |> string_of_bool |> print_endline;
  List.length(parse(square)) == 2 |> string_of_bool |> print_endline;
  parse(square) == squareParsed |> string_of_bool |> print_endline;

  square |> parse |> string_of_expressions |> print_endline;

  evalAndPrint("(+ 1 1)");

  evalAndPrint("(first (quote (1 2 3)))");

  evalAndPrint("(let (inc (lambda (n) (+ n 1))) (inc 6)) ");
  evalAndPrint("(let (square (lambda (n) (* n n))) (square 6)) ");

  evalAndPrint(
    "
       (def square
         (lambda (n)
           (* n n)))

       (def a 4)

       (square a)",
  );
};

let () = run();
