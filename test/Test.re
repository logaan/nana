open Lib;
open Lisp;
open CoreTypes;
open PrettyPrint;

let evalAndPrint = code =>
  code |> evalOnceOff |> string_of_expression |> print_endline;

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

  evalAndPrint("(println (quote foo))");

  evalAndPrint("(def a 4)
       (println a)");

  evalAndPrint(
    "
         (def square
           (lambda (n)
             (* n n)))

         (println square)",
  );

  evalAndPrint("((lambda () (println 1)))");

  evalAndPrint("((lambda () ((lambda () (println 2)))))");

  evalAndPrint(
    "
     (def square
       (lambda ()
         (* 4 4)))

     (square)",
  );

  evalAndPrint(
    "
      (def square
        (lambda (n)
          (* n n)))

      (square 4)",
  );

  evalAndPrint("(println println)");

  evalAndPrint("((println println) 3)");

  evalAndPrint("(println (println 3))");
};

let () = run();
