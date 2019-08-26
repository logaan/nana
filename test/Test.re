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

  try (
    evalAndPrint(
      "
     (def square
       (lambda (n)
         (* n n)))

     (def do
       (lambda (a b)
         b))

     (println (do (square 4) (square n)))",
    )
  ) {
  | Not_found => print_endline("n not found")


// Binding to n in inner should not change the binding of n in outer
evalAndPrint(
      "
(def do
  (lambda (a b)
    b))

(def inner
  (lambda (n)
    (println n)))

(def outer
  (lambda (n)
       (do (inner (* n 4)) (println n))))

(println (outer 1))

       9",
    )
// Expected output 4, 1, 1, 9

  };
};

let () = run();
