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
  | Not_found =>
    print_endline("n not found");

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
    );
    // Expected output 4, 1, 1, 9

    evalAndPrint(
      "
       (println (= true true))
       (println (= false false))
       (println (= true false))
       (println (= false true))

       (println (= 1 1))
       (println (= 1 2))

       (println (= (lambda (n) n)
                   (lambda (n) n)))

       (println (= (lambda (n) n)
                   (lambda (y) y)))
",
    );

    evalAndPrint(
      "(if true (println (quote worked)) (println (quote broken)))",
    );
    evalAndPrint(
      "(if false (println (quote broken)) (println (quote worked)))",
    );
    evalAndPrint(
      "(if (= 1 1) (println (quote worked)) (println (quote broken)))",
    );

    evalAndPrint(
      "

(def do
  (lambda (a b)
    b))

(def Y
  (lambda (f)
    ((lambda (x) (f (lambda (n) ((x x) n))))
     (lambda (x) (f (lambda (n) ((x x) n)))))))

(def count-to-10
  (Y
    (lambda (count-to-10)
      (lambda (n)
        (do (println n)
            (if (= n 10)
              (quote worked)
              (count-to-10 (+ n 1))))))))

(count-to-10 0)

       ",
    );
  };
};

let () = run();

/* Calling eval on the function and on each of the args */
