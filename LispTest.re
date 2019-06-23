open CoreTypes;
open Lisp;

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

let evalAndPrint = code => {
  let parsed = parse(code);
  let results =
    List.map(
      expression => eval(expression, StandardLibrary.environment),
      parsed,
    );
  print_endline(string_of_expressions(results));
};

let run = () => {
  List.length(tokenize(square)) == 19 |> string_of_bool |> print_endline;

  let some_atoms = "42 foo    bar 99      ";

  List.length(parse(some_atoms)) == 4 |> string_of_bool |> print_endline;
  List.length(parse(square)) == 2 |> string_of_bool |> print_endline;
  parse(square) == squareParsed |> string_of_bool |> print_endline;

  square |> parse |> string_of_expressions |> print_endline;

  evalAndPrint("(+ 1 1)");
};

let () = run();
