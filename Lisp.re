type expression =
  | Number(int)
  | Symbol(string)
  | List(list(expression));

let tokenize = str => {
  let expanded = Str.global_replace(Str.regexp("[()]"), " \\0 ", str);
  Str.split(Str.regexp("[ \n]+"), expanded);
};

type readResult =
  | Complete(list(expression))
  | Incomplete(list(expression), list(string));

exception UnbalancedParens;

let isNumber = str => {
  Str.string_match(Str.regexp("^[0-9]*$"), str, 0);
};

let rec read = (out, input) => {
  switch (input) {
  | [] => Complete(List.rev(out))

  | ["(", ...tail] =>
    switch (read([], tail)) {
    | Complete(_) => raise(UnbalancedParens)
    | Incomplete(parsed, newTail) => read([List(parsed), ...out], newTail)
    }

  | [")", ...tail] => Incomplete(List.rev(out), tail)

  | [head, ...tail] when isNumber(head) =>
    read([Number(int_of_string(head)), ...out], tail)

  | [head, ...tail] => read([Symbol(head), ...out], tail)
  };
};

let read_tokens = input =>
  switch (read([], input)) {
  | Complete(results) => results
  | Incomplete(_, _) => raise(UnbalancedParens)
  };

let parse = str => str |> tokenize |> read_tokens;

/* test suite */

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

List.length(tokenize(square)) == 19 |> string_of_bool |> print_endline;

let some_atoms = "42 foo    bar 99      ";

List.length(parse(some_atoms)) == 4 |> string_of_bool |> print_endline;
List.length(parse(square)) == 2 |> string_of_bool |> print_endline;
parse(square) == squareParsed |> string_of_bool |> print_endline;
