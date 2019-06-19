type expression =
  | Number(int)
  | Symbol(string)
  | List(list(expression));

let isNumber = str => {
  Str.string_match(Str.regexp("^[0-9]*$"), str, 0);
};

let square = "
   (dorun
   (defn square (n)
   (* n n))

   (println (square 2)))
   ";

let tokenize = str => {
  let expanded = Str.global_replace(Str.regexp("[()]"), " \\0 ", str);
  Str.split(Str.regexp("[ \n]+"), expanded);
};

List.length(tokenize(square)) == 22 |> string_of_bool |> print_endline;

type readResult =
  | Complete(list(expression))
  | Incomplete(list(expression), list(string));

exception UnbalancedParens;

let rec read = (out, input) => {
  switch (input) {
  | [] => Complete(List.rev(out))
  | [head, ...tail] =>
    if (head == ")") {
      Incomplete(out, tail);
    } else {
      let (latestOut, newTail) =
        switch (head) {
        | "(" =>
          switch(read([], tail)) {
          | Complete(_) => raise(UnbalancedParens)
          | Incomplete(parsed, newTail) => (List(parsed), newTail)
          }
        | head when isNumber(head) => (Number(int_of_string(head)), tail)
        | head => (Symbol(head), tail)
        };

      read(List.cons(latestOut, out), newTail);
    }
  };
};

let read_tokens = input =>
  switch (read([], input)) {
  | Complete(results) => results
  | Incomplete(_, _) => raise(UnbalancedParens)
  };

let parse = str => str |> tokenize |> read_tokens;

let some_atoms = "42 foo    bar 99      ";

List.length(parse(some_atoms)) == 4 |> string_of_bool |> print_endline;
