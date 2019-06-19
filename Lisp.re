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

let isNumber = str => Str.string_match(Str.regexp("^[0-9]*$"), str, 0);

let rec read = (out, input) =>
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

let read_tokens = input =>
  switch (read([], input)) {
  | Complete(results) => results
  | Incomplete(_, _) => raise(UnbalancedParens)
  };

let parse = str => str |> tokenize |> read_tokens;
