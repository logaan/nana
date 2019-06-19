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

let rec read = (expressions, tokens) =>
  switch (tokens) {
  | [] => Complete(List.rev(expressions))

  | ["(", ...tail] =>
    switch (read([], tail)) {
    | Complete(_) => raise(UnbalancedParens)
    | Incomplete(nestedExpressions, newTail) =>
      read([List(nestedExpressions), ...expressions], newTail)
    }

  | [")", ...tail] => Incomplete(List.rev(expressions), tail)

  | [head, ...tail] when isNumber(head) =>
    read([Number(int_of_string(head)), ...expressions], tail)

  | [head, ...tail] => read([Symbol(head), ...expressions], tail)
  };

let read_tokens = tokens =>
  switch (read([], tokens)) {
  | Complete(results) => results
  | Incomplete(_, _) => raise(UnbalancedParens)
  };

let parse = str => str |> tokenize |> read_tokens;
