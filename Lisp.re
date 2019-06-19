type expression =
  | Symbol(string)
  | Number(int)
  | List(list(expression));

let isNumber = (str) => {
  Str.string_match(Str.regexp("^[0-9]*$"), str, 0);
}

let square = "
(dorun
  (defn square (n)
    (* n n))

  (println (square 2)))
"

let tokenize = (str) => {
  let expanded = Str.global_replace(Str.regexp("[()]"),
                                    " \\0 ", str);
  Str.split(Str.regexp("[ \n]+"), expanded)
}

List.length(tokenize(square)) == 22
|> string_of_bool
|> print_endline;

print_endline("Hello, World.");
