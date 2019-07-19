open CoreTypes;
// open StandardLibrary;

type continuation = {
  left: list(expression),
  right: list(expression),
  up: option(continuation),
};

// We can only prepend on the left right?

let stepExpr = expr => expr;

let step = cont =>
  switch (cont) {
  | {left: _, right: [], up: _} => cont
  | {left: left, right: [expr, ...tailExprs], up: up} => {
      left: [stepExpr(expr), ...left],
      right: tailExprs,
      up: up,
    }
  };

/*
// 1 2
let expressions = [Number(1), Number(2)];

let step0 = {left: [], right: [Number(1), Number(2)], up: None};

let step1 = {left: [Number(1)], right: [Number(2)], up: None};

let step2 = {left: [Number(1), Number(2)], right: [], up: None};

// (+ 1 2)
let expressions = [List([Symbol("+"), Number(1), Number(2)])];

let step0 = {
  left: [],
  right: [List([Symbol("+"), Number(1), Number(2)])],
  up: None,
};

let step0 = {
  left: [],
  right: [Symbol("+"), Number(1), Number(2)],
  up: Some({left: [], right: [List([])], up: None}),
};

let step2 = {
  left: [Function(builtinPlus)],
  right: [Number(1), Number(2)],
  up: Some({left: [], right: [List([])], up: None}),
};

let step3 = {
  left: [Function(builtinPlus), Number(1)],
  right: [Number(2)],
  up: Some({left: [], right: [List([])], up: None}),
};

let step4 = {
  left: [Function(builtinPlus), Number(1), Number(2)],
  right: [],
  up: Some({left: [], right: [List([])], up: None}),
};

let step5 = {left: [Number(3)], right: [], up: None};

// (+ (+ 1 2) 3)
let expressions = [
  List([
    Symbol("+"),
    List([Symbol("+"), Number(1), Number(2)]),
    Number(3),
  ]),
];

let step0 = {
  left: [],
  right: [
    List([
      Symbol("+"),
      List([Symbol("+"), Number(1), Number(2)]),
      Number(3),
    ]),
  ],
  up: None,
};

let step1 = {
  left: [],
  right: [
    Symbol("+"),
    List([Symbol("+"), Number(1), Number(2)]),
    Number(3),
  ],
  up: Some({left: [], right: [List([])], up: None}),
};

let step2 = {
  left: [Function(builtinPlus)],
  right: [List([Symbol("+"), Number(1), Number(2)]), Number(3)],
  up: Some({left: [], right: [List([])], up: None}),
};

let step3 = {
  left: [],
  right: [Symbol("+"), Number(1), Number(2)],
  up:
    Some({
      left: [Function(builtinPlus)],
      right: [List([]), Number(3)],
      up: Some({left: [], right: [List([])], up: None}),
    }),
};

let step4 = {
  left: [Function(builtinPlus)],
  right: [Number(1), Number(2)],
  up:
    Some({
      left: [Function(builtinPlus)],
      right: [List([]), Number(3)],
      up: Some({left: [], right: [List([])], up: None}),
    }),
};

let step5 = {
  left: [Function(builtinPlus), Number(1)],
  right: [Number(2)],
  up:
    Some({
      left: [Function(builtinPlus)],
      right: [List([]), Number(3)],
      up: Some({left: [], right: [List([])], up: None}),
    }),
};

let step6 = {
  left: [Function(builtinPlus), Number(1), Number(2)],
  right: [],
  up:
    Some({
      left: [Function(builtinPlus)],
      right: [List([]), Number(3)],
      up: Some({left: [], right: [List([])], up: None}),
    }),
};
*/
