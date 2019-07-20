open CoreTypes;
// open StandardLibrary;

type continuation = {
  left: list(expression),
  right: list(expression),
  environment,
  up: option(continuation),
};

// We can only prepend on the left right?

let stepExpr = expr => expr;

let step = cont =>
  switch (cont) {
  | {left: _, right: [], environment: _, up: _} => cont
  | {left, right: [List(expr), ...tailExprs], environment, up} => {
      left: [],
      right: expr,
      environment,
      up: Some({left, right: [List([]), ...tailExprs], environment, up}),
    }
  | {left, right: [expr, ...tailExprs], environment, up} => {
      left: [stepExpr(expr), ...left],
      right: tailExprs,
      environment,
      up,
    }
  };

/*


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
