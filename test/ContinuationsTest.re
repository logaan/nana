open Lib;
open CoreTypes;
open Continuations;
open StandardLibrary;

let printBool = value => value |> string_of_bool |> print_endline;

// 1
// [Number(1)];

let step1_0 = {left: [], right: [Number(1)], environment, up: None};

let step1_1 = {left: [Number(1)], right: [], environment, up: None};

// 1 2
// [Number(1), Number(2)];

let step2_0 = {
  left: [],
  right: [Number(1), Number(2)],
  environment,
  up: None,
};

let step2_1 = {
  left: [Number(1)],
  right: [Number(2)],
  environment,
  up: None,
};

let step2_2 = {
  left: [Number(2), Number(1)],
  right: [],
  environment,
  up: None,
};

// (+ 1 2)
// [List([Symbol("+"), Number(1), Number(2)])];

let step3_0 = {
  left: [],
  right: [List([Symbol("+"), Number(1), Number(2)])],
  environment,
  up: None,
};

let step3_1 = {
  left: [],
  right: [Symbol("+"), Number(1), Number(2)],
  environment,
  up: Some({left: [], right: [List([])], environment, up: None}),
};

let step3_2 = {
  left: [Function(Plus)],
  right: [Number(1), Number(2)],
  environment,
  up: Some({left: [], right: [List([])], environment, up: None}),
};

let step3_3 = {
  left: [Number(1), Function(Plus)],
  right: [Number(2)],
  environment,
  up: Some({left: [], right: [List([])], environment, up: None}),
};

let step3_4 = {
  left: [Number(2), Number(1), Function(Plus)],
  right: [],
  environment,
  up: Some({left: [], right: [List([])], environment, up: None}),
};

// This might be too much of a jump. What if there had been other elements after
// the empty list in right?

let step3_5 = {left: [Number(3)], right: [], environment, up: None};

// The tests
let run = () => {
  printBool(step(step1_0) == step1_1);

  printBool(step(step2_0) == step2_1);
  printBool(step(step2_1) == step2_2);

  printBool(step(step3_0) == step3_1);
  printBool(step(step3_1) == step3_2);
  printBool(step(step3_2) == step3_3);
  printBool(step(step3_3) == step3_4);
  printBool(step(step3_4) == step3_5);
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
