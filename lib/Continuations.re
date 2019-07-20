open CoreTypes;

type continuation = {
  left: list(expression),
  right: list(expression),
  environment,
  up: option(continuation),
};

let stepExpr = expr => expr;

let step = cont =>
  switch (cont) {
  | {left: _, right: [], environment: _, up: _} => cont
  | {
      left,
      right: [evalsToFn, ...rightTail],
      environment,
      up: Some({left: _, right: [List([]), ..._], environment: _, up: _}),
    } => {
      // It's wrong to use evalPureExpression here. This could be a function
      // call that returns a function. That function call should be stepped
      // through.
      left: [Lisp.evalPureExpression(evalsToFn, environment), ...left],
      right: rightTail,
      environment,
      up: cont.up,
    }
  | {left, right: [List(expr), ...rightTail], environment, up} => {
      left: [],
      right: expr,
      environment,
      up: Some({left, right: [List([]), ...rightTail], environment, up}),
    }
  | {left, right: [expr, ...rightTail], environment, up} => {
      left: [stepExpr(expr), ...left],
      right: rightTail,
      environment,
      up,
    }
  };
