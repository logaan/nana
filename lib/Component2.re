open Lisp;
open CoreTypes;
open PrettyPrint;

type state = {
  text: string,
  result: expression,
  environment,
};

type action =
  | Update(string)
  | Run;

let reducer = (state, action) =>
  switch (action) {
  | Update(value) => {...state, text: value}
  | Run =>
    let (environment, result) =
      evalExpressions(state.environment, state.text);
    {...state, result, environment};
  };

let initialState = {
  text: "",
  result: Symbol("start"),
  environment: StandardLibrary.environment,
};

[@react.component]
let make = () => {
  let (state, dispatch) = React.useReducer(reducer, initialState);

  <div>
    <textarea
      onChange={event =>
        dispatch(Update(ReactEvent.Form.target(event)##value))
      }
      value={state.text}
    />
    <div>
      <button onClick={_event => dispatch(Run)}>
        {ReasonReact.string("Run")}
      </button>
    </div>
    <div> {ReasonReact.string(string_of_expression(state.result))} </div>
  </div>;
};
