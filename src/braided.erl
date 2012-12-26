%% @doc Braided attempts to provide functionality similar to Deferred.js
%%
%% @author Julian "Andrakis" Thatcher <julian@noblesamurai.com>
%% @version 0.0.1
%%

-module(braided).

-include("braided.hrl").

-ifndef(TEST).
	-define(TEST, 1).
-endif.
-ifdef(TEST).
	-include_lib("eunit/include/eunit.hrl").
-else.
	-define(debugFmt(_, __), ok).
-endif.

-export([go/1]).

-type generator() ::
	list() |
	fun((InState::term()) -> backresult()).

-type backresult() ::
	{value, OutValue::term()} | {valstate, OutValue::term(), OutState::term()} |
	{error, OutValue::term()} | {errstate, OutValue::term(), OutState::term()} |
	skip | {skipstate, OutState::term()} |
	% Early exit
	done                      | fail |
	{done, OutValue::term()}  | {donestate, OutValue::term(), OutState::term()} |
	{fail, OutValue::term()}  | {failstate, OutValue::term(), OutState::term()}.

-type callback() ::
	% Synchronous methods
	fun((Value::term()) -> backresult()) |
	fun((Value::term(), InState::term()) -> backresult()).


-type errback() :: undefined | callback().

-type final() ::
	fun((AllValues::term()) -> _) |
	fun((AllValues::term(), FinalState::term()) -> _).

-record(braid, {
	state,
	generator    :: generator(),
	callback     :: callback(),
	errback      :: errback(),
	done         :: final(),
	fail         :: final(),
	results = [] :: [term()],
	async = false:: boolean()
}).

%% @doc Start the braid operation. Does some initial setup, then passed on to i_loop/1.
-spec go(#braid{}) -> _.
go(#braid{ async = true } = B0) ->
	B1 = B0#braid{ async = false },
	spawn(fun() ->
		go(B1)
	end),
	ok;
go(#braid{} = B0) ->
	i_loop(B0).

i_loop(#braid{ generator = [H | T]} = B0) ->
	B1 = B0#braid{ generator = T },
	go_callback(H, B1);
i_loop(#braid{ generator = [] } = B0) ->
	handle_state(done, B0);
i_loop(#braid{ generator = Generator } = B0) when is_function(Generator) ->
	?debugFmt("Calling generator with state ~p~n", [B0#braid.state]),
	Val = if
		is_function(Generator, 1) -> Generator(B0#braid.state);
		true -> error({badarg, generator})
	end,
	?debugFmt("Generator got: ~p~n", [Val]),
	go_callback(Val, B0).

%% @doc Call the callback for a given input. Pass the result onto handle_back/2, or to the errback.
%% @private
-spec go_callback(Term::term(), #braid{}) -> _.
go_callback({valstate, OutVal, OutState}, B0) ->
	B1 = B0#braid{ state = OutState },
	go_callback(OutVal, B1);
go_callback(DoneOrFail, B0) when DoneOrFail == done; DoneOrFail == fail ->
	handle_state(DoneOrFail, B0);
go_callback(In, #braid{ callback = Callback } = B0) ->
	try
		Val0 = if
			is_function(Callback, 1) -> Callback(In);
			is_function(Callback, 2) -> Callback(In, B0#braid.state);
			true -> error({badarg, callback})
		end,
		handle_back(Val0, B0)
	catch _:_ = What ->
		?debugFmt("What? ~p~n~p", [What, erlang:get_stacktrace()]),
		Errback = B0#braid.errback,
		Val1 = if
			is_function(Errback, 1) -> Errback(What);
			is_function(Errback, 2) -> Errback(What, B0#braid.state);
			true -> error({badarg, errback})
		end,
		handle_back(Val1, B0)
	end.

%% @doc Handle a result from a callback or errback. Depending on the value passed in, one of the
%%      following occurs:
%% - If {value, Result} or {error, Result} is given, prepend Result to #braid.results
%% - If {valstate, Result, State} or {errstate, Result, State} is given, perform above and update
%%      #braid.state with State
%% - If {done, Result} is given, call #braid.done(Result)
%% - If {donestate, Result, State} is given, call #braid.done(Result, State)
%% - If {fail, Result} is given, call #braid.fail(Result)
%% - If {failstate, Result, State} is given, call #braid.fail(Result, State)
%% - If skip is given, don't do anything. Move on to the next input from #braid.generator.
%% - If {skipstate, State} is given, update #braid.state and continue with above.
%% - Otherwise, treat it as {value, Result}
%% @private
-spec handle_back(backresult(), #braid{}) -> _.
handle_back({State, Out}, #braid{ results = Results } = B0)
		when State == value; State == error; State == done; State == fail ->
	B1 = B0#braid{ results = [Out | Results] },
	?debugFmt("Got result: ~p~n", [Out]),
	handle_state(State, B1);
handle_back({State, OutVal, OutState}, #braid{ results = Results } = B0)
		when State == valstate; State == errstate; State == donestate; State == failstate ->
	B1 = B0#braid{
		results = [OutVal | Results],
		state = OutState
	},
	handle_state(State, B1);
handle_back(skip, B0) ->
	i_loop(B0);
handle_back({skipstate, OutState}, B0) ->
	B1 = B0#braid{ state = OutState },
	i_loop(B1);
handle_back(Out, B0) ->
	handle_back({value, Out}, B0).

%% @doc Handle a state update - in the case of the various value and error states, move onto
%%      the next generator item and continue.
%%      In the case of done or fail, call the respective handler with the results and state.
%% @private
-spec handle_state(value | valstate | error | errstate | done | donestate | fail | failstate,
		#braid{}) -> _.
handle_state(State, B0)
		when State == value; State == valstate; State == error; State == errstate ->
	i_loop(B0);
handle_state(DoneOrDoneState, #braid{ done = Done } = B0)
		when DoneOrDoneState == done; DoneOrDoneState == donestate ->
	Results = lists:reverse(B0#braid.results),
	if
		is_function(Done, 1) -> Done(Results);
		is_function(Done, 2) -> Done(Results, B0#braid.state);
		true -> nop
	end;
handle_state(FailOrFailState, #braid{ fail = Fail } = B0)
		when FailOrFailState == fail; FailOrFailState == failstate ->
	Results = lists:reverse(B0#braid.results),
	if
		is_function(Fail, 1) -> Fail(Results);
		is_function(Fail, 2) -> Fail(Results, B0#braid.state);
		true -> nop
	end.

-ifdef(TEST).

init_test() ->
	ok.

go_test() ->
	List = [1, 2],
	Callback = fun(Value) -> integer_to_list(Value) end,
	Errback = fun(Value) -> {err, Value} end,
	Done = fun(Values) -> Values end,

	B0 = #braid{
		generator = List,
		callback = Callback,
		errback = Errback,
		done = Done
	},

	?assertEqual(["1", "2"], go(B0)),

	ok.

go_async_test() ->
	List = [1, 2],
	Self = self(),
	Callback = fun(Value) -> integer_to_list(Value) end,
	Errback = fun(Value) -> {err, Value} end,
	Done = fun(Values) -> Self ! {async, Values} end,

	B0 = #braid{
		generator = List,
		callback = Callback,
		errback = Errback,
		done = Done,
		async = true
	},

	go(B0),

	?assertEqual(receive
		{async, ["1", "2"]} -> ok
	after
		1000 -> fail
	end, ok),

	ok.

go_errback_test() ->
	List = [1, 2, a, b],
	Callback = fun(Value) -> integer_to_list(Value) end,
	Errback = fun(_Value) -> skip end,
	Done = fun(Values) -> Values end,

	B0 = #braid{
		generator = List,
		callback = Callback,
		errback = Errback,
		done = Done
	},

	?assertEqual(["1", "2"], go(B0)),

	ok.

go_generator_callback_test() ->
	Generator = fun
		(X) when X < 3 -> {valstate, X, X + 1};
		(_) -> done
	end,
	Callback = fun(Value) -> integer_to_list(Value) end,
	Done = fun(Values) -> Values end,

	B0 = #braid{
		state = 1,
		generator = Generator,
		callback = Callback,
		done = Done
	},

	?assertEqual(["1", "2"], go(B0)),

	ok.

-endif.
