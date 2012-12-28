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

%% @doc Start the braid operation. Does some initial setup, then passed on to i_loop/1.
-spec go(#braid{}) -> _.
go(#braid{ async = true } = B0) ->
	B1 = B0#braid{ async = false },
	spawn(fun() ->
		go(B1)
	end),
	ok;
go(#braid{} = B0) ->
	case init(B0) of
		{ok, B1} -> i_loop(B1)
	end.

%% @doc Create the initial state for the braid.
-spec init(#braid{}) -> {ok, #braid{}}.
init(#braid{ parallel = true, parallel_limit = smp_detect } = B0) ->
	SchedulerCount = erlang:system_info(schedulers),
	B1 = B0#braid{ parallel_limit = SchedulerCount },
	init(B1);
init(B0) ->
	B1 = B0#braid{ ref = make_ref() },
	{ok, B1}.

%% @doc Main loop for a braided operation. Gets the next value from the generator, and
%%      passes it to the callback. If the generator is empty, moves to done state.
%% @private
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
go_callback(In, #braid{ parallel = true, parallel_tasks = Tasks, parallel_limit = Limit } = B0)
		when Tasks < Limit ->
	?debugFmt("(~p) [~p] Not at task limit [~p], spawning next~n", [self(), Tasks, Limit]),
	B1 = B0#braid{
		parallel_tasks = Tasks + 1
	},
	B2 = start_async(In, B1),
	i_loop(B2);
go_callback(In, #braid{ parallel = true } = B0) ->
	?debugFmt("(~p) At task limit, waiting~n", [self()]),
	% No space!
	B1 = wait_callback(B0),
	?debugFmt("(~p) Calling again now that we're free~n", [self()]),
	go_callback(In, B1);
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

%% @doc Start an async callback. Supports async_notify and async_callback.
%%      The receiver for all results is wait_callback/1.
%% @private
-spec start_async(In::term(), #braid{}) -> #braid{}.
start_async(In, #braid{ callback = {async_notify, Fun}, parallel_sort_index = SortIndex } = B0) ->
	B1 = B0#braid{ parallel_sort_index  = SortIndex + 1 },
	Out = #braided_out{ ref = B0#braid.ref, order = SortIndex },
	Self = self(),
	if
		is_function(Fun, 3) ->
			spawn(fun() -> Fun(In, Self, Out) end);
		is_function(Fun, 4) ->
			spawn(fun() -> Fun(In, B1#braid.state, Self, Out) end)
	end,
	B1;
start_async(In, #braid{ callback = {async_callback, Fun}, parallel_sort_index = SortIndex } = B0) ->
	B1 = B0#braid{ parallel_sort_index  = SortIndex + 1 },
	Self = self(),
	Callback = fun(Out) ->
		?debugFmt("(~p) Callback called with ~p~n", [Self, Out]),
		Self ! #braided_out{ ref = B1#braid.ref, order = SortIndex, out = Out }
	end,
	if
		is_function(Fun, 2) ->
			spawn(fun() -> Fun(In, Callback) end);
		is_function(Fun, 3) ->
			spawn(fun() -> Fun(In, B1#braid.state, Callback) end)
	end,
	B1.

%% @doc Wait for a callback result if currently at task limit, or no tasks remain.
%%      If not at task limit, and tasks remain, return to i_loop/1 to start more tasks.
-spec wait_callback(#braid{}) -> #braid{}.
wait_callback(#braid{ parallel_tasks = Tasks, parallel_limit = Limit } = B0)
		when Tasks > 0, Tasks >= Limit ->
	Ref = B0#braid.ref,
	receive
		#braided_out{ ref = Ref } = Out ->
			B1 = B0#braid{
				results = [Out | B0#braid.results],
				parallel_tasks = B0#braid.parallel_tasks - 1
			},
			wait_callback(B1)
	end;
wait_callback(B0) ->
	B0.

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
	if
		B0#braid.parallel_tasks > 0 ->
			?debugFmt("(~p) Need to wait a bit, ~p remain~n", [self(), B0#braid.parallel_tasks]),
			% This will cause wait_callback to wait for all results, and when finally
			% done, call handle_state again.
			B1 = B0#braid{ parallel_limit = 0, generator = [] },
			B2 = wait_callback(B1),
			handle_state(DoneOrDoneState, B2);
		true ->
			?debugFmt("(~p) All done, calling callback~n", [self()]),
			Results = get_results(B0),
			if
				is_function(Done, 1) -> Done(Results);
				is_function(Done, 2) -> Done(Results, B0#braid.state);
				true -> Results
			end
	end;
handle_state(FailOrFailState, #braid{ fail = Fail } = B0)
		when FailOrFailState == fail; FailOrFailState == failstate ->
	Results = lists:reverse(B0#braid.results),
	if
		is_function(Fail, 1) -> Fail(Results);
		is_function(Fail, 2) -> Fail(Results, B0#braid.state);
		true -> nop
	end.

%% @doc Get the results for the completed braid operation.
%%      Non-parallel only requires reversing of results.
%%      Paralell requires sorting by order.
%% @private
-spec get_results(#braid{}) -> [term()].
get_results(#braid{ parallel = false, results = Results }) ->
	lists:reverse(Results);
get_results(#braid{ parallel = true, results = Results }) ->
	Sorted = lists:keysort(#braided_out.order, Results),
	[ Out#braided_out.out || Out <- Sorted ].

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

% Maximum wait time in ms
-define(waitMax, 500).
-define(waitRandom, (fun() ->
	timer:sleep(random:uniform(?waitMax))
end)).

go_parallel_callback_test() ->
	?debugFmt("MARK~n", []),
	B0 = #braid{
		generator = [1, 2, 3, 4],
		callback = {async_callback, fun(Value, Callback) ->
			?debugFmt("Callback with ~p~n", [Value]),
			?waitRandom(),
			Callback(integer_to_list(Value))
		end},
		parallel = true
	},

	?assertEqual(["1", "2", "3", "4"], go(B0)),

	% With state
	State = make_ref(),
	B1 = B0#braid{
		state = State,
		callback = {async_callback, fun(Value, InState, Callback) ->
			?assertEqual(State, InState),
			?waitRandom(),
			Callback(integer_to_list(Value))
		end}
	},

	?assertEqual(["1", "2", "3", "4"], go(B1)),

	ok.

go_parallel_notify_test() ->
	?debugFmt("MARK 2~n", []),
	B0 = #braid{
		generator = [1, 2, 3, 4],
		callback = {async_notify, fun(Value, Pid, Out0) ->
			?debugFmt("Notify with ~p~n", [Value]),
			?waitRandom(),
			Out1 = Out0#braided_out{ out = integer_to_list(Value) },
			Pid ! Out1
		end},
		parallel = true
	},

	?assertEqual(["1", "2", "3", "4"], go(B0)),

	% With state
	State = make_ref(),
	B1 = B0#braid{
		state = State,
		callback = {async_notify, fun(Value, InState, Pid, Out0) ->
			?assertEqual(State, InState),
			?waitRandom(),
			Out1 = Out0#braided_out{ out = integer_to_list(Value) },
			Pid ! Out1
		end}
	},

	?assertEqual(["1", "2", "3", "4"], go(B1)),

	ok.

get_results_test() ->
	B0 = #braid{
		parallel = false,
		results = [4, 3, 2, 1]
	},

	?assertEqual([1, 2, 3, 4], get_results(B0)),

	B1 = #braid{
		parallel = true,
		results = [
			#braided_out{ order = 1, out = b },
			#braided_out{ order = 0, out = a },
			#braided_out{ order = 3, out = d },
			#braided_out{ order = 2, out = c }
		]
	},

	?assertEqual([a, b, c, d], get_results(B1)),

	ok.
-endif.
