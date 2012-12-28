%% @doc Braided attempts to provide functionality similar to Deferred.js
%%
%% @author Julian "Andrakis" Thatcher <julian@noblesamurai.com>
%% @version 0.0.1
%%

-ifndef(BRAIDED_HRL).
-define(BRAIDED_HRL, 1).

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

-record(braided_out, {
	ref          :: reference(),
	% Used for sorting parallel results when finishing
	order        :: integer(),
	out          :: term()
}).

-type async_callback() :: fun((OutValue::term()) -> _).

-type callback() ::
	% Synchronous methods
	fun((Value::term()) -> backresult()) |
	fun((Value::term(), InState::term()) -> backresult()) |
	% Note: Asynchronous methods cannot update the state, though can receive state.
	%       The behaviour of sending a new state is undefined.
	% Asynchronous methods - update and send the record to the given pid.
	{async_notify, fun((Value::term(), Pid::pid(), AsyncOut::#braided_out{}) -> _)} |
	{async_notify, fun((Value::term(), InState::term(), Pid::pid(), AsyncOut::#braided_out{}) -> _)} |
	% Asynchronous methods - call the result callback with result
	{async_callback, fun((Value::term(), ResultCallback::async_callback()) -> _)} |
	{async_callback, fun((Value::term(), InState::term(), ResultCallback::async_callback()) -> _)}.

-type errback() :: undefined | callback().

-type final() ::
	fun((AllValues::term()) -> _) |
	fun((AllValues::term(), FinalState::term()) -> _).

-type async_parallel() ::
	% Detect based on SMP configuration: one per enabled scheduler
	smp_detect |
	% Force a given parallel value
	pos_integer().

-record(braid, {
	state,
	generator = []              :: generator(),
	callback                    :: callback(),
	errback                     :: errback(),
	done = fun(V) -> V end      :: final(),
	fail                        :: final(),
	results = []                :: [term()],
	async = false               :: boolean(),
	parallel = false            :: boolean(),
	parallel_limit = smp_detect :: async_parallel(),
	% How many parallel tasks are running right now
	parallel_tasks = 0          :: integer(),
	% Keeps track of parallel sort order index
	parallel_sort_index = 0     :: integer(),
	ref                         :: reference()
}).

-endif.
