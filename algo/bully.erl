-module(bully).
-behavior(gen_server).

-export([ start/4, init/1 ]).

-record(state, {
	id	::	integer(),
	name	::	string(),
	nodes	::	list(string())
}).

-spec start(atom(), integer(), string(), list(string())) ->
	{ok, pid()} | ignore | {error, string()}.

start(Module, Id, Name, Nodes) ->
	gen_server:start(Module, [Id, Name, Nodes], []).

-spec init(Args :: list()) -> {ok, #state{id :: integer(), name :: string(),
					  nodes :: list(string())}}.

init([Id, Name, Nodes]) ->
	{ok, #state{id=Id, name=Name, nodes=Nodes}}.
	
