-module(actor).

-behaviour(gen_server).

%% ===========================================================================
%% exports
%% ===========================================================================

%% API
-export([ start_link/3, stop/1, call/2, call/3, cast/2 ]).

%% gen_server behaviours
-export([ code_change/3, handle_call/3, handle_cast/2, handle_info/2,
          init/1, terminate/2 ]).

%% ===========================================================================
%% records
%% ===========================================================================

-record(state, {
    mod             :: atom(),
    actor_state     :: term()
}).

%% ===========================================================================
%% callbacks
%% ===========================================================================

-callback init(Args :: list()) ->
    {ok, State :: any()} |
    {error, Reason :: string()}.

-callback handle(Request :: any(), State :: any()) ->
    {Result :: any(), State :: any()} |
    {Result :: any(), State :: any(), Timeout :: integer()} |
    {stop, Reason :: any(), State :: any()}.

%% ===========================================================================
%% Client API
%% ===========================================================================

%% ---------------------------------------------------------------------------
%% start/stop the server
%% ---------------------------------------------------------------------------
start_link(Mod, Name, Args) ->
    gen_server:start_link({local, Name}, ?MODULE, [Mod, Args], []).

stop(Name) ->
    gen_server:stop(Name).

%% ---------------------------------------------------------------------------
%% communication APIs
%% ---------------------------------------------------------------------------
call(Name, Msg) ->
    gen_server:call(Name, Msg).

call(Name, Msg, Timeout) ->
    gen_server:call(Name, Msg, Timeout).

cast(Name, Msg) ->
    gen_server:cast(Name, Msg).

%% ===========================================================================
%% gen_server callbacks
%% ===========================================================================
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Request, State) ->
    {noreply, State}.

init([Mod, Args]) ->
    case Mod:init(Args) of
        {error, _Reason} = Err ->
            Err;
        {ok, ActorState} ->
            {ok, #state{mod=Mod, actor_state=ActorState}}
    end.

terminate(_Shutdown, _Reason) ->
    normal.

%% ===========================================================================
%% private helpers
%% ===========================================================================


