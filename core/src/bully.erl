-module(bully).

-behaviour(actor).

%% ===========================================================================
%% actor callbacks
%% ===========================================================================
init([Args]) ->
    {ok, #state{}}.

handle({heartbeat, _} = Msg, State) -> %% heartbeat from peer
    handle_heartbeat(Msg, State).

%% ===========================================================================
%% private helpers
%% ===========================================================================

%% ---------------------------------------------------------------------------
%% handle heart beat message from peer
%% ---------------------------------------------------------------------------
-spec handle_heartbeat(_, #state{}) ->
    {ok, #state{}} | {error, #state{}}.

handle_heartbeat(_, #state{}) ->
    ok.
