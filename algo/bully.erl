-module(bully).
-export([start/1, startn/1, stopped/1]).

-record(context, {pids, id, iter = 0}).

%% start bully process on this node
start(Id) when is_integer(Id) ->
    pg2:create("bully"),
    pg2:join("bully", spawn(bully, stopped, [Id])).

%% start n process on this node
startn(N) when is_integer(N) ->
    [ start(I) || I <- lists:seq(1, N) ].

%% STOPPED state
stopped(Id) when is_integer(Id) ->
    election(#context{id=Id}).

% ELECTION state
election(Ctx) ->
    Pids = pg2:get_members("bully"),
    [ Pid ! {elect, self(), Ctx#context.id, Ctx#context.iter}
      || Pid <- Pids, Pid /= self() ],
    election_loop(Ctx#context{pids=Pids}, 0).

election_loop(Ctx, Accepts) when Accepts > length(Ctx#context.pids) / 2 ->
    leader(newctx(Ctx));
election_loop(Ctx, Accepts) ->
    receive
        {elect, From, Id, Iter} when Id > Ctx#context.id ->
            follower(newctx(Ctx), From, Id, Iter);
        {accept, Id, Iter} when Id < Ctx#context.id, Iter == Ctx#context.iter ->
            % io:format("~p accept ~p~n", [Ctx#context.id, Id]),
            election_loop(Ctx, Accepts + 1);
        Msg ->
            % io:format("~p election ignore ~p ~p ~n", [Ctx#context.id, Msg, Ctx]),
            election_loop(Ctx, Accepts)
        after 1000 ->
            io:format("~p election timeout ~n", [Ctx#context.id]),
            election(newctx(Ctx))
    end.

% LEADER State
leader(Ctx) ->
    io:format("leader ~p~n", [Ctx]),
    leader_heartbeat(Ctx).

leader_heartbeat(Ctx) ->
    io:format("retained leadership ~p~n", [Ctx#context.iter]),
    Pids = pg2:get_members("bully"),
    [ timer:send_after(1000, Pid, {heartbeat, self(), Ctx#context.id, Ctx#context.iter})
      || Pid <- Pids, Pid /= self() ],
    leader_heartbeat(Ctx#context{pids=Pids}, 0).
leader_heartbeat(Ctx, Accepts) when Accepts > length(Ctx#context.pids) / 2 ->
    leader_heartbeat(newctx(Ctx));
leader_heartbeat(Ctx, Accepts) ->
    receive
        {accept, Id, Iter} when Iter == Ctx#context.iter, Id < Ctx#context.id ->
            leader_heartbeat(Ctx, Accepts + 1);
        {elect, _, Id, _} when Id > Ctx#context.id ->
            election(newctx(Ctx));
        {elect, From, Id, _} when Id < Ctx#context.id ->
            From ! {elect, self(), Ctx#context.id, Ctx#context.iter},
            leader_heartbeat(Ctx, Accepts);
        Msg ->
            io:format("~p leader_heartbeat ignored ~p ~p ~n", [Ctx#context.id, Msg, Ctx]),
            leader_heartbeat(Ctx, Accepts)
        after 3000 ->
            io:format("timeout lost leadership ~n"),
            election(newctx(Ctx))
    end.

% FOLLOWER state
follower(Ctx, LeaderPid, LeaderId, Iter) ->
    LeaderPid ! {accept, Ctx#context.id, Iter},
    io:format("~p following ~p~n", [Ctx#context.id, LeaderId]),
    follower_loop(Ctx, LeaderPid, LeaderId).

follower_loop(Ctx, LeaderPid, LeaderId) ->
    receive
        {elect, From, Id, Iter} when Id > LeaderId ->
            follower(Ctx, From, Id, Iter);
        {_, LeaderPid, LeaderId, Iter} ->
            follower(Ctx, LeaderPid, LeaderId, Iter);
        Msg ->
            % io:format("~p follower ignored ~p~n", [Ctx#context.id, Msg]),
            follower_loop(Ctx, LeaderPid, LeaderId)
        after 3000 ->
            election(newctx(Ctx))
    end.

% helpers
newctx(Ctx) ->
    Ctx#context{iter=Ctx#context.iter + 1}.
