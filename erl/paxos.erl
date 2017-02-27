-module(paxos).
-export([ start_acceptor/1, acceptor_loop/2 ]).


start_acceptor(Nodes) when is_list(Nodes) ->
    spawn(paxos, acceptor_loop, [0, Nodes]).

%% Acceptor Implementation
acceptor_loop(Id, Nodes) when is_integer(Id), is_list(Nodes) ->
    Id.
