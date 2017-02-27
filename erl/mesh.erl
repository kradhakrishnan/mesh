-module(mesh).
-export([ start/1, boot_process/1, ping_loop/1 ]).


start(Nodes) ->
    spawn(mesh, boot_process, [Nodes]).

boot_process(Nodes) ->
    Pid = spawn(mesh, ping_loop, [Nodes]),
    Ref = monitor(process, Pid),
    receive
        {DOWN, Ref, process, Pid, _} ->
            io:format("~p died.", [Pid]),
            start(Nodes)
    end.

ping_loop(Nodes) ->
    [ net_adm:ping(Node) || Node <- Nodes, Node /= self() ],
    timer:sleep(1000).
