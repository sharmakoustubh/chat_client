-module(server_tests).
-include_lib("eunit/include/eunit.hrl").

server_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"check you can start the server",fun start_server/0},
      {"check port is already in use",fun port_already_in_use/0},
      {"register the user test",fun register_user/0},
      {"Login the user",fun login_user/0},
      {"Send message to the client",fun send_message_to_client/0}
           ]}.

setup() ->
    server:start().

cleanup(_) ->
    exit(whereis(server),kill),
    ensure_exited_server().

ensure_exited_server()->
    case whereis(server) of
	undefined ->
	    ok;
	_ ->
	    timer:sleep(10),
	    ensure_exited_server()
    end.       

port_already_in_use()->
    Expect = {error, eaddrinuse},
    Result = server:start(),
    ?assertMatch(Expect,Result).
    
start_server()->
    Pid= whereis(server),
    io:format(user,"~p~n",[Pid]),    
    ?assertMatch(true,is_pid(Pid)).

register_user()->
    Tid = ets:new(database, [set, {keypos, 1}]), 
    Result1 = server:register_client(["Kb","mypassword"], Tid),
    ?assertMatch("The user is registered",Result1),
    Result2 = ets:lookup(Tid, "Kb"),
    ?assertMatch([{"Kb",{"mypassword",{[]}}}],Result2).

login_user()->
    Tid = ets:new(database, [set, {keypos, 1}]), 
    Result1 = server:register_client(["Kb","mypassword"], Tid),
    ?assertMatch("The user is registered",Result1),
    Result2 = server:login_client(["Kb","mypassword"], Tid, 241692),
    ?assertMatch("The user is logged in",Result2).

send_message_to_client()->
    Tid = ets:new(database, [set, {keypos, 1}]), 
    
    Result1 = server:register_client(["A","passwordA"], Tid),
    ?assertMatch("The user is registered",Result1),
    Result2 = server:login_client(["A","passwordA"], Tid, 241692),
    ?assertMatch("The user is logged in",Result2),
    
    Result3 = server:register_client(["B","passwordB"], Tid),
    ?assertMatch("The user is registered",Result3),
    Result4 = server:login_client(["B","passwordB"], Tid, 241692),
    ?assertMatch("The user is logged in",Result4),
    
    Result5 = server:send_message(["B","hi"], Tid),    
    ?assertMatch("Message sent",Result5),
    
    Result6 = ets:lookup(Tid, "B"),
    ?assertMatch([{"B",{_,{[[]|{"hi",_,unseen}]}}}],Result6).

    
    %% Result5 = server:(["Kb","mypassword"], Tid, 241692),
    %% ?assertMatch("The user is logged in",Result2).


%% fetch_login_password()->
%%     Result = server:register_client(["Kb","mypassword"]),
%%     ?assertMatch({user_details,"Kb","mypasswordc",_},Result).


