-module(server_tests).
-include_lib("eunit/include/eunit.hrl").

chat_server_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"check you can start the server",fun start_server/0},
      {"check port is already in use",fun port_already_in_use/0},
      {"register the user test",fun register_user/0}
           ]}.

setup() ->
    ok = server:start().

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
    Result = server:register_client(["Kb","mypassword"]),
    ?assertMatch({user_details,"Kb","mypassword",_},Result).

%% login_user()->
%%     Result = server:login_client(["Kb","mypassword"]),
%%     ?assertMatch({user_details,"Kb","mypassword",_},Result).

%% fetch_login_password()->
%%     Result = server:register_client(["Kb","mypassword"]),
%%     ?assertMatch({user_details,"Kb","mypasswordc",_},Result).


