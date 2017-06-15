-module(chat_server_tests).
-include_lib("eunit/include/eunit.hrl").

chat_server_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"check you can start the server",fun start_server/0},
      {"check port is already in use",fun port_already_in_use /0}

     ]}.

setup() ->
    ok = chat_server:start().

cleanup(_) ->
    exit(whereis(chat_server),kill),
    ensure_exited_chat_server().

ensure_exited_chat_server()->
    case whereis(chat_server) of
	undefined ->
	    ok;
	_ ->
	    timer:sleep(10),
	    ensure_exited_chat_server()
    end.       
 
port_already_in_use()->
    Expect = {error, eaddrinuse},
    Result = chat_server:start(),
    ?assertMatch(Expect,Result).
    
start_server()->
    Pid= whereis(chat_server),
    io:format(user,"~p~n",[Pid]),    
    ?assertMatch(true,is_pid(Pid)).

