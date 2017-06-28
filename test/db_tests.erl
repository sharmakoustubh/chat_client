-module(db_tests).
-include_lib("eunit/include/eunit.hrl").

db_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
%%      {"user registration",fun check_user_registers_properly/0}
     {"start db", fun start_db/0}
     ]}.
%% %%      

setup() ->
    db:start().

cleanup(_) ->
    exit(whereis(db),kill),
    ensure_exited_db().

ensure_exited_db()->
    case whereis(db) of
	undefined ->
	    ok;
	_ ->
	    timer:sleep(10),
	    ensure_exited_db()
    end.       


start_db()->
    Pid= whereis(db),
    io:format(user,"~p~n",[Pid]),    
    ?assertMatch(true,is_pid(Pid)).

check_user_registers_properly()->
    Expect = {user_details,{username= "kb", password = [], message = []}},
    %% {user_details,{username=[],
    %% 			    password = [],
    %% 			    message = []}},
    Result = db:register("kb","passxyz", {user_details,{username=[],password = [],message = []}}),
    ?assertMatch(Expect,Result).
