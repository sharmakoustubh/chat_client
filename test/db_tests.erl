-module(db_tests).
-include_lib("eunit/include/eunit.hrl").

db_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"start db", fun start_db/0},
      {" check user exists or not", fun test_for_check_username_exists/0},
      {" check user is registered in the ets table", fun check_user_registers_properly/0},
      {" check user login credentials in the ets table", fun test_for_check_login_credentials/0}
  %%    {"user registration",fun check_user_registers_properly/0}
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

test_for_check_username_exists()->
    Tid = ets:new(database, [set, {keypos, 1}]),
    ets:insert(Tid, {abc, {xyz,{[]}}}), 
    Result = db:check_username_exists(Tid,abc),
    ?assertMatch(true,Result).

check_user_registers_properly()->
    Tid = ets:new(database, [set, {keypos, 1}]),
    Result = db:register_client(Tid, abc, xyz), 
    ?assertMatch(ok,Result).

test_for_check_login_credentials()->
    Tid = ets:new(database, [set, {keypos, 1}]),
    ets:insert(Tid, {abc, {xyz,{[]}}}), 
    Result = db:check_login_credentials_are_correct(Tid, abc, xyz),
    ?assertMatch(true,Result).

%% check_user_registers_properly()->
%%     Tid = ets:new(database, [set, {keypos, 1}]),
%%     Result = db:register_client(Tid, abc, xyz), 
%%     ?assertMatch(ok,Result).
