-module(db).
-export([start/0,
	 loop/1, 
	 register_client/3,
	 check_login_credentials_are_correct/3,
	 check_username_exists/2]).

-include("record_definition.hrl").

start() ->
    spawn(fun() -> register(?MODULE, self()), 
		   Tid = ets:new(database, [set, {keypos, 1}]), 
		   loop(Tid)
	  end).

loop(Tid)->
    Updated_database =  listen_to_server(Tid),
    loop(Updated_database).
    
listen_to_server(Tid)->
    receive 
    	{From, Ref, register_client, Username, Password} ->
    	    case check_username_exists(Username, Tid) of
    		false ->
    		    ok = register_client(Tid, Username, Password),
    		    From ! {Ref, {ok, "user registered"}};
		true ->
    		    From ! {Ref, {error, "the username   already exists"}},
    		    Tid
    	    end
	
    	%% {From, Ref, login, Username, Password} ->
    	%%     case check_login_credentials_are_correct(Username, Password, Tid) of
    	%% 	true ->
    	%% 	    New_messages = check_for_new_messages(Username, Tid),
    	%% 	    From ! {Ref, login_successful},
    	%% 	    From ! {Ref, {new_messages, New_messages}},
    	%% 	    Updated_database = update_login_status(From, Ref, Tid);
    	%% 	false ->
    	%% 	    From ! {Ref, {error, "the login credentials are not correct"}},
    	%% 	    Updated_database
    	%%     end

	%% {From, Ref, send_msg, Username1, Username2, Message} ->
	%%     case check_second_user_exists(Username2, Tid) of
	%% 	true ->
	%% 	    case check_user_is_online(Username2) of
	%% 		true->
	%% 		    From ! {Ref, second_user_is_online},
	%% 		    Updated_database = send_new_messages(Username2, Message, Tid),
	%% 		    From ! {Ref, message_sent},
	%% 		    Updated_database;
	%% 		false ->
	%% 		    From ! {Ref, {error, "the second user is not online"}},
	%% 		    Updated_database = store_the_messages(Username2, Message, Tid),
	%% 		    From ! {Ref, message_stored},
	%% 		    Updated_database
	%% 	    end;
	    %% 	false ->
	    %% 	    From ! {Ref, {error, "Second user does not exist"}},
	    %% 	    Updated_database
	    %% end
	
    after 0 -> 
    	    Tid
    end.

check_username_exists(Tid, Username)->
    case ets:lookup(Tid, Username) of
	[{_,Value}] ->
	    true;
	[] ->
	    false
    end.

register_client(Username, Password) ->
    Ref = make_ref(),
    ?MODULE ! {self(), Ref, register_client, Username, Password},
    receive
       	{Ref, {ok, "user registered"}}->
	    ok
    after 500 ->
	    {error, no_response}
    end.

register_client(Tid, Username, Password) ->
    ets:insert(Tid, {Username, {Password,{[]}}}),
    ok. 


check_login_credentials_are_correct(Tid, Username, Password)->
    case ets:lookup(Tid, Username) of
	[{_,{Password,_}}] ->
	    true;
	[] ->
	    false
    end.


%% check_for_new_messages(Username, Tid) ->
%%     Value = proplists:get_value(Username,Tid),
%%     Value#user_details.new_messages.
