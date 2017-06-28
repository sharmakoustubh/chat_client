-module(db).
-export([start/0,
	 loop/1, 
	 register/3,
	 check_username_exists/2]).

-include("record_definition.hrl").

start()->
    register(?MODULE,self()),
    io:format("Starting db~n"),
    loop([]).

loop(Database)->
    Updated_database =  listen_to_server(Database),
    loop(Database).
    
listen_to_server(Database)->
    receive 
    	{From, Ref, register, Username, Password} ->
    	    case check_username_exists(Username, Database) of
    		false ->
    		    Updated_database = register(Username, Password, Database),
    		    From ! {Ref, {ok, "user registered"}},
    		    Updated_database;
    		true ->
    		    From ! {Ref, {error, "the username already exists"}},
    		    Database
    	    end;
	
    	{From, Ref, login, Username, Password} ->
    	    case check_login_credentials_are_correct(Username, Password, Database) of
    		true ->
    		    New_messages = check_for_new_messages(Username, Database),
    		    From ! {Ref, login_successful},
    		    From ! {Ref, {old_messages, New_messages}},
    		    Updated_database = update_login_status(From, Ref, Database);
    		false ->
    		    From ! {Ref, {error, "the login credentials are not correct"}},
    		    Updated_database
    	    end;
	{From, Ref, send_msg, Username1, Username2, Message} ->
	    case check_second_user_exists(Username2, Database) of
		true ->
		    case check_user_is_online(Username2) of
			true->
			    From ! {Ref, second_user_is_online},
			    Updated_database = send_new_messages(Username2, Message, Database),
			    From ! {Ref, message_sent},
			    Updated_database;
			false ->
			    From ! {Ref, {error, "the second user is not online"}},
			    Updated_database = store_the_messages(Username2, Message, Database),
			    From ! {Ref, message_stored},
			    Updated_database
		    end;
		false ->
		    From ! {Ref, {error, "Second user does not exist"}},
		    Updated_database
	    end
	
    after 0 -> 
    	    Database
    end.


register( Username, Password, Database) ->
    New_Rec = #user_details{username = Username,
			    password = Password,
			    message = []},
    io:format(user, "the registered user record is ~p~n",[New_Rec]),
    New_entry_for_database = {Username, New_Rec},
    [New_entry_for_database | Database].

check_username_exists(Username, Database)->
    lists:keymember(Username, 1,  Database).

check_login_credentials_are_correct(Username, Password, Database)->
    Value = proplists:get_value(Username,Database), 
    case Value of 
	undefined ->
	    false;
	_ ->
	    Stored_password = Value#user_details.password,
	    case Stored_password of
		Password ->
		    true;
		_ ->
		    false
	    end 
    end.

check_for_new_messages(Username, Database) ->
    Value = proplists:get_value(Username,Database),
    Value#user_details.new_messages.
