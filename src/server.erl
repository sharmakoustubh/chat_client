-module(server).
-compile(export_all).
-define(INPUT_ERROR,"This command is not executable; Executable commands are register, login, exit").
-define(USER_ALREADY_EXISTS_ERROR,"The user already exists").
-define(USER_REGISTERED,"The user is registered").
-define(USER_LOGGEDIN,"The user is logged in").
-define(INCORRECT_LOGIN_CREDENTIALS,"The login credentials are incorrect").
-define(MESSAGE_SENT,"Message sent").
-define(MESSAGE_NOT_SENT,"can't locate the receiver in the database hence cannot send your message").
-include("record_definition.hrl").

start()->
    Ref = make_ref(),
    Parent = self(),
    io:format("Starting chat server~n"),
    Tid = ets:new(database, [set, {keypos, 1}, public]), 
    Result = gen_tcp:listen(5000, [list, {packet, 0}, {active, false}]),
    case Result of
	{ok,LSock}->	
	    spawn(fun()-> 
			  Parent ! {ok, Ref},
			  register(?MODULE,self()),
			  loop(LSock, Tid)
		  end),
	    receive
		{ok,Ref} ->
		    ok
	    after 1000 ->
		    {error, "Couldn't start the chat server"}
	    end;
	Error->
	    Error
    end.	

loop(LSock, Tid) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    io:format("client A is connected~n"),
    send(Sock,"type the command you wnat to execute followed by args for that"),
    spawn(fun()-> handle_connection(Sock, Tid) end),
    loop(LSock, Tid).

handle_connection(Sock, Tid) ->
    case gen_tcp:recv(Sock, 0) of
	{ok, Data} ->
	    io:format("Got data: ~p~n", [Data]),
	    format_process_send(Data, Sock, Tid),
	    handle_connection(Sock, Tid);
	{error, closed} ->
	    io:format("Connection closed~n", []);
	Error ->
	    io:format("Unhandled error: ~p~n", [Error])
    end.

format_process_send(Data, Sock, Tid)->
    Formatted_data = format(Data),
    Output = case process(Formatted_data, Sock, Tid) of
		 {error,Error}->
		     Error;
		 Result->
		     Result 
	     end,      
    send(Sock,Output).

format(Data)->
    case "\n" == Data of
	true ->
	    Data;
	false ->
	    Data -- "\n"
    end.

process(Data, Sock, Tid)->
    [Cmd|Rest] = string:tokens(Data," "),
    Elements_count = length(Rest),
    io:format(user,"Tokens data: ~p~n", [Rest]),
    case Cmd of
    	"register"->
	    case  Elements_count of
		2->
		    register_client(Rest, Tid);
		_->
		    ?INPUT_ERROR
	    end;
	
	"login"->
	    case  Elements_count of
		2->
		    login_client(Rest, Tid, Sock);
		_->
		    ?INPUT_ERROR
	    end;

	"send_message_to"->
	    case  Elements_count of
		E when E >=2 ->
		    io:format(user,"The message tokens are-----> ~p ~n",[Rest]),
		    send_message(Rest, Tid);
		_->
		    ?INPUT_ERROR
	    end;
	
	"exit"->
	    case Elements_count of
		0->		    
		    terminate(Sock);
		_->
		    ?INPUT_ERROR
	    end;
    	_->
    	    lists:flatten(io_lib:format("~s~n",["This command is not executable; Executable commands are register, login, send_message_to and exit"]))
    end.


register_client([Username, Password], Tid)->
    %%Result = #user_details{username = Username, password = Password},
    %% db:register(Username, Password),
    %% Result
    case check_username_exists(Username, Tid) of
	false ->
	    register_client_user(Username, Password, Tid),
	    ?USER_REGISTERED;
	true ->
	    ?USER_ALREADY_EXISTS_ERROR
    end.

register_client_user(Username, Password, Tid) ->
    ets:insert(Tid, {Username, {Password, [{"Welcome" , os:timestamp(), unseen}]}}).

check_username_exists(Username, Tid)->
    case ets:lookup(Tid, Username) of
	[{_,_Value}] ->
	    true;
	[] ->
	    false
    end.

login_client([Username, Password], Tid, Sock) ->
    case check_login_credentials_are_correct(Username, Password, Tid) of
	true ->
	    spawn(fun()-> message_loop(Username, Tid, Sock) end),
	    %% New_messages = check_for_new_messages(Username, Tid);
	    ?USER_LOGGEDIN;
	false ->
	    ?INCORRECT_LOGIN_CREDENTIALS
    end.

check_login_credentials_are_correct(Username, Password, Tid) ->
    case ets:lookup(Tid, Username) of
	[{_,{Password,_ }}] ->
	    true;
	[] ->
	    false
    end.

message_loop(Username, Tid, Sock)->
    case ets:lookup(Tid, Username) of
	[{Username,{Password, Messages }}]->
	    io:format(user,"Before the list comprehension state of receiver is----->>> ~p ~n",[ ets:lookup(Tid, Username)]),
	    Unseen_Messages = [lists:concat(add_space([], Msg)) || {Msg,_,unseen} <- Messages],
	  
	    io:format(user,"The unseen messages are----->>> ~p ~n",[Unseen_Messages]),
	    send(Sock, Unseen_Messages),

	    Seen_Messages = [{Msg, Time, seen} || {Msg, Time, unseen} <- Messages],
	    ets:delete(Tid, Username),
	    ets:insert(Tid, {Username,{Password,Seen_Messages}});
	[] ->
	    send(Sock, "the message loop can't locate the user in the database")
    end,
    timer:sleep(6000),
    message_loop(Username, Tid, Sock).

add_space(L,[]) ->
   L;
add_space(L,[H|T]) ->
   add_space(L ++[" "] ++ [H],T).

send_message([To, Message], Tid)->
    case ets:lookup(Tid, To) of
	[{To,{Password,Old_Messages}}]->
	    io:format(user,"Old  messages are----->>> ~p ~n",[Old_Messages]),
	    io:format(user,"New  messages are----->>> ~p ~n",[Message]),
	    io:format(user,"old state of the receiver is  ~p ~n",[ets:lookup(Tid, To)]),
	    ets:delete(Tid, To),
	    ets:insert(Tid, {To,{Password,{[ {Message, os:timestamp(), unseen} | Old_Messages ]}}}),
	    Temp = ets:lookup(Tid, To),
	    io:format(user,"Updated message value of the receiver is  ~p ~n",[Temp]),
	    ?MESSAGE_SENT;
	[] ->
	    ?MESSAGE_NOT_SENT
    end.

terminate(Sock)->
    gen_tcp:close(Sock).

send(Sock,Result)->
    Formatted_data = io_lib:format("~p~n",[Result]),
    gen_tcp:send(Sock, Formatted_data).
