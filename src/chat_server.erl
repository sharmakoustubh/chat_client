-module(chat_server).
-compile(export_all).

start()->
    Ref = make_ref(),
    Parent = self(),
    io:format("Starting chat server~n"),
    Result = gen_tcp:listen(50556, [list, {packet, 0}, {active, false}]),
    case Result of
	{ok,LSock}->	
	    spawn(fun()-> 
			  Parent ! {ok, Ref},
			  register(?MODULE,self()),
			  loop(LSock)
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

loop(LSock) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    spawn(fun()-> handle_connection(Sock) end),
    loop(LSock).


handle_connection(Sock) ->
    case gen_tcp:recv(Sock, 0) of
	{ok, Data} ->
	    io:format("Got data: ~p~n", [Data]),
	   % format_process_send(Data,Sock),
	    handle_connection(Sock);
	{error, closed} ->
	    io:format("Connection closed~n", []);
	Error ->
	    io:format("Unhandled error: ~p~n", [Error])
    end.

format_process_send(Data,Sock)->
    Formatted_data = format(Data),
    send(Sock,[]).

format(Data)->
    Data_without_nextline = Data -- "\n",
    string:tokens(Data_without_nextline," "). 
  
terminate(Sock)->
    gen_tcp:close(Sock).

send(Sock,Result)->
    Formatted_data = io_lib:format("~p~n",[Result]),
    gen_tcp:send(Sock, Formatted_data).
