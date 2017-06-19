-module(server).
-compile(export_all).
-define(INPUT_ERROR,"This command is not executable; Executable commands are list,run with module_name function_name function_arguments,info,exit").

start()->
    Ref = make_ref(),
    Parent = self(),
    io:format("Starting chat server~n"),
    Result = gen_tcp:listen(5000, [list, {packet, 0}, {active, false}]),
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
    io:format("client A is connected~n"),
    send(Sock,"type the command you wnat to execute followed by args for that"),
    spawn(fun()-> handle_connection(Sock) end),
    loop(LSock).


handle_connection(Sock) ->
    case gen_tcp:recv(Sock, 0) of
	{ok, Data} ->
	    io:format("Got data: ~p~n", [Data]),
	    format_process_send(Data,Sock),
	    handle_connection(Sock);
	{error, closed} ->
	    io:format("Connection closed~n", []);
	Error ->
	    io:format("Unhandled error: ~p~n", [Error])
    end.

%% terminate(Sock)->
%%     gen_tcp:close(Sock).

%% send(Sock,Result)->
%%     Formatted_data = io_lib:format("~p~n",[Result]),
%%     gen_tcp:send(Sock, Formatted_data).

format_process_send(Data,Sock)->
    Formatted_data = format(Data),
    Output = case process(Formatted_data,Sock) of
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

process(Data,Sock)->
    [Cmd|Rest] = string:tokens(Data," "),
    Elements_count = length(Rest),
    case Cmd of
    	"register"->
	    case  Elements_count of
		2->
		    register_client();
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
    	    lists:flatten(io_lib:format("~s~n",["This command is not executable; Executable commands are list,run,info,exit"]))
    end.
    
register_client()->
    ok.

terminate(Sock)->
    gen_tcp:close(Sock).

send(Sock,Result)->
    Formatted_data = io_lib:format("~p~n",[Result]),
    gen_tcp:send(Sock, Formatted_data).

