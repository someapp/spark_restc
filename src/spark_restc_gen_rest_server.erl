-module(spark_restc_gen_rest_server).
-behaviour(gen_server).
-compile([parse_transform, lager_transform]).

-export([
		 add_handler/2,
		 delete_handler/2,
		 notify/2
]).

-export([start/1,
         init/1]).
         
-export([start_link/1,
		 start_link/2]).
-export([handle_call/3,
		 handle_cast/2,
		 handle_info/2, 
		 terminate/2, 
		 code_change/3]).		

-record(state, {}).

-type state() :: #state{}.
-type message():: term().
-type reason() :: tuple().
-type ok_or_error():: {ok, state()} | {error, reason()}.

-define(SERVER, ?MODULE).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").
-endif.
-include_lib("lager/include/lager.hrl").

start_link(Args)->
  start_link([], Args).

start_link(HandlerModules, Args)->
  gen_server:start_link({local, ?SERVER},
  			 ?MODULE, [HandlerModules, Args], []).  

start(Args) -> start_link(Args).
  
init(Arg0)->
  [HandlerModules, Args] = Arg0,
  [Conf_path, Conf_file, Environment, UseMnesia] = Args, 
  Ret = case UseMnesia of
  	 true -> 
  	 	spark_restc_config_server:load_config_db(Environment, Conf_file);
  	 false -> 
  	 	spark_restc_config:load_config(Conf_path, Con_file)  
  end,
  lists:foreach(fun(HandlerMod, Args) -> 
    error_logger:info_msg("[~p] Starting handler ~p with options ~p",	
  			  [?SERVER, HandlerMod, Args]),

  	ok = init_it(HandlerMod, Args),
  	error_logger:info_msg("[~p] Handler ~p started",
  		 [?SERVER, HandlerMod])
  end, HandlerModules).

init_it(HandlerModule, Args)-> 
  case catch(HandlerModule:start(Args)) of
  		ok -> {ok, HandlerModule};
  		{HandlerModule, already_started} 
  			-> {stop, {already_started, HandlerModule}};
 		Error -> {stop, Error} 
  end. 


add_handler(RegisterName, HandlerMod, Args) when is_atom(HandlerMod)->
  gen_server:call(?SERVER, {add_handler , RegisterName, HandlerMod, Args}).

remove_handler(RegisterName, HandlerMod) when is_atom(HandlerMod)->
  gen_server:call(?SERVER, {remove_handler, RegisterName, HandlerMod}).

notify(EventMgr, {HandlerMod, Message}) when is_atom(HandlerMod)->
  gen_server:call(?SERVER, {notify, EventMgr, {HandlerMod, Message}}).

  
handle_call({add_handler , HandlerMod, Args}, From, State)->
  Reply = init_it(HandlerMod, Args),
  {ok, Reply, State};

handle_call({delete_handler, RegisteredName, HandlerMod}, From, State)->
  Reply = gen_event:delete_handler(RegisteredName, HandlerMod, stop),
  {ok, Reply, State};

handle_call({notify, RegisteredName, Message}, From, State)->
  Reply = gen_event:notify(RegisteredName, heartbeat),
  {ok, Reply, State};


handle_call(Request, From, State)->
  error_logger:warn_msg("[~p] Message ~p ~p",[?SERVER, Request, not_supported]),
  {ok, Reply, State}.

handle_cast(Request, State)->
  error_logger:warn_msg("[~p] Message ~p ~p",[?SERVER, Request, not_supported]),
  {noreply, State}.

handle_info(Request, State)->
  error_logger:warn_msg("[~p] Message ~p ~p",[?SERVER, Request, not_supported]),
  {noreply, State}.

terminate(stop, Why) ->
  error_logger:info_msg("[~p] terminated [~p] with reason ~p ",[?SERVER,stop, Why]),
  {ok, Why};
  
terminate(Msg, Why)->
  error_logger:info_msg("[~p] terminated [~p] with reason ~p ",
  		  [?SERVER,stop, Why]),
  {ok, Why}.
 
code_change(OldVsn, State, _Extra)->
  NewState = State,  
  {ok, NewState}.




-ifdef(TEST).




-endif. 
		 
