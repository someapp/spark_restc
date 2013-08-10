-module(spark_restc_gen_rest_server).
-behaviour(gen_server).
-compile([parse_transform, lager_transform]).

-export([
		 add_handler/1,
		 remove_handler/1,
		 notify/2
]).

-export([start/1,
         init/1].
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
  
init(HandlerModules, Args)->
  [Conf_path, Conf_file, Environment, UseMnesia] = Args 
  spark_restc_config_server:load_config_db(Environment, Conf_file),  

  lists:foreach(fun(HandlerMod, Args) -> 
    error_logger:info_msg("[~p] Starting handler ~p with options ~p",	
  			  [?SERVER, HandlerModule, Args]),

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


add_handler(ToEventMgr, HandlerName) when is_atom(HandlerName)->
  gen_server:call(?SERVER, {add_handler , ToEventMgr, HandlerName}).

remove_handler(FromEventMgr, HandlerName) when is_atom(HandlerName)->
  gen_server:call(?SERVER, {remove_handler, FromEventMgr, HandlerName}).

notify(EventMgr, {HandlerName, Message}) when is_atom(HandlerName)->
  gen_server:call(?SERVER, {notify, EventMgr, {HandlerName, Message}}).

  
handle_call({add_handler , ToEventMgr, HandlerName}, From, State)->

  {ok, Reply, State};

handle_call({remove_handler, FromEventMgr, HandlerName}, From, State)->

  {ok, Reply, State};

handle_call({notify, EventMgr, {HandlerName, Message}}, From, State)->

  {ok, Reply, State};


handle_call(Request, From, State)->

  {ok, Reply, State}.

handle_cast(Request, State)->
  {noreply, State}

handle_info(Request, State)->

  {noreply, State}.

termiante(stop, Why)->
  error_logger:info_msg("[~p] terminated [~p] with reason ~p ",
  		  [?SERVER,stop, Why]),
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
		 
