-module(spark_restc_gen_rest_server).
-behaviour(gen_server).
-compile([parse_transform, lager_transform]).

-export([get_active_handlers/0,
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
  start_link(default_handler, Args).

start_link(HandlerModule, Args)->
  gen_server:start_link({local, ?SERVER},
  			 ?MODULE, [HandlerModule, Args], []).  
  
init(HandlerModule, Args)->
  error_logger:info_msg("Starting handler ~p with options ~p",	
  			  [HandlerModule, Args]),
  case catch(HandlerModule:start(Args)) of
  		ok -> {ok, HandlerModule};
  		already_started -> {stop, {already_started, HandlerModule}};
 		Error -> {stop, Error} 
  end. 


get_active_handlers()->
  gen_server:call(?SERVER, {get_active_handlers}).

add_handler(Name) when is_atom(Name)->
  gen_server:call(?SERVER, {add_handler, Name}).

remove_handler(Name) when is_atom(Name)->
  gen_server:call(?SERVER, {remove_handler, Name}).

notify(Name, Level) when is_atom(Name)->
  gen_server:call(?SERVER, {notify, Name, Level}).

handle_call({get_active_handlers}, From, State)->

  {ok, Reply, State};
  
handle_call({add_handler, Name}, From, State)->

  {ok, Reply, State};

handle_call({remove_handler, Name}, From, State)->

  {ok, Reply, State};

handle_call({notify, Name, Level}, From, State)->

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
		 
