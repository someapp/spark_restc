-module(spark_restc_gen_rest_server).
-behaviour(gen_server).

-export([get_active_handlers/0,
		 start_handler/1,
		 kill_handler/1,
		 sniff_handler/2
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
-type ok_or_error():: {ok, state() | {error, reason()}.

-define(SERVER, ?MODULE).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").
-endif.




	
get_active_handlers()->
  gen_server:call(?SERVER, {get_active_handlers}).

start_handler(Name) when is_atom(Name)->
  gen_server:call(?SERVER, {start_handler, Name}).

kill_handler(Name) when is_atom(Name)->
  gen_server:call(?SERVER, {kill_handler, Name}).

sniff_handler(Name, Level) when is_atom(Name)->
  gen_server:call(?SERVER, {sniff_handler, Name, Level}).

handle_call({get_active_handlers}, From, State)->

  {ok, Reply, State};
  
handle_call({start_handler, Name}, From, State)->

  {ok, Reply, State};

handle_call({kill_handler, Name}, From, State)->

  {ok, Reply, State};

handle_call({sniff_handler, Name, Level}, From, State)->

  {ok, Reply, State};


handle_call(Request, From, State)->

  {ok, Reply, State}.

handle_cast(Request, State)->
  {noreply, State}

handle_info(Request, State)->

  {noreply, State}.

termiante(stop, Why)->
  {ok, Why};
  
terminate(Msg, Why)->
  {ok, Why}.
 
code_change(OldVsn, State, _Extra)->
  NewState = State,  
  {ok, NewState}.

-ifdef(TEST).




-endif. 
		 
