-module(spark_restc_gen_handler).
-behviour(gen_event).
-compile([parse_transform, lager_transform]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").
-endif.
-include_lib("lager/include/lager.hrl").

-export([start/3, stop/2]).

-export([
		init/1,
		handle_event/2,
		handle_call/2,
		handle_info/2,
		code_change/2,
		terminate/2

]).


-record(state, {
	current_state = undefined :: atom(),
	next_state = undefined :: atom()

}).

-type state() :: #state{}.
-type message():: term().
-type reason() :: tuple().
-type ok_or_error():: {ok, state()} | {error, reason()}.

start(EventHandler, Mod, Args)->
  case lists:member(Mod, gen_event:which_handlers(EventHandler)) of
  	true -> 
  		{EventHandler, already_started};
    false ->
        gen_event:add_handler(EventHandler, Mod, Args)
  end.

stop(EventHandler, Mod)->
   gen_event:delete_handler(EventHandler, Mod, stop ). 





handle_info(Message, HandlerModule)->
  {noreply, HandlerModule}.

terminate(stop, normal)->
  error_logger:info_msg("[~p] terminated [~p] with reason ~p ",
  			  [EventHandler,stop, normal]),
  {ok, normal};

terminate(stop, normalstopped)->
  error_logger:info_msg("[~p] terminated [~p] with reason ~p ",
  			  [EventHandler,stop, normalstopped]),
  {ok, normalstopped};
  
terminate(Message, Why)->
  error_logger:info_msg("[~p] terminated [~p] with reason ~p ",
  			  [EventHandler,stop, Why]),
  {ok, Why}.
  
code_change(_OldVsn, State, _Extra)->
  {ok, State}.


-ifdef(TEST).


-endif.
