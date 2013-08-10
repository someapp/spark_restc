-module(spark_restc_gen_rest_req).
-behviour(gen_event).
-compile([parse_transform, lager_transform]).

-export([
		post_request/2,
		post_request/3,
		verify_response/2
]).


-export([init/1,
		 handle_event/2,
		 handle_call/2,
		 handle_info/2,
		 terminate/2,
		 code_change/3
]).

-record(state, {}).

-type state() :: #state{}.
-type message():: term().
-type reason() :: tuple().

-type ok_or_error() :: {ok, state()} | {error, reason()} .


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").
-endif.
-include_lib("lager/include/lager.hrl").

-define(EVENT_HANDLER, ?MODULE).

handle_info({gen_event_EXIT, HandlerModule, Reason}, HandlerModule)->
  error_logger:info_msg("[~p] died with reason ~p ",
  			  [HandlerModule, Reason]),
  {stop, {handler_died, HandlerModule, Reason}, HandlerModule};

handle_info(Message, HandlerModule)->
  {noreply, HandlerModule}.

terminate(stop, normal)->
  error_logger:info_msg("[~p] terminated [~p] with reason ~p ",
  			  [?EVENT_HANDLER,stop, normal]),
  {ok, normal};

terminate(stop, normalstopped)->
  error_logger:info_msg("[~p] terminated [~p] with reason ~p ",
  			  [?EVENT_HANDLER,stop, normalstopped]),
  {ok, normalstopped};
  
terminate(Message, Why)->
  error_logger:info_msg("[~p] terminated [~p] with reason ~p ",
  			  [?EVENT_HANDLER,stop, Why]),
  {ok, Why}.
  
code_change(_OldVsn, State, _Extra)->
  {ok, State}.
  
code_change(_OldVsn, State, _Extra)->
  {ok, State}.
 
-ifdef(TEST).


-endif.
