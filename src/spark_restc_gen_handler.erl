-module(spark_restc_gen_handler).
-behviour(gen_event).
-compile([parse_transform, lager_transform]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").
-endif.
-include_lib("lager/include/lager.hrl").

-define(EVENT_HANDLER, ?MODULE).

-export([post_rest_request/0,
		 verify_rest_response/0
]).

-record(state, {
	current_state = undefined :: atom(),
	next_state = undefined :: atom()

}).

-type state() :: #state{}.
-type message():: term().
-type reason() :: tuple().
-type ok_or_error():: {ok, state()} | {error, reason()}.

start(Args)->
  case lists:member(?MODULE, gen_event:which_handlers(?EVENT_HANDLER)) of
  	true -> 
  		{?EVENT_HANDLER, already_started};
    false ->
        gen_event:add_handler(?EVENT_HANDLER, ?MODULE, Args)
  end.

stop()->
   gen_event:delete_handler(?EVENT_HANDLER, ?MODULE, stop ). 





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


-ifdef(TEST).


-endif.
