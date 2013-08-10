-module(spark_restc_gen_handler).
-behviour(gen_event).
-compile([parse_transform, lager_transform]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").
-endif.
-include_lib("lager/include/lager.hrl").

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
-type ok_or_error():: {ok, state() | {error, reason()}.







-ifdef(TEST)


-endif.
