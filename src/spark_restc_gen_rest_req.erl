-module(spark_restc_gen_rest_req).
-behviour(gen_event).

-export([init/1,
		 handle_event/2,
		 handle_call/2,
		 handle_info/2
		 terminate/2,
		 code_change/3
]).

-record(state, {}).

-type state() :: #state{}.
-type message():: term().
-type reason() :: tuple().
-type ok_or_error():: {ok, state() | {error, reason()}.



-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").
-endif.





-ifdef(TEST)


-endif.
