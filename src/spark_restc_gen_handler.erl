-module(spark_restc_gen_handler).
-behviour(gen_event).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").
-endif.

-record(state, {}).

-type state() :: #state{}.
-type message():: term().
-type reason() :: tuple().
-type ok_or_error():: {ok, state() | {error, reason()}.



-ifdef(TEST)


-endif.
