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


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").
-endif.



	
get_active_handlers()->


start_handler(Name) when is_atom(Name)->


-ifdef(TEST)




-endif. 
		 
