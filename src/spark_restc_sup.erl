-module(spark_restc_sup).

-behaviour(supervisor).

%% API
-export([start_link/1, start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Args]).

start_link() -> 
    Args = [<<"etc">>, <<"spark_restc.yaml">> , false], 
    start_link(Args).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(Args) ->
    Children = [
   			?CHILD(spark_restc_config_server, worket, Args),
    		?CHILD(spark_restc_gen_rest_server, worker, Args)
    ],
    {ok, { {one_for_one, 5, 10}, Children} }.

