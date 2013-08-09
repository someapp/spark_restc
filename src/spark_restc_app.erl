-module(spark_restc_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Conf_path = a, <<"etc">>},
  	Conf_file, <<spark_restc.yaml>>},
  	Use_mnesia_conf_store, false}
    spark_restc_sup:start_link().

stop(_State) ->
    ok.
