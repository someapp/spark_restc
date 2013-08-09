-module(spark_restc_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-define(APP, spark_restc_app).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	ok = ensure_dependency_started(),
    Conf_path0 = 
    		application:get_env(?APP, conf_path,undefined),
  	Conf_file = 
  			application:get_env(?APP, conf_file,undefined),
  	Use_mnesia_conf_store = 
  			application:get_env(?APP, use_mnesia_conf_store, false),
  	
  	Conf_path = real_dir_path(Conf_path0),		
  			
	error_logger:info_msg("[~p] Using config path ~p",
			[?APP, Conf_path]),
	error_logger:info_msg("[~p] Using config file: ~p",
			[?APP, Conf_file]),
	error_logger:info_msg("[~p] Using mnesia as config store? ~p",
			[?APP, Use_mnesia_conf_store]),
    spark_restc_sup:start_link(
    		[Conf_path, Conf_file, Use_mnesia_conf_store]).

stop(_State) ->
    ok.

ensure_dependency_started()->
  ?INFO_MSG("[~p] Starting depedenecies", [?APP]),
  Apps = [syntax_tools, 
		  compiler, 
		  crypto,
		  public_key,
		  ssl, 
		  goldrush,
		  lager, 
		  inets, 
		  restc],
  error_logger("[~p] Going to start apps ~p",
  	  [?APP, lists:flatten(Apps)]),
  app_util:start_apps(Apps),
  error_logger("[~p] Started depedenecies ~p",
      [?APP, lists:flatten(Apps)]).
