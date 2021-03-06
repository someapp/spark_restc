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
    Env =   application:get_env(?APP, environment,undefined),
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
    		[Conf_path, Conf_file, Use_mnesia_conf_store, Env]).

stop(_State) ->
    ok.

ensure_dependency_started()->
  error_logger:info_msg("[~p] Starting depedenecies", [?APP]),
  Apps = [syntax_tools, 
		  compiler, 
		  crypto,
		  public_key,
		  ssl, 
		  goldrush,
		  lager, 
		  inets, 
		  restc],
  error_logger:info_msg("[~p] Going to start apps ~p",
  	  [?APP, lists:flatten(Apps)]),
  app_util:start_apps(Apps),
  error_logger:info_msg("[~p] Started depedenecies ~p",
      [?APP, lists:flatten(Apps)]).
      
real_dir_path(Conf_path0)->
  {ok, Cwd} = file:get_cwd(),
  FileFullPath = filename:join([Cwd, Conf_path0]),
  true = file_lib:is_regular(FileFullPath),
  FileFullPath.
  
