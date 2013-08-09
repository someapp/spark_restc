%% -------------------------------------------------------------------
%% @doc
%%
%% @end
%% -------------------------------------------------------------------

-module(spark_restc_config_server).
-author('etsang@spark.net').
-behaviour(gen_server).

-export([
	 	load_config/1,
	 	version/0,
	 	spark_api_endpoint/0, 
	 	spark_app_id/0,
	 	spark_client_secret/0,
	 	spark_oauth_access_token/0, 
	 	spark_communityid_brandid_map/0,
	 	auth_profile_miniProfile/0, 
	 	profile_memberstatus/0, 
%	 	send_missed_im/0,
 		rest_client_timeout_in_sec/0,
	 	rest_call_retry_attempt/0
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




-type url() :: string() | undefined.
-type accessToken() :: string() | undefined.
-type config_val() :: term().
-type ok_or_error() :: {ok, config_val()} | {error, reason()}.
-type fatalError() :: {'EXIT', {error, {atom(), not_found}}}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").
-endif.


-define(DEFAULT_RESTCONN_TIMEOUT,5).
-define(DEFAULT_RESTCONN_RETRY,3).

-record(state,{
	filename = [] :: file:name()
	config_list = [] ::list()

}).

%% ===================================================================
%% Public API
%% ===================================================================

load_config(Filename)->
   yamerl_constr:file(Filename).

unload_config()-> unload_config(?APP_ENV).  
unload_config(Name)->
   ok = app_helper:ensure_config_unloaded(Name).

reload_config(Name)->
   unload_config(Name),
   load_config(Name).

config_version(List)->
  get_key_val(List, version, undefined).
  
system_app_id(List, Environment) when is_atom(Environment) ->
  get_key_val(List, app_id, "1017").

system_brand_id(List, Environment) when is_atom(Environment) ->
  get_key_val(List, brand_id, "1003").

system_member_id(List, Environment) when is_atom(Environment)->
  get_key_val(List, member_id, "27029711"). 
  
system_client_secret(List, Environment) when is_atom(Environment)->
  get_key_val(List, 
      client_secret, "93XDnCIn30rNYcHVgOJ77kzZzjkCmhrUm3NJ1bf5gNA=").

create_oauth_accesstoken(List, _Environment) ->
  get_key_val(List, create_oauth_accesstoken, 
  	"{base_url}/brandId/{brandId}/oauth2/accesstoken/application/{applicationId}").
  	
profile_miniProfile(List, _Environment) ->	
  get_key_val(List, profile_miniProfile,
  	"{base_url}/brandId/{brandId}/profile/attributeset/miniProfile/{targetMemberId}").  	

profile_memberstatus(List, _Environment) ->
  get_key_val(List, profile_memberstatus,
    "{base_url}/brandId/{brandId}/application/{applicationId}/member/{memberId}/status").

send_im_mail_message(List, _Environment)->
  get_key_val(List, send_im_mail_message,
    "{base_url}/brandId/{brandId}/application/{applicationId}/member/{memberId}/status"). 
 
community_brand_idMap(List)->	
  get_key_val(List, community2brandId, []). 
  
get_key_val([],_)-> {error, empty_config};
get_key_val(List, Key, Default)
  proplists:get_value(Key, List, Default).

start_link(Args)-> 
  error_logger:info_msg(info,"Start linking ~p with Args ~p",[?MODULE, Args]),
  gen_server:start_link(Args).

init(Args)->
  [Conf_path, Conf_file, Use_mnesia_conf_store] = Args,
  Filename = lists:concat([Path,"/", File]),
  true = ec_file:exists(Filename),
  ConfList = load_config(Filename),  
  {ok, #state{
  		filename = Filename,
  		config_list = ConfList
  }}.

handle_call()->


  {ok, Reply, State}.

handle_call()->


  {ok, Reply, State}.

handle_call()->


  {ok, Reply, State}.

handle_call()->


  {ok, Reply, State}.


handle_cast(Unsupported, State)->
  error_logger:info_msg("[~p] Unsupported message: ~p ",
  		[?SERVER, Unsupported]),
  {noreply, State}.

handle_info(Unsupported, State)->
  error_logger:info_msg("[~p] Unsupported message: ~p ",
  		[?SERVER, Unsupported]),
  {noreply, State}.

terminate(_Reason, _State)->
  error_logger:info_msg("[~p] Terminate with reason. ~p ",
  		[?SERVER, Reason]),
  ok.

code_change(_OldVsn, State, _Extra)->
  error_logger:info_msg("[~p] Upgraded.",
  		[?SERVER]),
  {ok, State}.


%%===================================================================
%% EUnit tests
%% ===================================================================
-ifdef(TEST).
-define(DEFAULTVAL, "DefaultTestVal").
-define(TESTAPPENV, ?APP_ENV).

spark_restc_test_() ->
    { setup,
      fun setup/0,
      fun cleanup/1,
      [
        fun load_config_test_case/0,
	fun spark_api_endpoint_test_case/0,
	fun spark_oauth_access_token_test_case/0,
	fun spark_communityid_brandid_map_test_case/0,
	fun profile_memberstatus_test_case/0,
	fun auth_profile_miniProfile_test_case/0,
	fun rest_client_timeout_in_sec_test_case/0,
	fun rest_call_retry_attempt_test_case/0,
	fun rabbitmq_endpoint_test_case/0,
	fun rabbitmq_client_timeout_in_sec_test_case/0,
	fun rabbitmq_client_retry_attempt_in_test_case/0,
	fun non_existent_var_test_case/0,
	fun get_config_value_env_noValue_test_case/0,
	fun get_config_value_env_defaultValue_test_case/0,
	fun get_config_value_env_undefinedVal_test_case/0,
	fun get_config_required_value_ok_test_cases/0,
	fun get_config_required_value_fail_test_cases/0
      ]
    }.

load_config_test_case()->
  ?assertEqual("http://api.stgv3.spark.net/v2", spark_api_endpoint()),
  ?assertEqual("1054",spark_app_id()).



spark_api_endpoint_test_case()->
    application:set_env(?TESTAPPENV, spark_api_endpoint, "http://www.test.endpoint"),
    ?assertMatch("http://www.test.endpoint", spark_api_endpoint()).

spark_oauth_access_token_test_case()->
    application:set_env(?TESTAPPENV, spark_create_oauth_accesstoken, "oauthTESTTOKEN"),
    ?assertMatch("oauthTESTTOKEN", spark_oauth_access_token()).	

spark_communityid_brandid_map_test_case()->
    TestData= [{spark, "1", "1001"}],
    application:set_env(?TESTAPPENV, community2brandId, TestData),
    ?assertEqual( TestData, spark_communityid_brandid_map()).

profile_memberstatus_test_case()->
    TestData = "/brandId/{brandId}/application/{applicationId}/member/{memberId}/status",
    application:set_env(?TESTAPPENV, community2brandId, TestData),
    ?assertEqual(TestData, profile_memberstatus()).

auth_profile_miniProfile_test_case()->
    application:set_env(?TESTAPPENV, auth_profile_miniProfile, "miniProfle/{appId}/{brandid}/"),
    ?assertEqual("miniProfle/{appId}/{brandid}/", auth_profile_miniProfile()).
	
rabbitmq_endpoint_test_case() ->
    application:set_env(?TESTAPPENV, rabbitmq_endpoint, "rabbitMQTEST"),
    ?assertMatch("rabbitMQTEST",rabbitmq_endpoint()).

rest_client_timeout_in_sec_test_case() ->
    application:set_env(?TESTAPPENV, rest_client_timeout_in_sec, 12),
    ?assertEqual(12, rest_client_timeout_in_sec()).
	
rest_call_retry_attempt_test_case() ->
    application:set_env(?TESTAPPENV, rest_call_retry_attempt, 15),
    ?assertEqual(15, rest_call_retry_attempt()).
	
rabbitmq_client_timeout_in_sec_test_case() ->
    application:set_env(?TESTAPPENV, rabbitmq_client_timeout_in_sec,10),
    ?assertEqual(10, rabbitmq_client_timeout_in_sec()).
 
rabbitmq_client_retry_attempt_in_test_case() ->
    ok = application:unset_env(?TESTAPPENV,rabbitmq_client_retry_attempt),
    ok = application:set_env(?TESTAPPENV,rabbitmq_client_retry_attempt, 19),
    ?assertEqual(19, rabbitmq_client_retry_attempt()).
	
non_existent_var_test_case() ->
    ?assertEqual(undefined, get_config_value_env(bogus)).


get_config_value_env_defaultValue_test_case()->
    application:set_env(?TESTAPPENV, test_var_with_value, ?DEFAULTVAL),
    ?assertMatch(?DEFAULTVAL,get_config_value_env(test_var_with_value, ?DEFAULTVAL) ).

get_config_value_env_noValue_test_case()->
    application:set_env(?TESTAPPENV,test_var_no_value, ?DEFAULTVAL),
    ?assertMatch(?DEFAULTVAL, get_config_value_env(test_var_no_value, ?DEFAULTVAL)).


get_config_value_env_undefinedVal_test_case() ->

    ?assertMatch(undefined, get_config_value_env(config_var_missing)).

get_config_required_value_ok_test_cases() ->
    application:set_env(?TESTAPPENV,required_key, required_val),
    Ret1 = get_config_value_env(required_key, required), 
    ?assertEqual(required_val, Ret1).

get_config_required_value_fail_test_cases()->

    application:unset_env(?TESTAPPENV,required_key_nok),
    Ret = 
	try 
	     get_config_value_env(required_key_nok, required)
        catch
	     exit:Reason -> {'EXIT', Reason};
	     R -> R
        end,
    ?assertMatch({'EXIT',{error, {required_key_nok, not_found}}}, Ret).

setup() ->   
    load_config().

cleanup(_Pid) ->
    unload_config().
    
-endif.
