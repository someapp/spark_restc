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
	    load_config_db/2,
	 	version/0,
	 	spark_api_endpoint/1, 
	 	spark_app_id/1,
	 	spark_client_secret/1,
	 	spark_oauth_access_token/0, 
	 	spark_communityid_brandid_map/0,
	 	auth_profile_miniProfile/0, 
	 	profile_memberstatus/0, 
	 	send_im_mail_message/0,
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

-include_lib("spark_restc_schema.hrl").

-define(DEFAULT_RESTCONN_TIMEOUT,5).
-define(DEFAULT_RESTCONN_RETRY,3).
-define(SERVER, ?MODULE).

-record(state,{
    use_mnesia = false,
	filename = [] :: file:name(),
	config_list = [] ::list()

}).

%% ===================================================================
%% Public API
%% ===================================================================

load_config(Filename)->
  % yamerl_constr:file(Filename).
  {ok, ConfList} = file:consult(Filename),
  {ok, ConfList}.

load_config_db(Environment, Filename)->
  {ok, List} = load_config(Filename),
  {ok, EnvConf} = environment(List, Environment),
  populate(EnvConf),
  {ok, 
  {ok, } = 
  {ok, } =
  {ok, 
  	{profile_memberstatus} = config_db_basic_populate(Table, Key, Val),
  List = [profile_memberstatus],
  populate(spark_restc_conf, List), 
  {ok, IdMap} = community_brand_idMap(List),
  populate

populate(Table, List) ->
  [List0 || {K,V} <- List],
  lists:map(
  	fun({Key,Val}) -> 
        {ok, {Key, updated}} = 
             config_db_basic_populate(Table, Key, Val)
 	end, List0).

populate_environment_conf(List)->
  populate(environment_conf, List).
 
populate_restc_conf(List) ->
  populate(spark_restc_conf, List).

populate_id_map(List) ->
  populate_table(id_map, List).
  
populate_table(Name, IdMap) when is_atom(Name)->
  error_logger:info_msg("Store idMap ~p into ets table", [IdMap]),
  Tab = ets:new(Name, [set, named_table]),
  lists:map(fun(L)-> true = ets:insert(Tab, L) end, IdMap);
populate_table(_, _)->
  {error, badarg}.   
    
config_version(List)->
  get_key_val(List, version, undefined).

environment(Environment) when is_atom(Environment)->
  gen_server:call(?SERVER, environment).

environment([], _) -> [];
environment(List, Environment)-> when is_atom(Environment)->
  get_key_val(List, Environment, []);
environment(_,_) -> {error, badarg}.

system_app_id(Environment)->
  gen_server:call(?SERVER, {system_app_id, Environment}).
 
system_app_id(List, Environment) ->
  {ok, SectionList} = environment(List, Environment),
  get_key_val(SectionList, app_id, "1017").

system_brand_id(Environment)->
  gen_server:call(?SERVER, {system_brand_id, Environment}).

system_brand_id(List, Environment) ->
  {ok, SectionList} = environment(List, Environment),
  get_key_val(SectionList, brand_id, "1003").

system_member_id(Environment)->
  gen_server:call(?SERVER, {system_member_id, Environment}).

system_member_id(List, Environment) ->
  {ok, SectionList} = environment(List, Environment),
  get_key_val(SectionList, member_id, "27029711"). 
  
system_client_secret(Environment)->
  gen_server:call(?SERVER, {system_client_secret, Environment}).  
  
system_client_secret(List, Environment)->
  get_key_val(SectionList, 
      client_secret, "93XDnCIn30rNYcHVgOJ77kzZzjkCmhrUm3NJ1bf5gNA=").

create_oauth_accesstoken(Environment)->
  gen_server:call(?SERVER, {create_oauth_accesstoken, Environment}).

create_oauth_accesstoken(List, _Environment) ->
  get_key_val(List, create_oauth_accesstoken, 
  	"{base_url}/brandId/{brandId}/oauth2/accesstoken/application/{applicationId}").
  	
profile_miniProfile(Environment)->
  gen_server:call(?SERVER, {profile_miniProfile, Environment}).  	 	
profile_miniProfile(List, _Environment) ->	
  get_key_val(List, profile_miniProfile,
  	"{base_url}/brandId/{brandId}/profile/attributeset/miniProfile/{targetMemberId}").  	

profile_memberstatus(Environment)->
  gen_server:call(?SERVER, {profile_memberstatus, Environment}).  

profile_memberstatus(List, _Environment) ->
  get_key_val(List, profile_memberstatus,
    "{base_url}/brandId/{brandId}/application/{applicationId}/member/{memberId}/status").

send_im_mail_message(Environment)->
  gen_server:call(?SERVER, {send_im_mail_message, Environment}).  

send_im_mail_message(List, _Environment)->
  get_key_val(List, send_im_mail_message,
    "{base_url}/brandId/{brandId}/application/{applicationId}/member/{memberId}/status"). 
 
community_brand_idMap()->	
  gen_server:call(?SERVER, community_brand_idMap). 
 
community_brand_idMap(List)->	
  get_key_val(List, community2brandId, []). 

  
start_link(Args)-> 
  error_logger:info_msg(info,"Start linking ~p with Args ~p",[?MODULE, Args]),
  gen_server:start_link(Args).

init(Args)->
  [Conf_path, Conf_file, Use_mnesia_conf_store] = Args,
  Filename = lists:concat([Path,"/", File]),
  true = ec_file:exists(Filename),
  ConfList = load_config(Filename),
  create_config_in_mnesia(Use_mnesia_conf_store),  
  {ok, #state{
  		filename = Filename,
  		use_mnesia = Use_mnesia_conf_store,
  		config_list = ConfList
  }}.

handle_call(environment, _From, State)->
  

  {ok, Reply, State}.


handle_call({system_app_id, Environment}, _From, State)->
  

  {ok, Reply, State}.

handle_call({system_brand_id, Environment}, _From, State)->
  

  {ok, Reply, State}.

handle_call({system_member_id, Environment}, _From, State)->
  

  {ok, Reply, State}.


handle_call({system_client_secret, Environment}, _From, State)->
  

  {ok, Reply, State}.


handle_call({create_oauth_accesstoken, Environment}, _From, State)->
  

  {ok, Reply, State}.



handle_call({profile_memberstatus, Environment}, _From, State)->


  {ok, Reply, State}.

handle_call({send_im_mail_message, Environment}, _From, State)->
  

  {ok, Reply, State}.

handle_call(community_brand_idMap, _From, State)->
  
  
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

create_config_in_mnesia(false) -> ok;
create_config_in_mnesia(true) ->
  Start = now(),
  Ret = case mnesia:create_schema([node()]) of
  	ok -> ok = app_util:start_app(mnesia),
      	  error_logger:info_msg("Create mod_spark_rabbit_config table", []),

  		 {atomic, ok} = mnesia:create_table(environment_conf,
  							[{ram_copies, [node()]},
  							{type, set},
  							{attributes, 
  								record_info(fields, 
  								environment_conf_schema)}
  							]);

  		 {atomic, ok} = mnesia:create_table(spark_restc_conf,
  							[{ram_copies, [node()]},
  							{type, set},
  							{attributes, 
  								record_info(fields, 
  								spark_restc_conf_schema)}
  							]);
	
  	{error,{S, {already_exists, S}}} -> 
        error_logger:warn_msg("Failure to create_schema: ~p", 
        	  [{S, {already_exists, S}}]),
        %ok = should_delete_schema(Schema),
        ok = app_util:start_app(mnesia);
    Else ->
        error_logger:info_msg("Failure to create_schema: ~p", [Else]),
        ok = app_util:start_app(mnesia)
  end,
  End = now(),
  error_logger:info_msg("Create config table ~p Start ~p End ~p", [?SERVER, Start, End]),
  Ret.  

now()->
  Now = app_util:os_now(),
  ec_date:nparse(format("Y-m-d\\TH:i:s.f", Now)).

config_db_basic_populate(Table, Key, Val)
		 when is_atom(Table) ,
			  is_atom(Key) ->
  Fun = fun(Key, Val) ->
  	 		mnesia:dirty_write({Table, Key, Val}),
  		end,
  {atomic, ok} = mnesia:transaction(Fun),
  {ok, {Key, updated}}.
    
get_key_val([],_)-> {error, empty_config};
get_key_val(List, Key, Default)
  proplists:get_value(Key, List, Default);
get_key_val(Table, onPredicate, Default) ->
  Fun = 
  fun()-> 
  	  qlc:eval(X || X <- mnesia:table(Table), onPredicate)  		
  end,
  case (mnesia:transaction(Fun)) of
  		{atomic, []} -> Default;
  		{atomic, Val} -> Val;
  		_ -> Default
  end.
  
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
