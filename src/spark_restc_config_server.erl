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
	 	config_version/1,
	 	system_app_id/1,
	 	system_app_id/2,
	 	system_brand_id/1,
	 	system_brand_id/2,
	 	system_member_id/1,
	 	system_member_id/2,
	 	system_client_secret/1,
	 	system_client_secret/2,
	 	create_oauth_accesstoken/1, 
	 	create_oauth_accesstoken/2,
	 	community_brand_idMap/0,
	    community_brand_idMap/1,
	 	profile_miniProfile/1,
	 	profile_miniProfile/2, 
	 	profile_memberstatus/1, 
	 	profile_memberstatus/2, 
	 	send_im_mail_message/1,
	 	send_im_mail_message/2
       ]).

-export([start/1,
         init/1]).
         
-export([start_link/1]).
-export([handle_call/3,
		 handle_cast/2,
		 handle_info/2, 
		 terminate/2, 
		 code_change/3]).

-type url() :: string() | undefined.
-type accessToken() :: string() | undefined.
-type config_val() :: term().
-type reason() :: term().
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
    version,
    environment,
    base_url,
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
  populate(environment_conf, EnvConf),
  {ok, Urls} = endpoints(List),
  populate(spark_restc_conf, Urls), 
  {ok, IdMap} = community_brand_idMap(List),
  populate_id_map(IdMap).

populate(Table, List) ->
  List0 = [{K,V} || {K,V} <- List],
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

config_version()->
  gen_server:call(?SERVER, version).    
config_version(List)->
  get_key_val(List, version, undefined).

environment(Environment) when is_atom(Environment)->
  gen_server:call(?SERVER, environment).

environment([], _) -> [];
environment(List, Environment) when is_atom(Environment)->
  get_key_val(List, Environment, []);
environment(_,_) -> {error, badarg}.

endpoints(List) ->
  get_key_val(List,endpoints ,[]).

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
  get_key_val(List, 
      client_secret, "93XDnCIn30rNYcHVgOJ77kzZzjkCmhrUm3NJ1bf5gNA=").

create_oauth_accesstoken(Environment)->
  gen_server:call(?SERVER, {create_oauth_accesstoken, Environment}).

create_oauth_accesstoken(List, _Environment) ->
  get_key_val(List, create_oauth_accesstoken, 
  	"{base_url}/brandId/{brandId}/oauth2/accesstoken/application/{applicationId}").
  	
profile_miniProfile(Environment)->
  gen_server:call(?SERVER, {profile_miniProfile, Environment}).  	 	
profile_miniProfile(List, Environment) ->	
  get_key_val(List, profile_miniProfile,
  	"{base_url}/brandId/{brandId}/profile/attributeset/miniProfile/{targetMemberId}").  	

profile_memberstatus(Environment)->
  gen_server:call(?SERVER, {profile_memberstatus, Environment}).  

profile_memberstatus(List, Environment) ->
  get_key_val(List, profile_memberstatus,
    "{base_url}/brandId/{brandId}/application/{applicationId}/member/{memberId}/status").

send_im_mail_message(Environment)->
  gen_server:call(?SERVER, {send_im_mail_message, Environment}).  

send_im_mail_message(List, Environment)->
  get_key_val(List, send_im_mail_message,
    "{base_url}/brandId/{brandId}/application/{applicationId}/member/{memberId}/status"). 
 
community_brand_idMap()->	
  gen_server:call(?SERVER, community_brand_idMap). 
 
community_brand_idMap(List)->	
  get_key_val(List, community2brandId, []). 

start(Args) -> start_link(Args).
  
start_link(Args)-> 
  error_logger:info_msg(info,"Start linking ~p with Args ~p",[?MODULE, Args]),
  gen_server:start_link(Args).

init(Args)->
  [Conf_path, Conf_file, Use_mnesia_conf_store, Environment] = Args,
  Filename = lists:concat([Conf_path,"/", Conf_file]),
  true = ec_file:exists(Filename),
  ConfList = load_config(Filename),
  Vsn = get_key_val(ConfList, version, 0),
  Env = get_key_val(ConfList, Environment, undefined),
  
  create_config_in_mnesia(Use_mnesia_conf_store),  
  {ok, #state{
    	version = Vsn,
    	environment = Env,
    	filename = Filename,
  		use_mnesia = Use_mnesia_conf_store,
  		config_list = ConfList
  }}.

handle_call(environment, _From, State)->
  Qlc = fun() -> 
  		qlc:eval(qlc:q(
  			[Env || 
  				#environment_conf_schema{
  					environment = Env} 
  				<- mnesia:table(environment_conf)]))
  end, 
  Reply =  handle_message(Qlc),
  {ok, Reply, State};

handle_call({system_app_id, Environment}, _From, State)->
  Qlc = fun() -> 
  		qlc:eval(qlc:q(
  			[AppId || 
  				#environment_conf_schema{
  					environment = Env, 
  					system_app_id =AppId} 
  					<- mnesia:table(environment_conf),
  		Env =:= Environment]))
  end, 
  Reply =  handle_message(Qlc), 
  {ok, Reply, State};

handle_call({system_brand_id, Environment}, _From, State)->
  Qlc = fun() -> 
  		qlc:eval(qlc:q(
  			[BrandId || 
  				#environment_conf_schema{
  					environment = Env, 
  					system_brand_id = BrandId} 
  					<- mnesia:table(environment_conf),
  		Env =:= Environment]))
  end,  
    
  Reply =  handle_message(Qlc), 
  {ok, Reply, State};

handle_call({system_member_id, Environment}, _From, State)->
  Qlc = fun() -> 
  		qlc:eval(qlc:q(
  			[MemberId || 
  				#environment_conf_schema{
  					environment = Env, 
  					system_member_id = MemberId} 
  					<- mnesia:table(environment_conf),
  		Env =:= Environment]))
  end,  
    
  Reply =  handle_message(Qlc), 
  {ok, Reply, State};

handle_call({system_client_secret, Environment}, _From, State)->
  Qlc = fun() -> 
  		qlc:eval(qlc:q(
  			[Secret || 
  				#environment_conf_schema{
  					environment = Env, 
  					system_client_secret = Secret} 
  					<- mnesia:table(environment_conf),
  		Env =:= Environment]))
  end,  
    
  Reply =  handle_message(Qlc),  
  {ok, Reply, State};

handle_call({version, Environment},_From, State)->
  Qlc = fun() -> 
  		qlc:eval(qlc:q(
  			[Vsn || 
  				#spark_restc_conf_schema{
  					version = Vsn, 
  					environment = Env} 
  					<- mnesia:table(spark_restc_conf),
  		Env =:= Environment]))
  end,  
    
  Reply =  handle_message(Qlc),  
  {ok, Reply, State};
    
handle_call({profile_miniProfile, Environment}, _From, State)->
  Qlc = fun() -> 
  		qlc:eval(qlc:q(
  			[MiniProfile || 
  				#spark_restc_conf_schema{
  					auth_profile_miniProfile
  						 = MiniProfile
  					} 
  					<- mnesia:table(spark_restc_conf)]
  		))
  end,  
    
  Reply =  handle_message(Qlc), 
  {ok, Reply, State};

handle_call({create_oauth_accesstoken, Environment}, _From, State)->
  Qlc = fun() -> 
  		qlc:eval(qlc:q(
  			[AccessToken || 
  				#spark_restc_conf_schema{
  					create_oauth_accesstoken 
  						= AccessToken} 
  					<- mnesia:table(spark_restc_conf)]
  		))
  end,  
    
  Reply =  handle_message(Qlc),  
  {ok, Reply, State};

handle_call({profile_memberstatus, Environment}, _From, State)->
  Qlc = fun() -> 
  		qlc:eval(qlc:q(
  			[MemberStatus || 
  				#spark_restc_conf_schema{
  					profile_memberstatus 
  					   = MemberStatus} 
  					<- mnesia:table(spark_restc_conf)]
  		))
  end,  
    
  Reply =  handle_message(Qlc), 
  {ok, Reply, State};

handle_call({send_im_mail_message, Environment}, _From, State)->
  Qlc = fun() -> 
  		qlc:eval(qlc:q(
  			[ImMail || 
  				#spark_restc_conf_schema{
  					send_im_mail_message = ImMail 
  					} 
  					<- mnesia:table(spark_restc_conf)]
  		))
  end,  
    
  Reply =  handle_message(Qlc), 
  {ok, Reply, State};

handle_call(community_brand_idMap, _From, State)->
  Qlc = fun() -> 
  		qlc:eval(qlc:q(
  			[IdMap || 
  				#spark_restc_conf_schema{
  					community2brandId = IdMap 
  					} 
  					<- mnesia:table(spark_restc_conf)]
  		))
  end,  
    
  Reply =  handle_message(Qlc), 
  {ok, Reply, State}.

handle_message(Qlc) ->
  Default = undefined,
  Ret = get_key_val(Qlc, Default),
  error_logger:info_msg("[~p], Value ~p",
  						[?SERVER,  Ret]),
  Ret.

handle_cast(Unsupported, State)->
  error_logger:info_msg("[~p] Unsupported message: ~p ",
  		[?SERVER, Unsupported]),
  {noreply, State}.

handle_info(Unsupported, State)->
  error_logger:info_msg("[~p] Unsupported message: ~p ",
  		[?SERVER, Unsupported]),
  {noreply, State}.

terminate(Reason, _State)->
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
  							]),

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
  End = get_now(),
  error_logger:info_msg("Create config table ~p Start ~p End ~p", [?SERVER, Start, End]),
  Ret.  

get_now()->
  Now = app_util:os_now(),
  ec_date:nparse(ec_date:format("Y-m-d\\TH:i:s.f", Now)).

config_db_basic_populate(Table, Key, Val)
		 when is_atom(Table),
			  is_atom(Key) ->
  Fun = fun(Key, Val) ->
  	 		mnesia:dirty_write({Table, Key, Val})
  		end,
  {atomic, ok} = mnesia:transaction(Fun),
  {ok, {Key, updated}}.
    
get_key_val([],_)-> {error, empty_config};
get_key_val(QLC, Default) ->
  case (mnesia:activity(QLC)) of
  		{atomic, <<"">>} -> Default;
  		{atomic, Val} -> Val;
  		_ -> Default
  end.
get_key_val(List, Key, Default) when is_list(List) ->
  proplists:get_value(Key, List, Default).
  
%%===================================================================
%% EUnit tests
%% ===================================================================
-ifdef(TEST).
-define(DEFAULTVAL, "DefaultTestVal").
-define(TESTAPPENV, ?APP_ENV).

    
-endif.
