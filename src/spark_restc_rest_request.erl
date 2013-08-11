%%%----------------------------------------------------------------------
%%%
%%% @author : Edward Tsang <etsang@spark.net>
%%% @doc Client wrapper of Rest Call to Spark RestApi
%%%
%%% Created : 20 Mar 2013
%%%---------------------------------------------------------------------
%%%
%%% Copyright (c)
%%%----------------------------------------------------------------------
%%% @end

%% @doc Wrapper client to rest call to internal service api
%% @end
-module(spark_restc_rest_request).
-author('etsang@spark.net').

-export([
	restcall_get_access_token/7,
	restcall_is_user_exists/5,
	restcall_authenticate_user/4,
	restcall_can_member_reply/5,
 	restcall_send_im_mail_message/4

]).

-define(APP_ENV,ejabberd_auth_spark).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").
-endif.


restcall_get_access_token(UserName, Email, Password, 
			  ResourceFullUrl, AppId, IdMap, ClientSecret) ->
  [_, BrandId] = get_login_data(UserName, IdMap),

  Url = get_create_access_token_url(ResourceFullUrl,AppId,BrandId, Email, Password, ClientSecret),
  Ret=case restc:request(post, json, Url, [200], [],[""]) of
       {ok, 200, _, Body} ->
                 spark_restc_parse_response:check_access_token(Body); 
       {error, Status, _H, _B} -> 
                 {error, {Status, _H, _B}}
  end,
  R = case Ret of
       {ok, Token} -> Token;
       {error, Reason} -> {error, Reason}
      end,
  R. 
restcall_authenticate_user(UserName, Password, ResourceFullUrl, IdMap) ->
  [TargetMemberId, BrandId] = get_login_data(UserName, IdMap),
  Url = get_member_miniprofile_url(BrandId, TargetMemberId, Password,
			   ResourceFullUrl),

  Ret = case restc:request(get, json, Url, [200], []) of
     {ok, 200, _, Body} -> spark_restc_parse_response:check_auth_response(Body);
     {error, Status, _H, _B} -> {error, {Status, _H, _B}}
  end,
  Ret.  

restcall_can_member_reply(UserName, SenderMemberId,
			  Password, ResourceFullUrl,
			  IdMap) ->
  error_logger:info_msg("[~p] Can Member Reply User: ~p, Sender: ~p",[?MODULE, UserName, SenderMemberId]),
  [TargetMemberId, BrandId] =
			get_login_data(UserName, IdMap),

  error_logger:info_msg("[~p] Can Member Reply User: ~p, BrandId: ~p",[?MODULE, TargetMemberId, BrandId]),


  [TargetSenderMemberId, SenderBrandId] = 
			get_login_data(SenderMemberId, IdMap),


  error_logger:info_msg("[~p] Can Member Reply Sender: ~p, BrandId: ~p",[?MODULE, TargetSenderMemberId, SenderBrandId]),
  

  Url = get_can_member_reply_url(BrandId,
				 TargetSenderMemberId,
				 Password,
				 ResourceFullUrl),


  error_logger:info_msg("[~p] Can Member Reply Sender Url: ~p",[?MODULE, Url]),


  Ret = case restc:request(get, json, Url, [200], []) of
     {ok, 200, _, Body} -> spark_restc_parse_response:check_can_member_reply(Body);
     {error, Status, _H, _B} -> {error, {Status, _H, _B}}
  end,
  Ret. 


restcall_is_user_exists(UserName, 
                        ResourceFullUrl,
                        IdMap,
  		        AppId, 
			ClientSecret)->
  [MemberId, BrandId] = get_login_data(UserName, IdMap),

  Url = get_member_subscription_status_url(BrandId, MemberId,
					 AppId, 
                                         ResourceFullUrl,
                                         ClientSecret),

  Ret =case restc:request(get, json, Url, [200], []) of
     {ok, _Status, _H, Body} -> spark_restc_parse_response:check_isUser_response(Body);
     {error, Status, _H, _B} -> {error, {Status, _H, _B}}
  end, 
  Ret.

restcall_send_im_mail_message(BrandId, OAuthToken, ResourceFullUrl,
			  Message) ->
  Url = get_send_mail_message_url(BrandId, OAuthToken, ResourceFullUrl), 
  Ret = case restc:request(post, json, Url, [200], [Message]) of
     	{ok, _Status, _H, Body} -> 	
		spark_restc_parse_response:check_send_im_message_reply(Body);
     	{error, Status, _H, _B} -> {error, {Status, _H, _B}}
  	end, 
  Ret.


%%@hidden 
get_memberId_communityId([])-> [];
get_memberId_communityId(UserName) ->
  case re:split(UserName,"-") of 
      [MemberId, CommunityId] -> [MemberId, CommunityId];
              {error, Reason} -> {error, Reason};
              Else -> {error, Else}
  end.

get_login_data(UserName, IdMap) ->
  [MemberId, CommunityId] = get_memberId_communityId(UserName), 
  BrandIdStr = find_value(CommunityId, IdMap),
  MemberIdStr = erlang:binary_to_list(MemberId),
  [MemberIdStr, BrandIdStr]. 


%% URL %%%%
get_send_mail_message_url(BrandId, OAuthToken, ResourceFullUrl)->
   ReplaceOpt = [global, {return, list}],	
   FullUrl = re:replace(ResourceFullUrl, "{brandId}", 
		BrandId, ReplaceOpt),
         
   restc:construct_url(
		FullUrl,
		[{"access_token", OAuthToken}]). 

get_can_member_reply_url(BrandId,
			 SenderMemberId,
			 OAuthToken,
			 ResourceFullUrl) ->
   ReplaceOpt = [global, {return, list}],	
   FullUrl = re:replace(ResourceFullUrl, "{brandId}",
		 BrandId, ReplaceOpt),
   FullUrl1 = re:replace(FullUrl, "{memberId}", 
		 SenderMemberId, ReplaceOpt),
  
   restc:construct_url(
		FullUrl1,
		[{"access_token", OAuthToken}]). 

get_create_access_token_url(ResourceFullUrl,
 			    AppId,
			    BrandId,
			    Email,
			    Password, ClientSecret)->

   ReplaceOpt = [global, {return, list}],	
   FullUrl = re:replace(ResourceFullUrl, "{brandId}", BrandId, ReplaceOpt),
   FullUrl1 = re:replace(FullUrl, "{applicationId}", AppId, ReplaceOpt),
  
   restc:construct_url(
		FullUrl1,
		[{"client_secret", ClientSecret},
		 {"email", Email},
		 {"password", Password}]).

get_member_miniprofile_url(BrandId, TargetMemberId, Password,
			   ResourceFullUrl)->

   ReplaceOpt = [global,{return,list}],	
   FullUrl = re:replace(ResourceFullUrl, "{brandId}", BrandId, ReplaceOpt),
   FullUrl1 = re:replace(FullUrl, "{targetMemberId}", TargetMemberId, ReplaceOpt),
   restc:construct_url(FullUrl1,[{"access_token", Password}]).

get_member_subscription_status_url(BrandId, MemberId,
			           AppId, 
                                   ResourceFullUrl,
                                   ClientSecret) ->

   ReplaceOpt = [global,{return,list}],	
   FullUrl = re:replace(ResourceFullUrl, "{brandId}", BrandId, ReplaceOpt),
   FullUrl1 = re:replace(FullUrl, "{applicationId}", AppId, ReplaceOpt),
   FullUrl2 = re:replace(FullUrl1, "{memberId}", MemberId, ReplaceOpt),
   restc:construct_url(
		FullUrl2,
		[{"client_secret", ClientSecret}]).


find_value(Key, List) ->
  Key1 = erlang:binary_to_list(Key),
  case lists:keyfind(Key1, 2, List) of
        {_Type, _Key, Result} -> Result;
        false -> {error, not_found};
        {error, Reason} -> {error, Reason}
  end.

%%%%%% EUNIT %%%%%%%%%%%%%%%%%%

-ifdef(TEST).
-define(DEFAULTVAL, "DefaultTestVal").
-define(TESTAPPENV, ?APP_ENV).
spark_restc_restrequest_test_() ->
    { setup,
      fun setup/0,
      fun cleanup/1,
      [
        {"Should get brandId",fun get_brandId_test_case/0},
	{"Should get an non empty accesstoken",fun get_accessToken_ok_test_case/0},
%	fun get_accessToken_missing_parameter_test_case/0,
%	fun get_accessToken_badMember_test_case/0,
%	fun get_accessToken_badpwd_test_case/0,
	{"Should return true for a pay member",fun check_user_membership_ok_test_case/0},
	{"Should return false for a nonpay member",fun check_user_membership_nok_test_case/0},
%	fun check_user_membership_badMember_test_case/0
%	fun check_user_membership_badreq_test_case/0
	{"Should return false for invalid member",fun check_user_status_nok_test_case/0},
	{"Should return true for any member",fun check_user_status_ok_test_case/0}
%	fun check_user_status_badreq_test_case/0
      ]
    }.

get_brandId_test_case()->
  PayId = "1116692650",
  BId = "1003",
  {ok, Jid} = application:get_env(?TESTAPPENV, test_pay_jid),
  [MemberId, BrandId] = get_login_data(Jid, IdMap),
  ?assertEqual(PayId, MemberId),
  ?assertEqual(BId, BrandId).


get_accessToken_ok_test_case()->
   {ok, Jid} = application:get_env(?TESTAPPENV, test_pay_jid),
   {ok, Email} = application:get_env(?TESTAPPENV, test_payuser_email),
   {ok, Password} = application:get_env(?TESTAPPENV, test_payuser_password),
   Token = get_access_token(Jid, Email, Password),
   ?assert(string:len(Token) > 0).

get_accessToken_missing_parameter_test_case()->
   {ok, Jid} = application:get_env(?TESTAPPENV, test_pay_jid),
   {ok, Email} = application:get_env(?TESTAPPENV, test_payuser_email),
   Ret = get_access_token(Jid, Email,[]),
   ?assertMatch({error, _}, Ret).

get_accessToken_badMember_test_case()->
   {ok, Jid} = application:get_env(?TESTAPPENV, test_pay_jid),
   {ok, Email} = application:get_env(?TESTAPPENV, test_payuser_email),
   Ret = get_access_token(Jid, Email,"123"),
   ?assertMatch({error, _}, Ret).

get_accessToken_badpwd_test_case()->
   {ok, Email} = application:get_env(?TESTAPPENV, test_payuser_email),
   Ret = get_access_token("27029171-3", Email,"123"),
   ?assertMatch({error, _}, Ret).

check_user_membership_ok_test_case()->
   {ok, Jid} = application:get_env(?TESTAPPENV, test_pay_jid),
   {ok, Email} = application:get_env(?TESTAPPENV, test_payuser_email),
   {ok, Password} = application:get_env(?TESTAPPENV, test_paysuser_password),
   Token = get_access_token(Jid, Email, Password),
   %?assertNot({error, _R} =:= Token ),
   ?debugFmt("Received token ~p~n",[Token]),
   Ret = restcall_authenticate_user(Jid, Token),
   ?assertEqual(true,Ret).

check_user_membership_nok_test_case()->
   {ok, Jid} = application:get_env(?TESTAPPENV, test_nonpay_jid),
   {ok, Email} = application:get_env(?TESTAPPENV, test_nonpayuser_email),
   {ok, Password} = application:get_env(?TESTAPPENV, test_nonpayuser_password),
   Token = get_access_token(Jid, Email, Password),
   Ret = restcall_authenticate_user(Jid, Token),
   ?assertEqual({ok,non_subscriber}, Ret).

check_user_membership_badMember_test_case()->
   {ok, Jid} = application:get_env(?TESTAPPENV, test_nonpay_jid),
   Token = get_access_token(Jid, "test@tet.net", "2134"),
   Ret = restcall_authenticate_user(Jid, Token),
   ?assertMatch({error,_}, Ret).

check_user_membership_badreq_test_case()->
   {ok, Jid} = application:get_env(?TESTAPPENV, test_nonpay_jid),
   Token = get_access_token(Jid, "", ""),
   Ret = restcall_authenticate_user(Jid, Token),
   ?assertMatch({error,_}, Ret).

check_user_status_nok_test_case()->
   {ok, Jid} = application:get_env(?TESTAPPENV, test_nonpay_jid),
   Ret = restcall_is_user_exists(Jid),
   ?assertEqual({ok,non_subscriber}, Ret).

check_user_status_ok_test_case()->
   {ok, Jid} = application:get_env(?TESTAPPENV, test_pay_jid),
   Ret = restcall_is_user_exists(Jid),
   ?assertMatch({ok,subscriber}, Ret).

check_user_status_badreq_test_case()->
   Ret = restcall_is_user_exists("27029171-3"),
   ?assertEqual(false, Ret).

ensure_dep_app_started()->
   app_helper:ensure_app_started(kernel),
   app_helper:ensure_app_started(stdlib),
   app_helper:ensure_app_started(inets),
   app_helper:ensure_app_started(public_key),
   app_helper:ensure_app_started(ssl),
   app_helper:ensure_app_started(crypto).

ensure_dep_app_stopped()->
   app_helper:ensure_app_stopped(kernel),
   app_helper:ensure_app_stopped(stdlib),
   app_helper:ensure_app_stopped(inets),
   app_helper:ensure_app_stopped(public_key),
   app_helper:ensure_app_stopped(ssl),
   app_helper:ensure_app_stopped(crypto).

setup() ->   
    application:load(?TESTAPPENV),
    application:set_env(?TESTAPPENV, test_pay_jid,"1116692650-3"),
%    application:set_env(?TESTAPPENV, test_pay_jid,"27029711-3"),


    application:set_env(?TESTAPPENV, test_nonpay_jid,"116361935-3"),	
    application:set_env(?TESTAPPENV, test_payuser_email, "etsang@spark.net"),

%    application:set_env(?TESTAPPENV, test_payuser_email, "wlee@spark.net"),
%    application:set_env(?TESTAPPENV, spark_client_secret, "bad_client_screte"),

     application:set_env(?TESTAPPENV,
			 spark_client_secret,
	  "nZGVVfj8dfaBPKsx_dmcRXQml8o5N-iivf5lBkrAmLQ1"),

    application:set_env(?TESTAPPENV, test_nonpayuser_email, "wlee2@spark.net"),
    application:set_env(?TESTAPPENV, test_nonpayuser_password, "1234"),
    application:set_env(?TESTAPPENV, test_pay_user_password, "1111"),
    application:set_env(?TESTAPPENV, community2brandId,
              [{spark,"1","1001"},
               {jdate,"3","1003"},
               {cupid,"10","1015"},
               {bbw,"23","90410"},
               {blacksingle,"24","90510"}]
    ),

    ensure_dep_app_started().

  
cleanup(_Pid) ->
  application:unset_env(?TESTAPPENV, test_pay_jid),
  application:unset_env(?TESTAPPENV, test_nonpay_jid),
  application:unset_env(?TESTAPPENV, test_payuser_email),
  application:unset_env(?TESTAPPENV, test_nonpayuser_email),
  application:unset_env(?TESTAPPENV, test_payuser_password),
  application:unset_env(?TESTAPPENV, test_nonpayuser_password),
  app_helper:ensure_app_stopped(?TESTAPPENV).

-endif.
