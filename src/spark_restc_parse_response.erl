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
-module(spark_restc_parse_response).
-author('etsang@spark.net').

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").
-endif.

-export([check_auth_response/1,
	 check_isUser_response/1, 
         check_access_token/1,
	 check_can_member_reply/1,
         check_send_im_message_reply/1
]).
check_auth_response([])-> {error, empty_body};
check_auth_response(Body) ->
    V1 = case proplists:get_value(<<"data">>,Body) of 
		undefined -> {error, missing_body};
	        List -> List
	end,

    case  proplists:get_value(<<"isPayingMember">>,V1) of
    	     true-> {ok, paymember};
             <<"true">> -> {ok, paymember};
	     {error, Reason} -> {error, Reason};
             _ -> {ok, non_paymember}
    end. 

check_isUser_response([])-> {error, empty_body};
check_isUser_response(Body)->
    V1 = case proplists:get_value(<<"data">>,Body) of 
		undefined -> {error, missing_body};
		List -> List
	end,

    case proplists:get_value
		  (<<"subscriptionStatus">>, V1) of
	  <<"Subscriber">>->
		  {ok, subscriber};
	  <<"Member">>->
		  {ok, non_subscriber};
	  <<"InvalidMember">>-> {nok, invalidMember};
%	  {error, Reason} -> {error, Reason};
%	  _ -> {nok, invalidMember}

	  _Else ->
		  {ok, non_subscriber}
    end.

check_access_token([]) -> {error, missing_body};
check_access_token(Body) ->
    V1 = case proplists:get_value(<<"data">>,Body) of 
		undefined -> {error, missing_body};
		List -> List
	end,
    R =case proplists:get_value
		  (<<"AccessToken">>, V1) of
	  {error, Reason} -> {error, Reason};
	  [] -> {error, no_access_token_empty_body};
	  undefined -> {error, no_access_token_undefined};
	  Token ->  {ok, erlang:binary_to_list(Token)}
    end, 
%    eunit:debugVal(R)
    R.

check_can_member_reply([]) -> {error, missing_body};
check_can_member_reply(Body) ->
    error_logger:info_msg("[~p] Can Member Reply Body: ~p",[?MODULE, Body]),
    case proplists:get_value(<<"status">>,Body) of 
		undefined -> {error, missing_body};
		<<"ok">> -> {ok, member_can_reply};
		<<"OK">> -> {ok, member_can_reply};
		<<"Ok">> -> {ok, member_can_reply};
		_ -> {nok, member_cannot_reply}
    end.

check_send_im_message_reply([]) -> {error, missing_body};
check_send_im_message_reply(Body) ->
    error_logger:info_msg("[~p] Can send Im Mail Body: ~p",[?MODULE, Body]),
    case proplists:get_value(<<"status">>,Body) of 
		undefined -> {error, missing_body};
		<<"ok">> -> {ok, im_mail_sent};
		<<"OK">> -> {ok, im_mail_sent};
		<<"Ok">> -> {ok, im_mail_sent};
		_ -> {nok, member_cannot_reply}
    end.
      
%%%%%% EUNIT %%%%%%%%%%%%%%%%%%

-ifdef(TEST).



-endif.
