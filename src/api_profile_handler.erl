-module(api_profile_handler).
-behaviour(gen_event).
-compile([parse_transform, lager_transform]).

-export([
		init/1,
		handle_event/2,
		handle_call/2,
		handle_info/2,
		code_change/3,
		terminate/2

]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").
-endif.
-include_lib("lager/include/lager.hrl").

-define(EVENT_HANDLER, ?MODULE).

-record(state, {
	mini_profile = <<"">>,
	id_map = []
}).

start(Args)->
  case lists:member(?MODULE, gen_event:which_handlers(?EVENT_HANDLER)) of
  	true -> 
  		{?EVENT_HANDLER, already_started};
    false ->
        gen_event:add_sup_handler(?EVENT_HANDLER, ?MODULE, Args)
  end.

stop()->
   gen_event:delete_handler(?EVENT_HANDLER, ?MODULE, stop ). 

init(Args)->
  [Conf_path, Conf_file, Environment, UseMnesia] = Args, 
  BaseUrl = 
  	spark_restc_config_server:spark_api_endpoint(Environment),
  MemberStatus_Url = spark_restc_config_server:profile_memberstatus(),
  MemberStatus = concate_url(BaseUrl, MemberStatus_Url), 
  
  MiniProfile_Url = spark_restc_config_server:auth_profile_miniProfile(),
  MiniProfile = concate_url(BaseUrl, MiniProfile_Url),  
  {ok, #state{mini_profile  = MiniProfile, member_status = MemberStatus}}.


concate_url(BaseUrl, ResourceUrl)->
  <<BaseUrl/binary, <<"/">>/binary, ResourceUrl/binary >>.

handle_event(heartbeat, State)->
 Heatbeat=ping,
 {ok, Heartbeat, State};


handle_event({get_mini_profile, Params, Payload}, State)->
  error_logger:info_msg("[~p] Request ~p ~p",
  		[?EVENT_HANDLER, get_mini_profile, Params]),
  		
  		
  		
  {ok, State};
   
handle_event({get_mini_profile, Params, Payload, hibernate}, State)->
  error_logger:info_msg("[~p] Request ~p ~p",
  		[?EVENT_HANDLER, get_mini_profile, Params]),
  {ok, State, hibernate}.

 
handle_call(Request, State)->
  error_logger:info_msg("[~p] Request ~p ~p",
  		[?EVENT_HANDLER, Request, unsupported]),
  {ok,{error, {Request, unsupported}}, State}.

handle_info(Message, State)->
  error_logger:info_msg("[~p] unknown message [~p]",
  			  [?EVENT_HANDLER, Message]),
  {noreply, State}.

terminate(stop, normal)->
  error_logger:info_msg("[~p] terminated [~p] with reason ~p ",
  			  [?EVENT_HANDLER,stop, normal]),
  {ok, normal};

terminate(stop, normalstopped)->
  error_logger:info_msg("[~p] terminated [~p] with reason ~p ",
  			  [?EVENT_HANDLER,stop, normalstopped]),
  {ok, normalstopped};
  
terminate(Message, Why)->
  error_logger:info_msg("[~p] terminated [~p] with reason ~p ",
  			  [?EVENT_HANDLER,stop, Why]),
  {ok, Why}.
  
code_change(_OldVsn, State, _Extra)->
  {ok, State}.


-ifdef(TEST).


-endif.
