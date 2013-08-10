-module(api_mail_handler).
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
	send_im_mail_message = <<"">>,
	id_map = []
}).


start(Args)->
  case lists:member(?MODULE, gen_event:which_handlers(?EVENT_HANDLER)) of
  	true -> 
  		{?EVENT_HANDLER, already_started};
    false ->
        gen_event:add_handler(?EVENT_HANDLER, ?MODULE, Args)
  end.

stop()->
   gen_event:delete_handler(?EVENT_HANDLER, ?MODULE, stop ). 
   
   
init(Args)->
  [Conf_path, Conf_file, Environment, UseMnesia] = Args, 
  BaseUrl = 
  	spark_restc_config_server:spark_api_endpoint(Environment),
  SendIMMail_Url = spark_restc_config_server:send_im_mail_message(),
  SendIMMail = concate_url(BaseUrl, SendIMMail_Url),    
  
  {ok, #state{send_im_mail_message  = SendIMMail}}.   
   

concate_url(BaseUrl, ResourceUrl)->
  <<BaseUrl/binary, <<"/">>/binary, ResourceUrl/binary >>.


handle_event({send_im_mail_message, Params, Payload}, State) ->
   
  error_logger:info_msg("[~p] Request ~p ~p",
  		[?EVENT_HANDLER, send_im_mail_message, Params]),
  {ok, State};
  
handle_event({send_im_mail_message, Params, Payload, hibernate}, State) ->
   
  error_logger:info_msg("[~p] Request ~p ~p",
  		[?EVENT_HANDLER, send_im_mail_message, Params]),
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
