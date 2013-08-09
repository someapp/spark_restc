-ifndef(SPARK_RESTC_SCHEMA_HRL).
-define(SPARK_RESTC_SCHEMA_HRL, true).
-type config_entry() :: {atom(), term()}.

-record(spark_restc_conf_schema,{
  environment = [] :: [config_entry()],	
  create_oauth_accesstoken = <<"">> :: bitstring(),
  auth_profile_miniProfile = <<"">> :: bitstring(),
  profile_memberstatus = <<"">> :: bitstring(),
  send_im_mail_message = <<"">> ::bitstring(),
  community2brandId = [] :: [config_entry()]
}).

-endif.

