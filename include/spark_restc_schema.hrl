-ifndef(SPARK_RESTC_SCHEMA_HRL).
-define(SPARK_RESTC_SCHEMA_HRL, true).
-type config_entry() :: {atom(), term()}.

-record(environment_conf_schema, {
  system_app_id = <<"">> ::bitstring(), 
  system_brand_id = <<"">> :: bitstring(),
  system_member_id = <<"">> ::bitstring(),
  system_client_secret = <<"">> :: bitstring()
}).

-record(spark_restc_conf_schema,{
  environment = undefined :: atom(),	
  create_oauth_accesstoken = <<"">> :: bitstring(),
  auth_profile_miniProfile = <<"">> :: bitstring(),
  profile_memberstatus = <<"">> :: bitstring(),
  send_im_mail_message = <<"">> ::bitstring(),
  community2brandId = undefined 
}).

-endif.

