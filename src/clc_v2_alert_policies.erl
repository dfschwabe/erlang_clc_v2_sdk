-module( clc_v2_alert_policies ).

-export([
  get/1,
  get/2,
  create/2
  ]).

-spec get( AuthRef::clc_v2_auth:auth_ref() ) -> map().
get( AuthRef ) ->
  {ok, Policies}  = clc_v2_http_client:get( AuthRef, ["alertPolicies", account_alias ] ),
  Policies.

-spec get( AuthRef::clc_v2_auth:auth_ref(), Id::binary() ) -> map().
get( AuthRef, Id ) ->
  {ok, Policy } = clc_v2_http_client:get( AuthRef, ["alertPolicies", account_alias, binary_to_list(Id) ] ),
  Policy.

-spec create( AuthRef::clc_v2_auth:auth_ref(), Spec::map() ) -> binary().
create( AuthRef, Spec ) ->
  Spec1 = to_api_spec(Spec),
  {ok, #{ <<"id">> := Id } } = clc_v2_http_client:post( AuthRef, ["alertPolicies", account_alias ], Spec1 ),
  Id.

to_api_spec( #{ name := Name, email_recipients := Recipients, triggers := Triggers } ) ->
  #{ name => Name,
     actions => [ #{ action => email,
                     settings => #{ recipients => Recipients } } ],
     triggers => Triggers
   }.
