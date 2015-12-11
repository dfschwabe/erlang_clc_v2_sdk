-module( clc_v2_alert_policies_tests ).
-include( "test_fixture.hrl" ).

%https://www.centurylinkcloud.com/api-docs/v2/#alert-policies-get-alert-policies
setup() ->
  ?meck( clc_v2_http_client, [non_strict]).

get_calls_http_client_get() ->
  ?stub( clc_v2_http_client, get, 2, { ok, data1 }),

  ?assertEqual( data1, clc_v2_alert_policies:get( auth_ref1 ) ),

  ?called( clc_v2_http_client, get, [auth_ref1, ["alertPolicies", account_alias]] ).

get_id_calls_http_client_get() ->
  ?stub( clc_v2_http_client, get, 2, { ok, data1 }),

  ?assertEqual( data1, clc_v2_alert_policies:get( auth_ref1, <<"id1">> ) ),

  ?called( clc_v2_http_client, get, [auth_ref1, ["alertPolicies", account_alias, "id1"]] ).

create_calls_http_client_post_with_expected_route() ->
  ?stub( clc_v2_http_client, post, 3, { ok, #{ <<"id">> => <<"id1">> } }),

  clc_v2_alert_policies:create( auth_ref1, #{ name => <<>>, email_recipients => [], triggers => []} ),

  ?called( clc_v2_http_client, post, [auth_ref1, ["alertPolicies", account_alias], ?any] ).

create_calls_http_client_post_with_expected_body() ->
  ?stub( clc_v2_http_client, post, 3, { ok, #{ <<"id">> => <<"id1">> } }),

  clc_v2_alert_policies:create( auth_ref1,
                                #{ name => <<"p1">>,
                                   email_recipients => [<<"r1@host.com">>, <<"r2@host.com">>],
                                   triggers =>
                                     [ #{ metric => metric1, duration => <<"00:00:01">>, threshold => 0.01 },
                                       #{ metric => metric2, duration => <<"01:01:01">>, threshold => 99.99 } ]
                                }),

  ?assertEqual( #{ name => <<"p1">>,
                   actions =>
                   [#{ action => email,
                       settings => #{ recipients => [<<"r1@host.com">>, <<"r2@host.com">>] } }],
                  triggers =>
                    [#{ metric => metric1, duration => <<"00:00:01">>, threshold => 0.01 },
                     #{ metric => metric2, duration => <<"01:01:01">>, threshold => 99.99 } ]
                },
               ?capture(clc_v2_http_client, post, 3, 3)).

create_returns_expected_value() ->
  Expected = <<"id1">>,
  ?stub( clc_v2_http_client, post, 3, { ok, #{ <<"id">> => Expected } }),

  Actual = clc_v2_alert_policies:create( auth_ref1, #{ name => <<>>, email_recipients => [], triggers => []} ),

  ?assertEqual(Expected, Actual).
