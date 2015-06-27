-module( clc_v2_tests ).
-include( "test_fixture.hrl" ).

clc_v2_datacenters_delgates_to_datacenters_get() ->
  ?meck(clc_v2_datacenters, [non_strict]),
  ?stub(clc_v2_datacenters, get, 0, datacenters1),

  ?assertEqual(datacenters1, clc_v2:datacenters()),

  ?called(clc_v2_datacenters, get, []).

login_creates_new_auth_worker_under_auth_supervisor() ->
  ?meck( clc_v2_auth_sup, [non_strict] ),
  ?stub( clc_v2_auth_sup, create_worker, 2, {ok, authref1}),

  ?assertEqual( {ok, authref1 }, clc_v2:login( username1, password1 ) ),

  ?called( clc_v2_auth_sup, create_worker, [username1, password1] ).
