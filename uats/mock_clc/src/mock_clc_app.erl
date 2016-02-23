-include("data.hrl").
-module(mock_clc_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  data_server_sup:start_link(),
  Dispatch = cowboy_router:compile(route_matchers()),
  {ok, _} = cowboy:start_http(http, 100, [{port, 8000}], [{env, [{dispatch, Dispatch}]}]).

stop(_State) ->
  ok.

route_matchers() ->
  [ {'_',
     [
      { io_lib:format("/v2/accounts/~s/customFields", [?ALIAS]),
          custom_fields_handler, [] },
      { "/v2/authentication/login",
          auth_handler, [] },
      { io_lib:format("/v2/alertPolicies/~s/[:id]", [?ALIAS]),
          alertpolicy_handler, [] },
      { io_lib:format("/v2/antiAffinityPolicies/~s/[:id]", [?ALIAS]),
          antiaffinitypolicy_handler, [] },
      { io_lib:format("/v2/autoscalePolicies/~s/[:id]", [?ALIAS]),
          autoscalepolicy_handler, [] },
      { io_lib:format("/v2/datacenters/~s/[:id/[:suffix]]", [?ALIAS]),
          [is_dc_capability()], datacenter_handler, [] },
      { io_lib:format("/v2/groups/~s/[:id/[:suffix]]", [?ALIAS]),
          [is_group_extension()], groups_handler, [] },
      { io_lib:format("/v2/invoice/~s/:year/:month", [?ALIAS]),
          invoice_handler, [] },
      { io_lib:format("/v2/servers/~s/:server_id/cpuAutoscalePolicy", [?ALIAS]),
          server_autoscalepolicy_handler, [] }
     ]
    } ].

is_group_extension() ->
	whitelist_constraint(suffix, [?GROUP_BILLING_PATH,
																?GROUP_AUTOSCALE_PATH,
																?GROUP_STATISTICS_PATH,
                                ?GROUP_ACTIVITIES_PATH]).

is_dc_capability() ->
	whitelist_constraint(suffix, [?DC_DEPLOY_CAPABILITY_PATH,
																?DC_BAREMETAL_CAPABILITY_PATH]).

whitelist_constraint(SegmentName, ValidValues) ->
	F = fun(Path) ->
				lists:member(Path, ValidValues)
			end,

  {SegmentName, function, F}.
