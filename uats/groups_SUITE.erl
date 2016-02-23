-module(groups_SUITE).
-include("uat_helper.hrl").

-export([all/0, suite/0, init_per_suite/1, end_per_suite/1]).
-export([
         clc_v2_create_group_creates_expected_group/1,
         clc_v2_group_returns_expected_group/1,
         clc_v2_group_billing_returns_expected_billing/1,
         clc_v2_group_horizontal_autoscale_policy_returns_expected_policy/1,
         clc_v2_group_statistics_returns_expected_statistics/1,
         clc_v2_group_scheduled_activities_returns_expected_activities/1,
         clc_v2_update_group_custom_fields_updates_expected_fields/1,
         clc_v2_update_group_defaults_updates_expected_defaults/1,
         clc_v2_update_group_horizontal_autoscale_policy_updates_group_policy/1,
         clc_v2_update_group_name_updates_name_and_description/1,
         clc_v2_update_group_parent_updates_group_parent/1,
         clc_v2_schedule_group_activities_creates_expected_schedule/1
        ]).

all() -> [
         clc_v2_create_group_creates_expected_group,
         clc_v2_group_returns_expected_group,
         clc_v2_group_billing_returns_expected_billing,
         clc_v2_group_horizontal_autoscale_policy_returns_expected_policy,
         clc_v2_group_statistics_returns_expected_statistics,
         clc_v2_group_scheduled_activities_returns_expected_activities,
         clc_v2_update_group_custom_fields_updates_expected_fields,
         clc_v2_update_group_defaults_updates_expected_defaults,
         clc_v2_update_group_horizontal_autoscale_policy_updates_group_policy,
         clc_v2_update_group_name_updates_name_and_description,
         clc_v2_update_group_parent_updates_group_parent,
         clc_v2_schedule_group_activities_creates_expected_schedule
        ].

suite() ->
      [{timetrap,{minutes,1}}].

init_per_suite(Config) ->
  ?SUITE_SETUP(Config).

end_per_suite(_Config) ->
  ?SUITE_TEARDOWN().

clc_v2_create_group_creates_expected_group(Config) ->
	Spec =	#{name => <<"New Group">>,
						description => <<"A new group.">>,
						parent_group_id => <<"groupx">>,
						custom_fields => [#{ id => <<"field1">>, value => <<"cmdb id value">> }]
					},
	Expected =	#{<<"name">> => <<"New Group">>,
								<<"description">> => <<"A new group.">>,
								<<"parentGroupId">> => <<"groupx">>,
								<<"customFields">> => [#{ <<"id">> => <<"field1">>, <<"value">> => <<"cmdb id value">> }]
							},

	{ok, Id} = clc_v1:create_group(?AUTH(Config), Spec),

  assert:equal(Expected, data_server:get(groups, Id)),
  ok.

clc_v2_group_returns_expected_group(Config) ->
	Id = <<"group1">>,
	Expected = mock_group(),
	data_server:put(groups, Id, Expected),

	{ok, Actual} = clc_v2:group(?AUTH(Config), Id),

	assert:equal(Expected, Actual),
  ok.

clc_v2_group_billing_returns_expected_billing(Config) ->
	Id = <<"group1">>,
	Expected = mock_billing(),
	data_server:put(groups_billing, Id, Expected),

	{ok, Actual} = clc_v2:group_billing(?AUTH(Config), Id),

	assert:equal(Expected, Actual),
	ok.

clc_v2_group_horizontal_autoscale_policy_returns_expected_policy(Config) ->
	Id = <<"group1">>,
	Expected = mock_autoscale_policy(),
	data_server:put(groups_autoscale, Id, Expected),

	{ok, Actual} = clc_v2:group_horizontal_autoscale_policy(?AUTH(Config), Id),

	assert:equal(Expected, Actual),
  ok.

clc_v2_group_monitoring_statistics_returns_expected_statistics(Config) ->
	Id = <<"group1">>,
	Type = hourly,
	Start = {{2016,2,25},{3,40,34}},
	End = {{2017,3,26},{5,51,45}},
	Interval = {1,23,45},
	Key = <<"group1_hourly_2016-02-25T03:40:34Z_2017-03-26T05:51:45Z_01:23:45">>,
	Expected = mock_statistics(),
	data_server:put(groups_statistics, Key, Expected),

	{ok, Actual} = clc_v2:group_monitoring_statistics(?AUTH(Config), Id, Type, Start, End, Interval),

	assert:equal(Expected, Actual),
  ok.

clc_v2_group_scheduled_activities_returns_expected_activities(Config) ->
  Id = <<"group1">>,
  Expected = mock_activities(),
  data_server:put(groups_activities, Id, Expected),

  {ok, Actual} = clc_v2:group_scheduled_activities(Id),

  assert:equal(Expected, Actual),
  ok.

clc_v2_update_group_custom_fields_updates_expected_fields(Config) ->
	Id = <<"group1">>,
	Spec = #{ <<"id1">> => true,
						<<"id2">> => 2,
						<<"id3">> => 3.0,
						<<"id4">> => "four",
						<<"id5">> => <<"five">> },

  ok = clc_v2:update_group_custom_fields(Id, Spec),

	assert:equal(	#{<<"op">> => <<"set">>,
									<<"member">> => <<"customFields">>,
									<<"value">> =>
										[ #{<<"id">> => <<"id1">>, <<"value">> => true},
											#{<<"id">> => <<"id2">>, <<"value">> => 2},
											#{<<"id">> => <<"id3">>, <<"value">> => 3.0},
											#{<<"id">> => <<"id4">>, <<"value">> => <<"four">>},
											#{<<"id">> => <<"id5">>, <<"value">> => <<"five">>}]
								},
								data_server:get(groups_custom_fields, Id)
              ),
  ok.

clc_v2_update_group_defaults_updates_expected_defaults(Config) ->
  ok.

clc_v2_update_group_horizontal_autoscale_policy_updates_group_policy(Config) ->
  ok.

clc_v2_update_group_name_updates_name_and_description(Config) ->
  ok.

clc_v2_update_group_parent_updates_group_parent(Config) ->
  ok.

clc_v2_schedule_group_activities_creates_expected_schedule(Config) ->
  ok.

mock_group() ->
    #{<<"changeInfo">> => #{<<"createdBy">> => <<"stuff">>,
      <<"createdDate">> => <<"2015-02-17T14:09:10Z">>,
      <<"modifiedBy">> => <<"stuff">>,
      <<"modifiedDate">> => <<"2015-02-17T14:09:10Z">>},
    <<"customFields">> => [],
    <<"description">> => <<"The default location for new servers created in your account.">>,
    <<"groups">> => [#{<<"changeInfo">> => #{<<"createdBy">> => <<"stuff">>,
         <<"createdDate">> => <<"2016-02-15T05:32:36Z">>,
         <<"modifiedBy">> => <<"stuff">>,
         <<"modifiedDate">> => <<"2016-02-15T05:32:36Z">>},
       <<"customFields">> => [#{<<"displayValue">> => <<"cmdb id value">>,
          <<"id">> => <<"field1">>,
          <<"name">> => <<"CMDB ID">>,
          <<"value">> => <<"cmdb id value">>}],
       <<"description">> => <<"A new group.">>,
       <<"groups">> => [],
       <<"id">> => <<"groupy">>,
       <<"links">> => [#{<<"href">> => <<"/v2/groups/stuff">>,
          <<"rel">> => <<"createGroup">>,
          <<"verbs">> => [<<"POST">>]},
        #{<<"href">> => <<"/v2/servers/stuff">>,
          <<"rel">> => <<"createServer">>,
          <<"verbs">> => [<<"POST">>]},
        #{<<"href">> => <<"/v2/groups/stuff/groupy">>,
          <<"rel">> => <<"self">>,
          <<"verbs">> => [<<"GET">>,<<"PATCH">>,<<"DELETE">>]},
        #{<<"href">> => <<"/v2/groups/stuff/groupx">>,
          <<"id">> => <<"groupx">>,
          <<"rel">> => <<"parentGroup">>},
        #{<<"href">> => <<"/v2/groups/stuff/groupy/defaults">>,
          <<"rel">> => <<"defaults">>,
          <<"verbs">> => [<<"GET">>,<<"POST">>]},
        #{<<"href">> => <<"/v2/groups/stuff/groupy/billing">>,
          <<"rel">> => <<"billing">>},
        #{<<"href">> => <<"/v2/groups/stuff/groupy/archive">>,
          <<"rel">> => <<"archiveGroupAction">>},
        #{<<"href">> => <<"/v2/groups/stuff/groupy/statistics">>,
          <<"rel">> => <<"statistics">>},
        #{<<"href">> => <<"/v2/groups/stuff/groupy/upcomingScheduledActivities">>,
          <<"rel">> => <<"upcomingScheduledActivities">>},
        #{<<"href">> => <<"/v2/groups/stuff/groupy/horizontalAutoscalePolicy">>,
          <<"rel">> => <<"horizontalAutoscalePolicyMapping">>,
          <<"verbs">> => [<<"GET">>,<<"PUT">>,<<"DELETE">>]},
        #{<<"href">> => <<"/v2/groups/stuff/groupy/scheduledActivities">>,
          <<"rel">> => <<"scheduledActivities">>,
          <<"verbs">> => [<<"GET">>,<<"POST">>]}],
       <<"locationId">> => <<"REGION">>,
       <<"name">> => <<"New Group">>,
       <<"serversCount">> => 0,
       <<"status">> => <<"active">>,
       <<"type">> => <<"default">>}],
    <<"id">> => <<"groupx">>,
    <<"links">> => [#{<<"href">> => <<"/v2/groups/stuff">>,
       <<"rel">> => <<"createGroup">>,
       <<"verbs">> => [<<"POST">>]},
     #{<<"href">> => <<"/v2/servers/stuff">>,
       <<"rel">> => <<"createServer">>,
       <<"verbs">> => [<<"POST">>]},
     #{<<"href">> => <<"/v2/groups/stuff/groupx">>,
       <<"rel">> => <<"self">>,
       <<"verbs">> => [<<"GET">>,<<"PATCH">>,<<"DELETE">>]},
     #{<<"href">> => <<"/v2/groups/stuff/groupz">>,
       <<"id">> => <<"groupz">>,
       <<"rel">> => <<"parentGroup">>},
     #{<<"href">> => <<"/v2/groups/stuff/groupx/defaults">>,
       <<"rel">> => <<"defaults">>,
       <<"verbs">> => [<<"GET">>,<<"POST">>]},
     #{<<"href">> => <<"/v2/groups/stuff/groupx/billing">>,
       <<"rel">> => <<"billing">>},
     #{<<"href">> => <<"/v2/groups/stuff/groupx/archive">>,
       <<"rel">> => <<"archiveGroupAction">>},
     #{<<"href">> => <<"/v2/groups/stuff/groupx/statistics">>,
       <<"rel">> => <<"statistics">>},
     #{<<"href">> => <<"/v2/groups/stuff/groupx/upcomingScheduledActivities">>,
       <<"rel">> => <<"upcomingScheduledActivities">>},
     #{<<"href">> => <<"/v2/groups/stuff/groupx/horizontalAutoscalePolicy">>,
       <<"rel">> => <<"horizontalAutoscalePolicyMapping">>,
       <<"verbs">> => [<<"GET">>,<<"PUT">>,<<"DELETE">>]},
     #{<<"href">> => <<"/v2/groups/stuff/groupx/scheduledActivities">>,
       <<"rel">> => <<"scheduledActivities">>,
       <<"verbs">> => [<<"GET">>,<<"POST">>]},
     #{<<"href">> => <<"/v2/servers/stuff/ca3stufftest03">>,
       <<"id">> => <<"REGIONALIASTEST03">>,
       <<"rel">> => <<"server">>},
     #{<<"href">> => <<"/v2/servers/stuff/ca3stufftest02">>,
       <<"id">> => <<"REGIONALIASTEST02">>,
       <<"rel">> => <<"server">>}],
    <<"locationId">> => <<"REGION">>,
    <<"name">> => <<"Default Group">>,
    <<"serversCount">> => 2,
    <<"status">> => <<"active">>,
    <<"type">> => <<"default">>}.

mock_billing() ->
	#{<<"date">> => <<"2016-02-15T05:10:49Z">>,
		<<"groups">> =>
			#{
				<<"groupx">> =>
					#{<<"name">> => <<"Default Group">>,
						<<"servers">> => #{}
					},
				<<"groupy">> =>
					#{<<"name">> => <<"New Group">>,
						<<"servers">> => #{}
					}
			}
	}.

mock_autoscale_policy() ->
	#{<<"groupId">> => <<"group1">>,
		<<"policyId">> => <<"policy1">>,
		<<"locationId">> => <<"REGION">>,
		<<"links">> => [
			#{<<"rel">> => <<"self">>,
				<<"href">> => <<"/v2/groups/ALIAS/group1/horizontalAutoscalePolicy">>},
			#{<<"rel">> => <<"group">>,
				<<"href">> => <<"/v2/groups/ALIAS/group1">>},
			#{<<"rel">> => <<"horizontalAutoscalePolicy">>,
				<<"href">> => <<"/v2/horizontalAutoscalePolicies/ALIAS/group1">>}
			]
	}.

mock_statistics() ->
	[	#{<<"name">> => <<"name1">>,
			<<"stats">> =>
			[ #{
					<<"cpu">> => 1.0,
					<<"cpuPercent">> => 0.32,
					<<"diskUsage">> => [
						#{<<"capacityMB">> => 14336,<<"id">> => <<"0:2">>},
						#{<<"capacityMB">> => 2048,<<"id">> => <<"0:1">>},
						#{<<"capacityMB">> => 512,<<"id">> => <<"0:0">>}
					],
					<<"diskUsageTotalCapacityMB">> => 16896.0,
					<<"guestDiskUsage">> => [],
					<<"memoryMB">> => 2048.0,
					<<"memoryPercent">> => 75.0,
					<<"networkReceivedKBps">> => 0.0,
					<<"networkTransmittedKBps">> => 0.0,
					<<"timestamp">> => <<"2016-02-15T04:00:00Z">>
				}]
	}].

mock_activities() ->
	[	#{
			<<"id">> => <<"activity1">>,
			<<"locationId">> => <<"REGION">>,
			<<"changeInfo">> =>
				#{<<"createdBy">> => <<"soandso">>,
					<<"createdDate">> => <<"2016-02-15T05:54:37Z">>,
					<<"modifiedBy">> => <<"suchandsuch">>,
					<<"modifiedDate">> => <<"2016-02-15T05:54:37Z">> },
			<<"links">> =>
				[	#{<<"rel">> => <<"group">>,
						<<"href">> => <<"/v2/groups/ALIAS/group1">>,
						<<"id">> => <<"group1">>,
						<<"name">> => <<"Default Group">> },
					#{<<"rel">> => <<"self">>,
						<<"href">> => <<"/v2/groups/ALIAS/group1/scheduledActivities/activity1">>,
						<<"verbs">> => [ <<"GET">>, <<"PUT">>, <<"DELETE">> ] }
				],
			<<"status">> => <<"on">>,
			<<"type">> => <<"reboot">>,
			<<"beginDateUTC">> => <<"2016-02-15T05:54:00Z">>,
			<<"repeat">> => <<"daily">>,
			<<"customWeeklyDays">> => [],
			<<"expire">> => <<"afterCount">>,
			<<"expireCount">> => 1,
			<<"timeZoneOffset">> => <<"-06:00:00">>,
			<<"isExpired">> => false,
			<<"occurrenceCount">> => 0,
			<<"nextOccurrenceDateUTC">> => <<"2016-02-16T05:54:00Z">>
		}
	].



