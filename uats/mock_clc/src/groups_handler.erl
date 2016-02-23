-include("data.hrl").
-module(groups_handler).

-export([init/3,
         rest_init/2,
         allowed_methods/2,
         content_types_provided/2,
         content_types_accepted/2,
         is_authorized/2,
         forbidden/2,
         unsupported/2,
         read/2,
         write/2,
         delete_resource/2]).

init(_ReqType, _Req, _Options) ->
  {upgrade, protocol, cowboy_rest}.

rest_init(Req, _Opts) ->
  {ok, Req, undefined_state}.

allowed_methods(Req, State) ->
  {[<<"GET">>, <<"POST">>, <<"PUT">>, <<"PATCH">>, <<"DELETE">>], Req, State}.

content_types_provided(Req, State) ->
  {[
    {<<"*">>, unsupported},
    {<<"application/json">>, read}
   ],Req, State}.

content_types_accepted(Req, State) ->
  {[
    {<<"*">>, unsupported},
    {<<"application/json">>, write}
   ],Req, State}.

is_authorized(Req, State) ->
	case auth_helper:is_authenticated(Req) of
		false -> {{false,<<"Bearer">>}, Req, State};
		true -> {true, Req, State}
	end.

forbidden(Req, State) ->
	{(not auth_helper:is_authorized(Req)), Req, State}.

unsupported(Req, State) ->
  Response = cowboy_req:reply(400, [], <<"unsupported content type">>, Req),
  {ok, Response, State}.

read(Req, State) ->
  Id = element(1, cowboy_req:binding(id, Req)),
  Suffix = element(1, cowboy_req:binding(suffix, Req)),
  Response = read_group(Id, Suffix, cowboy_req:parse_qs(Req)),
  {jiffy:encode(Response), Req, State}.

write(Req, State) ->
  {ok, Body, _} = cowboy_req:body(Req),
  {Status, Result} =  write_group(
                        cowboy_req:method(Req),
                        jiffy:decode(Body, [return_maps]),
                        cowboy_req:binding(id, Req)
                      ),

  {ok, Response} =  case Result of
                      undefined -> cowboy_req:reply(Status, Req);
                      _ -> cowboy_req:reply(Status, [], jiffy:encode(Result), Req)
                    end,

  {ok, Response, State}.

delete_resource(Req, State) ->
  Id = element(1, cowboy_req:binding(id, Req)),

  data_server:put(groups, Id, deleted),

  {true, Req, State}.


read_group(Id, undefined, _) ->
  data_server:get(groups, Id);
read_group(Id, ?GROUP_BILLING_PATH, _) ->
  data_server:get(groups_billing, Id);
read_group(Id, ?GROUP_AUTOSCALE_PATH, _) ->
  data_server:get(groups_autoscale, Id);
read_group(Id, ?GROUP_ACTIVITIES_PATH, _) ->
  data_server:get(groups_activities, Id);
read_group(Id, ?GROUP_STATISTICS_PATH, QueryParams) ->
	{_, Type} = lists:keyfind(<<"type">>, 1, QueryParams),
	{_, Start} = lists:keyfind(<<"start">>, 1, QueryParams),
	{_, End} = lists:keyfind(<<"end">>, 1, QueryParams),
	{_, Interval} = lists:keyfind(<<"sampleInterval">>, 1, QueryParams),
	Key = <<Type/binary, Start/binary, End/binary, Interval/binary>>,
  data_server:get(group_statistics, Key).

write_group(<<"POST">>, Body, undefined) ->
  Id = integer_to_binary(element(3, now())),
  data_server:put(groups, Id, Body),
  {201, #{ <<"id">> => Id }};
write_group(<<"PATCH">>, Body, Id) ->
  data_server:put(groups_custom_fields, Id, Body),
  {204, undefined}.

