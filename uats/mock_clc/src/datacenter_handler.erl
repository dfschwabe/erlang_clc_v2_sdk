-include("data.hrl").
-module(datacenter_handler).

-export([init/3,
         rest_init/2,
         allowed_methods/2,
         content_types_provided/2,
         get/2]).

init(_ReqType, _Req, _Options) ->
  {upgrade, protocol, cowboy_rest}.

rest_init(Req, _Opts) ->
  {ok, Req, undefined_state}.

allowed_methods(Req, State) ->
  {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
  {[{{<<"application">>, <<"json">>, []}, get}], Req, State}.

get(Req, State) ->
  Id = element(1, cowboy_req:binding(id, Req)),
  Response = get_datacenters(Id),
  {jiffy:encode(Response), Req, State}.

get_datacenters(undefined) ->
  data_server:get(datacenters);
get_datacenters(Id) ->
  data_server:get(datacenters, Id).
