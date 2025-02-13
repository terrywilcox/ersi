-module(ersi).

-behaviour(gen_server).

-define(WS_DOMAIN, <<"slack.com">>).
-define(WS_PATH, <<"/api/apps.connections.open">>).
-define(WS_PORT, 443).

-export([start_link/0]).

-export([init/1]).
-export([handle_cast/2]).
-export([handle_call/3]).
-export([handle_info/2]).
-export([terminate/3]).
-export([code_change/4]).

start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

handle_cast(_, State) ->
    {noreply, ok, State}.

handle_call(_, _From, State) ->
    {reply, ok, State}.

handle_info(_, State) ->
    {noreply, State}.

init([]) ->
    io:format("erdi_websocket init~n"),
    State = #{gateway_tls => [{verify, verify_peer},
                              {cacerts, public_key:cacerts_get()}],
              ws_tls => [{verify, verify_none}, {cacerts, public_key:cacerts_get()}]},
    Json =
        open_http(?WS_DOMAIN,
                  ?WS_PORT,
                  ?WS_PATH,
                  maps:get(gateway_tls, State)),
    #{<<"ok">> := <<"true">>,
      <<"url">> := <<"wss://", URL/binary>>} = json:decode(Json),
    [Domain, Path0] = binary:split(URL, <<"/">>),
    Path = <<"/", Path0/binary>>,
    % TODO Open Gun websocket connection
    % gun:open(with TLS certs) since it's wss:// (secure web socket)
    % gun:await_up
    % gun:ws_upgrade
    {ok, State}.

code_change(_, _, _, _) ->
    ok.

terminate(_, _, _) ->
    ok.

open_http(Domain, 443 = Port, Path, TLSOpts) ->
    {ok, Conn} = gun:open(Domain, Port, #{tls_opts => TLSOpts}),
    {ok, _Protocol} = gun:await_up(Conn),
    Headers = [{<<"Content-type">>, <<"application/x-www-form-urlencoded">>},
               {<<"Authorization">>, secret_token()}],
    StreamRef = gun:get(Conn, Path, Headers),
    {ok, Body} = gun:await_body(Conn, StreamRef),
    Body.

secret_token() ->
    list_to_binary(os:getenv("SLACK_OBS_INTEGRATION_APP_LEVEL_TOKEN")).
