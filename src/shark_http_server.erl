-module(shark_http_server).

-export([start_link/2, start_link/3, stop/0, loop/3]).

%%% Internal API
-export([request_headers/1]).

start_link(Port, App) ->
    start_link(Port, App, []).

start_link(Port, App, Props) ->
    BaseEnv = [{server_port, Port}|check_server_name(Props)],
    Loop = fun(Req) -> ?MODULE:loop(App, Req, BaseEnv) end,
    mochiweb_http:start([{port, Port},
                         {name, proplists:get_value(proc_name, Props, ?MODULE)},
                         {max, 10240},
                         {loop, Loop}]).

check_server_name(P) ->
    case proplists:get_value(server_name, P) of
	undefined ->
	    {ok, HostName} = inet:gethostname(),
	    [{server_name, HostName}|P];
	_ ->
	    P
    end.

stop() ->
    mochiweb_http:stop(?MODULE).

combine_headers(Overrides, Base) ->
    OverridesHeaders = mochiweb_headers:make(Overrides),
    CombinedHeaders = mochiweb_headers:default_from_list(Base,
							 OverridesHeaders),
    mochiweb_headers:to_list(CombinedHeaders).

response_headers(Headers) ->
    Default = [{"Server", "Landshark/DEV-1"}],
    combine_headers(Headers, Default).

request_headers(Headers) ->
    request_headers(Headers, []).

request_headers([], Acc) ->
    lists:reverse(Acc);
request_headers([{Name, Value}|Rest], Acc) ->
    request_headers(Rest, [{request_header_atom(Name), Value}|Acc]).

request_header_atom(Name) when is_atom(Name) ->
    request_header_atom(atom_to_list(Name));
request_header_atom(Name) ->
    request_header_atom(Name, []).

request_header_atom([], Acc) ->
    Name = string:to_lower(lists:reverse(Acc)),
    case Name of
	"content_type" -> content_type;
	"content_length" -> content_length;
	_ -> list_to_atom("http_"++Name)
    end;
request_header_atom([$-|T], Acc) ->
    request_header_atom(T, [$_|Acc]);
request_header_atom([H|T], Acc) ->
    request_header_atom(T, [H|Acc]).

%%
%% @doc Provides param properties for GET, POST, and POST++GET. In WSGI this
%% would be provided by middleware, parsing the query string and post content.
%% However, since this is readily available from the mochiweb request, we
%% provide it in the core environ. Arguably, this should be a part of any
%% Erlang wsgi equivalent.
%%
params(Req) ->
    GetParams = Req:parse_qs(),
    PostParams = Req:parse_post(),
    [{params_get, GetParams},
     {params_post, PostParams},
     {params, PostParams ++ GetParams}].

create_env(Req, BaseEnv) ->
    {mochiweb_request, Socket, Method, Uri, Version, Headers} = Req,
    {ok, {Address, Port}} = inet:peername(Socket),
    {Path, QueryString, _Fragment} = mochiweb_util:urlsplit_path(Uri),
    lists:flatten(
      [{server_protocol, {http, Version}},
       {request_method, Method},
       {request_uri, Uri},
       {path_info, Path},
       {script_name, ""},
       {query_string, QueryString},
       {remote_addr, Address},
       {remote_port, Port},
       request_headers(mochiweb_headers:to_list(Headers)),
       params(Req),
       BaseEnv]).

loop(App, Req, BaseEnv) ->
    Env = create_env(Req, BaseEnv),
    try call_app(App, Env) of
        {filepath, Path, Headers} ->
            Req:serve_file("", Path, response_headers(Headers));
        {Status, Headers, Body} ->
            Req:respond({Status, response_headers(Headers), Body});
        Other ->
            error_logger:error_report({bad_internal_response, Other}),
            internal_error(Req)
    catch
        throw:{Code, Msg} when is_integer(Code) ->
	    Req:respond({Code, [{"Content-Type", "text/plain"}], Msg});
        Type:Error ->
            error_logger:error_report({unhandled_error, Type, Error,
                                       erlang:get_stacktrace()}),
            internal_error(Req)
    end.

internal_error(Req) ->
    Req:respond({500, [{"Content-Type", "text/plain"}],
                 "Internal server error"}).

call_app({M, F}, App) ->
    M:F(App);
call_app({M, F, A}, App) ->
    M:F(A ++ [App]);
call_app(F, App) ->
    F(App).
