%%
%% Copyright Péter Dimitrov 2026, All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%

%% @doc Configuration helpers. Resolves {env, "VAR"} tuples at runtime.
-module(bc_config).

-export([get/2, get/3, resolve/1, canonical_user_id/0]).

%% @doc Get a config value, crash if not found.
-spec get(App :: atom(), Key :: atom()) -> term().
get(App, Key) ->
    case application:get_env(App, Key) of
        {ok, Val} -> resolve(Val);
        undefined -> error({missing_config, App, Key})
    end.

%% @doc Get a config value with a default.
-spec get(App :: atom(), Key :: atom(), Default :: term()) -> term().
get(App, Key, Default) ->
    case application:get_env(App, Key) of
        {ok, Val} -> resolve(Val);
        undefined -> Default
    end.

%% @doc Recursively resolve {env, "VAR"} tuples via os:getenv.
-spec resolve(term()) -> term().
resolve({env, Var}) when is_list(Var) ->
    case os:getenv(Var) of
        false -> error({missing_env_var, Var});
        Val   -> Val
    end;
resolve(Map) when is_map(Map) ->
    maps:map(fun(_K, V) -> resolve(V) end, Map);
resolve(List) when is_list(List) ->
    lists:map(fun resolve/1, List);
resolve(Tuple) when is_tuple(Tuple) ->
    list_to_tuple(lists:map(fun resolve/1, tuple_to_list(Tuple)));
resolve(Val) ->
    Val.

%% @doc Return the canonical user identity from BEAMCLAW_USER, or undefined.
%% When set, all channels use this value as-is (no prefix) so that a single
%% user shares one session across TUI, Telegram, HTTP, and WebSocket.
-spec canonical_user_id() -> binary() | undefined.
canonical_user_id() ->
    case os:getenv("BEAMCLAW_USER") of
        false -> undefined;
        Val   -> list_to_binary(Val)
    end.
