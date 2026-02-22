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

%% @doc Built-in read_file tool — reads a file path and returns its content.
%%
%% Requires no approval and runs at read_only autonomy, allowing the agent to
%% inspect files (documentation, configs, source) without an approval prompt.
-module(bc_tool_read_file).
-behaviour(bc_tool).

-export([definition/0, execute/3, requires_approval/0, min_autonomy/0]).

definition() ->
    #{name        => <<"read_file">>,
      description => <<"Read the contents of a file and return them as text.">>,
      parameters  => #{
          type       => object,
          properties => #{
              path => #{type => string, description => <<"Absolute or relative path to the file">>}
          },
          required   => [<<"path">>]
      },
      source => builtin}.

execute(#{<<"path">> := Path}, _Session, _Context) ->
    case file:read_file(binary_to_list(Path)) of
        {ok, Bin}       -> {ok, Bin};
        {error, Reason} -> {error, iolist_to_binary(atom_to_list(Reason))}
    end.

requires_approval() -> false.

min_autonomy() -> read_only.
