%/--------------------------------------------------------------------
%| Copyright 2017 Erisata, UAB (Ltd.)
%|
%| Licensed under the Apache License, Version 2.0 (the "License");
%| you may not use this file except in compliance with the License.
%| You may obtain a copy of the License at
%|
%|     http://www.apache.org/licenses/LICENSE-2.0
%|
%| Unless required by applicable law or agreed to in writing, software
%| distributed under the License is distributed on an "AS IS" BASIS,
%| WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%| See the License for the specific language governing permissions and
%| limitations under the License.
%\--------------------------------------------------------------------

%%% @doc
%%% AxB Config Console implementation.
%%% Functions in this module are invoked from the shell script.
%%%
-module(axb_config_console).
-compile([{parse_transform, lager_transform}]).
-export([config/1, completion/1]).
-include("axb_config.hrl").


%%% ============================================================================
%%% Public API.
%%% ============================================================================

%%  @doc
%%  This function handles all the console commands.
%%  The following are examples of its usage assuming the command is config.
%%
%%  `config some'                   -- shows all configs under this path.
%%  `config some.config'            -- shows the specified config.
%%  `config some.config.p1 v1'      -- sets parameter `p1' to the value `v1'.
%%  `config some.config.p1 --reset' -- remove the runtime config.
%%  `config some --reset'           -- remove the all the runtime configs under this path.
%%  `config --reset'                -- remove the all the runtime configs.
%%  `config --reload'               -- reload config from the environment file.
%%
config([]) ->
    ok = print_config([]),
    ok;

config(["--reload"]) ->
    ok = reload_config(),
    ok;

config(["--reset"]) ->
    ok = reset_config(parse_path([])),
    ok;

config([PathStr]) ->
    ok = print_config(parse_path(PathStr)),
    ok;

config([PathStr, "--reset"]) ->
    ok = reset_config(parse_path(PathStr)),
    ok;

config([PathStr, Value]) ->
    ok = set_config(parse_path(PathStr), Value),
    ok.


%%
%%  Support for auto-completion.
%%
completion([]) ->
    % TODO
    ok.



%%% ============================================================================
%%% Private functions.
%%% ============================================================================

%%
%%
%%
parse_path(PathStr) ->
    lists:map(fun erlang:list_to_atom/1, string:tokens(PathStr, ".")).


%%
%%
%%
format_path(Path) ->
    string:join(lists:map(fun erlang:atom_to_list/1, Path), ".").


%%
%%
%%
print_config(ConfigPrefix) ->
    case axb_config:get_config_info(ConfigPrefix) of
        {ok, Configs} ->
            print_config(ConfigPrefix, [], Configs);
        {error, {not_found, NotFoundSuffix}} ->
            io:format("ERROR: Suffix \"~s\" of the config path not found.~n~n", [format_path(NotFoundSuffix)]);
        {error, Reason} ->
            io:format("ERROR: ~p~n~n", [Reason])
    end.

print_config(ConfigPrefix, ConfigPath, #axb_config{config_type = ConfigType, description = Description, parameters = Parameters}) ->
    io:format("~s~n", [[
        io_lib:format("~s [~s] # ~s~n", [format_path(ConfigPrefix ++ lists:reverse(ConfigPath)), ConfigType, Description]),
        print_config_params([], Parameters)
    ]]);

print_config(ConfigPrefix, ConfigPath, Configs) when is_map(Configs) ->
    lists:foreach(
        fun ({PathPart, SubConfigs}) ->
            print_config(ConfigPrefix, [PathPart | ConfigPath], SubConfigs)
        end,
        lists:sort(maps:to_list(Configs))
    ).

print_config_params(Prefix, #axb_config_param{actual = Actual, cardinality = Cardinality, description = Description}) ->
    FormatParamLine = fun (Path, Value) ->
        PathStr  = format_path(lists:reverse(Path)),
        ValueStr = lists:flatten(io_lib:format("~p", [Value])),
        PathLen  = erlang:max(20, length(PathStr)),
        ValueLen = erlang:max(40 - (PathLen - 20), length(ValueStr)),
        io_lib:format("\t~-*s = ~-*s # ~s~n", [
            PathLen,  PathStr,
            ValueLen, ValueStr,
            Description
        ])
    end,
    case Cardinality of
        map -> [ FormatParamLine([K | Prefix], V) || {K, V} <- lists:sort(maps:to_list(Actual)) ];
        opt -> FormatParamLine(Prefix, Actual);
        one -> FormatParamLine(Prefix, Actual);
        _   -> FormatParamLine(Prefix, Actual)
    end;

print_config_params(Prefix, Parameters) when is_map(Parameters) ->
    lists:map(
        fun ({PathPart, SubParams}) ->
            print_config_params([PathPart | Prefix], SubParams)
        end,
        lists:sort(maps:to_list(Parameters))
    ).


%%
%%
%%
reset_config(Path) ->
    case axb_config:set_runtime_config(#{Path => undefined}) of
        ok ->
            io:format("OK: Runtime configuration was reset for ~p.~n~n", [format_path(Path)]),
            ok;
        {error, Reason} ->
            io:format("ERROR: Unable to reset runtime config for ~p, reason=~p.~n~n", [format_path(Path), Reason]),
            ok
    end.


%%
%%
%%
set_config(Path, Value) ->
    case axb_config:set_runtime_config(#{Path => Value}) of
        ok ->
            io:format("OK: Runtime configuration was set for ~p=~p.~n~n", [format_path(Path), Value]),
            ok;
        {error, Reason} ->
            io:format("ERROR: Unable to set runtime config for ~p=~p, reason=~p.~n~n", [format_path(Path), Value, Reason]),
            ok
    end.


%%
%%
%%
reload_config() ->
    case axb_config:reload_env_config() of
        ok ->
            io:format("OK: Configuration was reloaded from file.~n~n", []),
            ok;
        {error, Reason} ->
            io:format("ERROR: Unable to reload config from file, reason=~p.~n~n", [Reason]),
            ok
    end.


