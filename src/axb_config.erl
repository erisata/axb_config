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
%%% This module provides the interface to the axb_config application.
%%% It allows components to register own configuration and retrieve
%%% the actual values.
%%%
-module(axb_config).
-behaviour(gen_server).
-compile([{parse_transform, lager_transform}]).
-export([
    start_link/0,
    start_link/1,
    register_config/2,
    get_config_info/1,
    get_actual_config/2,
    get_actual_config/1,
    reload_env_config/0,
    set_runtime_config/1
]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-include("axb_config.hrl").
-include_lib("kernel/include/file.hrl").

-define(RESTART_DELAY, 5000).

%%
%%  Tree-like structure of the config values.
%%  All the following examples represent the same info:
%%
%%    * [{"a.b.c", 123}, {"a.b.d", 321}]
%%    * [{[a, b], #{c => 123, d => 321}}]
%%    * #{[a, b] => #{c => 123, d => 321}}
%%    * #{a => #{b => #{c => 123, d => 321}}}
%%
%%  Only first level names are considered for normalization
%%  (transformation to the nested maps, as in the last case),
%%  because all the deeper values cannot be distinguished
%%  from user values, in general case.
%%
-type config_values() ::
    [{Name :: string() | [atom()] | atom(), Value :: term()}] |
    #{Name :: string() | [atom()] | atom() => Value :: term()}.


%%% ============================================================================
%%% Public API.
%%% ============================================================================

%%  @doc
%%  Start the config service.
%%  No rutime values are specified at the startup.
%%
-spec start_link()
    -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, app_env, []).


%%  @doc
%%  Start the config service with initial
%%  configuration set to ConstConfig.
%%
-spec start_link(ConstConfig :: config_values())
    -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.

start_link(ConstConfig) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, {const_config, ConstConfig}, []).


%%  @doc
%%  Register configuration spec to the specific path.
%%
-spec register_config(ConfigPath :: [atom()], ConfigSpec :: #config{})
    -> ok | {error, Reason :: term()}.

register_config(ConfigPath, ConfigSpec) when is_list(ConfigPath) ->
    gen_server:call(?MODULE, {register_config, ConfigPath, ConfigSpec}).


%%  @doc
%%  Returns all the configs, under the specified path. The function
%%  returns particular #config{}, if the ConfigPath points it directly,
%%  or a map of configs under the specified preffix.
%%
-spec get_config_info(ConfigPath :: [atom()])
    -> {ok, #config{} | map()}.

get_config_info(ConfigPath) ->
    gen_server:call(?MODULE, {get_config_info, ConfigPath}).


%%  @doc
%%  Returns actual/effective configuration values, that should be used
%%  by the configured components. The path should point to the particular
%%  configuration, that was previously configured via register_config/2.
%%
%%  The configuration is returned as a map, with keys as atoms, and
%%  values of types, as specified in the #config_param.type.
%%
-spec get_actual_config(ConfigPath :: [atom()], ConfigVariant :: atom())
    -> {ok, map()}.

get_actual_config(ConfigPath, ConfigVariant) ->
    gen_server:call(?MODULE, {get_actual_config, ConfigPath, ConfigVariant}).


-spec get_actual_config(ConfigPath :: [atom()])
    -> {ok, map()}.

get_actual_config(ConfigPath) ->
    get_actual_config(ConfigPath, default).


%%  @doc
%%  Reloads environment configuration, re-reads it from the file.
%%
-spec reload_env_config()
    -> ok.

reload_env_config() ->
    gen_server:call(?MODULE, reload_env_config).


%%  @doc
%%  Set config values at runtime.
%%  The values can be reset by setting it (or any of its parents) to undefined.
%%
-spec set_runtime_config(RuntimeValues :: config_values())
    -> ok.

set_runtime_config(RuntimeValues) ->
    gen_server:call(?MODULE, {set_runtime_config, RuntimeValues}).



%%% =============================================================================
%%% Internal state.
%%% =============================================================================

%%
%%  Internal state.
%%
-record(state, {
    configs     :: #{atom => #{} | #config{}},  % Current parsed/normalized config.
    env_file    :: string(),                    % Name of the config file.
    env_period  :: integer() | undefined,       % Reload interval in MS for the ENV file.
    env_mtime   :: calendar:datetime(),         % Last modification time of the config file.
    env_config  :: #{atom => map() | term()},   % Normalized config, as it was read from the ENV file.
    rt_config   :: #{atom => map() | term()}    % Normalized config, as it was modified at runtime.
}).



%%% =============================================================================
%%% Callbacks for the gen_server.
%%% =============================================================================

%%  @private
%%  Initialization.
%%
init(app_env) ->
    case axb_config_app:get_env(env_file, undefined) of
        undefined                            -> init({const_config, #{}});
        ConstConfig when is_map(ConstConfig) -> init({const_config, ConstConfig});
        EnvFile     when is_list(EnvFile)    -> init({config_file,  EnvFile})
    end;

init({config_file, EnvFile}) ->
    EnvPeriod = axb_config_app:get_env(env_period, undefined),
    State = #state{
        configs    = #{},
        env_file   = EnvFile,
        env_period = EnvPeriod,
        env_mtime  = undefined,
        env_config = #{},
        rt_config  = #{}
    },
    try read_env_config(State) of
        {ok, NewState} ->
            case EnvPeriod of
                undefined -> ok;
                _ -> erlang:send_after(EnvPeriod, self(), check_env_config)
            end,
            {ok, NewState};
        {error, Reason} ->
            lager:error("Failed to read configuration from ~p, reason=~p", [EnvFile, Reason]),
            ok = timer:sleep(?RESTART_DELAY),
            {stop, Reason}
    catch
        ErrClass:Reason ->
            lager:error("Failed to read configuration from ~p, reason=~p:~p", [EnvFile, ErrClass, Reason]),
            ok = timer:sleep(?RESTART_DELAY),
            {stop, Reason}
    end;

init({const_config, ConstConfig}) ->
    State = #state{
        configs    = #{},
        env_file   = undefined,
        env_period = undefined,
        env_mtime  = undefined,
        env_config = normalize_config(ConstConfig),
        rt_config  = #{}
    },
    {ok, State}.


%%  @private
%%  Synchronous calls.
%%
handle_call({register_config, ConfigPath, ConfigSpec}, _From, State) ->
    #state{
        configs    = Configs,
        env_config = EnvConfig,
        rt_config  = RtConfig
    } = State,
    NewConfigs = add_config_spec(ConfigPath, ConfigSpec, Configs),
    ConfigsWithEnv = apply_config(EnvConfig, #config_param.environment, NewConfigs),
    ConfigsWithRt  = apply_config(RtConfig,  #config_param.runtime, ConfigsWithEnv),
    NewState = State#state{
        configs = ConfigsWithRt
    },
    {reply, ok, NewState};

handle_call({get_config_info, ConfigPath}, _From, State = #state{configs = Configs}) ->
    {reply, get_config(ConfigPath, Configs), State};

handle_call({get_actual_config, ConfigPath, ConfigVariant}, _From, State = #state{configs = Configs}) ->
    {reply, get_actual(ConfigPath, ConfigVariant, Configs), State};

handle_call(reload_env_config, _From, State) ->
    try read_env_config(State#state{env_mtime = undefined}) of
        {ok, NewState}  -> {reply, ok, NewState};
        {error, Reason} -> {reply, {error, Reason}, State}
    catch
        ErrClass:Reason -> {reply, {error, {ErrClass, Reason}}, State}
    end;

handle_call({set_runtime_config, RuntimeConfig}, _From, State = #state{configs = Configs, rt_config = RtConfig}) ->
    NewRtConfig = merge_values(normalize_config(RuntimeConfig), RtConfig),
    NewConfigs  = apply_config(NewRtConfig, #config_param.runtime, Configs),
    NewState = State#state{
        configs   = NewConfigs,
        rt_config = NewRtConfig
    },
    {reply, ok, NewState};

handle_call(_Unknown, _From, State) ->
    {reply, undefined, State}.


%%  @private
%%  Asynchronous events.
%%
handle_cast(_Event, State) ->
    {noreply, State}.


%%  @private
%%  Handle other messages.
%%
handle_info(check_env_config, State = #state{env_file = EnvFile, env_period = EnvPeriod}) ->
    _ = erlang:send_after(EnvPeriod, self(), check_env_config),
    try read_env_config(State) of
        {ok, NewState} ->
            {noreply, NewState};
        {error, Reason} ->
            lager:error("Failed to read configuration from ~p, reason=~p", [EnvFile, Reason]),
            {noreply, State}
    catch
        ErrClass:Reason ->
            lager:error("Failed to read configuration from ~p, reason=~p:~p", [EnvFile, ErrClass, Reason]),
            {noreply, State}
    end;

handle_info(_Unknown, State) ->
    {noreply, State}.


%%  @private
%%  Termination.
%%
terminate(_Reason, _State) ->
    ok.


%%  @private
%%  Code upgrades.
%%
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



%%% ============================================================================
%%% Internal functions.
%%% ============================================================================

%%  @private
%%  Get actual values for the specified config. Example:
%%
%%      get_actual([a, b], #{a => #{b => #config{parameters => #{
%%          x => #config_param{actual = 3}
%%          y => #{yy => #config_param{actual = 14}}
%%      }}}}) ->
%%          #{x => 3, y => #{yy => 14}}
%%
get_actual(ConfigPath, ConfigVariant, Configs) ->
    case get_config(ConfigPath, Configs) of
        {ok, #config{parameters = Parameters}} ->
            ExtractActual = fun
                ExtractActual(ParamsMap) when is_map(ParamsMap) ->
                    maps:map(fun (_N, V) -> ExtractActual(V) end, ParamsMap);
                ExtractActual(#config_param{actual = Actual, default = Default, cardinality = map}) ->
                    case maps:get(ConfigVariant, Actual, Default) of
                        undefined -> Default;
                        Other     -> Other
                    end;
                ExtractActual(#config_param{actual = Actual, cardinality = C}) when C =:= opt; C =:= one ->
                    Actual
            end,
            {ok, ExtractActual(Parameters)};
        {ok, Other = #{}} ->
            {error, {not_config, Other}};
        {error, Reason} ->
            {error, Reason}
    end.


%%  @private
%%  get_config returns the entire #config{}
%%  for the specified name.
%%
get_config([], Configs) ->
    {ok, Configs};

get_config([Name | Other], Configs) ->
    case maps:is_key(Name, Configs) of
        true  -> get_config(Other, maps:get(Name, Configs));
        false -> {error, {not_found, [Name | Other]}}
    end.


%%  @private
%%
%%
read_env_config(State = #state{configs = Configs, env_file = EnvFile, env_mtime = EnvMTime}) ->
    case file:read_file_info(EnvFile, [{time, local}]) of
        {ok, #file_info{mtime = EnvMTime}} ->
            {ok, State};
        {ok, #file_info{mtime = MTime}} ->
            lager:notice("Loading env config from file=~p, mtime=~p", [EnvFile, MTime]),
            {ok, EnvFileContents} = file:read_file(EnvFile),
            {ok, EnvProperties} = parse_properties(EnvFileContents),
            EnvConfig = normalize_config(EnvProperties),
            NewConfigs = apply_config(EnvConfig, #config_param.environment, Configs),
            NewState = State#state{
                configs    = NewConfigs,
                env_mtime  = MTime,
                env_config = EnvConfig
            },
            {ok, NewState};
        {error, Reason} ->
            {error, Reason}
    end.


%%  @private
%%  Parse properties from a file similar to the java properties file.
%%  This function returns [{Name :: string(), Value :: string()}].
%%
parse_properties(Contents) ->
    ContentsStr = unicode:characters_to_list(Contents, utf8),
    ContentLines = string:tokens(ContentsStr, "\r\n"),
    StripVal = fun (Val, Dir) ->
        string:strip(string:strip(Val, Dir, $\s), Dir, $\t)
    end,
    ParseLine = fun (Line) ->
        case StripVal(Line, left) of
            [] ->
                false;  % Empty line.
            [$# | _] ->
                false;  % Comment.
            StrippedLine ->
                EqPos = string:chr(StrippedLine, $=),
                Key = StripVal(string:substr(StrippedLine, 1, EqPos - 1), right),
                Val = StripVal(string:substr(StrippedLine, EqPos + 1), both),
                {true, {Key, Val}}
        end
    end,
    {ok, lists:filtermap(ParseLine, ContentLines)}.


%%  @private
%%  Transforms config_values() into nested map. Only
%%  first level of the composite keys are resolved
%%  to nested maps. Other levels are considered as
%%  values and therefore are not transformed.
%%
%%      Input = [
%%          {"registry.conn.type", "mock"},
%%          {[registry, conn, user], "some"}
%%      ]
%%      Input = #{
%%          "registry.conn.type" => "mock",
%%          [registry, conn, user] => "some"
%%      }
%%      Output #{
%%          registry => #{conn => #{
%%              type => "mock",
%%              user => "some"
%%          }},
%%      }
%%
normalize_config(Config) ->
    normalize_config(Config, #{}).


normalize_config(Map, Normalized) when is_map(Map) ->
    maps:fold(fun normalize_config/3, Normalized, Map);

normalize_config(Proplist, Normalized) when is_list(Proplist) ->
    lists:foldl(fun ({K, V}, Norm) ->
        normalize_config(K, V, Norm)
    end, Normalized, Proplist).


normalize_config(Atom, Value, Normalized) when is_atom(Atom) ->
    Normalized#{
        Atom => Value
    };

normalize_config([Atom], Value, Normalized) when is_atom(Atom) ->
    normalize_config(Atom, Value, Normalized);

normalize_config([Atom | Other], Value, Normalized) when is_atom(Atom) ->
    Normalized#{
        Atom => normalize_config(Other, Value, maps:get(Atom, Normalized, #{}))
    };

normalize_config(PropName = [C | _], Value, Normalized) when is_integer(C) ->
    KeyTokens = string:tokens(PropName, "."),
    KeyAtoms = lists:map(fun erlang:list_to_atom/1, KeyTokens),
    normalize_config(KeyAtoms, Value, Normalized).


%%  @private
%%  Convert values to the user specified type.
%%
decode_value(_Type, undefined) -> undefined;
decode_value(boolean, true)    -> true;
decode_value(boolean, false)   -> false;
decode_value(boolean, "true")  -> true;
decode_value(boolean, "false") -> false;
decode_value(string, Value) when is_list(Value)   -> Value;
decode_value(string, Value) when is_binary(Value) -> unicode:characters_to_list(erlang:binary_to_list(Value), utf8);
decode_value(string, Value) when is_atom(Value)   -> erlang:atom_to_list(Value);
decode_value(integer, Value) when is_integer(Value) -> Value;
decode_value(integer, Value) when is_list(Value)    -> erlang:list_to_integer(Value);
decode_value(integer, Value) when is_binary(Value)  -> erlang:binary_to_integer(Value);
decode_value(atom, Value) when is_atom(Value) -> Value;
decode_value(atom, Value) when is_list(Value) -> erlang:list_to_existing_atom(Value);
decode_value(term, Value) when is_list(Value) ->
    {ok, Scanned, _} = erl_scan:string(Value ++ "."),
    {ok, Term} = erl_parse:parse_term(Scanned),
    Term.


%%  @private
%%
%%
add_config_spec([Name], NewConfig, Configs) ->
    Configs#{Name => NewConfig};

add_config_spec([Name | Tail], NewConfig, Configs) ->
    Configs#{Name => add_config_spec(Tail, NewConfig, maps:get(Name, Configs, #{}))}.


%%  @private
%%
%%
merge_values(NewValues, Config) when not is_map(NewValues); not is_map(Config) ->
    NewValues;

merge_values(NewValues, Config) when is_map(NewValues), is_map(Config) ->
    maps:merge(Config, maps:map(fun (N, V) ->
        case maps:is_key(N, Config) of
            true  -> merge_values(V, maps:get(N, Config));
            false -> V
        end
    end, NewValues)).


%%  @private
%%  Update config with given values.
%%
%%  All the values, not provided in the NewValues, will be cleared
%%  in the configs.
%%

% Handle particular #config{}.
apply_config(NewValues, Field, Config = #config{parameters = Parameters}) ->
    ApplyParams = fun
        ApplyParams(NV, ConfigParam = #config_param{}) ->
            apply_config(NV, Field, ConfigParam);
        ApplyParams(undefined, ParamsMap) when is_map(ParamsMap) ->
            maps:map(fun (_N, V) -> ApplyParams(undefined, V) end, ParamsMap);
        ApplyParams(NV, ParamsMap) when is_map(NV), is_map(ParamsMap) ->
            maps:map(fun (N, V) -> ApplyParams(maps:get(N, NV, undefined), V) end, ParamsMap)
    end,
    Config#config{
        parameters = ApplyParams(NewValues, Parameters)
    };

% Handle particular #config_param{}.
apply_config(NewValue, Field, ConfigParam = #config_param{type = Type, cardinality = map}) ->
    DecodedValue = case NewValue of
        #{}       -> maps:map(fun (_N, V) -> decode_value(Type, V) end, NewValue);
        undefined -> undefined;
        _Other    -> #{default => decode_value(Type, NewValue)}
    end,
    TmpConfigParam = erlang:setelement(Field, ConfigParam, DecodedValue),
    UndefEmpty = fun
        (undefined)            -> #{};
        (Map) when is_map(Map) -> Map;
        (Other)                -> #{default => Other}
    end,
    #config_param{
        runtime     = Runtime,
        environment = Environment,
        static      = Static,
        default     = Default
    } = TmpConfigParam,
    NewActual = lists:foldl(
        fun (V, A) -> maps:merge(A, UndefEmpty(V)) end,
        #{}, [Default, Static, Environment, Runtime]
    ),
    TmpConfigParam#config_param{
        actual = NewActual
    };

apply_config(NewValue, Field, ConfigParam = #config_param{type = Type, cardinality = C}) when C =:= opt; C =:= one ->
    DecodedValue = decode_value(Type, NewValue),
    TmpConfigParam = erlang:setelement(Field, ConfigParam, DecodedValue),
    NewActual = case TmpConfigParam of
        #config_param{runtime     = Val} when Val =/= undefined -> Val;
        #config_param{environment = Val} when Val =/= undefined -> Val;
        #config_param{static      = Val} when Val =/= undefined -> Val;
        #config_param{default     = Val}                        -> Val
    end,
    TmpConfigParam#config_param{
        actual = NewActual
    };

% Traverse the tree of configs.
apply_config(undefined, Field, Config) when is_map(Config) ->
    maps:map(fun (_N, V) -> apply_config(undefined, Field, V) end, Config);

apply_config(NewValues, Field, Config) when is_map(NewValues), is_map(Config) ->
    maps:map(fun (N, V) ->
        apply_config(maps:get(N, NewValues, undefined), Field, V)
    end, Config).



%%% ============================================================================
%%% Unit tests.
%%% ============================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

parse_properties_test_() ->
    [
        {"Basic functionality.", ?_assertEqual(
            {ok, [{"registry.conn.type", "mock"}]},
            parse_properties([
                <<"###\n">>,
                <<"registry.conn.type=mock\n">>,
                <<"#registry.conn.type=mysql\n">>
            ])
        )},
        {"Checking spacing.", ?_assertEqual(
            {ok, [{"registry.conn.type", "mock"}]},
            parse_properties([
                <<" \n">>,
                <<"\n">>,
                <<"  registry.conn.type   =   mock   ">>
            ])
        )},
        {"Couple properties.", ?_assertEqual(
            {ok, [
                    {"registry.conn.type", "mock"},
                    {"registry.conn.type", "mysql"}
                ]},
            parse_properties([
                <<"###\n">>,
                <<"registry.conn.type=mock\n">>,
                <<"registry.conn.type=mysql\n">>
            ])
        )},
        {"Non ASCII letters.", ?_assertEqual(
            {ok, [
                {"registry.conn.typę", "mock"},
                {"registry.conn.type", "mįsql"}
            ]},
            parse_properties([
                <<"###\n">>,
                <<"registry.conn.typ", 16#c499:16, "=mock\n">>,             %binary representation of ę
                <<"registry.conn.type=m", 16#c4af:16, "sql\n">>             %binary representation of į
            ])
        )}
    ].


normalize_config_test_()->
    [
        {"Basic Properties", ?_assertEqual(
            #{
                registry => #{
                    conn => #{
                        type => "mock",
                        user => "some"
                    }
                }
            },
            normalize_config(
                [
                    {"registry.conn.type", "mock"},
                    {"registry.conn.user", "some"}
                ]
            )
        )},
        {"Basic Properties with list", ?_assertEqual(
            #{
                registry => #{
                    conn => #{
                        type => "mock",
                        user => "some"
                    }
                }
            },
            normalize_config(
                [
                    {"registry.conn.type", "mock"},
                    {[registry, conn, user], "some"}
                ]
            )
        )},
        {"Basic Properties with map", ?_assertEqual(
            #{
                registry => #{
                    conn => #{
                        type => "mock",
                        user => "some"
                    }
                }
            },
            normalize_config(
                #{
                    "registry.conn.type" => "mock",
                    [registry, conn, user] => "some"
                }
            )
        )},
        {"Emty List Of Properties", ?_assertEqual(
            #{},
            normalize_config([])
        )},
        {"Identical Properties", ?_assertEqual(
            #{
                registry => #{
                    conn => #{
                        type => "mock"
                    }
                }
            },
            normalize_config(
                [
                    {"registry.conn.type", "mock"},
                    {"registry.conn.type", "mock"}
                ]
            )
        )},
        {"Overwrite Properties", ?_assertEqual(
            #{
                registry => #{
                    conn => #{
                        type => "new_mock"
                    }
                }
            },
            normalize_config(
                [
                    {"registry.conn.type", "mock"},
                    {"registry.conn.type", "new_mock"}
                ]
            )
        )}
    ].


get_actual_test_()->
    Config = #config{parameters = #{
        x => #config_param{actual = 3},
        y => #{yy => #config_param{actual = 14}}
    }},
    ConfigCardinality = #config{parameters = #{
        x => #config_param{actual = 3},
        y => #config_param{actual = #{yy => 23, zz => 34}, cardinality = map, default = 12}
    }},
    WrongConfig = #{parameters => #{
                    x => #config_param{actual = 3},
                    y => #{yy => #config_param{actual = 14}}
    }},
    [
        {"Actual config", ?_assertEqual(
            {ok, #{x => 3, y => #{yy => 14}}},
            get_actual(
                [a, b], default,
                #{a => #{b => Config}}
            )
        )},
        {"Not config", ?_assertEqual(
            {error, {not_config, WrongConfig}},
            get_actual(
                [], default,
                WrongConfig
            )
        )},
        {"Config with cardinality", ?_assertEqual(
            {ok, #{x => 3, y => 23}},
            get_actual(
                [a, b], yy,
                #{a => #{b => ConfigCardinality}}
            )
        )},
        {"Config with cardinality (default value)", ?_assertEqual(
            {ok, #{x => 3, y => 12}},
            get_actual(
                [a, b], default,
                #{a => #{b => ConfigCardinality}}
            )
        )},
        {"Config with cardinality (undefined key)", ?_assertEqual(
            {ok, #{x => 3, y => 12}},
            get_actual(
                [a, b], wrong_key,
                #{a => #{b => ConfigCardinality}}
            )
        )}
    ].


get_config_test_() ->
    Config = #config{parameters = #{
        x => #config_param{actual = 3},
        y => #{yy => #config_param{actual = 14}}
    }},
    [
        {"Config", ?_assertEqual(
            {ok, Config},
            get_config(
                [a, b],
                #{a => #{b => Config}})
        )},
        {"Multiple", ?_assertEqual(
            {ok, #{b => Config}},
            get_config(
                [a],
                #{a => #{b => Config}}
            )
        )},
        {"Not found", ?_assertEqual(
            {error, {not_found, [z]}},
            get_config(
                [a, z],
                #{a => #{b => Config}}
            )
        )}
    ].


merge_values_test_() ->
    Config = #{a => #{b => 2}, x => 7},
    NewValues = #{a => #{b => 3, c => 4}},
    UndefinedValues = #{a => undefined},
    [
        {"Basic", ?_assertEqual(
            #{ a => #{b => 3, c => 4}, x => 7},
            merge_values(NewValues, Config)
        )},
        {"Undefined Values", ?_assertEqual(
            #{ a => undefined, x => 7},
            merge_values(UndefinedValues, Config)
        )}
    ].


apply_config_test_() ->
    Field = #config_param.runtime,
    Config = #{a => #{b => #config{
        parameters = #{
            host => #config_param{
                type = boolean
            }
        }
    }}},
    NewValues = #{a => #{b => #{host => "false"}}},
    NewConfig = #{a => #{b => #config{
        parameters = #{
            host => #config_param{
                type = boolean,
                runtime = false,
                actual = false
            }
        }
    }}},
    [
        {"Basic", ?_assertEqual(
            NewConfig,
            apply_config(NewValues, Field, Config)
        )},
        {"Undefined", ?_assertEqual(
            Config,
            apply_config(#{a => undefined}, Field, NewConfig)
        )},
        {"Empty map", ?_assertEqual(
            Config,
            apply_config(#{a => #{}}, Field, NewConfig)
        )},
        {"Deep config", ?_assertEqual(
            #{a => #{b => #config{
                parameters = #{
                    host => #config_param{
                        type = boolean,
                        runtime = false,
                        actual = false
                    },
                    d => #{k => #config_param{
                        type = integer,
                        runtime = 12,
                        actual = 12
                    }}
                }
            }}},
            apply_config(
                #{a => #{b => #{host => "false", d => #{k => 12}}}},
                Field,
                #{a => #{b => #config{
                    parameters = #{
                        host => #config_param{
                            type = boolean
                        },
                        d => #{k => #config_param{
                            type = integer
                        }}
                    }
                }}}
            )
        )},
        {"Config for paratemer with cardinality=map, implicit default value", ?_assertMatch(
            #{adapter_a := #{channel_b := #config{
                parameters = #{
                    base_dir := #config_param{
                        actual = #{default := "dir_for_default"}
                    }
                }
            }}},
            apply_config(
                #{adapter_a => #{channel_b => #{base_dir => "dir_for_default"}}},
                Field,
                #{adapter_a => #{channel_b => #config{
                    parameters = #{
                        base_dir => #config_param{
                            type        = string,
                            cardinality = map
                        }
                    }
                }}}
            )
        )},
        {"Config for paratemer with cardinality=map, explicit default value", ?_assertMatch(
            #{adapter_a := #{channel_b := #config{
                parameters = #{
                    base_dir := #config_param{
                        actual = #{default := "dir_for_default"}
                    }
                }
            }}},
            apply_config(
                #{adapter_a => #{channel_b => #{base_dir => #{default => "dir_for_default"}}}},
                Field,
                #{adapter_a => #{channel_b => #config{
                    parameters = #{
                        base_dir => #config_param{
                            type        = string,
                            cardinality = map
                        }
                    }
                }}}
            )
        )}
    ].


decode_value_test_() ->
    [
        {"Term functionality (integer).", ?_assertEqual(
            12,
            decode_value(integer, "12")
        )},
        {"Term functionality (boolean).", ?_assertEqual(
            true,
            decode_value(boolean, "true")
        )},
        {"Term functionality (string).", ?_assertEqual(
            "value",
            decode_value(string, "value")
        )},
        {"Term functionality (atom).", ?_assertEqual(
            value,
            decode_value(atom, "value")
        )},
        {"Term functionality (term).", ?_assertEqual(
            #{a => #{b => #{host => 34, d => #{k => 12}}}},
            decode_value(term, "#{a => #{b => #{host => 34, d => #{k => 12}}}}")
        )}
    ].


add_config_spec_test_() ->
    Config = #config{
        parameters = #{
            host => #config_param{
                type = string,
                default = "somehost"
            }
        }
    },
    NewConfigs = #{a => #{b => Config}},
    [
        {"Adding new config parameters.", ?_assertEqual(
            NewConfigs,
            add_config_spec([a, b], Config, #{})
        )},
        {"Adding new config parameters.", ?_assertEqual(
            #{a => #{
                b => Config,
                c => Config
            }},
            add_config_spec([a, c], Config, NewConfigs)
        )},
        {"Do not allow to overwrite the configs.", ?_assertError(
            _AnyError,
            add_config_spec([a, b, c], Config, NewConfigs)
        )}
    ].


-endif.


