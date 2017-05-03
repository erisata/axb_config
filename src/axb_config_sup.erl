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

%%% @private
%%% Main supervisor for the application.
%%%
-module(axb_config_sup).
-behaviour(supervisor).
-compile([{parse_transform, lager_transform}]).
-export([start_link/0]).
-export([init/1]).


%%% ============================================================================
%%% API functions.
%%% ============================================================================

%%  @doc
%%  Create this supervisor.
%%
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, {}).



%%% ============================================================================
%%% Callbacks for supervisor.
%%% ============================================================================

%%  @doc
%%  Supervisor initialization.
%%
init({}) ->
    SupFlags = #{
        strategy  => one_for_all,
        intensity => 10,
        period    => 10
    },
    ConfigSpec = #{
        id       => axb_config,
        start    => {axb_config, start_link, []},
        restart  => permanent,
        shutdown => 100,
        type     => worker,
        modules  => [axb_config]
    },
    {ok, {SupFlags, [ConfigSpec]}}.


