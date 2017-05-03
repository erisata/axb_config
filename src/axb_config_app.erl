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
%%% The `AxB Config' OTP Application.
%%%
-module(axb_config_app).
-behaviour(application).
-export([start/2, stop/1]).
-export([name/0, version/0, get_env/1, get_env/2]).

-define(APP, axb_config).


%%% ============================================================================
%%% Public API.
%%% ============================================================================

%%  @doc
%%  Returns application name.
%%
-spec name() -> atom().

name() ->
    ?APP.


%%  @doc
%%  Returns version of the application.
%%
version() ->
    case lists:keyfind(?APP, 1, application:which_applications()) of
        {_App, _Type, Version}  -> Version;
        false                   -> undefined
    end.


%%  @doc
%%  Returns environment parameter of this application.
%%
-spec get_env(Name :: atom()) -> undefined | {ok, Value :: term()}.

get_env(Name) ->
    application:get_env(?APP, Name).


%%  @doc
%%  Returns environment parameter of this application.
%%
-spec get_env(Name :: atom(), Default :: term()) -> Value :: term().

get_env(Name, Default) ->
    application:get_env(?APP, Name, Default).



%%% ============================================================================
%%% Application callbacks
%%% ============================================================================

%%  @doc
%%  Start the application.
%%
start(_StartType, _StartArgs) ->
    axb_config_sup:start_link().


%%  @doc
%%  Stop the application.
%%
stop(_State) ->
    ok.



%%% ============================================================================
%%% Helper functions.
%%% ============================================================================


