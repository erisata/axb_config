%%%
%%%
%%%
-module(axb_config_SUITE).
-compile([{parse_transform, lager_transform}]).
-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).
-export([
    test_basic/1,
    test_reload_config/1
]).
-include_lib("common_test/include/ct.hrl").
-include("axb_config.hrl").


%%% ============================================================================
%%% API for `common_test`.
%%% ============================================================================

%%
%%  List of testcases.
%%
all() -> [
    test_basic,
    test_reload_config
    ].


%%
%%  Suite nitialization.
%%
init_per_suite(Config) ->
    ok = application:set_env(lager, log_root, "../", [{persistent, true}]), % The `logs' dir.
    Config.


%%
%%  Suite cleanup.
%%
end_per_suite(_Config) ->
    ok.


%%
%%  Log test case name at start
%%
init_per_testcase(TestCase, Config) ->
    lager:debug("-------------------------------------- ~p start", [TestCase]),
    Config.


%%
%%  Log test case name at end
%%
end_per_testcase(TestCase, _Config) ->
    lager:debug("-------------------------------------- ~p end", [TestCase]),
    ok.



%%% ============================================================================
%%% Testcases.
%%% ============================================================================

%%
%%
%%
test_basic(_Config) ->
    {ok, Pid} = axb_config:start_link(#{"registry.conn.host" => "localhost"}),
    {ok, #{}} = axb_config:get_config_info([]),
    {error, {not_found, [a]}} = axb_config:get_config_info([a]),
    %
    % Check if register config correctly registers config.
    ok = axb_config:register_config(
        [registry, conn],
        #axb_config{parameters = #{host => #axb_config_param{type = string, default = "nowhere"}}}
    ),
    {ok, #axb_config{parameters = #{host := #axb_config_param{
        type = string,
        default = "nowhere",
        actual = "localhost",
        environment = "localhost"
    }}}} = axb_config:get_config_info([registry, conn]),
    {ok, #{host := "localhost"}} = axb_config:get_actual_config([registry, conn]),
    %
    % Check if runtime config overrides other values.
    ok = axb_config:set_runtime_config(#{"registry.conn.host" => "somehost"}),
    {ok, #axb_config{parameters = #{host := #axb_config_param{
        type = string,
        default = "nowhere",
        runtime = "somehost",
        actual = "somehost",
        environment = "localhost"
    }}}} = axb_config:get_config_info([registry, conn]),
    {ok, #{host := "somehost"}} = axb_config:get_actual_config([registry, conn]),
    %
    % Exit test
    true = erlang:unlink(Pid),
    true = erlang:exit(Pid, kill),
    ok = timer:sleep(100),
    ok.


%%
%%
%%
test_reload_config(Config) ->
    %
    % Create a private directory
    PrivDir = proplists:get_value(priv_dir, Config),
    %
    % Create a config file in the private directory
    ConfigFile = filename:join(PrivDir, "config.properties"),
    %
    % Set config values in the config file
    ok = file:write_file(ConfigFile, <<"registry.conn.host=localhost\n">>),
    ok = application:set_env(axb_config, env_file, ConfigFile),
    {ok, Pid} = axb_config:start_link(),
    %
    % Check if register config correctly registers config.
    ok = axb_config:register_config(
        [registry, conn],
        #axb_config{parameters = #{host => #axb_config_param{type = string, default = "nowhere"}}}
    ),
    {ok, #axb_config{parameters = #{host := #axb_config_param{
        type = string,
        default = "nowhere",
        actual = "localhost",
        environment = "localhost"
    }}}} = axb_config:get_config_info([registry, conn]),
    {ok, #{host := "localhost"}} = axb_config:get_actual_config([registry, conn]),
    %
    % Set new config values in the config file
    ok = file:write_file(ConfigFile, <<"registry.conn.host=newhost\n">>),
    %
    % Reload new config values from the config file
    ok = axb_config:reload_env_config(),
    %
    % Check if new config is reloaded correctly registers config.
    {ok, #axb_config{parameters = #{host := #axb_config_param{
        type = string,
        default = "nowhere",
        actual = "newhost",
        environment = "newhost"
    }}}} = axb_config:get_config_info([registry, conn]),
    {ok, #{host := "newhost"}} = axb_config:get_actual_config([registry, conn]),
    %
    % Exit test
    true = erlang:unlink(Pid),
    true = erlang:exit(Pid, kill),
    ok = timer:sleep(100),
    ok.

%%
%%
%%

