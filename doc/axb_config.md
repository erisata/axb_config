

# Module axb_config #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

This module provides the interface to the axb_config application.

__Behaviours:__ [`gen_server`](gen_server.md).

<a name="description"></a>

## Description ##
It allows components to register own configuration and retrieve
the actual values.

<a name="types"></a>

## Data Types ##




### <a name="type-config_values">config_values()</a> ###


<pre><code>
config_values() = [{Name::string() | [atom()] | atom(), Value::term()}] | #{Name::string() | [atom()] | atom() =&gt; Value::term()}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#code_change-3">code_change/3</a></td><td></td></tr><tr><td valign="top"><a href="#get_actual_config-1">get_actual_config/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_actual_config-2">get_actual_config/2</a></td><td></td></tr><tr><td valign="top"><a href="#get_config_info-1">get_config_info/1</a></td><td></td></tr><tr><td valign="top"><a href="#handle_call-3">handle_call/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_cast-2">handle_cast/2</a></td><td></td></tr><tr><td valign="top"><a href="#handle_info-2">handle_info/2</a></td><td></td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#register_config-2">register_config/2</a></td><td></td></tr><tr><td valign="top"><a href="#reload_env_config-0">reload_env_config/0</a></td><td></td></tr><tr><td valign="top"><a href="#set_runtime_config-1">set_runtime_config/1</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-1">start_link/1</a></td><td></td></tr><tr><td valign="top"><a href="#terminate-2">terminate/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="code_change-3"></a>

### code_change/3 ###

`code_change(OldVsn, State, Extra) -> any()`

<a name="get_actual_config-1"></a>

### get_actual_config/1 ###

<pre><code>
get_actual_config(ConfigPath::[atom()]) -&gt; {ok, #{}}
</code></pre>
<br />

<a name="get_actual_config-2"></a>

### get_actual_config/2 ###

<pre><code>
get_actual_config(ConfigPath::[atom()], ConfigVariant::atom()) -&gt; {ok, #{}}
</code></pre>
<br />

<a name="get_config_info-1"></a>

### get_config_info/1 ###

<pre><code>
get_config_info(ConfigPath::[atom()]) -&gt; {ok, #config{} | #{}}
</code></pre>
<br />

<a name="handle_call-3"></a>

### handle_call/3 ###

`handle_call(Unknown, From, State) -> any()`

<a name="handle_cast-2"></a>

### handle_cast/2 ###

`handle_cast(Event, State) -> any()`

<a name="handle_info-2"></a>

### handle_info/2 ###

`handle_info(Unknown, State) -> any()`

<a name="init-1"></a>

### init/1 ###

`init(X1) -> any()`

<a name="register_config-2"></a>

### register_config/2 ###

<pre><code>
register_config(ConfigPath::[atom()], ConfigSpec::#config{}) -&gt; ok | {error, Reason::term()}
</code></pre>
<br />

<a name="reload_env_config-0"></a>

### reload_env_config/0 ###

<pre><code>
reload_env_config() -&gt; ok
</code></pre>
<br />

<a name="set_runtime_config-1"></a>

### set_runtime_config/1 ###

<pre><code>
set_runtime_config(RuntimeValues::<a href="#type-config_values">config_values()</a>) -&gt; ok
</code></pre>
<br />

<a name="start_link-0"></a>

### start_link/0 ###

<pre><code>
start_link() -&gt; {ok, Pid::pid()} | ignore | {error, Reason::term()}
</code></pre>
<br />

<a name="start_link-1"></a>

### start_link/1 ###

<pre><code>
start_link(ConstConfig::<a href="#type-config_values">config_values()</a>) -&gt; {ok, Pid::pid()} | ignore | {error, Reason::term()}
</code></pre>
<br />

<a name="terminate-2"></a>

### terminate/2 ###

`terminate(Reason, State) -> any()`

