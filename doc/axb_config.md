

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


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#get_actual_config-1">get_actual_config/1</a></td><td>Equivalent to <a href="#get_actual_config-2"><tt>get_actual_config(ConfigPath, default)</tt></a>.</td></tr><tr><td valign="top"><a href="#get_actual_config-2">get_actual_config/2</a></td><td>
Returns actual/effective configuration values, that should be used
by the configured components.</td></tr><tr><td valign="top"><a href="#get_config_info-1">get_config_info/1</a></td><td>
Returns all the configs, under the specified path.</td></tr><tr><td valign="top"><a href="#register_config-2">register_config/2</a></td><td>
Register configuration spec to the specific path.</td></tr><tr><td valign="top"><a href="#reload_env_config-0">reload_env_config/0</a></td><td>
Reloads environment configuration, re-reads it from the file.</td></tr><tr><td valign="top"><a href="#set_runtime_config-1">set_runtime_config/1</a></td><td>
Set config values at runtime.</td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td>
Start the config service.</td></tr><tr><td valign="top"><a href="#start_link-1">start_link/1</a></td><td>
Start the config service with initial
configuration set to ConstConfig.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="get_actual_config-1"></a>

### get_actual_config/1 ###

<pre><code>
get_actual_config(ConfigPath::[atom()]) -&gt; {ok, #{}}
</code></pre>
<br />

Equivalent to [`get_actual_config(ConfigPath, default)`](#get_actual_config-2).

<a name="get_actual_config-2"></a>

### get_actual_config/2 ###

<pre><code>
get_actual_config(ConfigPath::[atom()], ConfigVariant::atom()) -&gt; {ok, #{}}
</code></pre>
<br />

Returns actual/effective configuration values, that should be used
by the configured components. The path should point to the particular
configuration, that was previously configured via register_config/2.

The configuration is returned as a map, with keys as atoms, and
values of types, as specified in the #axb_config_param.type.

<a name="get_config_info-1"></a>

### get_config_info/1 ###

<pre><code>
get_config_info(ConfigPath::[atom()]) -&gt; {ok, #axb_config{} | #{}}
</code></pre>
<br />

Returns all the configs, under the specified path. The function
returns particular #axb_config{}, if the ConfigPath points it directly,
or a map of configs under the specified preffix.

<a name="register_config-2"></a>

### register_config/2 ###

<pre><code>
register_config(ConfigPath::[atom()], ConfigSpec::#axb_config{}) -&gt; ok | {error, Reason::term()}
</code></pre>
<br />

Register configuration spec to the specific path.

<a name="reload_env_config-0"></a>

### reload_env_config/0 ###

<pre><code>
reload_env_config() -&gt; ok
</code></pre>
<br />

Reloads environment configuration, re-reads it from the file.

<a name="set_runtime_config-1"></a>

### set_runtime_config/1 ###

<pre><code>
set_runtime_config(RuntimeValues::<a href="#type-config_values">config_values()</a>) -&gt; ok
</code></pre>
<br />

Set config values at runtime.
The values can be reset by setting it (or any of its parents) to undefined.

<a name="start_link-0"></a>

### start_link/0 ###

<pre><code>
start_link() -&gt; {ok, Pid::pid()} | ignore | {error, Reason::term()}
</code></pre>
<br />

Start the config service.
No rutime values are specified at the startup.

<a name="start_link-1"></a>

### start_link/1 ###

<pre><code>
start_link(ConstConfig::<a href="#type-config_values">config_values()</a>) -&gt; {ok, Pid::pid()} | ignore | {error, Reason::term()}
</code></pre>
<br />

Start the config service with initial
configuration set to ConstConfig.

