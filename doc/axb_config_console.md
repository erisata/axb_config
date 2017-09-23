

# Module axb_config_console #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

AxB Config Console implementation.

<a name="description"></a>

## Description ##
Functions in this module are invoked from the shell script.
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#completion-1">completion/1</a></td><td></td></tr><tr><td valign="top"><a href="#config-1">config/1</a></td><td>
This function handles all the console commands.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="completion-1"></a>

### completion/1 ###

`completion(X1) -> any()`

<a name="config-1"></a>

### config/1 ###

`config(X1) -> any()`

This function handles all the console commands.
The following are examples of its usage assuming the command is config.

`config some`                   -- shows all configs under this path.
`config some.config`            -- shows the specified config.
`config some.config.p1 v1`      -- sets parameter `p1` to the value `v1`.
`config some.config.p1 --reset` -- remove the runtime config.
`config some --reset`           -- remove the all the runtime configs under this path.
`config --reset`                -- remove the all the runtime configs.
`config --reload`               -- reload config from the environment file.

