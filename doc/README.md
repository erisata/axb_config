

# AxB Config #

This application allows to maintain configuration parameters
by having a baseline configuration in the source code and
allowing to overridide separate parameters. The configuration
values are taken from various sources:

* `default` -- are the values, that are common for all the
instances of the configuration. E.g. port=22 for all the
ssh connections by default.

* `static` -- are the values provided by the developer and
hardcoded into the source code (not including the sensitive
data). E.g. host="some.org". These are provided in order
to minimize the ammount of fields an administrator must
provide. It can be considered as a baseline configuration.

* `system` -- are the values read from the sys.config of the
release. It can be considered as a release-specific
static/default configuration provided by the packager.

* `environment` -- are the values read for the application
    environment (file system). In most cases it will be a
    file in the `/etc` directory. This file is edited by The
administrator.

* `runtime` -- are provided by the administrator at runtime,
to override the configuration for a short time.

The application components can retrieve the actual configuration.
The actual configuration is derived by merging all the sources
by priority as listed above (runtime values have highest priority).


## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="axb_config.md" class="module">axb_config</a></td></tr>
<tr><td><a href="axb_config_console.md" class="module">axb_config_console</a></td></tr></table>

