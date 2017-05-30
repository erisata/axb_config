
%%
%%  Configuration parameter.
%%
-record(axb_config_param, {
   %name                :: [atom()],        % Name of the parameter is stored as a name in the #config{parameters} map.
    type                :: string | integer | boolean | enum | term,   % Type of the values.
    description         :: string(),        % Description of the parameter.
    cardinality = one   :: opt | one | map, % Parameter is optional, shoutd have single value or a map(config => val).
    actual              :: term(),          % Actual, effective value of the parameter.
    runtime             :: term(),          % Value, as provided at runtime, if any.
    environment         :: term(),          % Value, configured in the node's environment, if any.
    static              :: term(),          % Value, hardcoded in the component, if any.
    default             :: term()           % Default value, if any.
}).


%%
%%  Configuration structure, used by a particular component.
%%
-record(axb_config, {
   %config_name :: [atom()],                % Name is maintained separatelly, in a map; see axb_config.
    config_type :: ftp | sftp | atom(),     % Type of the configuration as a whole.
    description :: string(),                % Description of the configuration.
    parameters  :: #{Name :: atom() => (Value :: term()) | (NestedParams :: map())}
}).


