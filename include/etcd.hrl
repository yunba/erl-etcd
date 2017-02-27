-record(etcd_read_opts, {
    key = ""  :: list(),
    wait = undefined :: undefined | boolean(),
    recursive = undefined :: undefined | boolean(),
    sorted = undefined :: undefined | boolean(),
    modified_index = undefined :: undefined | integer(),
    quorum = undefined %% not sure about how to use it yet
    }).
-record(etcd_modify_opts, {
    key = "" :: list(),
    ttl = undefined :: integer() | undefined,            %% for udpate only
    refresh = undefined :: boolean() | undefined,        %% for update only
    value = undefined :: list() | undefined,             %% for update only
    recursive = undefined :: boolean() | undefined,      %% for delete only
    dir = undefined :: boolean() | undefined,
    prev_exist = undefined :: boolean() | undefined,
    prev_value = undefined :: list() | undefined,
    prev_index = undefined :: integer() | undefined
    }).
