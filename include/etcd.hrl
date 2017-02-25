-record(etcd_read_opts, {
    key = undefined,
    wait = false,
    recursive = false,
    sorted = false,
    modified_index = undefined,
    quorum = undefined
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
