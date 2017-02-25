-record(etcd_read_opts, {
    key = undefined,
    wait = false,
    recursive = false,
    sorted = false,
    modified_index = undefined
    }).
-record(etcd_modify_opts, {
    ttl = undefined,        %% for udpate only
    key = undefined,
    value = undefined,      %% for update only
    recursive = false,      %% for delete only
    refresh = undefined,
    prev_value = undefined,
    prev_index = undefined,
    prev_exist = undefined,
    dir = undefined
    }).
