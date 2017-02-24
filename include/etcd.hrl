-record(etcd_read_opts, {
    key = undefined,
    wait = false,
    recursive = false,
    sorted = false}).
-record(etcd_modify_opts, {
    key = undefined,
    value = undefined,
    recursive = false,
    prev_value = undefined,
    refresh = undefined,
    prev_index = undefined,
    prev_exist = undefined,
    dir = undefined}).
