-record(etcd_read_opts, {
    recursive = false,
    sorted = false}).
-record(etcd_modify_opts, {
    recursive = false,
    prev_value = undefined,
    refresh = undefined,
    prev_index = undefined,
    prev_exist = undefined,
    dir = undefined}).
