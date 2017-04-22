-module(etcd_util).

-export([url_encode/1]).

url_encode(Src) ->
    RemovePlus = binary:replace(list_to_binary(Src), <<"+">>, <<"%2B">>, [global]),
    ReplaceSpace = binary:replace(RemovePlus, <<" ">>, <<"+">>, [global]),
    Ret = binary_to_list(ReplaceSpace),
    Ret.
