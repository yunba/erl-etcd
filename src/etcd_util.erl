-module(etcd_util).

-export([url_encode/1]).
-export([make_sure_list/1, make_sure_binary/1]).

url_encode(Src) ->
    RemovePlus = binary:replace(list_to_binary(Src), <<"+">>, <<"%2B">>, [global]),
    ReplaceSpace = binary:replace(RemovePlus, <<" ">>, <<"+">>, [global]),
    Ret = binary_to_list(ReplaceSpace),
    Ret.

make_sure_binary(Data) ->
    if
        is_list(Data) ->
            list_to_binary(Data);
        is_integer(Data) ->
            integer_to_binary(Data);
        is_atom(Data) ->
            atom_to_binary(Data, latin1);
        is_float(Data) ->
            float_to_binary(Data);
        true ->
            Data
    end.

make_sure_list(Data) ->
    if
        is_binary(Data) ->
            binary_to_list(Data);
        is_integer(Data) ->
            integer_to_list(Data);
        is_atom(Data) ->
            atom_to_list(Data);
        is_float(Data) ->
            float_to_list(Data);
        true ->
            Data
    end.

