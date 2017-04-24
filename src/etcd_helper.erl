-module(etcd_helper).

-export([get_value_from_body/1]).

get_value_from_body(String) ->
    {Body} = jiffy:decode(String),
    case proplists:get_value(<<"node">>, Body) of
        {Node} ->  
            proplists:get_value(<<"value">>, Node);
        _ -> undefined
    end.
