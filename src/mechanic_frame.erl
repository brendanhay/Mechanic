-module(mechanic_frame).


-export([unframe/1, reframe/1]).


-spec unframe(list() | binary()) -> list().    

unframe([0|T]) -> unframe_tail(T);
unframe(<<0, T/binary>>) -> unframe_tail(T, <<>>);
unframe(Any) -> Any.

unframe_tail([255]) -> [];
unframe_tail([H|T]) -> [H|unframe_tail(T)].

unframe_tail(<<255>>, Acc) -> Acc;
unframe_tail(<<H, T/binary>>, Acc) -> unframe_tail(T, <<Acc/binary, H>>).


-spec reframe(list()) -> list().    

reframe(List) when is_list(List) ->
    [0, List, 255];
reframe(Bin) when is_binary(Bin) ->
    <<0, Bin/binary, 255>>.
%    Size = size(Bin),
%    <<128, Size, Bin/binary>>.
 
