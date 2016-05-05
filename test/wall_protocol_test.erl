%% Tests for wall protocol
%% @author Paul Popoff
%% @copyright 2016 Paul Popoff
%% @private

-module(wall_protocol_test).
-include_lib("eunit/include/eunit.hrl").


%% Test parameters
succeed() -> ok.
%fail() -> throw(failed).

suceeding_test() ->
    succeed().


%% ?assert_Exception: google it
math_test() ->
  [?_assert(1+1 =:= 2),
   ?_assert(1+2 =:= 3),
   ?_assert(2+2 =:= 4)
  ].

