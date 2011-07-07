-module(simple).
-parse_trans(verbose).
-include("../../include/inline_xml.hrl").
-export([test/0]).

test() ->
    X = '<foo attr="baz"><bar>x</bar><bar>y</bar></foo>',
    io:format("X = ~p~n", [X]),
    _Y = case X of
        '<foo attr="baz"><bar>x</bar><bar>y</bar></foo>' -> match;
        Other -> {nomatch, Other}
    end.
