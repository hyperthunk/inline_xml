-module(custom).
-compile(export_all).
-parse_trans(verbose).
-xml_api({erlsom, simple_form, []}).
-include_lib("inline_xml/include/inline_xml.hrl").

demo() ->
    Xml = '<foo attr="baz"><bar>x</bar><bar>y</bar></foo>',
    io:format("Xml: ~p~n", [Xml]),
    {"foo",
         [{"attr","baz"}],
         [{"bar",[],["x"]},{"bar",[],["y"]}]} = Xml.
