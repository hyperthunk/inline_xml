# Inline XML Parse Transform Utility

An inline_xml parse transform, based loosely on [this](http://www.google.com).

## Usage

Simply include the relevant header file and you're good to go:

```erlang
-module(testmod).
-compile(export_all).
-include_lib("inline_xml/include/inline_xml.hrl").

demo() ->
    Xml = '<foo><bar name="123" /></foo>',
    {xmlElement,foo,foo,[],
        {xmlNamespace,[],[]},
        [],1,[],
        [{xmlElement,bar,bar,[],
              {xmlNamespace,[],[]},
              [{foo,1}],
              1,
              [{xmlAttribute,name,[],[],[],[],1,[],"123",false}],
              [],[],_,
              undeclared}],
        [],_,undeclared} = Xml.
    io:format("~p~n", [Xml]).

```

The parse transform will currently handle (i.e., inline) XML (atoms) in each of
the following situations:

1. In an assignment (i.e., match) operation such as X = '<xml />'
2. In a match clause of a case expression

Example use cases are provided in the `examples` subdirectory.

### Alternative XML Libraries

By default, inline_xml uses the standard erlxml library to convert your *atom*
into an AST during the parse\_transform. You *can* tell inline_xml to use another
library instead, by defining the alternative API as an *M/F/A* triple in your
module like so:

```erlang
-module(custom).
-compile(export_all).
-xml_api({erlsom, simple_form, []}).
-include_lib("inline_xml/include/inline_xml.hrl").

demo() ->
    Xml = '<foo attr="baz"><bar>x</bar><bar>y</bar></foo>',
    {"foo",
         [{"attr","baz"}],
         [{"bar",[],["x"]},{"bar",[],["y"]}]} = Xml.
    io:format("~p~n", [Xml]).

```

In fact, the erlsom library has some additional support available, for example the
use of XML schemas (for use in erlsom's data binding mode) is supported. You can
enable this support by including the requisite header file and declaring your XSD
files in the following manner.

```erlang
-module(custom).
-compile(export_all).
-import_xsd(["priv/xsd/file1.xsd", "priv/xsd/file2.xsd"]).
-include_lib("inline_xml/include/inline_erlsom.hrl").

demo() ->
    Xml = '<foo attr="baz"><bar>x</bar><bar>y</bar></foo>',
    {"foo",
         [{"attr","baz"}],
         [{"bar",[],["x"]},{"bar",[],["y"]}]} = Xml.
    io:format("~p~n", [Xml]).

```

Looking into the `inline_xml_erlsom` module and the `inline_erlsom.hrl` header 
should give some insights into how the API customisations work.
