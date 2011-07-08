%% -----------------------------------------------------------------------------
%%
%% Copyright (c) 2011 Tim Watson (watson.timothy@gmail.com)
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%%
%% -----------------------------------------------------------------------------
-module(inline_xml).
-export([parse_transform/2]).
-record(state, {
    config = [],
    scope = desc,
    api = {xmerl_scan, string, [[{quiet, true}]]},
    processed = []
}).

parse_transform(Forms, Options) ->
    parse_trans:top(fun do_transform/2, Forms, Options).

do_transform(Forms, Context) ->
    {Forms2, _Acc2} =
        parse_trans:do_transform(fun xform_fun/4, #state{}, Forms, Context),
    parse_trans:revert(Forms2).

%% xform_fun(attribute, Form, Ctx, State) ->

xform_fun(attribute, Form, _Ctx, State) ->
    NewState = case erl_syntax_lib:analyze_attribute(Form) of
        {parse_trans, verbose} ->
            put(parse_trans.verbose, true),
            State;
        {xml_api, {xml_api, API}} when is_tuple(API) ->
            progress_message("Processing API override: ~p~n", [API]),
            State#state{ api=API };
        _ -> 
            State
    end,
    {[], Form, [], true, NewState};
xform_fun(Thing, Form, Ctx, State=#state{ scope=Scope })
    when Thing =:= variable   orelse
         Thing =:= match_expr orelse
         Thing =:= case_expr  orelse
         Thing =:= clause ->
     progress_message("[Thing]  ~p~n[Form]  ~p~n[Ctx]  ~p~n[Conf]  ~p~n",
                      [Thing, Form, Ctx, State]),
    {[], Form, [], true, State#state{ scope=[Thing|Scope] }};
xform_fun(atom, Form, _Ctx, State=#state{ scope=Scope }) ->
    case Scope of
        [variable,match_expr|Stack] ->
            maybe_inline_xml(Form, State#state{ scope=Stack });
        [clause,variable,case_expr|Stack2] ->
            maybe_inline_xml(Form, State#state{ scope=Stack2 });
        _ ->
            {[], Form, [], true, State}
    end;
xform_fun(Thing, Form, Ctx, State) ->
    progress_message("[Thing]  ~p~n[Form]  ~p~n[Ctx]  ~p~n[Conf]  ~p~n",
                     [Thing, Form, Ctx, State]),
    {[], Form, [], true, State}.

maybe_inline_xml({atom,L,Atom}=Form, State) ->
    progress_message("Checking if atom ~p is XML~n", [Atom]),
    Forms = case is_xml_literal(Atom) of
        {true, Xml} ->
            progress_message("Located XML ~p~n", [Xml]),
            case to_forms(L, Xml, State) of
                {ok, NewForms}  -> NewForms;
                ignored         -> Form
            end;
        false -> Form
    end,
    {[], Forms, [], true, State}.

is_xml_literal(Atom) ->
    case atom_to_list(Atom) of
        [$<|_]=Xml -> {true, Xml};
        _ -> false
    end.

to_forms(Line, XmlString, #state{ api={Mod, Func, ArgSuffix} }) ->
    progress_message("Applying ~p:~p with ~p~n",
                    [Mod, Func, [XmlString|ArgSuffix]]),
    try apply(Mod, Func, [XmlString|ArgSuffix]) of
        {Xml, []}       -> {ok, to_forms(Xml)};
        {ok, Result}    -> {ok, to_forms(Result)};
        {ok, Data, _}   -> {ok, to_forms(Data)};
        Other ->
            progress_message("Line: ~p: Error while parsing XML, "
                             "not expanding. Unsupported API Result: ~p~n",
                             [Line, Other]),
            ignored
    catch
        Error:Reason ->
            progress_message("Line: ~p: Error while parsing XML, "
                             "not expanding. ~p: ~p~n",
                             [Line, Error, Reason]),
            ignored
    end.

to_forms(XmlForms) ->
    erl_parse:abstract(XmlForms).

progress_message(Msg, Args) ->
    case is_verbose() of
        true ->
            io:format(Msg, Args);
        rebar_verbose ->
            rebar_log:log(debug, Msg, Args);
        _ -> ok
    end.

is_verbose() ->
    case application:get_env(rebar_global, verbose) of
        false     -> check_env();
        {ok, "0"} -> check_env();
        {ok, "1"} -> rebar_verbose
    end.

check_env() ->
    case os:getenv("PARSE_TRANS_VERBOSE") of
        false ->
            case get(parse_trans.verbose) of
                undefined -> false;
                – -> true
            end;
        Other ->
            io:format("os:getenv = ~p~n", [Other]),
            true
    end.
