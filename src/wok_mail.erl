-module(wok_mail).

-export([
  send/4,
  deliver/3,
  deliver/4
  ]).

%% @equiv deliver(Module, To, Data, [])
deliver(Module, To, Data) ->
  deliver(Module, To, Data, []).

%% @doc
%% @end
deliver(Module, To, Data, Options) ->
  send(
    Module:from(),
    To,
    buclists:keyfind(subject, 1, Options, Module:subject()),
    [{templates, Module:templates(), Data},
     {cc, buclists:keyfind(cc, 1, Options, []) ++ Module:cc()},
     {bcc, buclists:keyfind(bcc, 1, Options, []) ++ Module:bcc()},
     {callback, fun Module:done/1}]).

%% @doc
%% Send an email
%%
%% Options:
%% <pre>
%% {cc, [string()] | [binary()]}
%% {bcc, [string()] | [binary()]}
%% {templates, [{text|html, string()}], [{atom(), any()}]}
%% {body, string() | binary()}
%% {attachment, [string()] | [binary()]}
%% {callback, function()}
%% </pre>
%%
%% Example:
%% <pre>
%% send(
%%   "greg@example.com", 
%%   ["bob@example.com", "john@example.com"]
%%   "This is a mail",
%%   [{cc, ["tania@example.com", "tom@example.com"]},
%%    {bcc, "jane@example.com"},
%%    {templates, [{text, "template.txt.tmpl"}, {html, "template.html.tmpl"}], Data} 
%%    | {body, <<"hello world !!!">>},
%%    {attachments, ["/home/greg/photo.png"]}
%%    {callback, Fun module:function/1}]).
%% </pre>
%% @end
-spec send(string() | binary(), string() | binary() | [string()] | [binary()], string() | binary(), list()) -> {ok, pid()} | {error, any()}.
send(From, To, Subject, Options) ->
  BFrom = bucs:to_binary(From),
  BSubject = bucs:to_binary(Subject),
  Dest = [{<<"To">>, to_list_of_binary(To)}] ++
         case lists:keyfind(cc, 1, Options) of
    {cc, Data0} -> [{<<"Cc">>, to_list_of_binary(Data0)}];
    _ -> []
  end ++ case lists:keyfind(bcc, 1, Options) of
    {bcc, Data1} -> [{<<"Bcc">>, to_list_of_binary(Data1)}];
    _ -> []
  end,
  Attachments = case lists:keyfind(attachments, 1, Options) of
    {attachments, Data2} -> to_list_of_binary(Data2);
    false -> []
  end,
  Body = case lists:keyfind(template, 1, Options) of
    {template, Template, TemplateData} -> 
      HtmlTemplate = list_to_atom(atom_to_list(Template) ++ "_html"),
      TextTemplate = list_to_atom(atom_to_list(Template) ++ "_txt"),
      case bucs:module_exist(HtmlTemplate) of
        false -> [];
        true -> 
          {ok, Html} = HtmlTemplate:render(TemplateData), 
          [{html, ebinary:concat(Html)}]
      end ++ case bucs:module_exist(TextTemplate) of
        false -> [];
        true -> 
          {ok, Text} = TextTemplate:render(TemplateData), 
          [{text, ebinary:concat(Text)}]
      end;
    _ -> case lists:keyfind(body, 1, Options) of
        {body, Data3} -> [{text, bucs:to_binary(Data3)}];
        _ -> [{text, <<"">>}]
      end
    end,
  Callback = buclists:keyfind(callback, 1, Options, undefined),
  erlang:apply(provider(), send, [BFrom, Dest, BSubject, Body, Attachments, Callback]).

% private

to_list_of_binary(Data) ->
  case bucs:is_string(Data) of
    true -> [list_to_binary(Data)];
    false -> case is_binary(Data) of
        true -> [Data];
        false -> lists:map(fun bucs:to_binary/1, Data)
      end
  end.


template_engine(View) when is_list(View) ->
  [Ext|_] = lists:reverse(string:tokens(View, "._")),
  Engine = bucs:to_atom("wok_" ++ Ext ++ "_engine"),
  case code:ensure_loaded(Engine) of
    {module, Engine} ->
      {ok, Engine};
    _ ->
      error
  end;
template_engine(View) ->
  template_engine(bucs:to_string(View)).

provider() ->
  wok_smtp_mail.

