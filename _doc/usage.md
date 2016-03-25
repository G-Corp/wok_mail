# Installation

```erlang
{mailer, [
  {smtp, [
    {relay, "localhost"},
    {port, 25},
    {ssl, false},
    {username, "Username"},
    {password, "Password"}
  ]}
]}
```

# Create a mail controler

```erlang
-module(my_mailer).
-behaviour(wok_mailer).
-export([from/0, subject/0, cc/0, bcc/0, templates/0, done/1]).
-include_lib("wok_mail/include/wok_mailer.hrl").

from() ->
  "john.doe@example.com".

subject() ->
  "Welcome !".

cc() ->
  ["dave@example.com", "james@example.com"].

bcc() ->
  ["smith@example.com", "clark@example.com"].

templates() ->
  [{text, "mails/welcome.txt.tmpl"},
   {html, "mails/welcome.html.tmpl"}].

done(_Response) ->
  ok.
```

```elixir
defmodule My.Mailer do
  @behaviour Wok.Mailer
  use Wok.Mailer

  def from(), do:
    "john.doe@example.com"
  
  def subject(), do:
    "Welcome !"
  
  def cc(), do:
    ["dave@example.com", "james@example.com"]
  
  def bcc(), do:
    ["smith@example.com", "clark@example.com"]
  
  def templates(), do:
    [{text, "mails/welcome.txt.tmpl"},
     {html, "mails/welcome.html.tmpl"}]
  
  def done(_response) do
    :ok
  end
end
```

# Create templates

# Sending a mail


