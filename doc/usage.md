# Installation

To send a mail with `Wok`, start by adding `wok_mail` in your dependencies.

Then, create a `mailer` entry in your `wok` configuration.

```erlang
[
  {wok, [
    ...
    {mailer, [
      {smtp, [
        {relay, "localhost"},
        {port, 25},
        {username, "Username"},
        {password, "Password"}
      ]}
    ]}
    ...
  ]}
]
```

# Create a mail controler

To send an email, you need to create a mailer controler. This mailer must respect the `wok_mailer` behaviour.

A mailer must export 6 function :

* `from/0` return the email address of the sender
* `subject/0` return the mail subject
* `cc/0` return a list of email addresses
* `bcc/0` return a list of email addresses
* `templates/0` return the list of templates to use for each part of the mail body.
* `done/1` is the callback that will be called once the email has been sent . This callback receive a tuple `{ok, Reason}` or `{error, Reason}`.

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

You can use your favorite template engine to create the body of your message.

# Sending a mail

To send the mail, use the `deliver/2` or `deliver/3` function of your mailer.

```erlang
my_mailer:deliver("mike@example.com", TemplateData, Options).
```

```elixir
My.Mailer.deliver("mike@example.com", template_data, options)
```

The availables options are :

* `{cc, [string()] | [binary()]}` : A list of email addresses.
* `{bcc, [string()] | [binary()]}` : A list of email addresses.
* `{attachments, [string()] | [binary()]}` : A list of filename.
* `{provider, atom()}` : The provider to use.
* `{locale, binary()}` : The locale.

