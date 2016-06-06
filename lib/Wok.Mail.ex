# File: Wok.Mail.ex
# This file was generated from wok_mail.beam
# Using rebar3_elixir (https://github.com/botsunit/rebar3_elixir)
# MODIFY IT AT YOUR OWN RISK AND ONLY IF YOU KNOW WHAT YOU ARE DOING!
defmodule Wok.Mail do
  def unquote(:"deliver")(arg1, arg2, arg3) do
    :erlang.apply(:"wok_mail", :"deliver", [arg1, arg2, arg3])
  end
  def unquote(:"deliver")(arg1, arg2, arg3, arg4) do
    :erlang.apply(:"wok_mail", :"deliver", [arg1, arg2, arg3, arg4])
  end
  def unquote(:"send")(arg1, arg2, arg3, arg4) do
    :erlang.apply(:"wok_mail", :"send", [arg1, arg2, arg3, arg4])
  end
end
