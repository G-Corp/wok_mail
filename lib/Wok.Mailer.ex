defmodule Wok.Mailer do
  @callback from() :: String.t
  @callback subject() :: String.t
  @callback cc() :: [String.t]
  @callback bcc() :: [String.t]
  @callback template() :: [{:text|:html, String.t}]
  @callback done(any) :: any

  defmacro __using__(_) do
    quote do
      def deliver(to, data) do
        Wok.Mail.deliver(__MODULE__, to,data)
      end
      def deliver(to, data, options) do
        Wok.Mail.deliver(__MODULE__, to, data, options)
      end
    end
  end
end