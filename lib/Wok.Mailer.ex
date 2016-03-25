defmodule Wok.Mailer do
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
