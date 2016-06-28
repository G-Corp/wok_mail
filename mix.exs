defmodule Wok.Mail.Mixfile do
  use Mix.Project

  def project do
    [
      app: :wok_mail,
      version: "0.1.2",
      elixir: "~> 1.2",
      build_embedded: Mix.env == :prod,
      start_permanent: Mix.env == :prod,
      deps: deps
    ]
  end

  def application do
    [
       applications: [],
       env: []
    ]
  end

  defp deps do
    [
      {:bucs, "~> 0.1.0"},
      {:doteki, "~> 0.1.0"},
      {:gen_smtp, "~> 0.11.0"}    
    ]
  end
end