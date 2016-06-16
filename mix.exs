defmodule Wok.Mail.Mixfile do
  use Mix.Project

  def project do
    [
      app: :wok_mail,
      version: "0.1.1",
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
      {:bucs, git: "https://github.com/botsunit/bucs.git", tag: "0.0.2"},
      {:doteki, git: "https://github.com/botsunit/doteki.git", tag: "0.1.0"},
      {:gen_smtp, "~> 0.10.0"}    
    ]
  end
end