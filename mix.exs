defmodule Wok.Mail.Mixfile do
  use Mix.Project

  def project do
    [
      app: :wok_mail,
      version: "0.0.4",
      elixir: "~> 1.2",
      build_embedded: Mix.env == :prod,
      start_permanent: Mix.env == :prod,
      deps: deps
    ]
  end

  def application do
    [
       applications: [:kernel, :stdlib],
       env: []
    ]
  end

  defp deps do
    [
      {:bucs, git: "https://github.com/botsunit/bucs.git", branch: "master"},
      {:doteki, git: "https://github.com/botsunit/doteki.git", branch: "master"},
      {:gen_smtp, "~> 0.10.0"}    
    ]
  end
end