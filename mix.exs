defmodule Wok.Mail.Mixfile do
  use Mix.Project

  def project do
    [app: :wok_mail,
     version: "0.0.3",
     elixir: "~> 1.2",
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     deps: deps]
  end

  def application do
    [applications: []]
  end

  defp deps do
    [ 
      {:bucs, ~r/.*/, git: "https://github.com/botsunit/bucs.git", tag: "0.0.1"},  
      {:doteki, ~r/.*/, git: "https://github.com/botsunit/doteki.git", tag: "0.0.1"},  
      {:gen_smtp, ~r/.*/, git: "https://github.com/Vagabond/gen_smtp.git", tag: "0.10.0"},
    ]
  end
end
