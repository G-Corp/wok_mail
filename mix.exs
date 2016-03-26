defmodule Wok.Mail.Mixfile do
	use Mix.Project

	def project do
		[app: :wok_mail,
		 version: "0.0.1",
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
			{:bucs, ~r/.*/, git: "https://github.com/botsunit/bucs.git", branch: "master"},  
			{:doteki, ~r/.*/, git: "https://github.com/botsunit/doteki.git", branch: "master"},  
			{:gen_smtp, ~r/.*/, git: "https://github.com/Vagabond/gen_smtp.git", branch: "master"},
		]
	end
end
