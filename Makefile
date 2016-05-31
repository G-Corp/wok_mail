PROJECT = wok_mail

DEP_PLUGINS = mix.mk
BUILD_DEPS = mix.mk
ELIXIR_VERSION = ~> 1.2
ELIXIR_BINDINGS = wok_mail

dep_mix.mk = git https://github.com/botsunit/mix.mk.git master

DEPS = bucs doteki gen_smtp

dep_bucs = git https://github.com/botsunit/bucs.git 0.0.1
dep_doteki = git https://github.com/botsunit/doteki.git 0.0.1
dep_gen_smtp = git https://github.com/Vagabond/gen_smtp.git 0.10.0

DOC_DEPS = edown

dep_edown = git https://github.com/botsunit/edown.git master

CP = cp

EDOC_OPTS = {doclet, edown_doclet} \
						, {app_default, "http://www.erlang.org/doc/man"} \
						, {source_path, ["src"]} \
						, {overview, "overview.edoc"} \
						, {stylesheet, ""} \
						, {image, ""} \
						, {edown_target, gitlab} \
						, {top_level_readme, {"./README.md", "https://gitlab.botsunit.com/msaas/${PROJECT}"}}

include erlang.mk

docs:: edoc 
	@${CP} _doc/* doc

release: app mix.all

