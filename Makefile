PROJECT = wok_mail

DEPS = gen_smtp bucs doteki

dep_gen_smtp = git https://github.com/danikp/gen_smtp.git master
dep_bucs = git https://github.com/botsunit/bucs.git master
dep_doteki = git https://github.com/botsunit/doteki.git master

include erlang.mk
