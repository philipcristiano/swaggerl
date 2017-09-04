PROJECT = swaggerl
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

DEPS = lager jsx hackney
LOCAL_DEPS = inets ssl
TEST_DEPS = meck

SHELL_OPTS = -config sys
dep_jsx = git https://github.com/talentdeficit/jsx.git v2.8.2
dep_lager = git https://github.com/erlang-lager/lager.git 3.4.1
dep_meck = git https://github.com/eproxus/meck.git 0.8.4
dep_hackney = git https://github.com/benoitc/hackney.git 1.9.0

include erlang.mk


.PHONY: test
test: tests
