PROJECT = swaggerl
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

BUILD_DEPS = elvis_mk
DEPS = jsx hackney
LOCAL_DEPS = inets ssl
TEST_DEPS = meck
CT_OPTS ?= -create_priv_dir auto_per_tc

SHELL_OPTS = -config sys
dep_elvis_mk = git https://github.com/inaka/elvis.mk.git 1.0.0
dep_jsx = git https://github.com/talentdeficit/jsx.git v2.8.2
dep_meck = git https://github.com/eproxus/meck.git 0.8.4
dep_hackney = git https://github.com/benoitc/hackney.git 1.15.2

DEP_PLUGINS = elvis_mk

include erlang.mk


.PHONY: test
test: tests
