PROJECT = hello_erlang
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

DEPS = cowboy jsxn sync erlydtl
dep_cowboy_commit = master
dep_jsxn = hex 0.2.1
dep_sync = hex 0.1.3
dep_erlydtl = hex 0.12.1

DEP_PLUGINS = cowboy

include erlang.mk
