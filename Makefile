.PHONY: all deps clean release test

all: compile



compile: deps
	./rebar -j8 compile

deps:
	./rebar -j8 get-deps

clean:
	./rebar -j8 clean

relclean:
	rm -rf rel/etcd

generate: compile
	cd rel && .././rebar -j8 generate

run: generate
	./rel/etcd/bin/etcd start

console: generate
	./rel/etcd/bin/etcd console

foreground: generate
	./rel/etcd/bin/etcd foreground

erl: compile
	erl -pa ebin/ -pa deps/*/ebin/ -s etcd

test: compile
	ERL_AFLAGS="-config ${PWD}/rel/files/app.config" ./rebar compile ct suite=etcd skip_deps=true

