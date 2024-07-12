ERL = erl
REBAR = rebar3

all: compile
co: compile
cl: clean
prod: production

compile:
	@$(REBAR) compile

deps:
	@$(REBAR) upgrade

.PHONY: clean
clean:
	@$(REBAR) clean

eunit:
	@$(REBAR) eunit


run: compile
	@$(ERL) \
		+P 262144 \
		-name erlsdk@127.0.0.1 \
		-setcookie uekfqx8v92q7f22rf6b8guy2p7mud94v \
		-config config/sys.config \
		-pa _build/default/lib/cowboy/ebin \
		-pa _build/default/lib/cowlib/ebin \
		-pa _build/default/lib/erlsdk/ebin \
		-pa _build/default/lib/jsx/ebin \
		-pa _build/default/lib/quickrand/ebin \
		-pa _build/default/lib/ranch/ebin \
		-pa _build/default/lib/uuid/ebin \
		-s erlsdk start \
		-s observer start \
		-cwd trunk


.PHONY: test
test:
	@$(ERL) -make
	@$(ERL) -noshell \
		-config config/sys.config \
		-s ssl \
		-pa _build/default/lib/cowboy/ebin \
		-pa _build/default/lib/cowlib/ebin \
		-pa _build/default/lib/erlsdk/ebin \
		-pa _build/default/lib/jsx/ebin \
		-pa _build/default/lib/quickrand/ebin \
		-pa _build/default/lib/ranch/ebin \
		-pa _build/default/lib/uuid/ebin \
		-pa _build/test/lib/erlsdk/ebin \
		-s erlsdk_test run \
		-s init stop \
		-cwd test

