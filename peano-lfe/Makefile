# Makefile for Peano Arithmetic in LFE

REBAR3 = rebar3
LFE = lfe

.PHONY: all compile shell clean test

all: compile

compile:
	@echo "Compiling LFE modules..."
	@$(REBAR3) compile

shell: compile
	@echo "Starting LFE shell..."
	@$(REBAR3) lfe repl

clean:
	@echo "Cleaning..."
	@$(REBAR3) clean
	@rm -rf _build

test: compile
	@echo "Running tests..."
	@$(REBAR3) eunit

run-examples: compile
	@echo "Running examples..."
	@lfescript examples.lfe

# Create rebar.config if it doesn't exist
rebar.config:
	@echo '{deps, [{lfe, "2.1.2"}]}.' > rebar.config
	@echo '{plugins, [{rebar3_lfe, "0.4.8"}]}.' >> rebar.config
	@echo '{provider_hooks, [{pre, [{compile, {lfe, compile}}]}]}.' >> rebar.config

setup: rebar.config
	@echo "Setting up project..."
	@$(REBAR3) get-deps