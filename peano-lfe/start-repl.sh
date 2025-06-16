#!/bin/bash

# Start LFE REPL with proper code paths
export ERL_LIBS="_build/default/lib"
rebar3 lfe repl