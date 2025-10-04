#!/bin/bash
cd /workspace
erl <<EOF
mnesia:create_schema([node()]).
mnesia:start().
polis:start().
timer:sleep(2000).
test_substrate_spawn:test().
timer:sleep(2000).
q().
EOF

