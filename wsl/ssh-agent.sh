#!/bin/bash

# Using ConEmu on Windows, putting `eval $(ssh-agent -s)` leads to
# each window getting its own private ssh-agent, which is dumb. This
# wrapper script caches the ssh-agent configuration in a file, so it
# can be reused if the agent is already running.

SSH_AGENT_INFO="${HOME}/.ssh-agent.sh"

if [ -e $SSH_AGENT_INFO ]; then
    source $SSH_AGENT_INFO > /dev/null
    if ps --pid $SSH_AGENT_PID > /dev/null; then
        echo "echo Agent already running;"
        cat $SSH_AGENT_INFO
        exit
    fi
fi

echo "echo Agent started;"
ssh-agent -s | tee $SSH_AGENT_INFO
