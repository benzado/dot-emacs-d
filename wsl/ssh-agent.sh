#!/bin/bash

# Because of the way Windows Subsystem for Linux (WSL) works, putting
# `eval $(ssh-agent -s)` in our bash .profile results in each console
# window getting a separate ssh-agent instance, which is dumb. This
# wrapper script caches the ssh-agent configuration in a file, so that
# it can be reused if the agent is already running.

# Note that, like ssh-agent, this script echoes commands for the
# calling shell to execute; therefore, it expects to be invoked with
# `eval $(...)`.

ssh_agent_info="${HOME}/.ssh-agent.sh"

if [ -e $ssh_agent_info ]; then
    source $ssh_agent_info > /dev/null

    if ps --pid $SSH_AGENT_PID > /dev/null; then
        echo "echo Agent is running...;"
        cat $ssh_agent_info
        exit
    fi
fi

echo "echo Cleaning up stale files...;"
for ssh_tmp in /tmp/ssh-*; do
    echo "rm -v -r ${ssh_tmp};"
done

echo "echo Starting new agent...;"
if ! ssh-agent -s > $ssh_agent_info; then
    echo "echo Agent failed to start.;"
    exit 1
fi

cat $ssh_agent_info
echo "echo Adding private keys to agent...;"
echo "ssh-add;"
