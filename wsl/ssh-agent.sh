#!/bin/bash

# Using ConEmu on Windows, putting `eval $(ssh-agent -s)` leads to
# each window getting its own private ssh-agent, which is dumb. This
# wrapper script caches the ssh-agent configuration in a file, so it
# can be reused if the agent is already running.

# Note that, like ssh-agent, this script is expected to be invoked
# with `eval $(...)`, and therefore it echoes commands for the parent
# shell to execute.

ssh_agent_info="${HOME}/.ssh-agent.sh"

if [ -e $ssh_agent_info ]; then

    source $ssh_agent_info > /dev/null

    if ps --pid $SSH_AGENT_PID > /dev/null; then

        echo "echo Agent already running;"
        cat $ssh_agent_info

        # Clean up any stale /tmp/ssh-* directories; we do this here
        # because we've already loaded the SSH_* variables and know
        # the agent is running.
        hot_ssh_tmp=$(dirname $SSH_AUTH_SOCK)
        for ssh_tmp in /tmp/ssh-*; do
            if [[ "$ssh_tmp" != "$hot_ssh_tmp" ]]; then
                echo rm -v -r $ssh_tmp
            fi
        done

        exit
    fi
fi

echo "echo Agent started;"
ssh-agent -s | tee $ssh_agent_info
echo ssh-add
