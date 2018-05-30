#!/bin/bash
# acquired courtesy of
# http://superuser.com/questions/141044/sharing-the-same-ssh-agent-among-multiple-login-sessions#answer-141241

function sshag_findsockets {
    find /tmp -uid $(id -u) -type s -name agent.\* 2>/dev/null
}

function sshag_testsocket {
    if [ ! -x "$(which ssh-add)" ] ; then
        echo "ssh-add is not available; agent testing aborted" >&2
        return 1
    fi

    if [ X"$1" != X ] ; then
        export SSH_AUTH_SOCK=$1
    fi

    if [ X"$SSH_AUTH_SOCK" = X ] ; then
        return 2
    fi

    if [ -S $SSH_AUTH_SOCK ] ; then
        ssh-add -l > /dev/null
        if [ $? = 2 ] ; then
            echo "Socket $SSH_AUTH_SOCK is dead!  Deleting!" >&2
            rm -f $SSH_AUTH_SOCK
            return 4
        else
            return 0
        fi
    else
        echo "$SSH_AUTH_SOCK is not a socket!" >&2
        return 3
    fi
}

function sshag_init {
    # ssh agent sockets can be attached to a ssh daemon process or an
    # ssh-agent process.

    AGENTFOUND=0

    # Attempt to find and use the ssh-agent in the current environment
    if sshag_testsocket ; then AGENTFOUND=1 ; fi

    # If there is no agent in the environment, search /tmp for
    # possible agents to reuse before starting a fresh ssh-agent
    # process.
    if [ $AGENTFOUND = 0 ] ; then
        for agentsocket in $(sshag_findsockets) ; do
            if [ $AGENTFOUND != 0 ] ; then break ; fi
            if sshag_testsocket $agentsocket ; then AGENTFOUND=1 ; fi
        done
    fi

    # If at this point we still haven't located an agent, it's time to
    # start a new one
    if [ $AGENTFOUND = 0 ] ; then
        eval `ssh-agent`
    fi

    # Clean up
    unset AGENTFOUND
    unset agentsocket

    { echo "Keys:";  ssh-add -l | sed 's/^/    /'; } >&2

    # Display the found socket
    echo $SSH_AUTH_SOCK;
}


# If we are not being sourced, but rather running as a subshell,
# let people know how to use the output.
if [[ $0 =~ sshag ]]; then
    echo 'Output should be assigned to the environment variable $SSH_AUTH_SOCK.' >&2
    sshag_init
# Otherwise, make it convenient to invoke the search.
# When the alias is invoked, it will modify the shell environment.
else
    alias sshag="sshag_init"
fi
