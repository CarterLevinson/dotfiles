#
# ~/.bash_logout
#

# kill running ssh agents
if [[ "$SSH_AGENT_PID" != "" ]]; then
  eval ssh-agent -k
fi

# reset the virtual terminal
clear
reset
