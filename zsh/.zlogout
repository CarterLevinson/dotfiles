### ~/.zlogout
### commands to be run before logout

# kill running ssh agents
if [[ "$SSH_AGENT_PID" != ""]]; then
  eval ssh-agent -k
fi

# kill running gpg agents
gpgconf --kill gpg-agent
