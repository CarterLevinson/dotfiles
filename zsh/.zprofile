### ~/.zprofile
### commands to run on all shells

if [[ -z $DISPLAY ]] && [[ $XDG_VTNR -eq 1 ]]; then
  exec startx /usr/bin/i3 > /dev/null
fi

if [[ -z $DISPLAY ]] && [[ $XDG_VTNR -eq 2 ]]; then
  exec startx $HOME/.local/bin/xmonad  > /dev/null
fi
