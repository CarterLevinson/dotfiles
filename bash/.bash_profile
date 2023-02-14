#
# ~/.bash_profile
#

if [[ -z $DISPLAY ]] && [[ $XDG_VTNR -eq 1 ]]; then
  exec startx /usr/bin/xmonad  > /dev/null
fi

if [[ -z $DISPLAY]] && [[ $XDG_VTNR -eq 2 ]]; then
  exec startx /usr/bin/fluxbox > /dev/null
fi

if [[ -z $DISPLAY ]] && [[ $XDG_VTNR -eq 3 ]]; then
  exec startx /usr/bin/i3 > /dev/null
fi
