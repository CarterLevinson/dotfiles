#
# ~/.bash_profile
#

if   [[ -z $DISPLAY ]] && [[ $XDG_VTNR -eq 1 ]]; then
  exec startx ~/.local/bin/xmonad > /dev/null
fi
