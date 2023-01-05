### ~/.zshrc
### zsh $USER configuration file

# zmodload zsh/zprof

## source the zshell alias file
. $HOME/.zsh-aliases

## source shell completion config
. $HOME/.zsh-completion

## source the plugin files
PLUG=/usr/share/zsh/plugins

. $PLUG/fzf/fzf.plugin.zsh

. $PLUG/zsh-autosuggestions/zsh-autosuggestions.plugin.zsh

. $PLUG/fast-syntax-highlighting/fast-syntax-highlighting.plugin.zsh

. $PLUG/zsh-autopair/autopair.zsh

. $HOME/scripts/bd.zsh
## shell user settings

# directory options
setopt AUTO_PUSHD
setopt PUSHD_IGNORE_DUPS
setopt PUSHD_TO_HOME
setopt PUSHD_SILENT
setopt PUSHD_MINUS
setopt AUTOCD
# completion options
setopt MENU_COMPLETE
setopt GLOB_COMPLETE
setopt COMPLETE_ALIASES
setopt COMPLETE_IN_WORD
setopt ALWAYS_TO_END
setopt LIST_AMBIGUOUS
setopt LIST_TYPES
# history options
setopt APPEND_HISTORY
setopt INC_APPEND_HISTORY
setopt EXTENDED_HISTORY
setopt HIST_IGNORE_DUPS
setopt HIST_REDUCE_BLANKS
setopt HIST_IGNORE_SPACE
# misc options
setopt NO_BEEP
setopt CORRECT
setopt EXTENDED_GLOB
setopt PROMPT_SUBST
# shell vars
DIRSTACKSIZE=100
HISTSIZE=1000
SAVEHIST=10000
HISTFILE=$HOME/.zsh-history

## Prompts

autoload -Uz vcs_info                  # load version control info
zstyle ':vcs_info:*' enable git        # format the vcs_info_msg_0_ variable
zstyle ':vcs_info:git*' formats ' %b '
precmd() { vcs_info }                  # execute before every shell prompt

# set prompt strings
PROMPT='%F{blue}λ%f %F{cyan}%U%2~%u ${vcs_info_msg_0_}%f'
PROMPT+='%F{magenta}%#%f %F{blue}ξ%f %(?.%F{white}.%F{red})--->>%f '
RPROMPT='%F{blue}[%f%F{cyan}%T%f%F{blue}]%f'

## Keybindings

# edit shell command in vim, exit to load it in prompt
autoload edit-command-line; zle -N edit-command-line
bindkey '^e' edit-command-line

bindkey -s '^o' 'vf ~\n'
bindkey -s '^a' 'bc -q\n'
bindkey -s '^f' 'cf .\n'
bindkey -s '^b' 'fg\n'

bindkey '^ '  autosuggest-execute

# vim keybindings in comp mode
zmodload zsh/complist
bindkey -M menuselect 'h' vi-backward-char
bindkey -M menuselect 'k' vi-up-line-or-history
bindkey -M menuselect 'l' vi-forward-char
bindkey -M menuselect 'j' vi-down-line-or-history

# vim keybindings on shell prompt
bindkey -v

## cursor settings

# decrease mode switch latency
KEYTIMEOUT=1
# make zsh use same cursor style as vim
zle-keymap-select() {
  if [[ ${KEYMAP} == vicmd ]] ||
     [[ $1 = 'block' ]]; then
    echo -ne '\e[1 q'
  elif [[ ${KEYMAP} == main ]] ||
       [[ ${KEYMAP} == viins ]] ||
       [[ ${KEYMAP} = '' ]] ||
       [[ $1 = 'beam' ]]; then
    echo -ne '\e[5 q'
  fi
}

# call zle -N zle-keymap-select()
zle -N zle-keymap-select

# use beam shape cursor on startup.
echo -ne '\e[5 q'
# use beam shape cursor for each new prompt.
preexec() { echo -ne '\e[5 q'; }



# some simple numeric base conversion functions
bin-to-hex() {
  echo "obase=16;ibase=2;"  $1 | bc
}

hex-to-bin() {
  echo "obase=2;ibase=16;"  $1 | bc
}

hex-to-dec() {
  echo "obase=10;ibase=16;" $1 | bc
}

dec-to-hex() {
  echo "obase=16;ibase=10;" $1 | bc
}

bin-to-dec() {
  echo "obase=10;ibase=2;"  $1 | bc
}

dec-to-bin() {
   echo "obase=2;ibase=10;"  $1 | bc
}

# encrypt + compress file / directory with gpg/tar,
# optionally with a recipient
encrypt() {
  if ["$#" - eq 1]; then
    tar -c `basename $1` | gpg -c -o `basename $1`.tar.gpg
  elif ["$#" - eq 2]; then
    tar -c `basename $1` | gpg -e -r $2 -o `basename $1`.tar.gpg
  else
    echo "USAGE: $0 /path/to/encrypt [recipient]" >&2
    return 1
  fi
}

# decrypt + expand file / directory with gpg/tar
decrypt() {
  if [ "$#" -ne 1 ] || [["$1" != *.tar.gpg]]; then
    echo "USAGE: $0 /path/to/encrypted/file.tar.gpg" >&2
    return 1
  fi
  gpg -d --quiet $1 | tar -xvf
}

# returns true if inside a git repository
is-inside-git-repo() {
  git rev-parse --git-dir > /dev/null 2>&1
}

# simple shell function to change shell directory with ranger
# i'm sure we can break this down and make it cleaner. use rifle?
rcd() {
  tmp="$(mktemp)"
  ranger --choosedir="$tmp" "$@"
  if [ -f "$tmp" ]; then
    dir="$(cat "$tmp")"
    rm -f "$tmp"
    if [ -d "$dir" ]; then
      if [ "$dir" != "$(pwd)" ]; then
        cd "$dir"
      fi
    fi
  fi
}


# pacman functions
# pacman-fuzzy-installer() {
#   pacman -Slq | \
#     fzf -q "$1" -m --preview 'pacman -Si{1}' | \
#     xargs -ro sudo pacman -S
# }
#
# pacman-fuzzy-remover() {
#   pacman -Qq | \
#     fzf -q "$1" -m --preview 'pacman -Qi {1}' | \
#     xargs -ro sudo pacman -Rns
# }
#
# pacman-check-deps() {
#   pacman -Q $(pactree -u $1)
# }

# pacman-list-orphans() {
#   [[ -z $1 ]] && pacman -Qdt $1
# }
#
# pacman-list-explicit() {
#   pacman -Qet
# }

# pac-finstall() {}
# pac-fremove() {}

# paru-finstall() {}
# paru-reremove() {}

# list all directly installed packages
pac-ls() {
  pacman -Qi | \
    awk '/^Name/{name=$3} /^Installed Size/{print $4$5, name}' | \
    sort -h
}

# fman -- use fzf to interactively search manpages
fman() {
  man -k . | fzf --prompt='Man> ' | awk '{print $1}' | xargs -r manpages
}

# fkill - kill processes - list only the ones you can kill.
fkill() {
  local pid
  if [ "$UID" != "0" ]; then
    pid=$(ps -f -u $UID | sed 1d | fzf -m | awk '{print $2}')
  else
    pid=$(ps -ef | sed 1d | fzf -m | awk '{print $2}')
  fi
  if [ "x$pid" != "x" ]; then
    echo $pid | xargs kill -${1:-9}
  fi
}

# TODO: write a VO version that uses fd
# vo() {
  # local files
  # files=(${(f)"f"}
# }

# vg - fuzzy open with rg instead of locate
# vg() {
#   local file
#   file="$(rg --nobreak --noheading $@ | fzf -0 -1 | awk -F: '{print $1}')"
#
#   if [[ -n $file ]]
#   then
#      vi $file
#   fi
# }

# vf - fuzzy open with (neo)vim from anywhere
# ex: vf word1 word2 ... (even part of a file name)
# zsh autoload function
vf() {
  local files
  files=(${(f)"$(locate -Ai -0 $@ | grep -z -vE '~$' | fzf --read0 -0 -1 -m)"})
  if [[ -n $files ]]; then
    vi -- $files
    print -l $files[1]
  fi
}

# cf - fuzzy cd from anywhere
# ex: cf word1 word2 ... (even part of a file name)
# zsh autoload function
cf() {
  local file
  file="$(locate -Ai -0 $@ | grep -z -vE '~$' | fzf --read0 -0 -1)"
  if [[ -n $file ]];  then
    if [[ -d $file ]];  then
      cd -- $file
    else
      cd -- ${file:h}
    fi
  fi
}

# rga-fzf - use fzf and ripgrep to search various media
# ex: rga-fzf [rga options][rg options]PATTERN[PATH..]
# zsh autoload fn
rga-fzf() {
	RG_PREFIX="rga --files-with-matches"
	local file
	file="$(
		FZF_DEFAULT_COMMAND="$RG_PREFIX '$1'" \
			fzf --sort --preview="[[ ! -z {} ]] && rga --pretty --context 5 {q} {}" \
				--phony -q "$1" \
				--bind "change:reload:$RG_PREFIX {q}" \
				--preview-window="70%:wrap"
	)" &&
	echo "opening $file" &&
	$OPENER "$file"
}

# wrapper function to color manpages
# termcap
# ks       make the keypad send commands
# ke       make the keypad send digits
# vb       emit visual bell
# mb       start blink
# md       start bold
# me       turn off bold, blink and underline
# so       start standout (reverse video)
# se       stop standout
# us       start underline
# ue       stop underline
function man() {
	env \
		LESS_TERMCAP_md=$(tput bold; tput setaf 4) \
		LESS_TERMCAP_me=$(tput sgr0) \
		LESS_TERMCAP_mb=$(tput blink) \
		LESS_TERMCAP_us=$(tput setaf 2) \
		LESS_TERMCAP_ue=$(tput sgr0) \
		LESS_TERMCAP_so=$(tput smso) \
		LESS_TERMCAP_se=$(tput rmso) \
		PAGER="${commands[less]:-$PAGER}" \
		man "$@"
}

# Start the ssh-agent in the background and
# make sure only one instance is ever running
if ! pgrep -u "$USER" ssh-agent > /dev/null; then
  ssh-agent -t 1h > "$XDG_RUNTIME_DIR/ssh-agent.env"
fi

if [[ ! "$SSH_AUTH_SOCK" ]]; then
  source "$XDG_RUNTIME_DIR/ssh-agent.env" > /dev/null
fi

# allows zsh to stay running after invoking eval in i3
if [[ $1 == eval ]] then
    	"$@"
	set --
fi

# ghcup-env setup
[ -f "$HOME/.ghcup/env" ] && source "$HOME/.ghcup/env"

# zprof
