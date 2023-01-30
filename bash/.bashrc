#
# ~/.bashrc
#

# if not running interactively, don't do anything
[[ $- != *i* ]] && return

# source user local shell completions
for FILE in "$HOME/.local/etc/bash_completion.d"/*; do
  source $FILE;
done

# source git status plugin for prompt
source /usr/share/git/completion/git-prompt.sh

BLK=$(tput setaf 0)
RED=$(tput setaf 1)
GRN=$(tput setaf 2)
YLW=$(tput setaf 3)
BLU=$(tput setaf 4)
PUR=$(tput setaf 5)
CYN=$(tput setaf 6)
WHT=$(tput setaf 7)
CLR=$(tput sgr0)

PROMPT="[$PURλ $BLU\$ $PURξ$WHT] $CYN\W "
PROMPT+='$(__git_ps1 "$WHT $GRN%s ")'
PROMPT+="$WHT--->> $CLR"

PS1=$PROMPT

# set environment variables
export EDITOR=/usr/bin/vi
export VISUAL=/usr/bin/nvim
export BROWSER=/usr/bin/chromium
#export BROWSER=qutebrowser
#export BROWSER=firefox
export CLIPBOARD=/usr/bin/xclip
export READER=/usr/bin/zathura
export OPENER=/usr/bin/rifle
export VIEWER=/usr/local/bin/nsxiv
export TERMINAL=/usr/bin/kitty
export PAGER=/usr/bin/less

# export LESS='--RAW-CONTROL-CHARS'
# for colored man pages
# export LESS_TERMCAP_mb=$'\e[01;31m'       # begin blinking
# export LESS_TERMCAP_md=$'\e[01;37m'       # begin bold
# export LESS_TERMCAP_me=$'\e[0m'           # end all mode like so, us, mb, md, mr
# export LESS_TERMCAP_se=$'\e[0m'           # end standout-mode
# export LESS_TERMCAP_so=$'\e[45;93m'       # start standout mode
# export LESS_TERMCAP_ue=$'\e[0m'           # end underline
# export LESS_TERMCAP_us=$'\e[4;93m'        # start underlining

GPG_TTY=$(tty)
export GPG_TTY

export GOPATH=$HOME/.go
PATH=~/bin:$PATH
PATH=$PATH:~/.local/bin
PATH=$PATH:~/.cargo/bin
PATH=$PATH:${GOPATH}/bin
export PATH

export BAT_THEMEG=ansi
export QT_QPA_PLATFORMTHEME=qt5ct

FD_OPTS="--follow --hidden --ignore-vcs"
export FZF_DEFAULT_COMMAND="fd $FD_OPTS -tf -td"

# shell aliases
alias pip='pip3'
alias python='python3'
alias ipy='ipython'

alias ydl='youtube-dl'
alias vbm='VBoxManage'

alias md='mdless'
alias mutt='neomutt'
alias pm='pacman'
alias pac='pacman'

alias g='git'

alias nv='$VISUAL'
alias vi='$VISUAL'
alias vim='$EDITOR'
alias open='$OPENER'
alias ed='ex'

alias edit='vi -O'
alias R='R -q'
alias rad='radian -q --no-save'

alias info='info --vi-keys'
alias lynx='lynx -vikeys'

alias null='/dev/null'

alias cls='clear'
alias q='exit'
alias quit='exit'

alias ls='ls --color=auto'
alias la='ls -a'
alias ll='ls -l'
alias lh='ls -d .?*'
alias lhl='ls -ld .?*'
alias lal='ls -al'
alias lll='ls -alsh'

alias cp='cp --interactive'
alias rm='rm --interactive=once'
alias mkdir='mkdir -p -v'

alias reload='source ~/.bashrc'

alias cfh='cf ~'
alias cfr='cf /'
alias vdf='vf ~/.dotfiles'
alias vcfg='vf ~/.config'

alias b2d='convert-base 2 10'
alias b2h='convert-base 2 16'
alias d2b='convert-base 10 2'
alias d2h='convert-base 10 16'
alias h2b='convert-base 16 2'
alias h2d='convert-base 16 10'

# bash options
HISTCONTROL=ignoreboth
HISTTIMEFORMAT="%F %T "
HISTSIZE=2000
HISTFILESIZE=20000

shopt -s autocd
shopt -s histappend

# bind "\e]" : "fg\n"
# bind -m vi-insert '"\M-z"':"fg" # alt+z
# bind -m vi-command '"\M-z"':"fg" # alt+z


# cl() - cd and ls at once time
cl() {
  dir=$1 && dir=${dir:=$HOME}
  cd "$dir" && ls || return;
}

# cf() - cd into the directory of the selected file
cf() {
  find="fd $FD_OPTS -tf -td"
  file=$($find . "${1:-${PWD}}" | fzf -0 -1)
  if [[ -n "$file" ]]; then
    if [[ ! -d "$file" ]]; then
      dir=$(dirname "$file")
    else
      dir=$file
    fi
    cd "$dir" || return
  fi
}

# vf() - fuzzy open with (neo)vim from anywhere
vf () {
  find="fd $FD_OPTS -tf"
  files=$($find . "${1:-${PWD}}" | fzf -0 -1 -m)
	(( ${#files} )) || return
	"${VISUAL:-${EDITOR:-vi}}" "$@" "${files[@]}"
}

# fo() - fuzzy open files with $OPENER from anywhere
fo() {
  find="fd $FD_OPTS -tf"
  files=$($find . "${1:-${PWD}}" | fzf -0 -1 -m)
	(( ${#files} )) || return
	"$OPENER" "$@" "${files[@]}"
}

# rcd() - rangercd change shell dir using ranger
rcd() {
  tmp=$(mktemp)
  ranger --choosedir="$tmp" "$@" && dir=$(cat "$tmp")
  rm -f "$tmp"
  [[ -d "$dir" ]] && cd "$dir" || return
}

# convert-base(inbase, outbase, number)
convert-base() {
  if [[ "$#" -ne 3 ]]; then
    echo "USAGE: $0 inbase outbase number" >&2
    return 1
  fi
  echo "obase=${2};ibase=${1}; ${3}" | bc
}

# is-git-repo(): returns true if inside git repository
is-git-repo() {
  git rev-parse --git-dir > /dev/null 2>&1
}

# pac-ls(): list all directly installed packages
pac-ls() {
  paru -Qi | \
    awk '/^Name/{name=$3} /^Installed Size/{print $4$5, name}' | \
    sort -h
}

# pac-install(): fuzzy install packages with pacman and fzf
pac-install() {
  paru -Slq | fzf -q "$1" -m --preview 'paru -Si {1}' | xargs -ro 'paru -S'
}

# pac-remove(): fuzzy remove packages with pacman and fzf
pac-remove() {
  paru -Qq | fzf -q "$1" -m --preview 'paru -Qi {1}' | xargs -ro 'paru -Rns'
}

# encrypt(): compress file / directory with gpg/tar,
# optionally with a recipient
encrypt() {
  if [[ "$#" -eq 1 ]]; then
    base=$(basename "$1")
    tar -c "$base" | gpg -c -o "$base.tar.gpg"
  elif [[ "$#" -eq 2 ]]; then
    base=$(basename "$1")
    tar -c "$base" | gpg -e -r "$2" -p "$base.tar.gpg"
  else
    echo "USAGE: $0 /path/to/encrypt [recipient]" >&2
    return 1
  fi
}

# decrypt(): decrypt + expand with gpgp & tar
decrypt() {
  if [[ "$#" -ne 1 ]] || [[ "$1" != *.tar.gpg ]]; then
    echo "USAGE: $0 /path/to/encrypted/file.tar.gpg" >&2
    return 1
  fi
  gpg -d --quiet "$1" | tar -xvf
}

# ghcup-env setup
[[ -f "$HOME/.ghcup/env" ]] && source "$HOME/.ghcup/env"

# Start the ssh-agent in the background and
# make sure only one instance is ever running
if ! pgrep -u "$USER" ssh-agent > /dev/null; then
  ssh-agent > "$XDG_RUNTIME_DIR/ssh-agent.env"
fi

if [[ ! "$SSH_AUTH_SOCK" ]]; then
  source "$XDG_RUNTIME_DIR/ssh-agent.env" > /dev/null
  ssh-add -q ~/.ssh/cslevo_github
fi

# allows bash to stay running after invoking eval
if [[ $1 == eval ]]; then
  "$@"; set --
fi
