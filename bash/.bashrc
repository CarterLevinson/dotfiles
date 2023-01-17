#
# ~/.bashrc
#

# if not running interactively, don't do anything
[[ $- != *i* ]] && return

# source blesh
# source /usr/share/blesh/ble.sh --noattach

# source user local shell completions
for FILE in "$HOME/.local/etc/bash_completion.d"/*; do
  source $FILE;
done

# source fzf keybindings plugin
source /usr/share/fzf/key-bindings.bash

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

PROMPT="[${PUR}λ ${BLU}\$ ${PUR}ξ${WHT}] ${CYN}\W"
PROMPT+="$(__git_ps1 ${WHT}'  %s') "
PROMPT+="${WHT}--->> ${CLR}"

PS1=$PROMPT

# set environment variables
export EDITOR=vi
export VISUAL=nvim
export BROWSER=chromium
#export BROWSER=qutebrowser
# export BROWSER=firefox
export CLIPBOARD=xclip
export READER=zathura
export OPENER=rifle
export VIEWER=nsxiv
export TERMINAL=alacritty
# export TERMINAL=st
export PAGER='less'

export QT_QPA_PLATFORMTHEME=qt5ct
export BAT_THEME=ansi

# for colored man pages
export LESS_TERMCAP_mb=$'\e[01;31m'       # begin blinking
export LESS_TERMCAP_md=$'\e[01;37m'       # begin bold
export LESS_TERMCAP_me=$'\e[0m'           # end all mode like so, us, mb, md, mr
export LESS_TERMCAP_se=$'\e[0m'           # end standout-mode
export LESS_TERMCAP_so=$'\e[45;93m'       # start standout mode
export LESS_TERMCAP_ue=$'\e[0m'           # end underline
export LESS_TERMCAP_us=$'\e[4;93m'        # start underlining

GPG_TTY=$(tty)
export GPG_TTY

export GOPATH=$HOME/.go
PATH=~/bin:$PATH
PATH=$PATH:~/.local/bin
PATH=$PATH:~/.cargo/bin
PATH=$PATH:${GOPATH}/bin
export PATH

# shell aliases
alias pip=pip3
alias python=python3
alias ipy=ipython

alias ydl=youtube-dl
alias vbm=VBoxManage

alias md=mdless
alias mutt=neomutt
alias pm=pacman
alias pac=pacman

alias g=git

alias v=$EDITOR
alias vi=$VISUAL
alias open=$OPENER

alias R='R -q'
alias rad='radian -q --no-save'

alias info='info --vi-keys'
alias lynx='lynx -vikeys'
# alias tree='tree -dirsfirst -F'

alias null=/dev/null

alias c=clear
alias cls=clear
alias q=exit
alias quit=exit

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

# bash options
HISTCONTROL=ignoreboth
HISTTIMEFORMAT="%F %T "
HISTSIZE=2000
HISTFILESIZE=20000

shopt -s autocd
shopt -s histappend

# cl() - cd and ls at once time
cl() {
  dir=$1 && dir=${dir:=$HOME}
  cd "$dir" && ls || return;
}

# cf() - cd into the directory of the selected file
cf() {
  file=$(fzf +m -q "$1") && dir=$(dirname "$file")
  cd "$dir" || return
}

# vf() - fuzzy open with (neo)vim from anywhere
vf () {
  files=()
	while IFS= read -r -d '' file; do
		files+=("$file")
	done < <(fzf --multi --print0)

	(( ${#files} )) || return
	"${VISUAL:-${EDITOR:-vi}}" "$@" "${files[@]}"
}

# rcd() - rangercd change shell dir using ranger
rcd() {
  tmp=$(mktemp)
  ranger --choosedir="$tmp" "$@"
  dir=$(cat "$tmp") || rm -f "$tmp"
  [[ -d "$dir" ]] && cd "$dir" || return
}

# convert-base(outbase, inbase, number)
convert-base() {
  echo "obase=${1};ibase=${2}; ${3}" | bc
}

bin-to-dec() {
  convert-base "10" "2" "$1"
}

bin-to-hex() {
  convert-base "16" "2" "$1"
}

dec-to-bin() {
  convert-base "2" "10" "$1"
}

dec-to-hex() {
  convert-base "16" "10" "$1"
}

hex-to-bin() {
  convert-base "2" "16" "$1"
}

hex-to-dec() {
  convert-base "10" "16" "$1"
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

# Start the ssh-agent in the background and
# make sure only one instance is ever running
if ! pgrep -u "$USER" ssh-agent > /dev/null; then
  ssh-agent > "$XDG_RUNTIME_DIR/ssh-agent.env"
fi

if [[ ! "$SSH_AUTH_SOCK" ]]; then
  source "$XDG_RUNTIME_DIR/ssh-agent.env" > /dev/null
  ssh-add -q ~/.ssh/cslevo_github
fi

# ghcup-env setup
[[ -f "$HOME/.ghcup/env" ]] && source "$HOME/.ghcup/env"

# allows bash to stay running after invoking eval
if [[ $1 == eval ]]; then
  "$@"; set --
fi
# [[ ${BLE_VERSION-} ]] && ble-attach
