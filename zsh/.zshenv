### ~/.zshenv
### set user environment variables

# env vars
export EDITOR=/usr/bin/vi
export VISUAL=/usr/bin/nvim
export BROWSER=/usr/bin/qutebrowser
export READER=/usr/bin/zathura
export TERMINAL=/usr/bin/kitty
export CLIPBOARD=/usr/bin/xclip
export VIEWER=/usr/bin/vimiv
export OPENER=/usr/bin/rifle
# export MAIL=/var/spool/mail/carterlevo
export GPG_TTY=$(tty)
# program options
export FZF_DEFAULT_COMMAND="fd --type f --hidden --follow --exclude .git"
export FZF_DEFAULT_OPTS="--height=45% --reverse --info=inline"
# export FZF_CTRL_T_OPTS="-m --preview 'bat --color=always --line-range=:500 {}'"
export FZF_ALT_C_COMMAND="fd --type d --hidden --follow --exclude .git . ~"
export FZF_ALT_C_OPTS="--preview 'exa --tree --recurse --git {} | head -200'"
export FZF_COMPLETION_TRIGGER="~~"
# theme vars
export QT_QPA_PLATFORMTHEME=qt5ct
export BAT_THEME=ansi
# change from annoying default of ~/go
export GOPATH=$HOME/.go
PATH=$HOME/bin:$HOME/.ghcup/bin:$PATH
PATH=$PATH:$HOME/.cargo/bin
PATH=$PATH:$HOME/.local/bin:$GOROOT/bin:$GOPATH/bin
# set the user PATH variable
export PATH
