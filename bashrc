# bashrc for OSX/Arch Linux

export PLATFORM=$(uname -s)
[ -f /etc/bashrc ] && . /etc/bashrc
[ -f /etc/bash_completion ] && . /etc/bash_completion

shopt -s histappend # Append to the history file
shopt -s checkwinsize # Check the window size after each command
shopt -s nocaseglob #case insensitive completion
[[ $- =~ i ]] && stty -ixoff -ixon # Disable CTRL-S and CTRL-Q

bind '"\C-g":" nvim $(find ~/notes/* -type f | selecta)\n"'
bind '"\C-q":" cd ~/code/$(find ~/code/* -maxdepth 0 -printf \"%f\n\"| selecta)\n"'

export LANG=en_US.UTF-8
export HISTCONTROL=ignoreboth:erasedups
export HISTSIZE=
export HISTFILESIZE=
export EDITOR=nvim
export SB_ROOT=~/code

[ -z "$TMPDIR" ] && TMPDIR=/tmp

if [ -z "$PATH_EXPANDED" ]; then
    # IFS stands for internal field seperator
    join_by() { local IFS="$1"; shift; echo "$*"; }

    binary_directories=(
    ~/tools/bin
    /opt/bin
    $HOME/.cargo/bin
    /usr/local/bin
    /usr/local/share/python
    /usr/local/opt/go/libexec/bin
    /usr/local/texlive/2016/bin/x86_64-darwin/
    $PATH
    )

    export PATH=$(join_by : "${binary_directories[@]}")
    export PATH_EXPANDED=1
fi

source /usr/local/share/chruby/chruby.sh
source /usr/local/share/chruby/auto.sh
chruby 2.3.1

[ -f /usr/share/nvm/init-nvm.sh ] && . /usr/share/nvm/init-nvm.sh
# Do I want fzf in bash? [ -f ~/.fzf.bash ] && source ~/.fzf.bash

alias em='emacs -nw'
alias tags='ctags -R $(git rev-parse --show-toplevel || echo ".")'
alias space='df -h'
alias .space='du -h'
alias l='ls -alF'
alias pgstart='sudo systemctl start postgresql'
alias brd='nohup boot repl -p 9999 -s wait &> /dev/null &'
alias y='launch-repl'
alias lrd='$(nohup lein repl :headless :port 9999 0>&- &>/dev/null &)'
alias lrc='lein repl :connect 9999'
alias lrx='tear-down-repls'
alias lra="ps ex | ag 'lein.*.repl' | grep -v 'ag lein' | cut -d ' ' -f1"
alias ts0="printf '\e[8;50;100t'"
alias ts1="printf '\e[8;50;160t'"
alias ts2="printf '\e[8;20;100t'"
alias ..='cd ..'
alias gr='cd $(git rev-parse --show-toplevel || echo ".")'
alias t='tmux attach -t vty || tmux new -s vty'
alias json='python -m json.tool'
alias vimdiff='nvim -d'

# Ice dev
alias ice='cd ~/.config/nvim/plugged/ice.nvim'
export NVIM_RUBY_LOG_FILE=~/helpful.log

if [ "$PLATFORM" == Darwin ]; then
    export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:.:/usr/local/lib
    export JAVA_HOME='/Library/Java/JavaVirtualMachines/jdk1.8.0_121.jdk/Contents/Home'
    export COPYFILE_DISABLE=true
    alias ls='ls -G'
    export LSCOLORS="dxgxcxdxexegedabagacad"
    alias acpi="pmset -g batt"
    alias ctags="`brew --prefix`/bin/ctags"
    alias pgstart='pg_ctl -D /usr/local/var/postgres -l /usr/local/var/postgres/server.log start'
    alias find='gfind'
else
    eval "`dircolors -b`"
    alias ls='ls --color=auto'
    alias grep='grep --color=auto'
    alias screenshot=xscrot
    export LS_COLORS='di=33:ln=36:so=32:pi=33:ex=34:bd=34;46:cd=34;43:su=0;41:sg=0;46:tw=0;42:ow=0;43:'
fi

RED="\[\e[0;31m\]"
GREEN="\[\e[0;32m\]"
YELLOW="\[\e[0;33m\]"
BLUE="\[\e[34m\]"
MAGENTA="\[\e[35m\]"
CYAN="\[\e[36m\]"
NORMAL="\[\033[m\]"

git_branch() {
    echo -e "$(git branch 2>/dev/null| sed -n '/^\*/s/^\* //p')"
}

git_state() {
    if git rev-parse --git-dir >/dev/null 2>&1; then
        if git diff --quiet 2>/dev/null >&2; then
            color=$GREEN
        else
            color=$RED
        fi
        echo -ne "${color}$(git_branch)${NORMAL}"
    else
        echo -ne "${CYAN}!${NORMAL}"
    fi
}

is_job_active_prompt() {
    if [[ $(jobs -p) -eq "" ]]; then
        color=$CYAN
    else
        color=$YELLOW
    fi
    echo -ne "${color}\$${NORMAL}"
}

PROMPT_COMMAND='PS1="\W($(git_state)) $(is_job_active_prompt) "'

viw() {
    vi `which "$1"`
}

cs() {
    cd *$1*
}
