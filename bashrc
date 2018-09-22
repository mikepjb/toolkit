shopt -s histappend # Append to the history file
shopt -s checkwinsize # Check the window size after each command
shopt -s nocaseglob #case insensitive completion
[[ $- =~ i ]] && stty -ixoff -ixon # Disable CTRL-S and CTRL-Q

bind '"\C-g":" vim $(find ~/notes/* -type f | selecta)\n"'
bind '"\C-q":" cd ~/src/$(find ~/src/* -maxdepth 0 -printf \"%f\n\"| selecta)\n"'

export LANG=en_US.UTF-8
export HISTCONTROL=ignoreboth:erasedups
export HISTSIZE=
export HISTFILESIZE=
export EDITOR=vim
export PLATFORM=$(uname -s)
export SB_ROOT=~/src
export GOPATH=$HOME

if [ -z "$PATH_EXPANDED" ]; then
    # IFS stands for internal field seperator
    join_by() { local IFS="$1"; shift; echo "$*"; }

    binary_directories=(
    ~/trove/bin
    $GOPATH/bin
    $HOME/.npm-global/bin
    /usr/local/bin
    $PATH
    )

    export PATH=$(join_by : "${binary_directories[@]}")
    export PATH_EXPANDED=1
fi

source /usr/local/share/chruby/chruby.sh
source /usr/local/share/chruby/auto.sh
chruby 2.5.0

alias git-root='git rev-parse --show-toplevel || echo "."'
alias tags='ctags -f $(git-root)/.git/tags -R $(git-root)'
alias gr='cd $(git-root)'
alias space='df -h'
alias .space='du -sh * | sort -h'
alias pgstart='sudo systemctl start postgresql'
alias t0="printf '\e[8;50;100t'"
alias t1="printf '\e[8;50;160t'"
alias t2="printf '\e[8;20;100t'"
alias ..='cd ..'
alias t='tmux attach -t vty || tmux new -s vty'
alias y='tmux attach -t vty2 || tmux new -s vty2'
alias be='bundle exec'
alias de='export $(egrep -v "^#" .env | xargs)'
alias cl='for code in {0..16}; do echo -e "\e[38;05;${code}m $code: Test"; done'
alias json="python -m json.tool"
alias csv="column -t -s, | less -S" #use with cat a.csv | csv

if [ "$PLATFORM" == Darwin ]; then
    export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:.:/usr/local/lib
    export JAVA_HOME=$(/usr/libexec/java_home)
    export COPYFILE_DISABLE=true
    alias acpi="pmset -g batt"
    alias ctags="`brew --prefix`/bin/ctags"
    alias pgstart='pg_ctl -D /usr/local/var/postgres -l /usr/local/var/postgres/server.log start'
    alias find='gfind'
    alias sort='gsort'
fi

viw() {
    vi `which "$1"`
}

git_branch() {
    echo -e "$(git branch 2>/dev/null| sed -n '/^\*/s/^\* //p')"
}

git_state() {
    if git rev-parse --git-dir >/dev/null 2>&1; then
        echo -ne "$(git_branch)"
    else
        echo -ne "!"
    fi
}

PROMPT_COMMAND='PS1="\W($(git_state)) \$ "'
