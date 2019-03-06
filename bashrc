shopt -s histappend # Append to the history file
shopt -s checkwinsize # Check the window size after each command
shopt -s nocaseglob #case insensitive completion
[[ $- =~ i ]] && stty -ixoff -ixon # Disable CTRL-S and CTRL-Q

if [ "$TERM" != "dumb" ]; then
  bind '"\C-g":" vim $(find ~/notes/* -type f | pick)\n"'
  bind '"\C-q":" cd ~/src/$(find ~/src/* -maxdepth 0 -printf \"%f\n\"| pick)\n"'
fi

export LANG=en_US.UTF-8
export HISTCONTROL=ignoreboth:erasedups
export HISTSIZE=
export HISTFILESIZE=
export EDITOR=vim
export PLATFORM=$(uname -s)
export SB_ROOT=~/src
export GOPATH=$HOME
export GO111MODULE=on # allow go modules in GOPATH
export XDG_CONFIG_HOME=$HOME/.config
export SSH_AUTH_SOCK=$HOME/.ssh/ssh-agent.socket
export _JAVA_AWT_WM_NONREPARENTING=1

if [ -z "$PATH_EXPANDED" ]; then
    # IFS stands for internal field seperator
    join_by() { local IFS="$1"; shift; echo "$*"; }

    binary_directories=(
    ~/toolkit/bin
    $GOPATH/bin
    ~/.nvm/versions/node/v11.2.0/bin
    $HOME/.npm-global/bin
    /usr/local/bin
    $PATH
    )

    export PATH=$(join_by : "${binary_directories[@]}")
    export PATH_EXPANDED=1
fi

source /usr/local/share/chruby/chruby.sh
source /usr/local/share/chruby/auto.sh
chruby 2.5.1

alias nvm="unalias nvm && [ -s "$NVM_DIR/nvm.sh" ] && \. \"$NVM_DIR/nvm.sh\""
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
alias ag='echo "use ripgrep fool!"'

if [ "$PLATFORM" == Darwin ]; then
    export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:.:/usr/local/lib
    export JAVA_HOME=$(/usr/libexec/java_home)
    export COPYFILE_DISABLE=true
    alias acpi="pmset -g batt"
    alias ctags="`brew --prefix`/bin/ctags"
    alias pgstart='pg_ctl -D /usr/local/var/postgres -l /usr/local/var/postgres/server.log start'
    alias find='gfind'
    alias sort='gsort'
    alias gimp='/Applications/GIMP-2.10.app/Contents/MacOS/gimp'
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
