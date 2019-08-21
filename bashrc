# -*- mode: sh -*-
shopt -s histappend # Append to the history file
shopt -s checkwinsize # Check the window size after each command
shopt -s nocaseglob #case insensitive completion
[[ $- =~ i ]] && stty -ixoff -ixon # Disable CTRL-S and CTRL-Q

if [ "$TERM" != "dumb" ]; then
  bind '"\C-g":" vi $(find ~/notes/* -type f | fzf)\n"'
  bind '"\C-q":" cd ~/src/$(find ~/src/* -maxdepth 0 -printf \"%f\n\"| fzf)\n"'
fi

export LANG=en_US.UTF-8
export HISTCONTROL=ignoreboth:erasedups
export HISTSIZE=
export HISTFILESIZE=
export EDITOR=nvim
export PLATFORM=$(uname -s)
export SB_ROOT=~/src
export GOPATH=$HOME
export GO111MODULE=on # allow go modules in GOPATH
export XDG_CONFIG_HOME=$HOME/.config
export PAGER='less -S'
export NVM_DIR="$HOME/.config/nvm"
export NVM_SOURCE='/usr/share/nvm' # AUR nvm package
export SSH_AUTH_SOCK=$HOME/.ssh/ssh-agent.socket
export ALTERNATE_EDITOR="" # emacs will start in daemon if not running

if [ -z "$PATH_EXPANDED" ]; then
    # IFS stands for internal field seperator
    join_by() { local IFS="$1"; shift; echo "$*"; }

    binary_directories=(
    ~/toolkit/bin
    $GOPATH/bin
    ~/.cargo/bin
    ~/.config/nvm/versions/node/v12.4.0/bin
    $HOME/.npm-global/bin
    /usr/local/bin
    $PATH
    )

    export PATH=$(join_by : "${binary_directories[@]}")
    export PATH_EXPANDED=1
fi

c=/usr/local/share/chruby/chruby.sh; [[ -f $c ]] && source $c

alias pacfiles="pacman -Ql" # list files associated with a package
alias nvm="unalias nvm && [ -s "$NVM_SOURCE/nvm.sh" ] && \. \"$NVM_SOURCE/nvm.sh\""
alias git-root='git rev-parse --show-toplevel || echo "."'
alias tags='ctags -f $(git-root)/.git/tags -R $(git-root)'
alias gr='cd $(git-root)'
alias space='df -h'
alias .space='du -sh * | sort -h'
alias t0="printf '\e[8;50;100t'"
alias t1="printf '\e[8;50;160t'"
alias t2="printf '\e[8;20;100t'"
alias ..='cd ..'
alias t='tmux attach -t vty || tmux new -s vty'
alias y='tmux attach -t vty2 || tmux new -s vty2'
alias be='bundle exec'
alias de='export $(egrep -v "^#" .env | xargs)'
alias cl='for code in {0..16}; do echo -e "\e[38;05;${code}m $code: Test"; done'
alias ag='echo "use ripgrep fool!"'
alias rg='rg -M 180 --colors=path:fg:yellow --colors=path:style:bold'
alias pm='mutt -F ~/.mutt/mikepjb.muttrc'
alias sm='mbsync -a'
alias xclip='xclip -sel clip'
alias kill-emacs='emacsclient --eval "(kill-emacs)"'
alias e='emacsclient -nw'
alias net='sudo netstat -lnp'

# data related
alias jv="jq -C | less -R"
alias json="python -m json.tool"
alias showcsv="column -t -s, | less -S" #use with head -100 a.csv | showcsv
headers() { head -1 "$1" | sed "s/,/\n/g"; } # prints headers line by line
# multi search/replace # find ./ -type f -exec sed -i -e 's/apple/orange/g' {} \;

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

jobs_marker() {
  local n=$(jobs | wc -l)
  ((n)) && echo -n '&' || echo -n '$'
}

PROMPT_COMMAND='PS1="\W($(git_state)) $(jobs_marker) "'
