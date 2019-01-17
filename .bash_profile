[ -f /usr/local/etc/bash_completion ] && . /usr/local/etc/bash_completion

export PATH="/usr/local/bin:$PATH"

#export PYTHONPATH=${PYTHONPATH}:/System/Library/Frameworks/Python.framework/Versions/2.7/Extras/lib/python

# History options
# ###############
HISTFILESIZE=1000000000
HISTSIZE=1000000


# End 

export CLICOLOR=1
export LSCOLORS=GxBxCxDxexegedabagaced
 
parse_git_branch() {
    git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ (\1)/'
}

PS1_OLD=${PS1}
PROMPT_COMMAND=__prompt_command

__prompt_command() {
    local EXIT="$?"

    local rCOL='\[\e[0m\]'

    local BLUE='\[\e[0;94m\]'
    local MAGENTA='\[\e[0;35m\]'
    local bRED='\[\e[1;91m\]'
    local GREEN='\[\e[0;32m\]'
    local BGREEN='\[\e[0;92m\]'

    PS1="${BLUE}\u${rCOL}:${MAGENTA}\w${rCOL}"
    # Add the git branch info to the prompt
    PS1+="${BGREEN}\$(parse_git_branch)${rCOL}"

    # Add the exit status to the prompt
    if [ $EXIT == 0 ]; then
        PS1+=" ${GREEN}:)${rCOL} "
    else
        PS1+=" ${bRED}:(${rCOL} "
    fi

    # Close things out with the classic '$'
    PS1+="$ "
}

# Simple (better) prompt
#export PS1="\u:\w\$(parse_git_branch)$ "


alias ll="ls -al"
alias g2="grep -rinI --color"
alias e2="emacs -nw"

if [[ $- == *i* ]]; then
    # history search for bash
    bind '"\e[A": history-search-backward' # push up-arrow
    bind '"\e[B": history-search-forward'  # push down-arrow
fi
