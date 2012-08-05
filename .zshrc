# Created by bhh1988 for 4.3.10

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# Terminal title bar
precmd() {
  [[ -t 1 ]] || return
  case $TERM in
    (*xterm*|rxvt|(dt|k|E)term) print -Pn "\e]2;%n @ %m : %d\a"
      ;;
  esac
}

START_SSH_AGENT=
if [ -f ~/.ssh-agent -a "$START_SSH_AGENT" = "true" ]; then
	. ~/.ssh-agent
fi

# Turn colors on for prompt
autoload -U colors
colors

# Red prompt, military time. Right prompt is current directory
# and history event number.

PATH_ELEM_NUM='3'
PROMPT="%{$fg[red]%}%B%($((PATH_ELEM_NUM+1))/|.../|)%${PATH_ELEM_NUM}d%(!|#|>)%b%{$reset_color%} "
RPROMPT="%{$fg[red]%}%B%? %* %h%b"

# For loading pre-made prompts.
autoload -U promptinit
promptinit

# Set the default editor to vim. Used by zsh for keybindings.
export EDITOR='vim'

# Set some keybindings
bindkey -v
bindkey '^R' history-beginning-search-backward
# For some reason, '^S' refuses to be mapped...
#bindkey '^S' history-beginning-search-forward
bindkey '^T' history-beginning-search-forward
bindkey '^O' accept-line-and-down-history
bindkey '^J' down-history
bindkey '^K' up-history
bindkey '^G' end-of-history
bindkey '^E' end-of-line
bindkey '^A' beginning-of-line
bindkey '^B' vi-backward-blank-word
bindkey '^F' vi-forward-blank-word-end
bindkey '^U' backward-kill-line
bindkey '^P' kill-line
bindkey '^H' vi-backward-char
bindkey '^L' vi-forward-char
bindkey '^[[3~' delete-char
bindkey '^?' backward-delete-char
bindkey '^W' backward-kill-word
bindkey '^[[3;3~' kill-word
bindkey '\e[Z' reverse-menu-complete

# History
HISTFILE=~/.history
# Appends history to the history file at the end of each session.
setopt APPEND_HISTORY
# Maximum size of the history file
SAVEHIST=10000
# Maximum size of the current session's history buffer
HISTSIZE=1000

# Color ls, color grep
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# Enable zsh's completion system
autoload -U compinit
compinit

# User-host autocompletion
zstyle ':completion:*' users-hosts bhh1988@myth.stanford.edu bhh1988@corn.stanford.edu bhh1988@pup.stanford.edu bhuh@s01-adl-ctrl06.prod.root bhh1988@m-dev-adlearn02.advertising.aol.com

# Menu-style autocompletion
zstyle ':completion:*' menu select 

# Alias autocompletion
setopt completealiases

# Colored completion list
zmodload -i zsh/complist
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

# ALIASES
#==========================================================

# global aliases, don't have to be at the beginning of a line

function cs()
{
	cd $1 && ls;
	pwd > ~/.lastpwd
}

function pu()
{
	# hate typing in the '+' everytime. If first arg
	# is a number, just assume they mean +num (unless
	# there is a directory named that number! )
	if [[ "$1" =~ "^[0-9]+$" && ! -d "$1" ]]; then
		pushd "+$1" && ls
	else
		pushd $1 && ls
	fi
}

function po()
{
	if [[ "$1" =~ "^[0-9]+$" ]]; then
		popd "+$1" && ls
	else
		popd $1 && ls
	fi
}

tmpfilename='superrarefilenamecannotexist'
function swap()
{
	mv $1 $tmpfilename
	mv $2 $1
	mv $tmpfilename $2
}

function flush_history()
{
    fc -W
}

function read_history()
{
    fc -R
}

alias -g H='| head'
alias -g T='| tail'
alias -g L='| less'
alias -g ...='../..'
alias -g ....='../../..'
alias -g .....='../../../..'

# read documents
alias -s txt=$EDITOR
alias -s c=$EDITOR
alias -s h=$EDITOR
alias -s cpp=$EDITOR
alias -s hpp=$EDITOR

alias ..='cd .. && ls'
alias ...='cd ../.. && ls'
alias ....='cd ../../.. && ls'
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'
alias ls='ls --color=auto'
alias .='source'
alias pa='ps -A'
alias pta='ps -eLF'
alias cl='clear'
alias dirs='dirs -p'
alias today='date +"%A, %B %-d, %Y"'
alias b='byobu'

#alias diff='diff -bB'

# ENVIRONMENT VARIABLES

if [ "$ZSH_STARTED" = "" -o "$SHELL" = "screen-bce" ]; then
	cs `cat ~/.lastpwd`
else
	echo '.zshrc sourced!'
fi

export ZSH_STARTED='true'
