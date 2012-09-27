# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

alias ..='cd .. && ls'
alias ...='cd ../.. && ls'
alias ....='cd ../../.. && ls'
alias gvim='gvim -f'
alias picture='eog'
alias pdf='evince'
alias scan='xsane'
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'
alias .='source'
alias pa='ps -A'
alias pta='ps -eLF'
alias cl='clear'
alias dirs='dirs -p'
alias today='date +"%A, %B %-d, %Y"'
alias b='byobu'

function cs()
{
    if [[ "$#" = "0" ]]; then
        cd && ls;
    else
        cd "$1" && ls;
        pwd > ~/.lastpwd
    fi
}

function pu()
{
	# hate typing in the '+' everytime. If first arg
	# is a number, just assume they mean +num (unless
	# there is a directory named that number! )
	if [[ "$1" =~ "^[0-9]+$" && ! -d "$1" ]]; then
		pushd "+$1" && ls
	elif [[ "$#" = "0" ]]; then
        pushd
    else
		pushd "$1" && ls
	fi
}

function po()
{
	if [[ "$1" =~ "^[0-9]+$" ]]; then
		popd "+$1" && ls
	elif [[ "$#" = "0" ]]; then
        popd
    else
		popd "$1" && ls
	fi
}

tmpfilename='superrarefilenamecannotexist'
function swap()
{
	mv "$1" "$tmpfilename"
	mv "$2" "$1"
	mv "$tmpfilename" "$2"
}
