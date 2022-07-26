# -*- mode: sh -*-# zsh configuration
export LANG=en_US.UTF-8
export LC_TIME=C
export LC_NUMERIC=C

stty pass8

unlimit
limit stack 8192
limit core 0
limit -s

umask 022

export PATH=~/bin:/opt/local/bin:/sbin:/bin:/usr/sbin:/usr/bin:/usr/local/bin:/usr/local/sbin:~/bin:~/arm-eabi/bin
if [ -x /usr/libexec/path_helper ]; then
    eval `/usr/libexec/path_helper -s`
fi

export PAGER="less"
export EDITOR="emacs -nw"
export BROWSER="w3m"
export LESS="FRQXi"
export PS_FORMAT="user,group,pid,rss,sz,stime,time,cmd"

# 0-black, 1-red, 2-green, 3-yellow, 4-blue, 5-magenta 6-cyan, 7-white
Cr() { echo '%{\033[3'$1'm%}'; }
hc=`Cr 6`; wc=`Cr 3`; tc=`Cr 7`; w=`Cr 7`; n=`Cr 9`; r=`Cr 1`; y=`Cr 6`; gr=`Cr 2`
[ $UID = 0 ] && at=$r%B'#'%b || at=$w'@'
err="%(?..$r%B%?%b )"
wc=`Cr 3`
pc=`Cr 4`
ffc=`Cr 6`
gc=`Cr 2`
rc=`Cr 1`

# make ls looks nice
# if [ -f ~/.dircolors ]; then
#    eval $(dircolors ~/.dircolors)
# fi
# export LSCOLORS="Cxfxcxdxbxegedabagacad"

fpath=(~/.zsh.d $fpath)

# automatically remove duplicates from these arrays
typeset -U path cdpath fpath manpath

# History
HISTFILE=~/.zhistory
HISTSIZE=50000
SAVEHIST=100000
setopt append_history
setopt extended_history
setopt hist_ignore_all_dups
setopt hist_ignore_space
setopt hist_reduce_blanks
setopt autocd
setopt auto_pushd
setopt pushd_ignore_dups

# Another important options
unsetopt extended_glob # it's quite annoying
setopt notify # report the status of backgrounds jobs immediately
setopt completeinword
setopt hash_list_all
REPORTTIME=5
watch=(notme root)

# ZLE
bindkey -e
bindkey "[2~" transpose-words
bindkey "[3~" delete-char
bindkey "[1~" beginning-of-line
bindkey "[4~" end-of-line
bindkey "[A" up-line-or-history
bindkey "[B" down-line-or-history
bindkey '^[z' delete-to-char
# for rxvt
bindkey "\e[8~" end-of-line
bindkey "\e[7~" beginning-of-line


# Loading builtins
autoload -U zmv
# load on reference
zmodload -a zsh/zpty zpty

# press esc-e for editing command line in $EDITOR or $VISUAL
autoload -U edit-command-line
zle -N edit-command-line
bindkey '\ee' edit-command-line

# subword matching, thx piranha
autoload -U select-word-style
select-word-style n
# don't contains -_/= - and thus breaks words on them
zstyle ':zle:*' word-chars '*?.[]~&;!#$%^(){}<>'


######## Completion #######
#hostsmy=(${${${${(f)"$(<$HOME/.ssh/known_hosts)"}:#[0-9]*}%%\ *}%%,*})
#???#zstyle ':completion:*:processes' command 'ps -au$USER'
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.zcompcache
zstyle ':completion:*' completer _expand _complete _correct _approximate
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z} r:|[._-]=* r:|=*' 'm:{a-zA-Z}={A-Za-z} r:|[._-]=* r:|=*' 'm:{a-zA-Z}={A-Za-z} r:|[._-]=* r:|=*' 'm:{a-zA-Z}={A-Za-z} r:|[._-]=* r:|=*'
zstyle ':completion:*:match:*' original only
zstyle ':completion:*' max-errors 1 numeric

# Completing process IDs with menu selection
zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:kill:*'   force-list always

# cd will never select the parent directory (e.g.: cd ../<TAB>)
zstyle ':completion:*:cd:*' ignore-parents parent pwd

zstyle :compinstall filename '.zshrc'

zstyle -e ':completion::*:*:*:hosts' hosts 'reply=(${=${${(f)"$(cat {/etc/ssh_,~/.ssh/known_}hosts(|2)(N) /dev/null)"}%%[# ]*}//,/ })'

compctl -o wget make man rpm iptables
compctl -k $hosts ssh telnet ping mtr traceroute
compctl -j -P "%" kill
compctl -g '*.gz' + -g '*(-/)' gunzip gzcat
compctl -g '*.rar' + -g '*(-/)' rar unrar
compctl -g '*.bz2' + -g '*(-/)' bunzip2 bzcat
compctl -g '*(-*)' + -g '*(-/)' strip
compctl -g '*.ps *.eps' + -g '*(-/)' gs ghostview psnup psduplex ps2ascii
compctl -g '*.dvi *.pdf *.ps *.ps.gz' + -g '*(-/)' evince epdfview
compctl -g '*.xpm *.xpm.gz' + -g '*(-/)' xpmroot sxpm pixmap xpmtoppm
compctl -g '*.fig' + -g '*(-/)' xfig
compctl -g '*(-/) .*(-/)' cd
compctl -g '(^(*.o|*.class|*.jar|*.gz|*.gif|*.a|*.Z|*.bz2))' + -g '.*' less vim
compctl -g '(^(*.o|*.class|*.jar|*.gif|*.a))' + -g '.*' most
compctl -g '*.ltx' + -g '*(-/)' pdflatex
compctl -g '*.wav' auCDtect
compctl -g '*.fb2 *.fb2.zip' FBReader
compctl -f -k "(`cat ~/.ssh/known_hosts | perl -lnae '@C=split(",", $F[0]); print for @C'`)" ssh scp ping


autoload -U compinit
compinit -i

autoload bashcompinit
bashcompinit
# source ~/.git-completion.sh

# xterm header
case $TERM in
xterm*|rxvt*)
    precmd () {
        print -Pn "\033]0;%n@%M (%y) - %/\a"
        print -Pn "\033]1;%n@%m (tty%l)\a"
    }
    preexec () {
        print -Pn "\033]0;%n@%M (%y) - %/ - ($1)\a"
        print -Pn "\033]1;%n@%m (tty%l)\a"
    }
;;
screen)
    preexec () {
    # set screen title
        echo -ne "\ek${1[(w)1]}\e\\"
    }
    precmd () {
    #set screen title
        echo -ne "\ekzsh\e\\"
    }
;;
esac


# Search file, containing string in name
#function ff() { find . -type f -iname '*'$*'*' -ls ; }
function ff() { ls -lh **/*$** ; }

function sup()
{
    if [ -z "${@}" ]; then
        echo "Enter command!"
    else
        CMD="${@}"
        sudo su - -c "$CMD"
    fi
}

# rename file to lowercase
function lowercase()
{
    zmv "($1)" '${(L)1}'
}

function isomake()
{
	if [ -z "$1" ]; then
		echo "isomake: first parameter - iso-file name"
		echo "isomake: second parameter - input dir/file name"
	else
		mkisofs -v -J -r -o $1 $2
	fi
}

function apt-show()
{
    if [ -z "$1" ]; then
        echo "First argument - name of package"
    else
        apt-cache show $1|egrep --color -v \(^Size\|^MD5sum\|^Filename\|^Suggests\|^SHA\|^Architecture\|^Maintainer\|^Section\|^Priority\)
    fi
}

# tail -f, possibly colorized
function t()
{
    if [ -x "`whence -c ccze`" ]; then
        tail -f $1 | ccze -A
    else
        tail -f $1
    fi
}

function ram()
{
    if [ -z "$1" ]; then
        echo "First argument - pattern to grep from processes"
    else
        SUM=0
        for i in `ps aux|grep -i $1|awk '{print $6}'`; do
            SUM=`expr $i + $SUM`
        done
        echo $SUM
    fi
}

function split2flac {
    if [ -z "$2" ]; then
        echo "Usage: split2flac cue-file sound-file"
    else
        cuebreakpoints $1 | shnsplit -o flac $2
        cuetag $1 split-track*.flac
    fi
}

function myeditor {
    if [ -z `ps -C emacs -o pid=` ]; then
        vim ${@}
    else
        # emacsclient -t -c ${@}
        emacs -nw ${@}
    fi
}

function gkill {
    awk '{print $2}'|xargs kill
}

# sorted du -sh
function duf {
    du -sk ${@} | sort -n | while read size fname; do
        for unit in k M G T P E Z Y; do
            if [ $size -lt 1024 ]; then
                echo -e "${size}${unit}\t${fname}"
                break
            fi
            size=$((size/1024))
        done
    done
}

#############        ALIASES         ###############
# Nocorrect
#alias mv="nocorrect mv"
#alias cp="nocorrect cp"
#alias mkdir="nocorrect mkdir"

## LFTP
if [ -x "`whence -c lftp`" ]; then
    alias ftp="lftp"
    function sftp() { lftp sftp://`whoami`@$1 }
fi
## Mutt new generation
if [ -x "`whence -c muttng`" ]; then
        alias m="muttng"
else
        alias m="mutt"
fi
## color ls
if [ `uname` = "Linux" ]; then
        alias ls="/bin/ls --color"
else
        alias ls="/bin/ls -G"
fi
## GNU Find
if [ `uname` != "Linux" -a -x "`whence -c gfind`" ]; then
    alias find="noglob gfind"
else
    alias find="noglob find"
fi
## Editor
if [ -x "`whence -c emacsclient`" ]; then
    alias e="emacs -nw"
    alias ent="emacsclient --no-wait"
    alias et="emacsclient -t"
    export ALTERNATE_EDITOR="emacs -nw"
else
    alias e=$EDITOR
fi

if [ -x "`whence -c rlwrap`" ]; then
    alias nc='rlwrap nc'
fi

# extensions
alias -s jpg=gliv
alias -s png=gliv
alias -s gif=gliv
alias -s flv=smplayer
alias -s avi=smplayer

# ls
alias ll="ls -lh"
alias la="ls -lA"
alias lsd="ls -ld *(-/DN)"
alias lsa="ls -ld .*"

# Other
alias rm="rm -f"
alias grep="egrep"
alias mc="mc"
alias sd="screen -D -r"
alias l=$PAGER
alias g="egrep -i --color -n"
alias ag="rg -i"
alias agg="rg -i --no-ignore --ignore-vcs"
alias f="fd -H --no-ignore"
function gr() { egrep -i --color -r $1 . }
alias h="head"
alias p="ping"
alias df="df -h"
alias bc="bc -l"
alias rezsh="source ~/.zshrc"
alias cal="cal -m"
alias sudo='sudo PATH=$PATH ' # do not touch my PATH!
alias s="sudo "
alias untar="tar zxf"


alias ll="ls -l"
alias grep="grep -I --color"
alias py6='python2.6'
alias py7='python2.7'
alias pych='pylint'
alias ruch='reek -c ~/.reekrc'
alias rocaml="rlwrap ocaml"

alias psc="ps -C"
alias psfg="ps -ylfC"
function psk() { ps -C $1 -o pid= | xargs kill }

function genint() { ocamlc -i "$1" > "$1i" }

# bash to docker container
function dobash() {
    docker exec -i -t $1 bash
}

function gog() {
    cd $GOPATH/src/github.com/$1/$2
}

function magog() {
    cd $GOPATH/src/github.com/little-arhat/$1
}

function wman() {
    if [ -z "$2" ]; then
        $BROWSER https://linux.die.net/man/3/$1
    else
        $BROWSER https://linux.die.net/man/$1/$2
    fi
}

function git-touche() {
    git log --name-only --pretty=format:%n --grep="$1" | sort -u
}

alias config='git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'
alias c++='clang++ -std=c++2a'

# local specific aliases and settings
# local settings can override some settings
if [ -f ~/.zshenv ]; then
    source ~/.zshenv
fi

# for emacs' tramp
[ $TERM = "dumb" ] && unsetopt zle && PS1='$ ' && unalias ls || return 0
