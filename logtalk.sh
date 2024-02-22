LOGTALKHOME=$HOME/logtalk3
LOGTALKUSER=$HOME/logtalk3
PATH=$PATH:$LOGTALKHOME/tools/diagrams
PATH=$PATH:$LOGTALKHOME/tools/lgtdoc/xml
PATH=$PATH:$LOGTALKHOME/scripts
PATH=$PATH:$LOGTALKHOME/integration
PATH=$PATH:$HOME/lvm
PATH=$PATH:$HOME/xsb-code/XSB/bin
MANPATH=$MANPATH:$LOGTALKHOME/man
export LOGTALKHOME LOGTALKUSER PATH MANPATH

alias tplgt='$LOGTALKHOME/integration/tplgt.sh'
alias swilgt='$LOGTALKHOME/integration/swilgt.sh'
alias scryerlgt='$LOGTALKHOME/integration/scryerlgt.sh'
alias gplgt='$LOGTALKHOME/integration/gplgt.sh'
alias cxlgt='$LOGTALKHOME/integration/cxlgt.sh'
alias bplgt='$LOGTALKHOME/integration/bplgt.sh'
alias xsblgt='$LOGTALKHOME/integration/xsblgt.sh'
alias yaplgt='$LOGTALKHOME/integration/yaplgt.sh'
alias ciaolgt='$LOGTALKHOME/integration/ciaolgt.sh'
alias eclipselgt='$LOGTALKHOME/integration/eclipselgt.sh'
alias valtplgt='valgrind tpl -q -l $LOGTALKHOME/integration/logtalk_tp.pl -g "{tester}, halt."'
