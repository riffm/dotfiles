autoload -U colors && colors
autoload -U promptinit compinit
compinit
promptinit
prompt walters

setopt promptsubst
setopt interactivecomments
setopt hist_ignore_dups
setopt noflowcontrol

zstyle ':completion:*' menu select=2
zstyle ':completion:*' completer _expand _complete _ignored _approximate
zstyle ':completion::complete:*' use-cache 1
setopt completealiases
