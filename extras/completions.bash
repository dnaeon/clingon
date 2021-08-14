#!/usr/bin/env bash
#
# Enable the bash completions by simply sourcing this file
# and configuring APP to your app's name, e.g.
#
# $ APP=my-app source /path/to/clingon/extras/completions.bash
#

APP=${APP:-$(basename ${BASH_SOURCE})}

function _clingon_bash_completions() {
    local current="${COMP_WORDS[${COMP_CWORD}]}"
    local suggestions=$( ${COMP_WORDS[@]:0:${COMP_CWORD}} --bash-completions )
    COMPREPLY=( $(compgen -W "${suggestions}" -- "${current}") )
}

complete -o bashdefault \
         -o default \
         -o nospace \
         -F _clingon_bash_completions ${APP}
