#!/usr/bin/env bash
#
# Enable the bash completions by simply sourcing this file
# and configuring APP to your app's name, e.g.
#
# $ APP=my-app source /path/to/clingon/extras/completions.bash
#

APP=${APP:-$(basename ${BASH_SOURCE})}

function _clingon_app_completions() {
    local cur prev words cword
    _init_completion -s || return

    local _suggestions=$( "${words[@]:0:${cword}}" --bash-completions )
    local _options=$( grep -E '^-' <<<${_suggestions} )
    local _sub_commands=$( grep -v -E '^-' <<<${_suggestions} )

    if [[ "${cur}" == "-"* ]]; then
        # Options only
        COMPREPLY=( $(compgen -W "${_options}" -- "${cur}") )
    else
        # Sub-commands only
        COMPREPLY=( $(compgen -W "${_sub_commands}" -- "${cur}") )
    fi
}

complete -o bashdefault \
         -o default \
         -o nospace \
         -F _clingon_app_completions ${APP}
unset APP
