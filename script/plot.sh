#!/bin/bash -e

. "$(dirname "${BASH_SOURCE[0]}")/defs.bash"

BENCHERL_CMD="${0##*/}"

usage_exit()
{
    local xc="${1:-1}"
    local fd
    if [[ $xc -eq 0 ]]
    then
        fd=1
    else
        fd=2
    fi
    cat >&$fd <<EOF
Usage: $BENCHERL_CMD <benchmark-title> <measurements-directory> <graphs-directory>

Collects all of the *.time files in the specified measurements directory
and generates appropriate SVG graphs in the specified graphs directory.
NOTE that *.speedup files in the measurements directory are overwritten!

EOF
    exit $xc
}

[[ $# -eq 3 ]] || usage_exit

for d in "$2" "$3"
do
    if [[ ! -d "$d" ]]
    then
        echo "${BENCHERL_CMD}: error: '$d' is not a directory." >&2
        exit 2
    fi
done

if ! /bin/ls "$2"/*.time 1>/dev/null 2>&1
then
    echo 'Nothing to do.'
    exit
fi

check_required_commands $required_plot_commands

plot_benchmark "$@"
