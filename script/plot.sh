#!/bin/bash -e

declare -rx BENCHERL_CMD="${0##*/}"

_be_usage_message="Usage: $BENCHERL_CMD \
<benchmark-title> <measurements-directory> <graphs-directory>

Collects all of the *.time files in the specified measurements directory
and generates appropriate SVG graphs in the specified graphs directory.
NOTE that *.speedup files in the measurements directory are overwritten!"

. "$(dirname "${BASH_SOURCE[0]}")/defs.bash"

[[ $# -eq 3 ]] || usage_exit

[[ -d "$2" ]] || error_exit 2 "'$d' is not a directory."

[[ -d "$3" ]] || mkdir "$3"

if ! /bin/ls "$2"/*.time 1>/dev/null 2>&1
then
    echo 'Nothing to do.'
    exit
fi

check_required_commands $required_plot_commands

plot_benchmark "$@"
