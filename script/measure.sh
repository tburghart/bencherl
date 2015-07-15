#!/bin/bash -e

[[ -n "$BENCHERL_CMD" ]] || declare -rx BENCHERL_CMD="${0##*/}"

_be_usage_message="Usage: $BENCHERL_CMD [option ...]
Options:
  -h        Display this help and exit.
  -l        List available benchmarks and exit.
  -d        Display pre-configuration environment variables and exit.
            These are the variables you can use in your configuration file.
  -D        Same as '-d', but displays declaration attributes for each variable.
  -p        Display post-configuration environment variables and exit.
            This shows how your conviguration was interpretted.
  -P        Same as '-p', but displays declaration attributes for each variable.
  -c <FILE> Read configuration from FILE (default 'conf/run.conf').
            If FILE does not contain '/' it is assumed to be in 'conf'.
  -n <NAME> Use NAME as the label of this run (default Y.M.D-H:M:S).
            If NAME does not contain '/' it is assumed to be in 'results'.
            Files in pre-existing output directories WILL BE OVERWRITTEN!

The following environment variables are recognized:

BENCHERL_CONF:
    Same as the '-c <FILE>' option. Command line takes precedence.
BENCHERL_LABEL:
    Same as the '-n <NAME>' option. Command line takes precedence.
BENCHERL_WORK:
    Directory for temporary files. If the directory does not exist it is
    created. Default is a transient directory under '/tmp'."

. "$(dirname "${BASH_SOURCE[0]}")/defs.bash"

_be_pre_func=''
_be_post_func=''

while getopts ':c:n:dDhlpP' opt
do
    case $opt in
        'h' )
            usage_exit 0
            ;;
        'l' )
            _be_pre_func='list_benchmarks'
            ;;
        'd' )
            _be_pre_func='display_environment'
            ;;
        'D' )
            _be_pre_func='display_environment -d'
            ;;
        'p' )
            _be_post_func='display_environment'
            ;;
        'P' )
            _be_post_func='display_environment -d'
            ;;
        'c' )
            BENCHERL_CONF="$OPTARG"
            ;;
        'n' )
            BENCHERL_LABEL="$OPTARG"
            ;;
        ':' | \? )
            usage_exit 1 "Invalid option: -$OPTARG"
            ;;
        * )
            usage_exit 1 "Unrecognized parameter '$opt'"
            ;;
    esac
done
if [[ $# -ge  $OPTIND ]]
then
    if [[ $OPTIND -gt 1 ]]
    then
        let 'OPTIND -= 1'
        shift $OPTIND
    fi
    usage_exit 1 "Unrecognized parameter '$1'"
fi
if [[ -z "$BENCHERL_CONF" ]]
then
    BENCHERL_CONF="$BENCHERL_ROOT/conf/run.conf"
elif [[ "$BENCHERL_CONF" != */* ]]
then
    BENCHERL_CONF="$BENCHERL_ROOT/conf/$BENCHERL_CONF"
fi
if [[ ! -f "$BENCHERL_CONF" || ! -r "$BENCHERL_CONF" ]]
then
    error_exit 2 "Can't read configuration file '$BENCHERL_CONF'"
fi

if [[ -n "$_be_pre_func" ]]
then
    $_be_pre_func
    exit
fi

. "$BENCHERL_CONF"

_be_config_mode='main'
. "$BENCHERL_SCRIPTS/normalize.bash"

if [[ -n "$_be_post_func" ]]
then
    $_be_post_func
    exit
fi

IFS="${IFS_DEFAULT},"
declare -a _be_include=($BENCHERL_INCLUDE)
declare -a _be_exclude=($BENCHERL_EXCLUDE)
IFS="$IFS_DEFAULT"
if  [[ ${#_be_include[@]} -eq 0 && ${#_be_exclude[@]} -ne 0 ]] \
    || list_member '*' "${_be_include[@]}"
then
    unset _be_include
    declare -a _be_include=($(list_benchmarks))
fi

declare -a _be_benchmarks
for _be_bench in "${_be_include[@]}"
do
    if ! list_member "$_be_bench" "${_be_exclude[@]}"
    then
        [[ -d "$BENCHERL_ROOT/bench/$_be_bench" ]] || \
            error_exit 2 "Specified benchmark '$_be_bench' not found."
        list_member "$_be_bench" "${_be_benchmarks[@]}" || \
            _be_benchmarks[${#_be_benchmarks[@]}]="$_be_bench"
    fi
done
[[ ${#_be_benchmarks[@]} -gt 0 ]] || error_exit 1 'No benchmarks selected.'

unset _be_include _be_exclude

[[ -d "$BENCHERL_RESULTS" ]] || mkdir "$BENCHERL_RESULTS"

for _be_bench in "${_be_benchmarks[@]}"
do
    BENCHERL_BENCHNAME="$_be_bench"
    BENCHERL_BENCHDIR="$BENCHERL_ROOT/bench/$_be_bench"
    export  BENCHERL_BENCHNAME BENCHERL_BENCHDIR
    if [[ -f "$BENCHERL_BENCHDIR/conf/bench.conf" ]]
    then
        "$BENCHERL_SCRIPTS/confbench.bash"
    else
        . "$BENCHERL_SCRIPTS/runbench.bash"
    fi
done
