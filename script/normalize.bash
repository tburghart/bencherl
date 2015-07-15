#!/usr/bin/false This file is meant to be sourced, not run!
# ======================================================================
#
# normalize and export BENCHERL_xxx variables after sourcing a config file
#
# assumes that defs.bash has already been sourced
#
# BENCHERL_name_A is BENCHERL_name broken into array elements on
# appropriate delimiters, and vice-versa
#
# default values are as described in conf/sample.conf
#
# ======================================================================
set -e

_be_unset_vars+=''
# do NOT include _be_ifsin in _be_unset_vars - it gets special handling
_be_ifsin="$IFS"
IFS="$IFS_DEFAULT"

if [[ -n "$BENCHERL_COMPAT" ]]
then
    BENCHERL_COMPAT=$(parse_true_false "$BENCHERL_COMPAT" 'false')
    _be_config_compat_main="$BENCHERL_COMPAT"
    _be_config_compat_bench="$BENCHERL_COMPAT"
fi
if  [[ "$_be_config_mode" == 'main' && "$_be_config_compat_main" == 'true' ]] \
||  [[ "$_be_config_mode" == 'bench' && "$_be_config_compat_bench" == 'true' ]]
then
    if [[ -n "$CHECK_SANITY" ]]
    then
        BENCHERL_CHECK_SANITY="$CHECK_SANITY"
        unset CHECK_SANITY
    fi
    if [[ -n "$COOKIE" ]]
    then
        BENCHERL_COOKIE="$COOKIE"
        unset COOKIE
    fi
    if [[ -n "$ERL_ARGS" ]]
    then
        BENCHERL_VMARGS="$ERL_ARGS"
        unset ERL_ARGS
    fi
    if [[ -n "$EXCLUDE_BENCH" ]]
    then
        BENCHERL_EXCLUDE="$EXCLUDE_BENCH"
        unset EXCLUDE_BENCH
    fi
    if [[ -n "$INCLUDE_BENCH" ]]
    then
        BENCHERL_INCLUDE="$INCLUDE_BENCH"
        unset INCLUDE_BENCH
    fi
    if [[ -n "$ITERATIONS" ]]
    then
        BENCHERL_ITERATIONS="$ITERATIONS"
        unset ITERATIONS
    fi
    if [[ -n "$MASTER_NODE" ]]
    then
        BENCHERL_MASTER="$MASTER_NODE"
        unset MASTER_NODE
    fi
    if [[ -n "$NUMBER_OF_SCHEDULERS" ]]
    then
        BENCHERL_NUM_SCHEDULERS="$NUMBER_OF_SCHEDULERS"
        unset NUMBER_OF_SCHEDULERS
    fi
    if [[ -n "$NUMBER_OF_SLAVE_NODES" ]]
    then
        BENCHERL_NUM_SLAVES="$NUMBER_OF_SLAVE_NODES"
        unset NUMBER_OF_SLAVE_NODES
    fi
    if [[ -n "$OTPS" ]]
    then
        BENCHERL_OTPS="$OTPS"
        unset OTPS
    fi
    if [[ -n "$OUTPUT_FORMAT" ]]
    then
        BENCHERL_OUTPUT="$OUTPUT_FORMAT"
        unset OUTPUT_FORMAT
    fi
    if [[ -n "$PLOT" ]]
    then
        BENCHERL_PLOT="$PLOT"
        unset PLOT
    fi
    if [[ -n "$SKIP_SLAVE_SETUP" ]]
    then
        BENCHERL_SETUP_SLAVES="$(toggle_true_false $SKIP_SLAVE_SETUP)"
        unset SKIP_SLAVE_SETUP
    fi
    if [[ -n "$SLAVE_NODES" ]]
    then
        BENCHERL_SLAVES="$SLAVE_NODES"
        unset SLAVE_NODES
    fi
    if [[ -n "$USE_LONG_NAMES" ]]
    then
        BENCHERL_USE_LONGNAMES="$USE_LONG_NAMES"
        unset USE_LONG_NAMES
    fi
    if [[ -n "$VERSION" ]]
    then
        BENCHERL_LENGTH="$VERSION"
        unset VERSION
    fi
fi

if [[ -z "$BENCHERL_LABEL" ]]
then
    BENCHERL_LABEL="$(date +%Y.%m.%d-%H.%M.%S)"
    BENCHERL_RESULTS="$BENCHERL_ROOT/results/$BENCHERL_LABEL"
elif [[ "$BENCHERL_LABEL" == */* ]]
then
    if [[ -e "$BENCHERL_LABEL" ]]
    then
        [[ -d "$BENCHERL_LABEL" ]] || error_exit 2 \
            "Pre-existing result path '$BENCHERL_LABEL' is not a directory"
    else
        mkdir "$BENCHERL_LABEL"
    fi
    cd "$BENCHERL_LABEL"
    BENCHERL_RESULTS="$(pwd)"
    cd "$BENCHERL_CWD"
else
    BENCHERL_RESULTS="$BENCHERL_ROOT/results/$BENCHERL_LABEL"
fi
export  BENCHERL_LABEL BENCHERL_RESULTS

if ! is_integer "$BENCHERL_ITERATIONS" || [[ "$BENCHERL_ITERATIONS" -lt 1 ]]
then
    BENCHERL_ITERATIONS='1'
fi
export  BENCHERL_ITERATIONS

case "$BENCHERL_LENGTH" in
    short|intermediate|long )
        ;;
    medium )
        BENCHERL_LENGTH='intermediate'
        ;;
    * )
        BENCHERL_LENGTH='short'
        ;;
esac
export  BENCHERL_LENGTH

case "$BENCHERL_OUTPUT" in
    min|max|avg|avg_min_max|plain )
        ;;
    * )
        BENCHERL_OUTPUT='avg'
        ;;
esac
export  BENCHERL_OUTPUT

[[ -n "$BENCHERL_COOKIE" ]] || BENCHERL_COOKIE="bencherl$RANDOM"
export  BENCHERL_COOKIE

BENCHERL_PLOT="$(parse_true_false "$BENCHERL_PLOT" 'false')"
export  BENCHERL_PLOT

BENCHERL_CHECK_SANITY="$(parse_true_false "$BENCHERL_CHECK_SANITY" 'false')"
export  BENCHERL_CHECK_SANITY

_be_unset_vars+=' _be_node_suffix'
[[ -n "$BENCHERL_HOSTNAME" ]] || BENCHERL_HOSTNAME="$(hostname)"
BENCHERL_USE_LONGNAMES="$(parse_true_false "$BENCHERL_USE_LONGNAMES" 'true')"
if $BENCHERL_USE_LONGNAMES
then
    _be_node_suffix="@$BENCHERL_HOSTNAME"
else
    _be_node_suffix=''
fi
export  BENCHERL_HOSTNAME BENCHERL_USE_LONGNAMES

declare -i  _be_count _be_index _be_alt_x
_be_unset_vars+=' _be_count _be_index _be_alt_x _be_data'

[[ -n "$BENCHERL_NUM_SCHEDULERS" ]] || BENCHERL_NUM_SCHEDULERS='1'

unset BENCHERL_NUM_SCHEDULERS_A
declare -ai BENCHERL_NUM_SCHEDULERS_A=( \
    $(filter_num_list 1 $(parse_num_list $BENCHERL_NUM_SCHEDULERS)))
if  [[ ${#BENCHERL_NUM_SCHEDULERS_A[@]} -gt 1 \
    && ${BENCHERL_NUM_SCHEDULERS_A[0]} -ne 1 ]]
then
    BENCHERL_NUM_SCHEDULERS_A=(1 ${BENCHERL_NUM_SCHEDULERS_A[*]})
fi
IFS=','
BENCHERL_NUM_SCHEDULERS="${BENCHERL_NUM_SCHEDULERS_A[*]}"
export  BENCHERL_NUM_SCHEDULERS BENCHERL_NUM_SCHEDULERS_A
IFS="$IFS_DEFAULT"

[[ -n "$BENCHERL_MASTER" ]] || BENCHERL_MASTER="master$_be_node_suffix"
export  BENCHERL_MASTER

BENCHERL_SETUP_SLAVES="$(parse_true_false "$BENCHERL_SETUP_SLAVES" 'true')"
export  BENCHERL_SETUP_SLAVES

[[ -n "$BENCHERL_NUM_SLAVES" ]] || BENCHERL_NUM_SLAVES='0'

unset BENCHERL_NUM_SLAVES_A BENCHERL_SLAVES_A
declare -ai BENCHERL_NUM_SLAVES_A=( \
    $(filter_num_list 0 $(parse_num_list $BENCHERL_NUM_SLAVES)))
declare -a  BENCHERL_SLAVES_A

_be_index="${#BENCHERL_NUM_SLAVES_A[@]} - 1"
_be_count="${BENCHERL_NUM_SLAVES_A[$_be_index]}"
if [[ "$_be_count" -gt 0 ]]
then
    BENCHERL_SLAVES_A=(${BENCHERL_SLAVES//,/ })
    _be_index="${#BENCHERL_SLAVES_A[@]}"
    while [[ $_be_index -lt $_be_count ]]
    do
        _be_alt_x='_be_index + 1'
        BENCHERL_SLAVES_A[$_be_index]="slave$_be_alt_x$_be_node_suffix"
        _be_index=$_be_alt_x
    done
fi
IFS=','
BENCHERL_NUM_SLAVES="${BENCHERL_NUM_SLAVES_A[*]}"
BENCHERL_SLAVES="${BENCHERL_SLAVES_A[*]}"
export  BENCHERL_NUM_SLAVES BENCHERL_NUM_SLAVES_A
export  BENCHERL_SLAVES BENCHERL_SLAVES_A
# IFS remains ','

unset BENCHERL_OTPS_A
declare -a BENCHERL_OTPS_A
_be_unset_vars+=' _be_erl _be_erlc _be_elem _be_elem_name _be_elem_val'
_be_count='0'
for _be_elem in $BENCHERL_OTPS
do
    _be_elem_name="${_be_elem%%=*}"
    _be_elem_val="${_be_elem#*=}"
    if [[ -z "$_be_elem_val" ]]
    then
        _be_erl="$(type -P erl)"
    elif [[ -f "$_be_elem_val" && -x "$_be_elem_val" \
        && "${_be_elem_val##*/}" == 'erl' ]]
    then
        _be_erl="$_be_elem_val"
    else
        _be_erl="$_be_elem_val/bin/erl"
    fi
    _be_erlc="${_be_erl}c"
    if  [[ -f "$_be_erl" && -x "$_be_erl" \
        && -f "$_be_erlc" && -x "$_be_erlc" ]]
    then
        [[ -n "$_be_elem_name" ]] || \
            _be_elem_name="OTP$(otp_info "$_be_erl" | awk '{print $1}')"
        BENCHERL_OTPS_A[$_be_count]="${_be_elem_name}=${_be_erl}"
        let '_be_count += 1'
    else
        echo "$BENCHERL_CMD: skipping invalid OTP '$_be_elem'" >&2
    fi
done
if [[ $_be_count -lt 1 ]]
then
    IFS="$IFS_DEFAULT"
    _be_erl="$(type -P erl)"
    [[ -n "$_be_erl" && -f "$_be_erl" && -x "$_be_erl" ]] || \
        error_exit 2 "no executable 'erl' program found"
    _be_erlc="${_be_erl}c"
    [[ -f "$_be_erlc" && -x "$_be_erlc" ]] || \
        error_exit 2 "executable '$_be_erlc' program not found"
    _be_elem_name="OTP$(otp_info "$_be_erl" | awk '{print $1}')"
    BENCHERL_OTPS_A[0]="${_be_elem_name}=${_be_erl}"
    IFS=','
fi
BENCHERL_OTPS="${BENCHERL_OTPS_A[*]}"
export  BENCHERL_OTPS BENCHERL_OTPS_A
# IFS remains ','

[[ -n "$BENCHERL_VMARGS" ]] || BENCHERL_VMARGS='Default='

unset BENCHERL_VMARGS_A
declare -a BENCHERL_VMARGS_A
_be_count='0'
for _be_elem in $BENCHERL_VMARGS
do
    _be_elem_name="${_be_elem%%=*}"
    _be_elem_val="${_be_elem#*=}"
    if [[ -z "$_be_elem_name" ]]
    then
        if [[ -z "$_be_arg_vals" ]]
        then
            _be_elem_name='NoArgs'
        else
            let '_be_alt_x = (_be_count + 1)'
            _be_elem_name="Args$_be_alt_x"
        fi
    fi
    BENCHERL_VMARGS_A[$_be_count]="${_be_elem_name}=${_be_elem_val}"
    let '_be_count += 1'
done
BENCHERL_VMARGS="${BENCHERL_VMARGS_A[*]}"
export  BENCHERL_VMARGS BENCHERL_VMARGS_A

IFS="$IFS_DEFAULT"
unset $_be_unset_vars _be_unset_vars

IFS="$_be_ifsin"
unset _be_ifsin
