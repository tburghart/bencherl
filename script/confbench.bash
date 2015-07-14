#!/bin/bash -e
# ======================================================================
#
# wrapper to isolate benchmark-specific environment before running
#
# ======================================================================

declare -r  _be_parent="$BENCHERL_CMD"
declare -rx BENCHERL_CMD="${0##*/}"

_be_usage_message="Usage: $BENCHERL_CMD

!!! WARNING
!!! This script should ONLY be invoked by bencherl application scripts!
!!! Direct invocation is likely to fail unpredictably!"

. "$(dirname "$0")/defs.bash"

[[ -n "$_be_parent" && -f "$BENCHERL_SCRIPTS/$_be_parent" ]] || \
    error_exit 3 'Appears to be executed without proper context.'

#
# this wrapper script is only used when there's a separate configuration for
# the target benchmark, so we don't need to check for existence of the file
#
. "$BENCHERL_BENCHDIR/conf/bench.conf"

_be_config_mode='bench'
. "$BENCHERL_SCRIPTS/normalize.bash"

. "$BENCHERL_SCRIPTS/runbench.bash"
