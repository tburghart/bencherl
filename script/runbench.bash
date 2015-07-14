#!/usr/bin/false This file is meant to be sourced, not run!

#
# There are too many variables that need to be set properly ... assume they are
#
unset _be_bench_deps _be_extra_code

IFS="${IFS_DEFAULT},"
declare -a  _be_bench_deps=($DEPENDENCIES)

IFS="$IFS_DEFAULT"
declare -a  _be_extra_code=($EXTRA_CODE_PATH)

_be_have_deps=$(parse_true_false ${#_be_bench_deps[@]})
_be_have_extra=$(parse_true_false ${#_be_extra_code[@]})

_be_bench_result="$BENCHERL_RESULTS/$BENCHERL_BENCHNAME"
_be_bench_work="$BENCHERL_WORK/bench/$BENCHERL_BENCHNAME"
_be_suite_work="$BENCHERL_WORK/suite"
_be_apps_work="$BENCHERL_WORK/apps"

_be_bench_mdir="$_be_bench_result/measurements"
_be_bench_odir="$_be_bench_result/output"

_be_work_mdir="$_be_bench_work/measurements"
_be_work_odir="$_be_bench_work/output"

for _be_dir in \
    "$_be_bench_mdir" "$_be_bench_odir" "$_be_work_mdir" "$_be_work_odir"
do
    [[ -d "$_be_dir" ]] || mkdir -p "$_be_dir"
done

# make sure none of this stuff gets passed through in the environment
unset APPD EBIN ERL ERLC ERL_LIBS ERL_LIB_DIR ERLCFLAGS ERLC_FLAGS ERLC_OPTS OTP

if $BENCHERL_USE_LONGNAMES
then
    _be_node_param="-name $BENCHERL_MASTER"
else
    _be_node_param="-sname $BENCHERL_MASTER"
fi

declare -i  _be_sched_cnt=${#BENCHERL_NUM_SCHEDULERS_A[@]}
declare -i  _be_slave_cnt=${#BENCHERL_NUM_SLAVES_A[@]}
declare -i  _be_sched_max=${BENCHERL_NUM_SCHEDULERS_A[$(($_be_sched_cnt - 1))]}
declare -i  _be_slave_max=${BENCHERL_NUM_SLAVES_A[$(($_be_slave_cnt - 1))]}
declare -i  _be_sched_cur _be_slave_cur

for _be_otp in "${BENCHERL_OTPS_A[@]}"
do
    declare -x  BENCHERL_OTP="${_be_otp%%=*}"
    declare -x  BENCHERL_ERL="${_be_otp#*=}"

    _be_erlc="${BENCHERL_ERL}c"

    _be_suite_ebin="$_be_suite_work/$BENCHERL_OTP/ebin"
    _be_bench_ebin="$_be_bench_work/$BENCHERL_OTP/ebin"

    _be_erlrt_info="$(otp_info "$BENCHERL_ERL")"
    _be_erlrt_code="${_be_erlrt_info#* }"
    _be_erlrt_info="${_be_erlrt_info%% *}"

    printf "*** Building benchmark '%s' with %s (OTP Release %u) ...\\n" \
        "$BENCHERL_BENCHNAME" "$BENCHERL_OTP" "$_be_erlrt_info"

    unset _be_bench_code
    declare -a  _be_bench_code=(${_be_extra_code[*]})
    if $_be_have_deps
    then
        if [[ ${#_be_bench_code[@]} -gt 0 ]]
        then
            _be_erlc_opts="-pa ${_be_bench_code[*]}"
        else
            _be_erlc_opts=''
        fi
        for _be_app in "${_be_bench_deps[@]}"
        do
            cd "$BENCHERL_ROOT/app/$_be_app"
            _be_appd="$_be_apps_work/$_be_app/$BENCHERL_OTP"
            _be_ebin="$_be_appd/ebin"
            [[ -d "$_be_ebin" ]] || mkdir -p "$_be_ebin"
            make app \
                "OTP=$BENCHERL_OTP" "ERL=$BENCHERL_ERL" "ERLC=$_be_erlc" \
                "OTPREL=$_be_erlrt_info" "EBIN=$_be_ebin" "APPD=$_be_appd" \
                "ERLC_OPTS=$_be_erlc_opts" "ERL_LIB_DIR=$_be_erlrt_code"
            _be_bench_code[${#_be_bench_code[@]}]="$_be_ebin"
        done
        unset _be_deps _be_appd _be_erlc_opts
    fi

    cd "$BENCHERL_ROOT/suite"
    _be_ebin="$_be_suite_ebin"
    [[ -d "$_be_ebin" ]] || mkdir -p "$_be_ebin"
    make suite \
        "OTP=$BENCHERL_OTP" "ERL=$BENCHERL_ERL" "ERLC=$_be_erlc" \
        "OTPREL=$_be_erlrt_info" "EBIN=$_be_ebin" "ERL_LIB_DIR=$_be_erlrt_code"
    _be_bench_code[${#_be_bench_code[@]}]="$_be_ebin"

    cd "$BENCHERL_BENCHDIR"
    _be_ebin="$_be_bench_ebin"
    [[ -d "$_be_ebin" ]] || mkdir -p "$_be_ebin"
    make bench \
        "OTP=$BENCHERL_OTP" "ERL=$BENCHERL_ERL" "ERLC=$_be_erlc" \
        "OTPREL=$_be_erlrt_info" "EBIN=$_be_ebin" \
        "ERLC_OPTS=-pa ${_be_bench_code[*]}" "ERL_LIB_DIR=$_be_erlrt_code"
    _be_bench_code[${#_be_bench_code[@]}]="$_be_ebin"

    declare -x  BENCHERL_CODE_DIRS="${_be_bench_code[*]}"

    if [[ -n "$WORKING_DIRECTORY" && -d "$WORKING_DIRECTORY" ]]
    then
        cd "$WORKING_DIRECTORY"
    else
        cd "$BENCHERL_CWD"
    fi

    for _be_arg in "${BENCHERL_VMARGS_A[@]}"
    do
        declare -x  BENCHERL_VMARGS="${_be_arg%%=*}"
        declare -x  BENCHERL_VMOPTS="${_be_arg#*=}"
        _be_fn_pre="${BENCHERL_OTP}.${BENCHERL_VMARGS}"
        #
        # each num-schedulers iteration is done with the highest num-slaves
        # each num-slaves iteration is done with the highest num-schedulers
        #
        # avoid running the overlapping test twice
        #
        _be_mode='sched'
        _be_time_file="$_be_work_mdir/${_be_fn_pre}.${_be_mode}.time"
        declare -x  BENCHERL_NSLAVE="$_be_slave_max"
        for _be_sched_cur in ${BENCHERL_NUM_SCHEDULERS_A[@]}
        do
            _be_id="${_be_fn_pre}.${_be_sched_cur}.${_be_slave_max}"
            _be_out_file="$_be_work_odir/${_be_id}.output"
            [[ ! -f "$_be_out_file" ]] || continue

            declare -x  BENCHERL_NSCHED="$_be_sched_cur"

            [[ ! -f "$BENCHERL_BENCHDIR/conf/pre_bench" ]] || \
                "$BENCHERL_BENCHDIR/conf/pre_bench"

            echo "{bench, '$BENCHERL_BENCHNAME'}." >"$BENCHERL_BECONFIG"
            echo "{version, '$BENCHERL_LENGTH'}." >>"$BENCHERL_BECONFIG"
            echo "{erl_exec, \"$BENCHERL_ERL\"}." >>"$BENCHERL_BECONFIG"
            echo "{erl_args, \"$BENCHERL_VMOPTS" $EXTRA_ERL_ARGS \
                    "+S$_be_sched_cur:$_be_sched_cur" \
                    "-setcookie '$BENCHERL_COOKIE'" \
                    "-noshell -pa $BENCHERL_CODE_DIRS\"}." >>"$BENCHERL_BECONFIG"
            echo "{number_of_slaves, $BENCHERL_NSLAVE}." >>"$BENCHERL_BECONFIG"
            echo "{number_of_schedulers, $BENCHERL_NSCHED}." >>"$BENCHERL_BECONFIG"
            echo "{slaves, [$BENCHERL_SLAVES]}." >>"$BENCHERL_BECONFIG"
            echo "{master, '$BENCHERL_MASTER'}." >>"$BENCHERL_BECONFIG"
            echo "{iterations, $BENCHERL_ITERATIONS}." >>"$BENCHERL_BECONFIG"
            echo "{outfile, \"$_be_out_file\"}." >>"$BENCHERL_BECONFIG"
            echo "{measfile, \"$_be_time_file\"}." >>"$BENCHERL_BECONFIG"
            echo "{datadir, \"$BENCHERL_BENCHDIR/data\"}." >>"$BENCHERL_BECONFIG"
            echo "{what, '$_be_mode'}." >>"$BENCHERL_BECONFIG"
            echo "{use_long_names, $BENCHERL_USE_LONGNAMES}." >>"$BENCHERL_BECONFIG"
            echo "{number_of_cores, $BENCHERL_CORES}." >>"$BENCHERL_BECONFIG"
            echo "{output_format, '$BENCHERL_OUTPUT'}." >>"$BENCHERL_BECONFIG"
            echo "{setup_slaves, $BENCHERL_SETUP_SLAVES}." >>"$BENCHERL_BECONFIG"

            printf "*** Running '%s' with %u schedulers and %u slaves ...\\n" \
                "$BENCHERL_BENCHNAME" "$BENCHERL_NSCHED" "$BENCHERL_NSLAVE"

            "$BENCHERL_ERL" \
                $BENCHERL_VMOPTS $EXTRA_ERL_ARGS \
                "+S$_be_sched_cur:$_be_sched_cur" \
                $_be_node_param -setcookie "$BENCHERL_COOKIE" \
                -noshell -pa $BENCHERL_CODE_DIRS \
                -s run_bench main -s erlang halt

            [[ ! -f "$BENCHERL_BENCHDIR/conf/post_bench" ]] || \
                "$BENCHERL_BENCHDIR/conf/post_bench"
        done
    done
    # end of "${BENCHERL_VMARGS[@]}"
done
# end of "${BENCHERL_OTPS_A[@]}"

if ls "$_be_work_odir"/* 1>/dev/null 2>&1
then
    cp -p "$_be_work_odir"/* "$_be_bench_odir"
fi
if ls "$_be_work_mdir"/* 1>/dev/null 2>&1
then
    cp -p "$_be_work_mdir"/* "$_be_bench_mdir"
fi

