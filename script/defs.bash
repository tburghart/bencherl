#!/usr/bin/false This file is meant to be sourced, not run!
# ======================================================================
#
# shared bash definitions
#
# a LOT of stuff in here has subtle bash dependencies including, but by no
# means limited to:
#
#   non-posix 'echo' options
#   $'escaped-string' expansion
#   'read' into array assignment
#   redirect variable to standard input with '<<<'
#
# it would be a mistake to assume that this files would be handled properly
# by any other shell, including '/bin/sh'
#
# ======================================================================
#
# we DON'T want POSIX behavior, that's why we're using bash!
#
set +o posix

#
# identify package location
# other environment setup is performed at the end of this file
#
declare -xr BENCHERL_SCRIPTS="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
declare -xr BENCHERL_ROOT="$(dirname "$BENCHERL_SCRIPTS")"
declare -xr BENCHERL_CWD="$(pwd)"

#
# _be_required_{bench|plot}_commands are space-delimited lists of commands
# used by functions in this module
#
# the concatenation of these provides the default list checked by the
# check_required_commands function if it is invoked without arguments
#
_be_required_bench_commands+=''
_be_required_plot_commands+=''

#
# default config-file compatibility
#
_be_config_mode='main'
_be_config_compat_main='false'
_be_config_compat_bench='true'

#
# some easy-access constants
#
readonly  N=$'\n'
readonly  R=$'\r'
readonly  S=$'\x20'
readonly  T=$'\t'
#
# include carriage-return to avoid choking on Windows
#
readonly  IFS_DEFAULT=$'\x20\t\n\r'
readonly  IFS_COMMA=','
readonly  IFS_LINES=$'\n\r'
readonly  IFS_SPACE=$'\x20'
readonly  IFS_TABS=$'\t'

#
# temp file handling
#
[[ -n "$_be_mktemp_template" ]] || \
    readonly _be_mktemp_template="/tmp/$RANDOM.$RANDOM.XXXXXXX"
trap "/bin/rm -rf ${_be_mktemp_template//X/?}" EXIT

#
# display_environment [-d]
#
# show the BENCHERL environment variables
#
display_environment()
{
    if [[ "$1" == '-d' ]]
    then
        declare -p $(set | \
            sed -n 's/^\(BENCHERL[[:alnum:]_]*\)=.*$/\1/p' | sort -u)
    else
        set | egrep '^BENCHERL[[:alnum:]_]+=' | sort
    fi
}

#
# create and return the path to an empty temporary directory
#
temp_dir()
{
    mktemp -d "$_be_mktemp_template"
}

#
# create and return the path to an empty temporary file
#
temp_file()
{
    mktemp "$_be_mktemp_template"
}

#
# echoes the number of arguments passed
# use for finding the number of elements in a whitespace-delimited list
#
count_list()
{
    echo $#
}

#
# echoes the first argument ... hey, we're Erlang-y
#
list_head()
{
    echo "$1"
}

#
# echoes the last argument
#
list_last()
{
    local val
    for val in "$@" ; do true ; done
    echo "$val"
}

#
# is_integer value
#
is_integer()
{
    if [[ "$1" =~ ^[+-]?[[:digit:]]+$ ]]
    then
        return 0
    else
        return 1
    fi
}

#
# error_exit exit-code [error-message ...]
#
# displays error: [$BENCHERL_CMD:] messages ...
#
error_exit()
{
    local -i xc="${1:-1}"
    local -i fd='xc ? 2 : 1'
    shift
    if [[ -n "$BENCHERL_CMD" ]]
    then
        echo "error: ${BENCHERL_CMD}: $*" >&$fd
    else
        echo "error: $*" >&$fd
    fi
    exit $xc
}

#
# usage_exit exit-code [error-message ...]
#
# if messages are specified, displays as if by error_exit()
#
# usage message should be in $_be_usage_message w/o leading or trailing newlines
#
usage_exit()
{
    local -i xc="${1:-1}"
    local -i fd='xc ? 2 : 1'
    shift
    if [[ $# -gt 0 ]]
    then
        if [[ -n "$BENCHERL_CMD" ]]
        then
            printf '\nerror: %s: %s\n' "$BENCHERL_CMD" "$*" >&$fd
        else
            printf '\nerror: %s\n' "$*" >&$fd
        fi
    fi
    if [[ -n "$_be_usage_message" ]]
    then
        printf '\n%s\n\n' "$_be_usage_message" >&$fd
    elif [[ -n "$BENCHERL_CMD" ]]
    then
        printf '\nUsage: %s ...\n\n' "$BENCHERL_CMD" >&$fd
    fi
    exit $xc
}


#
# sorts and deduplicates a list of $IFS-delimited single words
# regardless of the input delimiters, the output is space-delimited
#
sort_and_dedup_text()
{
    local w
    echo $(for w in $*
    do
        echo $w
    done | sort -u)
}

#
# sorts and deduplicates a list of $IFS-delimited single numbers
# regardless of the input delimiters, the output is space-delimited
#
sort_and_dedup_num()
{
    local n
    echo $(for n in $*
    do
        echo $n
    done | sort -gu)
}

#
# checks that the specified commands, or $_be_required_{bench|plot}_commands
# if none are specified, exist in some callable form
#
# an error message is printed to standard error for each missing command
#
# the return value is the number of commands not found, so zero == success
#
check_required_commands()
{
    if [[ $# -lt 1 ]]
    then
        local -a cmds=($(sort_and_dedup_text \
            $_be_required_bench_commands $_be_required_plot_commands))
    else
        local -a cmds=($(sort_and_dedup_text $*))
    fi
    local cnt=0
    local cmd
    for cmd in "${cmds[@]}"
    do
        if ! type -t "$cmd" 1>/dev/null 2>&1
        then
            echo "Error: command '$cmd' not found." >&2
            let 'cnt += 1'
        fi
    done
    return  $cnt
}

#
# parses a comma/space delimited list of numbers or erlang-style integer sequences
#
parse_num_list()
{
    local ifsin="$IFS"
    local ival
    local list
    IFS="${IFS_DEFAULT},"
    list="$(for ival in $*
    do
        if [[ "$ival" == *..* ]]
        then
            seq ${ival%%.*} ${ival##*.}
        else
            echo $ival
        fi
    done)"
    IFS="$ifsin"
    sort_and_dedup_num $list
}

#
# filter_num_list floor integer ...
#
filter_num_list()
{
    local ifsin="$IFS"
    IFS="$IFS_DEFAULT"
    local -i floor="$1"
    shift
    local delim=''
    local val
    for val in "$@"
    do
        if [[ "$val" -ge "$floor" ]]
        then
            echo -n "$delim$val"
            [[ -n "$delim" ]] || delim=' '
        fi
    done
    [[ -z "$delim" ]] || echo
    IFS="$ifsin"
}

#
# parse_true_false value [... default]
#
# evaluates the first non-empty parameter (or '' if there is none)
# and echoes 'true' or 'false'
#
# the parameter is evaluated as follows:
#
#   empty ('') ==> false
#   zero ([+-]nnn) ==> false
#   case-insensitive n|no|false ==> false
#   anything else ==> true
#
parse_true_false()
{
    local ifsin="#IFS"
    IFS="$IFS_DEFAULT"
    local val=''
    for val in "$@" ; do [[ -z "$val" ]] || break ; done
    if [[ -z "$val" ]]
    then
        val='false'
    elif [[ "$val" =~ ^[+-]?[[:digit:]]+$ ]]
    then
        if [[ $val -eq 0 ]]
        then
            val='false'
        else
            val='true'
        fi
    elif [[ "$val" =~ ^(([Nn][Oo]?)|([Ff][Aa][Ll][Ss][Ee]))$ ]]
    then
        val='false'
    else
        val='true'
    fi
    echo "$val"
    IFS="$ifsin"
}

#
# operates like parse_true_false, returning the oposite result
#
toggle_true_false()
{
    local ifsin="#IFS"
    IFS="$IFS_DEFAULT"
    if $(parse_true_false "$@")
    then
        echo false
    else
        echo true
    fi
    IFS="$ifsin"
}

#
# otp_info path-to-erl-executable
#
# echoes major-version library-directory
#
otp_info()
{
    local erl="$1"
    [[ -e "$erl" ]] || error_exit 2 \
        "Erlang runtime '$erl' not found."
    [[ -f "$erl" ]] || error_exit 3 \
        "Erlang runtime '$erl' is not a file."
    [[ -x "$erl" ]] || error_exit 3 \
        "Erlang file '$erl' is not executable."

    local cmds='VS = case erlang:system_info(otp_release) of'
    cmds+=' [$R | S] -> S; S -> S end'
    cmds+=', {MV, _} = string:to_integer(VS)'
    cmds+=', io:format("~b ~s~n", [MV, code:lib_dir()])'
    cmds+=', halt().'
    "$erl" -noshell -eval "$cmds"
}

#
# echoes a space-delimited list of available benchamrk names
#
list_benchmarks()
{
    local ifsin="#IFS"
    IFS="$IFS_DEFAULT"
    local bench
    local delim=''
    for bench in "$BENCHERL_ROOT/bench"/*
    do
        if [[ -d "$bench" ]]
        then
            echo -n "$delim${bench##*/}"
            [[ -n "$delim" ]] || delim=' '
        fi
    done
    [[ -z "$delim" ]] || echo
    IFS="$ifsin"
}

#
# plot_graph title X-label Y-label input-data output-graph
#
declare _be_create_plot_temp
plot_graph()
{
    local ifsin="$IFS"
    local title="${1//_/-}"
    local xlabel="$2"
    local ylabel="$3"
    local ifile="$4"
    local ofile="$5"

    [[ -n "$_be_create_plot_temp" && -f "$_be_create_plot_temp" ]] || \
        _be_create_plot_temp="$(temp_file)"

    local pfile="$_be_create_plot_temp"
    local label="$(tr -sC '[:alnum:]' '_' <<< "$title")"
    label="${label#_}"
    label="${label%_}"

    cat >"$pfile" <<EOF
set title '$title'
set autoscale
set key right font ',14'
set xtic auto
set ytic auto
set xlabel '$xlabel'
set ylabel '$ylabel'
set term svg size 720,432 dynamic name '$label'
set object 1 rect from screen 0, 0, 0 to screen 1, 1, 0 behind
set object 1 rect fc  rgb 'white'  fillstyle solid 1.0
set output '$ofile'
set grid
EOF
    local lines="$(cut -f1,2 "$ifile" | sort -u)"
    local delim=''
    local line
    echo -n 'plot' >>"$pfile"
    IFS="$IFS_LINES"
    for line in $lines
    do
        # clean up the title
        label="$(tr -s $'\t' $'\x20' <<< "$line")"
        label="${label# }"
        label="${label% }"
        # for errorbars instead use:
        #echo -n "$delim '-' using 1:2:3:4 title '$label' w yerrorlines lw 2"
        echo -n "$delim '-' using 1:2 title '${label//_/-}' w l lw 2"
        [[ -n "$delim" ]] || delim=','
    done >>"$pfile"
    echo >>"$pfile"
    for line in $lines
    do
        fgrep "$line" "$ifile" | cut -f3,4
        echo 'e'
    done >>"$pfile"
    echo >>"$pfile"
    echo 'exit' >>"$pfile"

    IFS="$ifsin"
    gnuplot "$pfile"
}
_be_required_plot_commands+=' fgrep gnuplot'

#
# calc_speedup input-file output-file
# where:
#   input-file      = input measurement file in the documented form
#   output-file     = the file to write speedup info to
#
calc_speedup()
{
    local ifsin="$IFS"
    local ifile="$1"
    local ofile="$2"
    #
    # this is ridiculously inefficient, but it works
    #
    local index count line label nscheds
    IFS="$IFS_LINES"
    for line in $(<"$ifile")
    do
        IFS="$IFS_SPACE"
        local -a times=($(cut -f4- <<< "$line"))
        count="${#times[@]}"
        if [[ $count -gt 0 ]]
        then
            label="$(cut -f1,2 <<< "$line")"
            local -a bases=($(fgrep "${label}${T}1${T}" "$ifile" | cut -f4-))
            if [[ ${#bases[@]} -eq $count ]]
            then
                nscheds="$(cut -f3 <<< "$line")"
                printf '%s\t%s\t' "$label" "$nscheds"
                index=0
                while [[ $index -lt $count ]]
                do
                    bc -l <<< "scale=6; ${bases[$index]}/${times[$index]}"
                    let 'index += 1'
                done | tr "$N" "$S"
                echo
            fi
        fi
        IFS="$IFS_LINES"
    done >"$ofile"

    IFS="$ifsin"
}
_be_required_plot_commands+=' fgrep bc'

#
# plot_benchmark_type benchmark Y-type X-type measurements graphs
# where:
#   benchmark       = the name of the current benchmark
#   X-type          = 'sched' or 'node'
#   Y-type          = 'time' or 'speedup'
#   measurements    = directory containing <otp>.<config>.Y-type.X-type files
#   graphs          = directory to write graphs to
#
plot_benchmark_type()
{
    local ifsin="$IFS"
    local bench="$1"
    local xtype="$2"
    local ytype="$3"
    local mdir="$4"
    local gdir="$5"
    local xaxis yaxis
    IFS="$IFS_DEFAULT"
    case "$xtype" in
        'node' )
            xaxis='Number of Nodes'
            ;;
        'sched' )
            xaxis='Number of Schedulers'
            ;;
        * )
            error_exit 1 "plot_benchmark_type: X-type '$xtype' is not valid."
            ;;
    esac
    case "$ytype" in
        'time' )
            yaxis='Milliseconds'
            ;;
        'speedup' )
            yaxis='Speedup'
            ;;
        * )
            error_exit 1 "plot_benchmark_type: Y-type '$ytype' is not valid."
            ;;
    esac
    local fext="$xtype.$ytype"

    local tmp ifile
    local otps='' cfgs=''
    for ifile in "$mdir"/*.*.$fext
    do
        # break out if there are no files
        [[ -f "$ifile" ]] || break

        tmp="$(basename "$ifile" ".$fext")"
        otps+=" ${tmp%%.*}"
        cfgs+=" ${tmp##*.}"
    done
    if [[ -n "$otps" ]]
    then
        otps="$(sort_and_dedup_text $otps)"
        local -i notps="$(count_list $otps)"

        cfgs="$(sort_and_dedup_text $cfgs)"
        local -i ncfgs="$(count_list $cfgs)"

        local label ofile
        local patt="^\\([^${T}]*\\)${T}\\(.*\\)\$"

        if [[ $ncfgs -gt 1 ]]
        then
            local otp
            for otp in $otps
            do
                ofile="$mdir/$otp.$fext"
                for ifile in "$mdir"/$otp.*.$fext
                do
                    label="$(basename "$ifile" ".$fext")"
                    sed "s/${patt}/\\1${T}${label}${S}\\2/" "$ifile"
                done >"$ofile"

                plot_graph \
                    "$bench - $otp" "$xaxis" "$yaxis" \
                    "$ofile" "$gdir/$otp.$fext.svg"
            done
        fi
        if [[ $notps -gt 1 ]]
        then
            local cfg
            for cfg in $cfgs
            do
                ofile="$mdir/$cfg.$fext"
                for ifile in "$mdir"/*.$cfg.$fext
                do
                    label="$(basename "$ifile" ".$fext")"
                    sed "s/${patt}/\\1${T}${label}${S}\\2/" "$ifile"
                done >"$ofile"

                plot_graph \
                    "$bench - $cfg" "$xaxis" "$yaxis" \
                    "$ofile" "$gdir/$cfg.$fext.svg"
            done
        fi
    fi
    IFS="$ifsin"
}

#
# plot_benchmark benchmark measurements graphs
# where:
#   benchmark       = the name of the current benchmark
#   measurements    = directory containing <otp>.<config>.X-type.Y-type files
#   graphs          = directory to write graphs to
#
plot_benchmark()
{
    local ifsin="$IFS"
    local name="$1"
    local mdir="$2"
    local gdir="$3"
    local file
    local type
    IFS="$IFS_DEFAULT"
    for type in node sched
    do
        if /bin/ls "$mdir"/*.*.$type.time 1>/dev/null 2>&1
        then
            for file in "$mdir"/*.*.$type.time
            do
                calc_speedup "$file" "${file%.time}.speedup"
            done
            plot_benchmark_type "$name" $type time "$mdir" "$gdir"
            plot_benchmark_type "$name" $type speedup "$mdir" "$gdir"
        fi
    done
    IFS="$ifsin"
}

#
# check_benchmark_output output-directory
#
# this just confirms that all the files in the output directory are identical
#
check_benchmark_output()
{
    local odir="$1"
    local first=''
    local file
    for file in "$odir"/*.output
    do
        # check for empty directory
        [[ -f "$file" ]] || break

        if [[ -z "$first" ]]
        then
            first="$file"

        elif ! cmp -s "$first" "$file"
        then
            return 1
        fi
    done
}


# ======================================================================
#
# Set up the common environment variables
#
# ======================================================================

#
# by default, runtime I/O goes into the /tmp directory for speed
#
if [[ -z "$BENCHERL_WORK" ]]
then
    BENCHERL_WORK="$(temp_dir)"
elif [[ ! -d "$BENCHERL_WORK" ]]
then
    mkdir "$BENCHERL_WORK"
fi
readonly  BENCHERL_WORK
export    BENCHERL_WORK

declare -xr BENCHERL_OS="$(uname -s)"
case "$BENCHERL_OS" in
    Darwin )
        declare -ixr BENCHERL_CORES="$(sysctl hw.ncpu | awk '{print $2}')"
        declare -ixr BENCHERL_LCPUS="$(sysctl hw.logicalcpu | awk '{print $2}')"
        declare -ixr BENCHERL_PCPUS="$(sysctl hw.physicalcpu | awk '{print $2}')"
        ;;
    FreeBSD )
        declare -ixr BENCHERL_CORES="$(sysctl hw.ncpu | awk '{print $2}')"
        declare -ixr BENCHERL_LCPUS="$BENCHERL_CORES"
        if [[ $BENCHERL_CORES -gt 1 ]]
        then
            # assume the lowest number is logical CPUs per physical CPU
            declare -i _be_cpu_div="$( \
                sysctl kern.sched.topology_spec | sed -En \
                's/^[[:space:]]*<cpu[[:space:]]+count="([[:digit:]]+)".*$/\1/p' \
                | sort -gu | head -1)"
            declare -ixr BENCHERL_PCPUS="$(($BENCHERL_LCPUS / $_be_cpu_div))"
            unset _be_cpu_div
        else
            declare -ixr BENCHERL_PCPUS="$BENCHERL_CORES"
        fi
        ;;
    Linux )
        declare -ixr BENCHERL_CORES="$(egrep \
            '^processor[[:space:]]*:' /proc/cpuinfo | wc -l)"
        declare -ixr BENCHERL_LCPUS="$BENCHERL_CORES"
        declare -ixr BENCHERL_PCPUS="$(egrep \
            '^core id[[:space:]]*:' /proc/cpuinfo | sort -u | wc -l)"
        ;;
    SunOS )
        declare -ixr BENCHERL_LCPUS="$(count_list $(kstat -m cpu_info \
            | grep 'module: cpu_info' | awk '{print $4}' | sort -u))"
        declare -ixr BENCHERL_PCPUS="$(count_list $(kstat -m cpu_info \
            | grep core_id | awk '{print $2}' | sort -u))"
        declare -ixr BENCHERL_CORES="$BENCHERL_LCPUS"
        error_exit 3 "$BENCHERL_OS CPU calculation is UNTESTED - check it!"
        ;;
    * )
        error_exit 3 "Not prepared for OS '$BENCHERL_OS'"
        ;;
esac
if [[ -z "$BENCHERL_CORES" || "$BENCHERL_CORES" -lt 1 ]]
then
    # this shouldn't have slipped through, but obviously it did
    error_exit 3 "$BENCHERL_OS CPU calculation failed!"
fi

# declare -xr BENCHERL_BECONFIG="$BENCHERL_WORK/run_bench.config"
declare -xr BENCHERL_BECONFIG="$BENCHERL_ROOT/scratch/run_bench.conf"

