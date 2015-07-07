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
# identify package location
# other environment setup is performed at the end of this file
#
BENCHERL_SCRIPTS="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BENCHERL_ROOT="$(dirname "$BENCHERL_SCRIPTS")"

#
# required_{bench|plot}_commands are space-delimited lists of commands
# used by functions in this module
#
# the concatenation of these provides the default list checked by the
# check_required_commands function if it is invoked without arguments
#
required_bench_commands+=''
required_plot_commands+=''

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
[[ -n "$mktemp_template" ]] || readonly mktemp_template="/tmp/$RANDOM.$RANDOM.XXXXXXX"
trap "/bin/rm -rf ${mktemp_template//X/?}" EXIT

#
# show the relevant environment
#
display_environment()
{
    set | egrep '^BENCHERL[[:alnum:]_]*=' | sort
}

#
# create and return the path to an empty temporary directory
#
temp_dir()
{
    mktemp -d "$mktemp_template"
}

#
# create and return the path to an empty temporary file
#
temp_file()
{
    mktemp "$mktemp_template"
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
    echo $1
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
# checks that the specified commands, or $required_commands if none are
# specified, exist in some callable form
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
            $required_bench_commands $required_plot_commands))
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
    sort_and_dedup_num $list
    IFS="$ifsin"
}

#
# plot_graph title X-label Y-label input-data output-graph
#
plot_graph()
{
    local ifsin="$IFS"
    local title="${1//_/-}"
    local xlabel="$2"
    local ylabel="$3"
    local ifile="$4"
    local ofile="$5"

    [[ -n "$_create_plot_temp_" && -f "$_create_plot_temp_" ]] || \
        _create_plot_temp_="$(temp_file)"

    local pfile="$_create_plot_temp_"
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
required_plot_commands+=' fgrep gnuplot'

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
                    bc -l <<< "scale=6${N}${bases[$index]} / ${times[$index]}" | tr "$N" "$S"
                    let 'index += 1'
                done
                echo
            fi
        fi
        IFS="$IFS_LINES"
    done >"$ofile"

    IFS="$ifsin"
}
required_plot_commands+=' fgrep bc'

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
            echo "Error: plot_benchmark_type: X-type '$xtype' is not valid." >&2
            return  1
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
            echo "Error: plot_benchmark_type: Y-type '$ytype' is not valid." >&2
            return  1
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
        local notps="$(count_list $otps)"

        cfgs="$(sort_and_dedup_text $cfgs)"
        local ncfgs="$(count_list $cfgs)"

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
# all runtime I/O goes into the /tmp directory for speed
#
BENCHERL_WORK="$(temp_dir)"

BENCHERL_OS="$(uname -s)"
case "$BENCHERL_OS" in
    Darwin )
        BENCHERL_PCPUS="$(sysctl hw.physicalcpu | awk '{print $2}')"
        BENCHERL_LCPUS="$(sysctl hw.logicalcpu | awk '{print $2}')"
        BENCHERL_CORES="$(sysctl hw.ncpu | awk '{print $2}')"
        ;;
    *BSD )
        BENCHERL_PCPUS="$(sysctl hw.physicalcpu | awk '{print $2}')"
        BENCHERL_LCPUS="$(sysctl hw.logicalcpu | awk '{print $2}')"
        BENCHERL_CORES="$(sysctl hw.ncpu | awk '{print $2}')"
        echo "$BENCHERL_OS CPU calculation is UNTESTED - check it!" >&2
        return 3
        ;;
    Linux )
        BENCHERL_CORES="$(cat /proc/cpuinfo | grep processor | wc -l)"
        echo "$BENCHERL_OS CPU calculation is INCOMPLETE!"
        return 3
        ;;
    SunOS )
        BENCHERL_PCPUS="$(count_list $(kstat -m cpu_info | grep core_id | awk '{print $2}' | sort -u))"
        BENCHERL_LCPUS="$(count_list $(kstat -m cpu_info | grep 'module: cpu_info' | awk '{print $4}' | sort -u))"
        echo "$BENCHERL_OS CPU calculation is UNTESTED - check it!" >&2
        return 3
        ;;
    * )
        echo "Error: Not prepared for OS '$BENCHERL_OS'" >&2
        return 3
        ;;
esac
[[ -n "$BENCHERL_CORES" ]] || BENCHERL_CORES="${BENCHERL_LCPUS:-${BENCHERL_PCPUS}}"
if [[ -z "$BENCHERL_CORES" || "$BENCHERL_CORES" -lt 1 ]]
then
    # this shouldn't have slipped through, but obviously it did
    echo "Error: $BENCHERL_OS CPU calculation failed!" >&2
    return 3
fi

# BENCHERL_CONFIG="$BENCHERL_WORK/run_bench.config"
BENCHERL_CONFIG="$BENCHERL_ROOT/scratch/run_bench.conf"

#
# all accessible from child processes
#
export  BENCHERL_ROOT BENCHERL_SCRIPTS BENCHERL_WORK BENCHERL_CONFIG
export  BENCHERL_OS BENCHERL_CORES BENCHERL_LCPUS BENCHERL_PCPUS

