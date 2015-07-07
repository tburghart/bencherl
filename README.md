## `bencherl` - A scalability benchmark suite for Erlang

This is an almost complete rewrite of `bencherl`, so you may find the following informative:
  * [The Original Authors' Documentation](http://release.softlab.ntua.gr/bencherl/)
  * [The Basics of How This Version Works](docs/Overview.md)
  * [What You Can Set in Configuration File(s), and What Happens When You Do](docs/Environment.md)
  * [The Scripts, and What They Do](docs/Scripts.md)
  * [The Functions, and What They Do](docs/Functions.md)

## Original README

### How to get ready to build the benchmark suite ###

Make sure you have the following installed on your machine:

* make
* [gnuplot](http://www.gnuplot.info/) 
* [Erlang/OTP](http://www.erlang.org/)

### How to build the benchmark suite ###

	$ make all
	or just
	$ make
	to omit the web interface. If you want to add that later, see the next section.

### How to build the web interface ###

The web interface requires OTP R16B03-1 or later.

	$ make ui

You have to build the web interface again if you move your bencherl folder:

	$ make clean-ui
	$ make ui

### How to run the benchmark suite ###

Specify what you want to run and how in `conf/run.conf`, and then use 
`bencherl` to run the benchmark suite.

	$ ./bencherl

### How to specify a mnemonic name for a run ###

Use the `-m` option of the `bencherl` script.

	$ ./bencherl -m everything-but-big

### How to specify which benchmarks to run ###

Set the `INCLUDE_BENCH` variable in `conf/run.conf`, if you want to specify 
which benchmarks to run.
 
	INCLUDE_BENCH=bang,big

Set the `EXCLUDE_BENCH` variable in `conf/run.conf`, if you want to specify
which benchmarks not to run.

	EXCLUDE_BENCH=dialyzer_bench

The values of both variables are one or more benchmark names separated with 
commas.

By default, all benchmarks are run.

### How to list all benchmarks ###

Use the `-l` option of the `bencherl` script.

	$ ./bencherl -l

### How to specify the number of schedulers to run benchmarks with ###

Set the `NUMBER_OF_SCHEDULERS` variable in `conf/run.conf`.

The value of this variable can be either one or more integers separated with 
commas:
	
	NUMBER_OF_SCHEDULERS=1,2,4,8,16,32,64

or a range of integers:

	NUMBER_OF_SCHEDULERS=1..16

By default, benchmarks are run with as many schedulers as the number of logical
processors.
 
### How to specify the versions/flavors of Erlang/OTP to compile and run benchmarks with ###

Set the `OTPS` variable in `conf/run.conf`.

The value of this variable is one or more alias-path pairs separated with commas.

	OTPS="R14B04=/usr/local/otp_src_R14B04,R15B01=/usr/local/otp_src_R15B01"

By default, benchmarks are compiled and run with the `erlc` and `erl` programs
found in the OS path.

### How to specify the `erl` command-line arguments to run benchmarks with ###

Set the `ERL_ARGS` variable in `conf/run.conf`.

The value of this variable is one or more alias-arguments pairs separated with
commas.

	ERL_ARGS="SOME_ARGS=+sbt db +swt low,SOME_OTHER_ARGS=+sbt u"

### How to specify the number of slave nodes to run the benchmarks with ###

Set the `NUMBER_OF_SLAVE_NODES` variable in `conf/run.conf`.

The value of this variable can be either one or more integers separated with
commas:

	NUMBER_OF_SLAVE_NODES=1,2,4,6,8

or a range of integers:

	NUMBER_OF_SLAVE_NODES=2..4

Benchmarks are executed with at most as many slave nodes as specified in the 
`SLAVE_NODES` variable.

By default, benchmarks are run with one master node and no slave nodes.

### How to specify the slave nodes to run benchmarks with ###

Set the `SLAVE_NODES` variable in `conf/run.conf`.

The value of this variable is zero or more long or short node names separated 
with commas. 

	SLAVE_NODES=somenode@somehost,someothernode@someotherhost

The `USE_LONG_NAMES` variable determines whether long or short names are
expected.

By default, benchmarks are run with no slave nodes.

### How to specify the master node to run benchmarks with ###

Set the `MASTER_NODE` variable in `conf/run.conf`.

The value of this variabe is the short or the long name of the master node.

    MASTER_NODE=somenode@somehost

The `USE_LONG_NAMES` variable determines whether long or short names are
expected.

The default long name of the master node is:

    master@`hostname -f`

and its default short name:

    master@`hostname`

### How to specify the magic cookie that master and slave nodes share ###

Set the `COOKIE` variable in `conf/run.conf`.

	COOKIE="some_cookie"

The default cookie is `cookie`.

### How to specify which version of the benchmarks to run ###

Set the `VERSION` variable in `conf/run.conf`.

The value of this variable can be `short`, `intermediate` or `long`.

	VERSION=short

The default version is `short`.

### How to specify whether to produce scalability graphs or not ###

Set the `PLOT` variable in `conf/run.conf`.

The value of this variable can be either 0 (do not produce any scalability 
graphs) or 1 (produce scalability graphs).

	PLOT=1

The default value is 1.

### How to specify whether to perform a sanity check or not ###

Set the `CHECK_SANITY` variable in `conf/run.conf`.

The value of this variable can be either 0 (do not perform sanity check) or
1 (perform sanity check).

	SANITY_CHECK=1

By default, the sanity of the benchmark execution results is not checked.

### How to specify the number of iterations ###

Set the `ITERATIONS` variable in `conf/run.conf`.

The value of this variable is an integer that is greater than or equal to 1.

	ITERATIONS=5

The default number of iterations is 1.

### What number to use if multiple iterations are run ###

By default, only the minimum runtime of any of the runs is reported.
Set the `OUTPUT_FORMAT` variable in `conf/run.conf` to change this behaviour.

The value of this variable can be any of the following five options:
	`min` reports only the minimum runtime
	`max` reports only the maximum runtime
	`avg` reports the arithmetic mean of all iterations' runtimes
	`avg_min_max` reports all three of the above, in the order `avg`, `min`, `max`
	`plain` reports the runtime of each iteration individually

Note that the web interface only shows the first reported value and ignores any further numbers.

### What is the result of running the benchmark suite ###

A new directory is created under the `results` directory. The name of this 
directory is the mnemonic name or, if no mnemonic name has been specified, a
string that contains the date and time when the run started.

In the result directory, there is one subdirectory for each one of the 
benchmarks that was run, with the same name as the benchmark. Each such 
directory has three sub-directories:
* `graphs`, which contains the scalability graphs;
* `output`, which contains the output that the benchmark produced during its execution;
* `measurements`, which contains the scalability measurements collected during the execution of the benchmark
.

## Web interface (UI) ##

The web interface can be used to view graphs for benchmark results
stored in the `results` folder.

### How to start the web server that serves the web interface of the benchmark suite ###

Use `bencherlui` with the `-u` (up) option to start the web server.

    $ ./bencherlui -u

By default the web interface can be found at "`http://localhost:8001`".

You can change the port of the web server in `ui/bencherlui/boss.config`.

### How to stop the web server that serves the web interface of the benchmark suite ###

Use `bencherlui` with the `-d` (down) option to stop the web server.

    $ ./bencherlui -d

