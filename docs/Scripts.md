## Scripts

`bencherl` functionality is provided by a series of shell scripts, some of
which can be invoked directly and some that are only meant to be executed
by other scripts.  Each of the scripts is described below.

Refer to the [Overview](Overview.md) file for information on how the pieces
work together.

### Directly Executable

#### `bencherl`

This is the main script that provides functionality similar to the original.

Run `bencherl -h` for usage instructions.

#### `bencherlui`

Starts and stops the result set web server.

Run `bencherlui -h` for usage instructions.

#### `script/measure.sh`

Collects configured benchmark measurements. This is the script invoked
by `bencherl` to run benchmarks and collect their metrics, and may be
used directly.

Run `script/measure.sh -h` for usage instructions.

#### `script/plot.sh`

Generates plots of specified benchmark measurements. This is the script
invoked by `bencherl` to generate graphs of benchmark metrics, and may
be used directly.

Run `script/plot.sh -h` for usage instructions.

### Internal Use ONLY

These files are designed to be sourced or executed only in tightly
constrained environments. In most cases, they can't be executed at
all outside one of the directly executable scripts, and would be of
limited utility if they were.

#### `script/confbench.bash`

This is a wrapper used to isolate benchmarks that have separate
configuration files from the globally configured environment.

#### `script/defs.bash`

This file defines the global environment and functions used by the scripts.

#### `script/normalize.bash`

Consumes and normalizes the settings from a configuration file.

#### `script/runbench.bash`

Runs a specific benchmark with all of it's configured parameters.
