## Environment

`bencherl` is configured through variables that can be set in configuration
files or in the environment before the scripts are run.  Configuration
files (where they're found is discussed below) are sourced by the `bash`
shell and can use (almost) any legal shell capabilities to set up the
necessary variables.

Refer to the [Overview](Overview.md) file for information on how the pieces
work together.

### Configuration File Locations


### Predefined Variables

The following variables are defined (readonly, exported) at the time when your
configuration file is sourced and can be used to calculate values you set.
Each of these variables is also present in the execution environment when your
test is run, accessible via `os:getenv(...)`.

##### `BENCHERL_CWD`
This is the Current Working Directory when the executing script was started
and, unless overridden by `WORKING_DIRECTORY`, will be the directory when
your test is run.

##### `BENCHERL_ROOT`
This is the root of the `bencherl` distribution tree.

##### `BENCHERL_WORK`
This is a directory for transient files. Unless set in the environment when
the current script is run, it will be a uniqely-named directory under `/tmp`.
You should ***not*** overwrite any files that may exist in this directory,
but you can use `mktemp` or similar means to create whatever you need under
it. If the directory was created by the current script, it is deleted when
the script exits.

##### `BENCHERL_OS`

This is the name of the current operating system, generally as returned by
`uname -s`.

##### `BENCHERL_CORES`

This is the number of active CPU cores, and will normally be the same as
`$BENCHERL_LCPUS`.

##### `BENCHERL_LCPUS`

This is the number of logical CPU cores in the system. It will always be
greater than or equal to `$BENCHERL_PCPUS`.

##### `BENCHERL_PCPUS`

This is the number of physical CPU cores in the system. Note that at present
there is no indication whether the cores reside in one or multiple packages.

### Reserved Names

Shell variables whose names begin with `_be_` are reserved for use by the
scripts and functions. The effect of use or definition of variables matching
this pattern is undefined, but probably not desirable.

Other than the predefined and reserved variables, you may use whatever
variables you want in your configuration file(s), though you're advised to
clean up anything that's not a known configuration value or exported for use
in your test.

### Configuration Scope


### Recognized Variables


