## Overview

I started out tinkering with `bencherl` with limited goals in mind:
  1. Make it more flexible.
  2. Make it less brittle.
  3. Make it understandable and maintainable.

Those modest goals, as they often do, mushroomed into an almost complete rewrite.
As the changes became more pervasive, I decided it'd be a Good Thing&trade; to
maybe write down a bit about what I was doing, in hopes that others (and myself,
at some later date) could understand what's going on.

The basics really haven't changed - `bencherl` is a framework for benchmarking
[Erlang/OTP](http://www.erlang.org/) and applications that use it. Refer to
[The Original Bencherl Documentation](http://release.softlab.ntua.gr/bencherl/)
to understand how benchmarks are set up and invoked.

As for what I've done to it, I hope the following will be informative.

### Scripts

Originally, `bencherl` was one shell script that, while it may not have
started out that way, is very much dependent on specific behaviors of the
[Bourne-Again Shell](https://www.gnu.org/software/bash/), commonly known as
`bash`. It would be nice if the scripts could be written to run in any
POSIX-compliant shell, but that would be a ***lot*** more work, so one of the
first things I did was formalize that dependency by adding the appropriate
_hashbang_ header to the script, and using `bash` functionality explicitly.

  * Disclaimer: Bash is ***not*** my preferred shell scripting interpretter,
    but in this case it offers exactly one feature that's not available in
    `ksh` (which I'd rather use when plain POSIX just won't do). It's also
    installed on _most_ systems these days, and available for any where it
    isn't, so it's the tool I've chosen.

  * Note that the scripts expect to find the shell at `/bin/bash`. If it's
    not included in your system by default, and you install it from a package,
    there's a fair chance that it won't get installed there. You can modify
    the scripts accordingly, but you'll be much better off in the long run if
    you add an appropriate symbolic link to it from `/bin/bash` and add it to
    `/etc/shells` manually if need be.

#### Refactoring

One of the first things I needed when I started was the ability to accumulate
results accross multiple systems and _then_ plot the graphs, and this turned
out to be far more laborious than expected. What I needed, at a minimum, was
to separate the _measurement_ and _plotting_ functionality. Since these use a
bunch of common configuration and behavior, I added the `script` directory
under `bencherl's` root and set about separating functions from script logic
into discreet files there.

Aside from the standalone `measure` and `plot` scripts there, you'll also find
some helper scripts that are meant _only_ to be invoked by other scripts.
One of these is `invoke`, which is used by the `measure` script to provide a
clean separation of execution contexts for benchmark runs at the possible
expense of slower execution time.
Originally, `bencherl` used subshells to run the benchmarks, and they made the
script even harrier than it already was, so I decided that given all that goes
on just to turn the measurement crank one time, spawning one more (relatively
lightweight) program is a small price to pay for confidence that the
environment is not being polluted.

You can find more information about what each of the scripts does, and how
they depend on each other, in the [Scripts](Scripts.md) document.

### Functions

Also in the `scripts` directory is the `defs.bash` file, containing all of the
functions used by the various scripts.
I've taken the approach of consolidating _all_ functions into one file so they
can be combined in ways I haven't thought of yet.
I've also tried to make the functions as free-standing as possible, so that
they don't rely on environment variables beyond those that are explicitly
invariant (more on that below).

You can find more information about what each of the functions does in the
[Functions](Functions.md) document.

### Environment

One of the biggest issues with unraveling `bencherl` was figuring out the
state of the environment in each of the original functions, and removing the
interdependencies on environmental factors.
To accomplish that, aside from all of the rewriting, I've adopted a pattern
whereby environment variables whose name begins with `BENCHERL_` are exported
and invariant, and all others are considered transient within the script using
them. Functions ***do not*** use ***any*** environment variables other than
those defined by the shell or the `BENCHERL_` ones, and explicitly preserve
and reset the value of `IFS` if they need it to have a specific value.

The dependence on multiple values of `IFS` was one of the _major_ issues in
refactoring the script, and in due time I hope to be able to eliminate any
reliance on it having a non-default value, but for the time being I'm just
trying to make sure I know what its value _is_ when I'm depending on it.

You can find details on what environment variables are set and recognized in
the [Environment](Environment.md) document.



