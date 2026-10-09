# Model paths

The Julia entry points `dataGen2.jl`, `mainSweep.jl`, and `singleRun.jl`
read these environment variables at startup:

| Variable | Purpose | Default |
| --- | --- | --- |
| `ANTITRUST_DATA_DIR` | Directory for `ctrl.csv` and all agents, output, search, before, and after CSVs | `../antiTrustData` relative to this code directory |
| `ANTITRUST_CONTROL_FILE` | JLD2 control file written by `dataGen2.jl` and read/updated by `mainSweep.jl` | `ctrl.jld2` in this code directory |
| `ANTITRUST_EXPERIMENT` | Which single sweep scenario `dataGen2.jl` generates: `baseline`, `vpn`, `deletion`, or `sharing` (see below) | none -- required, `dataGen2.jl` errors if unset or invalid |

Each experiment isolates exactly one post-entry intervention against the
DuckDuckGo-entry-only baseline: `vpn` adds VPN access, `deletion` adds the
data-deletion right, `sharing` adds the data-sharing right, and `baseline`
adds neither. Both privacy-level populations (`.5` and `1.0`) run within
each experiment. A given control file and data directory belong to exactly
one experiment -- `dataGen2.jl` reads `ANTITRUST_EXPERIMENT` once, when it
generates the control file, so each experiment needs its own
`ANTITRUST_CONTROL_FILE` / `ANTITRUST_DATA_DIR` pair.

The settings are independent: changing the data directory does not change the
control-file default. Missing output directories are created. `~` is expanded;
relative environment-variable values are resolved against the launch directory.
The sweep sends resolved absolute paths to its workers, and loads its Julia source
files relative to the code directory.

For example, from the repository:

```sh
export ANTITRUST_DATA_DIR=/scratch/antitrust/run01/data
export ANTITRUST_CONTROL_FILE=/scratch/antitrust/run01/state/ctrl.jld2
export ANTITRUST_EXPERIMENT=vpn
julia dataGen2.jl
julia -p 63 mainSweep.jl
```

Each `mainSweep.jl` launch uses whatever worker count you pass via `-p` and
processes one batch of up to that many pending runs (one run per worker,
since a worker redefines the model's generated types the first time it runs
a row and can't safely reuse them for a second) -- relaunch `mainSweep.jl`
until the control file has no pending rows left. `-p 63` leaves one core for
the driver process on a 64-core machine; lower it if several sweeps share a
machine (see the Apptainer section). Use the same path and experiment
settings for generation and every sweep launch of a given run. `singleRun.jl`
uses the data directory without loading the control file or
`ANTITRUST_EXPERIMENT`. R analysis scripts still have their existing input
paths.

# Apptainer: one container per experiment

`container/run_experiment.sh` runs a whole experiment (control-file generation
plus every sweep batch) as a single Apptainer invocation, given the
experiment name and a data directory:

```sh
# one-time build of the shared image
apptainer build container/antiTrust.sif container/antiTrust.def

# on a dedicated 64-core machine per experiment, each defaults to 63 workers
container/run_experiment.sh baseline /scratch/antitrust/baseline
container/run_experiment.sh vpn      /scratch/antitrust/vpn
container/run_experiment.sh deletion /scratch/antitrust/deletion
container/run_experiment.sh sharing  /scratch/antitrust/sharing
```

The image itself contains only Julia and the model's package dependencies --
no copy of this repository's `.jl` files is baked in or bind-mounted
read-only. Julia writes package precompile cache next to the code it runs,
so a read-only code mount fails; instead, `run_experiment.sh` `rsync`s this
repository into a writable per-experiment directory (by default under
`$HOME/.cache/antitrust2/code-<experiment>`) and binds *that* read-write at
`/model`. Running two experiments at once is then safe -- each has its own
writable code copy and compile cache, so they can't corrupt one another's.
The data directory you pass is bound read-write at `/results`; `ctrl.csv`/
`ctrl.jld2` land under `<data-dir>/state`, and the agents/output/search/
before/after CSVs under `<data-dir>/data`. Interrupting and rerunning the
same `(experiment, data-dir)` pair resumes: `dataGen2.jl` only runs if no
control file exists yet, and `mainSweep.jl` skips rows already marked
complete.

Each `run_experiment.sh` call launches `julia -p $ANTITRUST_SWEEP_WORKERS
mainSweep.jl` in a loop (default 63 -- one 64-core machine running one
experiment). If you run more than one experiment on the *same* machine
instead of giving each its own, divide the cores between them, e.g. for four
experiments sharing one 64-core box:

```sh
ANTITRUST_SWEEP_WORKERS=15 container/run_experiment.sh baseline /scratch/antitrust/baseline &
ANTITRUST_SWEEP_WORKERS=15 container/run_experiment.sh vpn      /scratch/antitrust/vpn &
ANTITRUST_SWEEP_WORKERS=15 container/run_experiment.sh deletion /scratch/antitrust/deletion &
ANTITRUST_SWEEP_WORKERS=15 container/run_experiment.sh sharing  /scratch/antitrust/sharing &
wait
```

`run_experiment.sh` runs the container with whichever of `apptainer` or
`singularity` it finds on `PATH` -- their `exec`/`--bind`/`--env` syntax is
the same, so a cluster that only has a `singularity` module works without
changes. See `container/antiTrust.def` and `container/run_experiment.sh` for
the underlying `build`/`exec` commands, and Apptainer's documentation for
[environment variables](https://apptainer.org/docs/user/main/environment_and_metadata.html)
and [bind mounts](https://apptainer.org/docs/user/main/bind_paths_and_mounts.html)
if you need to customize them.

## Slurm

`container/slurm/run_experiment.slurm` submits the four experiments as a job
array, one array task per experiment, each getting its own exclusive node
(`0=baseline 1=vpn 2=deletion 3=sharing`):

```sh
sbatch container/slurm/run_experiment.slurm        # all four
sbatch --array=1 container/slurm/run_experiment.slurm   # just vpn
```

It `module load singularity`s, then calls `run_experiment.sh` with
`ANTITRUST_SWEEP_WORKERS` set from `$SLURM_CPUS_ON_NODE - 1` (so it adapts if
a partition's nodes aren't 64 cores) and with the data/code/image paths
pointed at `/scratch`, not `$HOME` -- edit the three path variables near the
top of the script (`REPO_DIR`, `DATA_ROOT`, `CODE_ROOT`) and the
`--mail-user` for your account before submitting. The repository (with the
image already built via `apptainer build`/`singularity build`, since that
step typically needs privileges compute nodes don't have) needs to live
somewhere under those same paths, reachable from the compute nodes.
