# Model paths

The Julia entry points `dataGen2.jl`, `mainSweep.jl`, and `singleRun.jl`
read these environment variables at startup:

| Variable | Purpose | Default |
| --- | --- | --- |
| `ANTITRUST_DATA_DIR` | Directory for `ctrl.csv` and all agents, output, search, before, and after CSVs | `../antiTrustData` relative to this code directory |
| `ANTITRUST_CONTROL_FILE` | JLD2 control file written by `dataGen2.jl` and read/updated by `mainSweep.jl` | `ctrl.jld2` in this code directory |

The settings are independent: changing the data directory does not change the
control-file default. Missing output directories are created. `~` is expanded;
relative environment-variable values are resolved against the launch directory.
The sweep sends resolved absolute paths to its workers, and loads its Julia source
files relative to the code directory.

For example, from the repository:

```sh
export ANTITRUST_DATA_DIR=/scratch/antitrust/run01/data
export ANTITRUST_CONTROL_FILE=/scratch/antitrust/run01/state/ctrl.jld2
julia dataGen2.jl
julia -p 15 mainSweep.jl
```

The current sweep runner uses up to 15 workers and processes one batch of up to
15 pending runs per launch. Use the same two path settings for generation and
each sweep launch. `singleRun.jl` uses the data directory without loading the
control file. R analysis scripts still have their existing input paths.

# Apptainer example

Assuming your image contains Julia and the model's dependencies, bind the code
and a writable results directory, then pass **container-visible paths** through
`--env`:

```sh
mkdir -p /scratch/antitrust/run01

apptainer exec \
  --bind /absolute/path/to/antiTrust2:/model:ro \
  --bind /scratch/antitrust/run01:/results \
  --env ANTITRUST_DATA_DIR=/results/data \
  --env ANTITRUST_CONTROL_FILE=/results/state/ctrl.jld2 \
  model.sif julia /model/dataGen2.jl

apptainer exec \
  --bind /absolute/path/to/antiTrust2:/model:ro \
  --bind /scratch/antitrust/run01:/results \
  --env ANTITRUST_DATA_DIR=/results/data \
  --env ANTITRUST_CONTROL_FILE=/results/state/ctrl.jld2 \
  model.sif julia -p 15 /model/mainSweep.jl
```

See Apptainer's documentation for [environment variables](https://apptainer.org/docs/user/main/environment_and_metadata.html)
and [bind mounts](https://apptainer.org/docs/user/main/bind_paths_and_mounts.html).
