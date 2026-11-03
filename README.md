# antiTrust2

An agent-based model of search-engine competition and privacy regulation. It
asks whether policy remedies aimed at user data can erode a dominant search
engine's position once a privacy-focused rival enters the market.

The model is written in Julia; the statistical analysis and figures are in R.
The incumbent engine is modeled on Google (labeled "Hoogle" in the figures)
and the entrant on DuckDuckGo.

## The model

- **Agents.** 100 agents sit on a Watts–Strogatz small-world network. Each
  agent has idiosyncratic search interests (a Beta distribution it draws
  queries from) and a privacy preference drawn from `Beta(1, privacyVal)`.
  A lower `privacyVal` means more agents care strongly about privacy.
- **Search.** Agents search every tick. The incumbent records each agent's
  past results and, once it has 30 of them, fits a Beta distribution to guide
  its guesses, so it finds results faster for users it knows. The entrant
  keeps no history and guesses uniformly. Search effort is the number of
  guesses until a result lands within `searchResolution` of the target.
- **Preferences.** Each agent has a *bliss point* for search effort, set
  between instant results and uninformed search by its privacy preference.
  Privacy-minded agents prefer the less personalized experience; the rest
  prefer speed. The figures plot outcomes against this privacy index.
- **Behavior.** Each tick, a Poisson-distributed number of agents try a new
  action (switch engine, use a VPN, exercise a legal right). Their network
  neighbors are prompted to try the same. After searching, agents keep the
  change if it moved them closer to their bliss point and reverse it if not.

Everyone starts on the incumbent. The entrant arrives at tick 30, and at
tick 50 one intervention switches on:

| Experiment | Intervention at tick 50 |
| --- | --- |
| `baseline` | None: entrant only |
| `vpn` | Agents can use a VPN, opting out of tracking under a fresh alias |
| `deletion` | Agents can make the incumbent delete their search history |
| `sharing` | Agents can make the incumbent share their history with the entrant |

Each run lasts 500 ticks. Each experiment runs 500 times: 250 runs at
`privacyVal` 0.5 (more privacy-minded) and 250 at 1.0, with fresh random
seeds per run.

## Repository layout

| Path | Contents |
| --- | --- |
| `objects2.jl` | Agents, search engines, laws, and the generated action space |
| `searchFunctions.jl`, `modelFunctions.jl`, `initFunctions.jl` | Search, agent behavior, initialization |
| `modelMain.jl`, `parameterSet2.jl` | One model run, parameterized by a row of the control file |
| `dataGen2.jl` | Builds the control file (one row per run) for one experiment |
| `mainSweep.jl`, `sweepRecovery.jl` | Runs pending control-file rows in parallel, resumably |
| `experimentConfig.jl`, `pathConfig.jl` | Experiment selection and data paths |
| `singleRun.jl` | A single standalone run, for development |
| `container/` | Apptainer image, per-experiment runner, Slurm job array |
| `dataAnalysisPVal.R`, `analysisFunctions.R` | Statistical tests and figures |
| `replot_from_artifacts.R`, `mathPlots.R` | Restyling saved plots; Beta illustration figures |
| `test/` | Julia and R tests |
| `RUNNING.md` | Detailed reference for paths, containers, and Slurm |

## Running the simulations

Each experiment writes to its own data directory. Running all four takes a
cluster or several multi-core machines; see `RUNNING.md` for details.

### With Apptainer (recommended)

```sh
# one-time image build (Julia 1.10 plus all package dependencies)
apptainer build container/antiTrust.sif container/antiTrust.def

# one experiment per machine; each uses all but one core by default
container/run_experiment.sh baseline /scratch/antitrust/baseline
container/run_experiment.sh vpn      /scratch/antitrust/vpn
container/run_experiment.sh deletion /scratch/antitrust/deletion
container/run_experiment.sh sharing  /scratch/antitrust/sharing
```

Set `ANTITRUST_SWEEP_WORKERS` to use fewer cores, for example when several
experiments share one machine. Interrupting and rerunning the same command
resumes where it stopped.

On a Slurm cluster, edit the paths and email near the top of
`container/slurm/run_experiment.slurm`, then submit all four experiments as
a job array:

```sh
sbatch container/slurm/run_experiment.slurm            # all four
sbatch --array=1 container/slurm/run_experiment.slurm  # just vpn
```

### With a local Julia install

Install Julia 1.10 and the dependencies:

```sh
julia -e 'import Pkg; Pkg.add(["CSV", "DataFrames", "DataFramesMeta", "Distributions",
                               "Graphs", "JLD2", "Combinatorics", "Plots"])'
```

Then generate and run one experiment:

```sh
export ANTITRUST_EXPERIMENT=vpn
export ANTITRUST_DATA_DIR=~/antitrust2-data/vpn/data
export ANTITRUST_CONTROL_FILE=~/antitrust2-data/vpn/state/ctrl.jld2
julia dataGen2.jl
julia -p 7 mainSweep.jl   # repeat until no pending runs remain
```

Each `mainSweep.jl` launch runs one batch of up to `-p` runs, so relaunch it
until every run is complete. Use the same three variables for generation and
every launch of an experiment.

### Output

Each experiment's data directory holds `ctrl.csv` (the run parameters) and
five headerless CSVs per run, named by type and run key:

| File | One row per | Columns |
| --- | --- | --- |
| `output…` | agent and tick | key, tick, agent, engine in use, entry and intervention ticks, opted out |
| `agents…` | agent | key, agent, bliss point, subjective and uniform expected search effort |
| `search…` | agent and tick | key, tick, agent, engine, opted out, mean search effort, utility |
| `before…`, `after…` | action taken | key, tick, agent, action, engine (and whether it was kept) |

## Running the analysis

The analysis needs R 4.x with ggplot2, dplyr, tidyr, cowplot and data.table:

```r
install.packages(c("ggplot2", "dplyr", "tidyr", "cowplot", "data.table"))
```

It expects the four experiments side by side, as `run_experiment.sh` lays
them out:

```
antitrust2-data/
  baseline/data/   vpn/data/   deletion/data/   sharing/data/
```

Set `dataRoot` (that folder) and `imageDir` (where figures go) near the top
of `dataAnalysisPVal.R`, then:

```sh
Rscript dataAnalysisPVal.R
```

It compares each intervention with the baseline at three levels, using
permutation tests throughout:

- **Run level:** mean share of searches on the incumbent from tick 50 on,
  separately for each `privacyVal`. Figures `vpnDist.png`,
  `deletionDist.png`, `sharingDist.png`.
- **Agent level:** moving average of incumbent share along the agent privacy
  index, with labels permuted within each `privacyVal`. Figures `base.png`,
  `vpnAgent.png`, `deletionAgent.png`, `sharingAgent.png`.
- **Over time:** median incumbent share per tick, separately for each
  `privacyVal`. Figures `vpnTimeByPrivacy.png`, `DeletionTimeByPrivacy.png`,
  `SharingTimeByPrivacy.png`.

Results go to `effect_size_summary.csv` in `dataRoot` (effect sizes,
null-distribution quantiles and p-values), plus a timestamped
`analysis_artifacts/` folder of plot-ready data and ggplot objects.
`replot_from_artifacts.R` restyles the run-level plots from the latest one
without rerunning the analysis.

## Tests

```sh
for t in test/*.R; do Rscript "$t"; done    # analysis functions
for t in test/*.jl; do julia "$t"; done     # model and sweep code
```

## License

MIT; see `LICENSE`.
