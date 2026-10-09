#!/usr/bin/env bash
# Run one of the four sweep experiments (baseline, vpn, deletion, sharing) in
# its own Apptainer (or Singularity -- whichever is on PATH) container
# instance, writing its data under a directory you supply.
#
# Usage: run_experiment.sh <baseline|vpn|deletion|sharing> <data-dir> [code-dir] [sif-image]
#
#   data-dir   Required. Where this experiment's output goes: ctrl csv/state
#              under <data-dir>/state, and all run CSVs under <data-dir>/data.
#              Created if missing.
#   code-dir   Writable copy of the model code for this experiment (Julia
#              needs to write package precompile cache next to the code it
#              runs, so the code can't be bind-mounted read-only). Defaults
#              to a per-experiment directory under $HOME/.cache. Reused and
#              re-synced on every call, so a run can be resumed.
#   sif-image  Path to the built image. Defaults to container/antiTrust.sif
#              next to this script. Build it first with:
#                  apptainer build container/antiTrust.sif container/antiTrust.def
#
# ANTITRUST_SWEEP_WORKERS (env, optional) sets how many Julia workers each
# sweep batch uses (`julia -p <N>`). Defaults to 63, i.e. all but one core of
# a 64-core machine -- the driver process itself is lightweight, so leaving
# it exactly one core is enough. Lower this on a smaller machine.
set -euo pipefail

usage() {
    echo "Usage: $0 <baseline|vpn|deletion|sharing> <data-dir> [code-dir] [sif-image]" >&2
    exit 1
}

[ "$#" -ge 2 ] || usage

experiment="$1"
dataDir="$2"

case "$experiment" in
    baseline|vpn|deletion|sharing) ;;
    *) echo "Unknown experiment '$experiment' (expected baseline, vpn, deletion, or sharing)" >&2; usage ;;
esac

scriptDir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repoDir="$(cd "$scriptDir/.." && pwd)"
codeDir="${3:-${XDG_CACHE_HOME:-$HOME/.cache}/antitrust2/code-$experiment}"
sifImage="${4:-$scriptDir/antiTrust.sif}"
sweepWorkers="${ANTITRUST_SWEEP_WORKERS:-63}"

if command -v apptainer >/dev/null 2>&1; then
    runtime=apptainer
elif command -v singularity >/dev/null 2>&1; then
    runtime=singularity
else
    echo "Neither 'apptainer' nor 'singularity' found on PATH (module load one of them first)" >&2
    exit 1
fi

[ -f "$sifImage" ] || {
    echo "Image not found: $sifImage" >&2
    echo "Build it with: $runtime build '$sifImage' '$scriptDir/antiTrust.def'" >&2
    exit 1
}

mkdir -p "$codeDir" "$dataDir"

# Each experiment gets an isolated writable code copy (and, inside it, its
# own Julia depot for package precompile cache) so concurrent experiments
# never share -- and can't corrupt -- the same compiled-code cache, and the
# read-only git checkout this script runs from is never written to.
rsync -a --delete --exclude='.git/' --exclude='.julia-depot/' "$repoDir"/ "$codeDir"/

echo "Running experiment '$experiment'"
echo "  code:    $codeDir (writable)"
echo "  data:    $dataDir"
echo "  image:   $sifImage"
echo "  workers: $sweepWorkers"
echo "  runtime: $runtime"

"$runtime" exec \
    --bind "$codeDir:/model" \
    --bind "$dataDir:/results" \
    --env ANTITRUST_EXPERIMENT="$experiment" \
    --env ANTITRUST_DATA_DIR=/results/data \
    --env ANTITRUST_CONTROL_FILE=/results/state/ctrl.jld2 \
    --env JULIA_DEPOT_PATH=/model/.julia-depot:/opt/julia-depot \
    --env ANTITRUST_SWEEP_WORKERS="$sweepWorkers" \
    "$sifImage" bash -c '
        set -eu
        if [ ! -f "$ANTITRUST_CONTROL_FILE" ]; then
            julia /model/dataGen2.jl
        fi
        # mainSweep.jl only starts one batch (up to one pending row per
        # worker) per launch, so keep relaunching it until none are left.
        while true; do
            julia -p "$ANTITRUST_SWEEP_WORKERS" /model/mainSweep.jl
            julia /model/container/pending_count.jl && break
        done
    '

echo "Experiment '$experiment' complete. Data: $dataDir/data  Control file: $dataDir/state/ctrl.jld2"
