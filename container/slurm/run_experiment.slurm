#!/bin/bash

## Runs the antiTrust2 sweep experiments as a job array: each array task
## gets its own exclusive node and runs exactly one experiment end-to-end
## (control-file generation plus every sweep batch) through
## container/run_experiment.sh.
##
## Submit all four experiments:          sbatch container/slurm/run_experiment.slurm
## Submit just one, e.g. vpn (index 1):   sbatch --array=1 container/slurm/run_experiment.slurm
## Array index -> experiment: 0=baseline 1=vpn 2=deletion 3=sharing

## Give your job a name to distinguish it from other jobs you run.
#SBATCH --job-name=antitrust2

## General partitions: all-HiPri, bigmem-HiPri   --   (12 hour limit)
##                     all-LoPri, bigmem-LoPri, gpuq  (5 days limit)
## Restricted: CDS_q, CS_q, STATS_q, HH_q, GA_q, ES_q, COS_q  (10 day limit)
#SBATCH --partition=normal

## One array task per experiment.
#SBATCH --array=0-3

## Separate output and error messages into 2 files per array task.
## NOTE: %u=userID, %x=jobName, %N=nodeID, %j=jobID, %A=arrayID, %a=arrayTaskID
#SBATCH --output=/scratch/%u/%x-%a-%N-%j.out  # Output file
#SBATCH --error=/scratch/%u/%x-%a-%N-%j.err   # Error file

## Slurm can send you updates via email
#SBATCH --mail-type=BEGIN,END,FAIL         # ALL,NONE,BEGIN,END,FAIL,REQUEUE,..
#SBATCH --mail-user=jschule4@gmu.edu     # Put your GMU email address here

## Each experiment gets an exclusive whole node (64 cores). --mem=0 asks for
## all memory on that node instead of a fixed amount, since --exclusive
## already means no other job will share it.
#SBATCH --mem=0
#SBATCH --nodes=1
#SBATCH --exclusive

## Specify how much time your job needs. (default: see partition above)
#SBATCH --time=05-00:00  # Total time needed for job: Days-Hours:Minutes

## Load the relevant modules needed for the job. This runs before
## `set -u` because module scripts often read unset variables.
module load apptainer

set -euo pipefail

## --- Edit these three for your layout ---
## REPO_DIR must be on storage the compute node can see (e.g. /scratch, not
## a networked $HOME): it's where run_experiment.sh and the built .sif live.
REPO_DIR=/scratch/jschule4/antiTrust2
DATA_ROOT=/scratch/jschule4/antitrust2-data
CODE_ROOT=/scratch/jschule4/antitrust2-code
## -----------------------------------------

experiments=(baseline vpn deletion sharing)
experiment="${experiments[$SLURM_ARRAY_TASK_ID]}"

## Leave one core for run_experiment.sh's own batch-relaunch loop; this
## tracks whatever SLURM actually handed the node, so it still works if a
## partition's node size is ever something other than 64.
export ANTITRUST_SWEEP_WORKERS=$((SLURM_CPUS_ON_NODE - 1))

"$REPO_DIR"/container/run_experiment.sh \
    "$experiment" \
    "$DATA_ROOT/$experiment" \
    "$CODE_ROOT/$experiment" \
    "$REPO_DIR/container/antiTrust.sif"
