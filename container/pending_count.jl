###########################################################################################################
#            Sweep-batch loop helper                                                                      #
###########################################################################################################

# mainSweep.jl only runs one batch (up to one pending row per worker) per
# launch (see RUNNING.md). This script reports how many rows in the control
# file are still not marked complete, so a shell loop knows whether to launch
# another batch. Exit code 0 means nothing pending; 2 means more batches are
# needed. Other exit codes indicate an error and must stop the shell loop.

using DataFrames
using JLD2

include(joinpath(@__DIR__, "..", "pathConfig.jl"))

modelPaths = resolveModelPaths()
ctrlFrame = JLD2.load(modelPaths.controlFile, "ctrlFrame")
pending = count(!, ctrlFrame.complete)
println(pending, " pending of ", size(ctrlFrame, 1))
exit(pending == 0 ? 0 : 2)
