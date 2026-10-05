###########################################################################################################
#            Antitrust Model Main Code                                                                    #
#            July 2022                                                                                    #
#            John S. Schuler                                                                              #
#            OECD Version                                                                                 #
#                                                                                                         #
###########################################################################################################
using Distributed
using Combinatorics
@everywhere using CSV
@everywhere using DataFrames
@everywhere using Distributions
@everywhere using InteractiveUtils
@everywhere using Graphs 
@everywhere using Random
@everywhere using JLD2
@everywhere using Dates

# Use the driver's resolved paths on every worker, including when launched elsewhere.
@everywhere modelCodeDir=$(@__DIR__)
@everywhere include(joinpath(modelCodeDir, "pathConfig.jl"))
modelPaths=prepareModelPaths(resolveModelPaths())
@everywhere modelPaths=$modelPaths

include(joinpath(@__DIR__, "sweepRecovery.jl"))
@load modelPaths.controlFile ctrlFrame
failedKeys=runSweepBatch!(ctrlFrame, modelPaths, @__DIR__)
isempty(failedKeys) || error("Sweep jobs failed and will be retried on the next launch: " * join(failedKeys, ", "))
