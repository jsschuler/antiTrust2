# Resolve paths once at startup, before any worker changes its working directory.
function resolveModelPaths(;
    dataDir=get(ENV, "ANTITRUST_DATA_DIR", joinpath(@__DIR__, "..", "antiTrustData")),
    controlFile=get(ENV, "ANTITRUST_CONTROL_FILE", joinpath(@__DIR__, "ctrl.jld2")))
    isempty(strip(dataDir)) && throw(ArgumentError("ANTITRUST_DATA_DIR must not be empty"))
    isempty(strip(controlFile)) && throw(ArgumentError("ANTITRUST_CONTROL_FILE must not be empty"))
    return (dataDir=abspath(expanduser(dataDir)), controlFile=abspath(expanduser(controlFile)))
end

function prepareModelPaths(paths)
    mkpath(paths.dataDir)
    mkpath(dirname(paths.controlFile))
    return paths
end

# modelPaths is initialized by the entry point and sent to all sweep workers.
modelDataFile(prefix, key="") = joinpath(modelPaths.dataDir, prefix * key * ".csv")
