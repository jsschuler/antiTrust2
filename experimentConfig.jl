###########################################################################################################
#            Per-Experiment Scenario Selection                                                            #
###########################################################################################################

# Each experiment isolates exactly one post-entry intervention (VPN access, the
# deletion right, or the data-sharing right) against the DuckDuckGo-entry-only
# baseline, so a single container only has to generate and run the sweep rows
# for its own scenario.

const validExperiments = ("baseline", "vpn", "deletion", "sharing")

const duckEntryTick = 30
const interventionTick = 50
const neverTick = -10

function experimentTicks(experiment::AbstractString)
    experiment in validExperiments || throw(ArgumentError(
        "experiment must be one of " * join(validExperiments, ", ") *
        " (got: " * repr(experiment) * ")"))
    vpnTick, deletionTick, sharingTick =
        experiment == "baseline" ? (neverTick, neverTick, neverTick) :
        experiment == "vpn"      ? (interventionTick, neverTick, neverTick) :
        experiment == "deletion" ? (neverTick, interventionTick, neverTick) :
                                    (neverTick, neverTick, interventionTick)
    return (duckTick=duckEntryTick, vpnTick=vpnTick, deletionTick=deletionTick, sharingTick=sharingTick)
end

function experimentFromEnv(env=ENV)
    haskey(env, "ANTITRUST_EXPERIMENT") || throw(ArgumentError(
        "ANTITRUST_EXPERIMENT must be set to one of: " * join(validExperiments, ", ")))
    return env["ANTITRUST_EXPERIMENT"]
end

:experimentConfig
