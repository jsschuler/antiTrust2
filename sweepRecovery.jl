# Only a recorded successful completion excludes a run from future batches.
function initializeSweepRecovery!(ctrlFrame)
    if !(:complete in propertynames(ctrlFrame))
        any(ctrlFrame.initialized) && error(
            "Legacy control file has started jobs but no completion records. " *
            "Generate a fresh control file in a new run directory to preserve old outputs.")
        ctrlFrame[!, :complete] = falses(size(ctrlFrame, 1))
    end
    length(unique(ctrlFrame.key)) == size(ctrlFrame, 1) ||
        error("Control-file keys must be unique before retrying jobs")
    return ctrlFrame
end

# Write beside the destination and rename only after JLD2 has closed the file.
# An interrupted write leaves the previous checkpoint readable.
function saveSweepControl(ctrlFrame, controlFile)
    temporary, io = mktemp(dirname(controlFile))
    close(io)
    try
        JLD2.jldsave(temporary; ctrlFrame=ctrlFrame)
        Base.Filesystem.rename(temporary, controlFile)
    finally
        ispath(temporary) && rm(temporary)
    end
    return nothing
end

function clearRunOutputs(dataDir, key)
    for prefix in ("agents", "output", "search", "before", "after")
        filename = prefix * key * ".csv"
        basename(filename) == filename || error("Run key must not contain path separators")
        rm(joinpath(dataDir, filename); force=true)
    end
    return nothing
end

function nextSweepStage(worker, state, codeDir)
    if state == :initGen
        return remotecall(Core.eval, worker, Main, :(googleGen()))
    end
    files = Dict(
        :rowLoad => "parameterSet2.jl", :paramGen => "objects2.jl",
        :objects => "initFunctions.jl", :Google => "searchFunctions.jl",
        :searchFuncs => "agentGen.jl", :agentGen => "modelFunctions.jl",
        :modelFuncs => "NetPlot.jl", :svg => "modelMain.jl")
    haskey(files, state) || error("Unexpected sweep stage: $state")
    return remotecall(Base.include, worker, Main, joinpath(codeDir, files[state]))
end

function runSweepBatch!(ctrlFrame, paths, codeDir)
    initializeSweepRecovery!(ctrlFrame)
    pending = findall(!, ctrlFrame.complete)
    isempty(pending) && return String[]
    workerIds = filter(!=(myid()), workers())
    isempty(workerIds) && error("Pending sweep jobs need Julia workers; launch with julia -p N mainSweep.jl")
    # One run per worker per launch (see the loop below), so a batch uses
    # every worker the driver was started with -- there is no separate cap
    # to keep in sync with `julia -p N`.
    count = min(length(workerIds), length(pending))
    active = Dict{Int, Tuple{Int, Future}}()
    failed = String[]

    # One run per worker per launch avoids redefining the model's generated types.
    for (worker, row) in zip(workerIds[1:count], pending[1:count])
        ctrlFrame.initialized[row] = true
        saveSweepControl(ctrlFrame, paths.controlFile)
        try
            clearRunOutputs(paths.dataDir, ctrlFrame.key[row])
            params = NamedTuple(ctrlFrame[row, :])
            future = remotecall(Core.eval, worker, Main, :(paramVec = $params; :rowLoad))
            active[worker] = (row, future)
        catch err
            err isa InterruptException && rethrow()
            ctrlFrame.initialized[row] = false
            saveSweepControl(ctrlFrame, paths.controlFile)
            push!(failed, ctrlFrame.key[row])
            @error "Could not start sweep job; it will be retried next launch" key=ctrlFrame.key[row] exception=(err, catch_backtrace())
        end
    end

    while !isempty(active)
        for worker in collect(keys(active))
            row, future = active[worker]
            isready(future) || continue
            try
                state = fetch(future)
                if state == :complete
                    ctrlFrame.complete[row] = true
                    saveSweepControl(ctrlFrame, paths.controlFile)
                    delete!(active, worker)
                    println("Completed ", ctrlFrame.key[row])
                else
                    active[worker] = (row, nextSweepStage(worker, state, codeDir))
                end
            catch err
                err isa InterruptException && rethrow()
                ctrlFrame.complete[row] = false
                ctrlFrame.initialized[row] = false
                saveSweepControl(ctrlFrame, paths.controlFile)
                delete!(active, worker)
                push!(failed, ctrlFrame.key[row])
                @error "Sweep job failed; it will be retried next launch" key=ctrlFrame.key[row] exception=(err, catch_backtrace())
            end
        end
        isempty(active) || sleep(0.05)
    end
    return failed
end
