using Test
using Distributed

include(joinpath(@__DIR__, "..", "pathConfig.jl"))

@testset "Configurable model paths" begin
    withenv("ANTITRUST_DATA_DIR" => nothing, "ANTITRUST_CONTROL_FILE" => nothing) do
        defaults = resolveModelPaths()
        @test defaults.dataDir == normpath(joinpath(@__DIR__, "..", "..", "antiTrustData"))
        @test defaults.controlFile == normpath(joinpath(@__DIR__, "..", "ctrl.jld2"))
    end
    @test resolveModelPaths(dataDir="~/results", controlFile="~/state/ctrl.jld2") ==
        (dataDir=joinpath(homedir(), "results"), controlFile=joinpath(homedir(), "state", "ctrl.jld2"))
    @test_throws ArgumentError resolveModelPaths(dataDir=" ")
    @test_throws ArgumentError resolveModelPaths(controlFile="")

    mktempdir() do temp_dir
        cd(temp_dir) do
            withenv("ANTITRUST_DATA_DIR" => "results with spaces",
                    "ANTITRUST_CONTROL_FILE" => "state/custom.jld2") do
                global modelPaths = prepareModelPaths(resolveModelPaths())
                @test modelPaths.dataDir == joinpath(pwd(), "results with spaces")
                @test modelPaths.controlFile == joinpath(pwd(), "state", "custom.jld2")
                @test isdir(modelPaths.dataDir)
                @test isdir(dirname(modelPaths.controlFile))
                @test prepareModelPaths(modelPaths) == modelPaths
            end
            mkdir("other-working-directory")
            cd("other-working-directory") do
                for prefix in ("ctrl", "agents", "output", "search", "before", "after")
                    @test modelDataFile(prefix) == joinpath(modelPaths.dataDir, prefix * ".csv")
                end
            end
        end

        # A worker must use the driver's absolute paths even with a different cwd.
        worker = only(addprocs(1; exeflags=`--startup-file=no --compiled-modules=no`))
        try
            config_file = normpath(joinpath(@__DIR__, "..", "pathConfig.jl"))
            @everywhere [worker] include($config_file)
            @everywhere [worker] modelPaths=$modelPaths
            result = remotecall_fetch(worker, temp_dir) do work_dir
                cd(work_dir) do
                    Main.modelDataFile("search", "-worker")
                end
            end
            @test result == joinpath(modelPaths.dataDir, "search-worker.csv")
        finally
            rmprocs(worker)
        end
    end
end
