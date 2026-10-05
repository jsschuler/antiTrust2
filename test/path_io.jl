using Test
using CSV
using DataFrames
using JLD2

@testset "Configured paths across generation, sweep, and simulation" begin
    mktempdir() do temp_dir
        data_dir = joinpath(temp_dir, "results with spaces")
        control_file = joinpath(temp_dir, "state", "custom.jld2")
        work_dir = mkdir(joinpath(temp_dir, "unrelated-working-directory"))
        withenv("ANTITRUST_DATA_DIR" => data_dir, "ANTITRUST_CONTROL_FILE" => control_file) do
            cd(work_dir) do
                # Generate actual control files from an unrelated launch directory.
                redirect_stdout(devnull) do
                    Base.include(Main, joinpath(@__DIR__, "..", "dataGen2.jl"))
                end
                @test isfile(joinpath(data_dir, "ctrl.csv"))
                @test isfile(control_file)
                control = JLD2.load(control_file, "ctrlFrame")
                @test nrow(control) > 0
                @test nrow(CSV.read(joinpath(data_dir, "ctrl.csv"), DataFrame)) == nrow(control)

                # An exhausted queue exercises the sweep's control read/write without workers.
                control.initialized .= true
                control.complete .= true
                JLD2.jldsave(control_file; ctrlFrame=control)
                Base.include(Main, joinpath(@__DIR__, "..", "mainSweep.jl"))
                @test JLD2.load(control_file, "ctrlFrame") == control

                # Run two real simulation ticks and inspect all unconditional output files.
                small_control = copy(control[1:1, :])
                small_control.key .= "path-smoke"
                small_control.seed1 .= 1234
                small_control.seed2 .= 5678
                small_control.agtCnt .= 10
                small_control.modRun .= 2
                for name in (:duckTick, :vpnTick, :deletionTick, :sharingTick)
                    small_control[!, name] .= -10
                end
                model = Module(gensym(:PathSmoke))
                Core.eval(model, :(using Random, Statistics, Distributions, DataFrames, CSV, Graphs))
                Core.eval(model, :(paramVec = $(small_control[1, :])))
                Base.include(model, joinpath(@__DIR__, "..", "pathConfig.jl"))
                Core.eval(model, :(modelPaths = prepareModelPaths(resolveModelPaths())))
                redirect_stdout(devnull) do
                    for file in ("parameterSet2.jl", "objects2.jl", "initFunctions.jl", "searchFunctions.jl")
                        Base.include(model, joinpath(@__DIR__, "..", file))
                    end
                    Core.eval(model, :(googleGen()))
                    for file in ("agentGen.jl", "modelFunctions.jl", "modelMain.jl")
                        Base.include(model, joinpath(@__DIR__, "..", file))
                    end
                end
                for (prefix, row_count) in (("agents", 10), ("output", 20), ("search", 20))
                    file = joinpath(data_dir, prefix * "path-smoke.csv")
                    @test isfile(file)
                    @test nrow(CSV.read(file, DataFrame; header=false)) == row_count
                end
                @test isempty(readdir(work_dir))
            end
        end
    end
end
