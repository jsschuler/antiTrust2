using Test
using Random
using Statistics
using Distributions
using DataFrames
using CSV

include(joinpath(@__DIR__, "..", "pathConfig.jl"))
include(joinpath(@__DIR__, "..", "objects2.jl"))
include(joinpath(@__DIR__, "..", "initFunctions.jl"))
include(joinpath(@__DIR__, "..", "searchFunctions.jl"))
include(joinpath(@__DIR__, "..", "modelFunctions.jl"))

googleGen()
duckGen()
agtList = agent[]
for (i, engine) in enumerate(engineList)
    mask = alias(false)
    # Start just below the personalization threshold, including imported DDG data.
    engine.aliasData[mask] = collect(range(0.1, 0.8; length=29))
    push!(agtList, agent(i, mask, 0.5, Beta(2.0, 5.0), Gamma(2.0, 1.0),
        10.0, 5.0, 5.0, Dict{Int64, Float64}(), engine, nothing, nothing, nothing))
end
agtCnt = length(agtList)
key = "recording-test"
tick = 0
searchResolution = 0.05

@testset "Record each Google search once" begin
    Random.seed!(123)
    goog, duck = agtList
    google_history = goog.currEngine.aliasData[goog.mask]
    duck_history = copy(duck.currEngine.aliasData[duck.mask])

    result = subsearch(goog, goog.currEngine, searchResolution)
    @test length(google_history) == 30
    @test last(google_history) == result[4]
    subsearch(duck, duck.currEngine, searchResolution)
    @test duck.currEngine.aliasData[duck.mask] == duck_history

    mktempdir() do temp_dir
        global modelPaths = prepareModelPaths(resolveModelPaths(
            dataDir=joinpath(temp_dir, "results"), controlFile=joinpath(temp_dir, "ctrl.jld2")))
        mkdir(joinpath(temp_dir, "work"))
        cd(joinpath(temp_dir, "work")) do
            results = search(goog, 3)
            @test length(google_history) == 33
            @test google_history[end-2:end] == [r[4] for r in results]

            # Exercise the full tick path that previously appended every result twice.
            for current_tick in 1:2
                global tick = current_tick
                previous_history = copy(google_history)
                allSearches(current_tick)
                @test length(google_history) == 33 + 50 * current_tick
                @test google_history[1:length(previous_history)] == previous_history
                @test duck.currEngine.aliasData[duck.mask] == duck_history

                rows = collect(CSV.File(modelDataFile("search", key); header=false))
                for agt in agtList
                    logged = filter(r -> r.Column2 == current_tick && r.Column3 == agt.agtNum, rows)
                    @test length(logged) == 1
                    @test logged[1].Column6 == agt.history[current_tick]
                    @test isfinite(agt.history[current_tick]) && agt.history[current_tick] >= 1
                end
            end
        end
    end
end
