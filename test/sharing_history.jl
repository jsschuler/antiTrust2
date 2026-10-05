using Test
using Distributions
using DataFrames
using CSV

include(joinpath(@__DIR__, "..", "pathConfig.jl"))
include(joinpath(@__DIR__, "..", "objects2.jl"))
include(joinpath(@__DIR__, "..", "searchFunctions.jl"))

googleGen()
duckGen()
sharingGen(1)
for (i, engine) in enumerate(engineList)
    eval(actQuoteFunc(lawList[1], engine, i))
end

key = "sharing-test"
tick = 1
actionHistory = Dict{agent, Dict{action, Union{Int64, Nothing}}}()
sharingDict = Dict{agent, Bool}()

@testset "Sharing transfers an independent snapshot" begin
    # Exercise the real action and CSV logging without writing simulation data.
    mktempdir() do temp_dir
        global modelPaths = prepareModelPaths(resolveModelPaths(
            dataDir=joinpath(temp_dir, "results"), controlFile=joinpath(temp_dir, "ctrl.jld2")))
        mkdir(joinpath(temp_dir, "work"))
        cd(joinpath(temp_dir, "work")) do
            for target_index in 1:2, keep in (false, true), history in (Float64[], [0.2, 0.4])
                source = engineList[3 - target_index]
                target = engineList[target_index]
                mask = alias(false)
                source.aliasData[mask] = copy(history)
                target.aliasData[mask] = [0.9]
                agt = agent(1, mask, 0.5, Beta(2.0, 5.0), Gamma(2.0, 1.0),
                    10.0, 5.0, 5.0, Dict{Int64, Float64}(), source, nothing, nothing, nothing)
                act = actionList[target_index]
                actionHistory[agt] = Dict{action, Union{Int64, Nothing}}()
                sharingDict[agt] = false

                beforeAct(agt, act)
                @test agt.currEngine === target
                @test target.aliasData[mask] == history
                @test target.aliasData[mask] !== source.aliasData[mask]
                afterAct(agt, keep, act)
                @test agt.currEngine === (keep ? target : source)

                # Google's normal update must never change DuckDuckGo's snapshot.
                google_engine, duck_engine = engineList
                duck_snapshot = copy(duck_engine.aliasData[mask])
                update(0.6, mask, google_engine)
                @test duck_engine.aliasData[mask] == duck_snapshot

                # Mutating either stored history must also leave the other independent.
                source_snapshot = copy(source.aliasData[mask])
                push!(target.aliasData[mask], 0.7)
                @test source.aliasData[mask] == source_snapshot
                target_snapshot = copy(target.aliasData[mask])
                empty!(source.aliasData[mask])
                @test target.aliasData[mask] == target_snapshot
            end
        end
    end
end
