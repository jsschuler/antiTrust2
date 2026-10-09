using Test

include(joinpath(@__DIR__, "..", "experimentConfig.jl"))

@testset "Per-experiment scenario selection" begin
    @test experimentTicks("baseline") == (duckTick=30, vpnTick=-10, deletionTick=-10, sharingTick=-10)
    @test experimentTicks("vpn") == (duckTick=30, vpnTick=50, deletionTick=-10, sharingTick=-10)
    @test experimentTicks("deletion") == (duckTick=30, vpnTick=-10, deletionTick=50, sharingTick=-10)
    @test experimentTicks("sharing") == (duckTick=30, vpnTick=-10, deletionTick=-10, sharingTick=50)
    @test_throws ArgumentError experimentTicks("not-a-real-experiment")

    # exactly one intervention is ever active per experiment
    for experiment in validExperiments
        ticks = experimentTicks(experiment)
        activeInterventions = count(!=(neverTick), (ticks.vpnTick, ticks.deletionTick, ticks.sharingTick))
        @test activeInterventions <= 1
    end

    @test experimentFromEnv(Dict("ANTITRUST_EXPERIMENT" => "vpn")) == "vpn"
    @test_throws ArgumentError experimentFromEnv(Dict{String,String}())
end
