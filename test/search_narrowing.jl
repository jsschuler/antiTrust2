using Test
import Random
import Distributions

# Record draws from the real search functions without changing their algorithms.
module SearchModel
import Random
import Distributions
using Distributions: Uniform, Beta, Gamma, fit, cdf, pdf

const draws = Any[]
const conditional_guesses = Float64[]

function rand(dist, n)
    length(draws) < 1000 || error("Search failed to converge")
    values = Random.rand(dist, n)
    push!(draws, (dist, values[1]))
    return values
end

function quantile(dist, p)
    value = Distributions.quantile(dist, p)
    push!(conditional_guesses, value)
    return value
end

include(joinpath(@__DIR__, "..", "objects2.jl"))
include(joinpath(@__DIR__, "..", "initFunctions.jl"))
include(joinpath(@__DIR__, "..", "searchFunctions.jl"))
searchResolution = 0.05
end

function check_search(run_search, resolution)
    empty!(SearchModel.draws)
    empty!(SearchModel.conditional_guesses)
    count, final_guess = run_search()
    target = SearchModel.draws[1][2]
    distribution, first_guess = SearchModel.draws[2]
    guesses = [first_guess; SearchModel.conditional_guesses]

    @test count isa Int
    @test count == length(guesses)
    # Only the target and first guess may be unrestricted distribution draws.
    @test length(SearchModel.draws) == length(guesses) + 1
    @test abs(last(guesses) - target) <= resolution
    if final_guess !== nothing
        @test final_guess == last(guesses)
    end

    lower, upper = 0.0, 1.0
    for (i, guess) in enumerate(guesses)
        @test lower <= guess <= upper
        if i > 1
            # The conditional CDF must equal the uniform draw, including for Beta.
            mass = Distributions.cdf(distribution, upper) - Distributions.cdf(distribution, lower)
            conditional_cdf = (Distributions.cdf(distribution, guess) - Distributions.cdf(distribution, lower)) / mass
            @test isapprox(conditional_cdf, SearchModel.draws[i + 1][2]; atol=1e-8)
        end
        if i < length(guesses)
            @test abs(guess - target) > resolution
            if guess > target
                upper = guess
            else
                lower = guess
            end
            @test lower <= target <= upper
        end
    end
    return count
end

@testset "Searches preserve and narrow the conditional distribution" begin
    counts = Int[]
    for seed in 1:20, resolution in (1.0, 0.05, 0.001)
        SearchModel.searchResolution = resolution
        for distribution in (Distributions.Uniform(), Distributions.Beta(2.0, 5.0))
            Random.seed!(seed)
            push!(counts, check_search(resolution) do
                (SearchModel.waitTime(distribution, Distributions.Beta(5.0, 2.0)), nothing)
            end)
            Random.seed!(seed)
            push!(counts, check_search(resolution) do
                (SearchModel.waitIter([Distributions.Beta(5.0, 2.0), distribution]), nothing)
            end)
        end

        for engine_type in (SearchModel.google, SearchModel.duckDuckGo), history_size in (0, 40)
            engine = engine_type(Dict{SearchModel.alias, Array{Float64}}(),
                Dict{Int64, Int64}(), Dict{SearchModel.alias, Array{Float64}}())
            mask = SearchModel.alias(false)
            engine.aliasData[mask] = history_size == 0 ? Float64[] : collect(range(0.1, 0.8; length=history_size))
            agt = SearchModel.agent(1, mask, 0.5, Distributions.Beta(5.0, 2.0),
                Distributions.Gamma(2.0, 1.0), 10.0, 5.0, 5.0,
                Dict{Int64, Float64}(), engine, nothing, nothing, nothing)
            Random.seed!(seed)
            push!(counts, check_search(resolution) do
                result = SearchModel.subsearch(agt, engine, resolution)
                (result[3], result[4])
            end)
        end
    end
    @test minimum(counts) == 1
    @test maximum(counts) > 2
end
