###########################################################################################################
#            Parameter Generation Code                                                                    #
#            July 2022                                                                                    #
#            John S. Schuler                                                                              #
#            OECD Version                                                                                 #
#                                                                                                         #
###########################################################################################################
using Distributed
using Combinatorics
using CSV
using DataFrames
using Distributions
using InteractiveUtils
using Graphs 
using Random
using JLD2
using Dates
using Plots
using Statistics
# now select a seed 
seed1=684049
Random.seed!(seed1)
#strSeed=string(seed)
## global parameters
# now, we need the parameters for the Exponential distributions generating the two Beta parameters for each agent 
agtCnt=1
# how many times to run the model?
modRuns=500
# how many ticks for each model run?
modTime=500
# when does DuckDuckGo enter?
#duckTime::Int64=25
#lawTime::Int64=25

# now, we need to generate the parameters for the agent's interests
# represented by beta distributions. 
# we parameterize the beta distribution by its mode. Given a mode, the two 
# betas are related linearly. The greater the coefficients, the lower the variance. 
# We can generate modes using a beta distribution 
modeGen::Beta{Float64}=Beta(5,5)
# and we generate the betas using an exponential distribution
betaGen::Exponential{Float64}=Exponential(5)
# jointly, these generate agent preferences
# also, agents have a privacy preference from 0 to 1 with a mode at 0. 
# this also comes from a beta random variable
# how many agents care a lot about privacy?
# higher value means fewer care 
# was 2.0
privacyVal::Float64=2.0
privacyBeta::Beta{Float64}=Beta(1.0,privacyVal)
# how close does the offered search result have to be before the agent accepts it?
searchResolution::Float64=.05
# we need a Poisson process for how many agents act exogenously 
switchPct::Float64=.05
poissonDist::Poisson{Float64}=Poisson(switchPct*agtCnt)
# and a probability distribution for how much agents search 
#searchCountDist::NegativeBinomial{Float64}=NegativeBinomial(1.0,.1)
#searchCountDist::NegativeBinomial{Float64}=DiscreteUniform(100,100)
# set the Graph structure
pctConnected=.1
expDegree=floor(Int64,.2*agtCnt)
β=.2
agtGraph=watts_strogatz(agtCnt, expDegree, β)
# Finally, we need a Poisson parameter to how much agents search
searchQty=DiscreteUniform(100,100)
# now set ticks
#ordering=paramVec[14]
# generate ticks at random 
#tickList=sort(rand(DiscreteUniform(1,modTime),length(ordering)))
# set these to -10 by default so they don't fire 
#duckTick=-10
#vpnTick=-10
#deletionTick=-10
#sharingTick=-10

# We need an agent memory parameter 
# that is, for how many ticks after an agent tries an action will it refuse to try again?
agtMemory::Int64=10


#for h in 1:4
#    if h in ordering
#        idx=findall(x -> x==h,ordering)[1]
#        println("idx")
#        println(idx)
#        if idx==1
#            global duckTick
#            duckTick=tickList[ordering[idx]]
#        elseif idx==2
#            global vpnTick
#            vpnTick=tickList[ordering[idx]]
#        elseif idx==3
#            global deletionTick
#            deletionTick=tickList[ordering[idx]]
#        else
#            global sharingTick
#            sharingTick=tickList[ordering[idx]]
#        end
#    end
#end


#println("Event Ticks")
#println(duckTick)
#println(vpnTick)
#println(deletionTick)
#println(sharingTick)

# include files
key="NULL"
include("objects.jl")
include("initFunctions.jl")
googleGen()
include("searchFunctions.jl")
include("agentGen.jl")
seed2=257
include("modelFunctions.jl")
include("NetPlot.jl")

# now, consider its parameters 
theAgt=agtList[1]
Google=engineList[1]
duckGen()
DuckDuckGo=engineList[2]
# we need a function that can alter the agent;s privacy preferences 
function agtPref(agt::agent,privacy)
    privacy::Float64=rand(privacyBeta,1)[1]
    myPrefs=agt.betaObj
    # now, calculate the expected wait time 
    uniPref=Uniform()
    selfArray=Float64[]
    unifArray=Float64[]
    for t in 1:10000
        push!(selfArray,waitTime(myPrefs,myPrefs))
        push!(unifArray,waitTime(uniPref,myPrefs))
    end
    # calculate bliss points under either privacy extreme case
    agt.expSubj=mean(selfArray)
    agt.expUnif=mean(unifArray)
    # for a minimally privacy conscious agent, a lower expected search time is better
    # and thus 0 is the bliss point. 
    # for a maximally privacy conscious agent, the expected search time under a uniform model is the bliss point
    # from here, further inefficiency is not preferred
    # we can always allow the scaling factor to be 1 since only the rank ordering matters 
    # we can use the distance between the expectation under the uniform and the expectation
    # under the subjective distribution as an index of how idiosyncratic the agent's interests are 
    # this also implies that for fixed privacy preferences, agents with less idiosyncratic views are 
    # less sensitive to privacy considerations
    # now, the privacy parameter sets a bliss point between 0 and the expectation under uniform sampling
    agt.blissPoint::Float64=privacy*(agt.expUnif)
    # now, fit a gamma distribution with a scale of 1 to this
    gammaK::Float64=agt.blissPoint+1
    agt.gammaObj=Gamma(gammaK,1)
    return nothing
end

# now set the agent to have a low privacy level 
agtPref(theAgt,.05)

function util(x::Float64)
    global theAgt
    return util(theAgt,x)
end

# ok explore the agent's results, first have the agent search 500 ticks with Google 
tick=0
for t in 1:500
    global tick
    tick=tick+1
    allSearches(tick)
end

# now, run a search with Google vs Duck Duck Go 100 times, replacing the last search 
Random.seed!(8789893)
# remove the agent's history 
pop!(theAgt.history,500)
# remove Google's learned history
for j in 1:100
    pop!(Google.aliasData[theAgt.mask])
end

function runTest()
    global theAgt
    googLong=[]
    duckLong=[]
    for k in 1:1000
        # now, run the Google Search and save the number of ticks 
        theAgt.currEngine=Google
        googSearch=search(theAgt,100)
        waitHist=[]
        for g in googSearch
            push!(waitHist,g[3])
        end
        push!(googLong,mean(waitHist))
        # now set engine to Duck Duck Go
        theAgt.currEngine=DuckDuckGo 
        duckSearch=search(theAgt,100)
        waitHist=[]
        for d in duckSearch
            push!(waitHist,d[3])
        end
        push!(duckLong,mean(waitHist))
    end

    # now compare the data
    tickFrame=DataFrame(goog=googLong,duck=duckLong)
    return mean(util.(tickFrame.goog) .> util.(tickFrame.duck))
end

function seedTest(seed)
    googBetter=[]
    Random.seed!(seed)
    agtPref(theAgt,.05)
    push!(googBetter,runTest())
    Random.seed!(seed)
    agtPref(theAgt,.25)
    push!(googBetter,runTest())
    Random.seed!(seed)
    agtPref(theAgt,.5)
    push!(googBetter,runTest())
    Random.seed!(seed)
    agtPref(theAgt,.75)
    push!(googBetter,runTest())
    Random.seed!(seed)
    agtPref(theAgt,.95)
    push!(googBetter,runTest())
    return googBetter
end