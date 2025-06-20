###########################################################################################################
#            Antitrust Model Main Code                                                                    #
#            July 2022                                                                                    #
#            John S. Schuler                                                                              #
#            Experiment Version                                                                           #
#                                                                                                         #
###########################################################################################################
cores=16
using Distributed
using Combinatorics
@everywhere using CSV
@everywhere using DataFrames
using DataFramesMeta
@everywhere using Distributions
@everywhere using InteractiveUtils
@everywhere using Graphs 
@everywhere using Random
@everywhere using JLD2
@everywhere using Dates

# now Step 1: Generate the control structure

sweeps=5
reps=50

# generate a seed 
seed1Vec=sort(repeat(rand(DiscreteUniform(1,10000),sweeps),2*reps))
seed2Vec=rand(DiscreteUniform(1,10000),2*sweeps*reps)

# how many agents care a lot about privacy?
# higher value means fewer care 
#privacyValVec=sort(repeat(rand(Uniform(.1,.1),sweeps),reps))
privacyValVec=repeat([.5,1.0],sweeps*reps)
#privacyBeta=Beta.(1.0,privacyVal)
# how close does the offered search result have to be before the agent accepts it?
searchResolutionVec=repeat([.05,.05],sweeps*reps)
# we need a Poisson process for how many agents act exogenously 
#switchPctVec=sort(repeat(rand(Uniform(0.05,0.05),sweeps),reps))
switchPctVec=repeat([.05,.05],sweeps*reps)
#agtCntVec=sort(repeat(rand(DiscreteUniform(100,100),sweeps),reps))
agtCntVec=repeat([100,100],sweeps*reps)
#poissonDist=sort(repeat(Poisson.(switchPct.*agtCnt),reps))
# and a probability distribution for how much agents search 
# set the Graph structure
#pctConnectedVec=sort(repeat(rand(Uniform(),sweeps),reps))
pctConnectedVec=repeat([.2,.2],sweeps*reps)
expDegreeVec=floor.(Int64,pctConnectedVec.*agtCntVec)
#βVec=sort(repeat(rand(Uniform(),sweeps),reps))
βVec=repeat([.5,.5],sweeps*reps)
# Finally, we need a Poisson parameter to how much agents search
#searchQtyVec=sort(repeat(rand(Uniform(100,100),sweeps),reps))
searchQtyVec=repeat([100,100],sweeps*reps)
modRunVec=repeat([500,500],sweeps*reps)

currTime=now()

ctrlFrame=DataFrame()
ctrlFrame[!,"dateTime"]=repeat([currTime],2*sweeps*reps)
ctrlFrame[!,"seed1"]=seed1Vec
ctrlFrame[!,"seed2"]=seed2Vec
ctrlFrame[!,"key"]=string.(repeat([currTime],2*sweeps*reps)).*"-".*string.(seed1Vec) .*"-".*string.(seed2Vec).*"-"
ctrlFrame[!,"privacyVal"]=privacyValVec
ctrlFrame[!,"searchResolution"]=searchResolutionVec
ctrlFrame[!,"switchPct"]=switchPctVec
ctrlFrame[!,"agtCnt"]=agtCntVec
ctrlFrame[!,"pctConnected"]=pctConnectedVec
#println("Debug")
#println(size(ctrlFrame))
#println(length(expDegreeVec))
ctrlFrame[!,"expDegree"]=expDegreeVec
ctrlFrame[!,"β"]=βVec
ctrlFrame[!,"searchQty"]=searchQtyVec
ctrlFrame[!,"modRun"]=modRunVec
ctrlFrame[!,"order"].=0
ctrlFrame[!,"initialized"]=repeat([false],size(ctrlFrame)[1])
# now, we want to vary the time between Google and Duck Duck Go

# leave all events in to keep the data structure the same
# but -10 tick means they never happen
duckTick=[30]
#vpnTick=[-10,5,50,110]
vpnTick=[-10,50]
deletionTick=[-10,50]
sharingTick=[-10,50]
duckFrame=DataFrame(:duckTick => duckTick)
vpnFrame=DataFrame(:vpnTick => vpnTick)
delFrame=DataFrame(:deletionTick => deletionTick)
sharFrame=DataFrame(:sharingTick => sharingTick)

tickFrame=crossjoin(duckFrame,vpnFrame,delFrame,sharFrame)


ctrlFrame=crossjoin(ctrlFrame,tickFrame)
ctrlFrame.seed2=rand(DiscreteUniform(1,10000),size(ctrlFrame)[1])
ctrlFrame.key=ctrlFrame.key.*string.(1:size(ctrlFrame)[1])
ctrlFrame[!,"initialized"]=repeat([false],size(ctrlFrame)[1])
# now introduce restrictions 
# we only want runs where only one of vpnTick, deletionTick, or sharingTick is not -10
ctrlFrame=filter(row -> (row.duckTick != -10 && row.vpnTick == -10 && row.deletionTick == -10 && row.sharingTick == -10) ||
                        (row.duckTick != -10 && row.vpnTick != -10 && row.deletionTick == -10 && row.sharingTick == -10) ||
                        (row.duckTick != -10 && row.vpnTick == -10 && row.deletionTick != -10 && row.sharingTick == -10) ||
                        (row.duckTick != -10 && row.vpnTick == -10 && row.deletionTick == -10 && row.sharingTick != -10) ||
                        (row.duckTick == -10 && row.vpnTick != -10 && row.deletionTick != -10 && row.sharingTick == -10) ||
                        (row.duckTick == -10 && row.vpnTick != -10 && row.deletionTick == -10 && row.sharingTick != -10) ||
                        (row.duckTick == -10 && row.vpnTick == -10 && row.deletionTick != -10 && row.sharingTick != -10), ctrlFrame)
# and we need to set the order

println(ctrlFrame)
CSV.write("../antiTrustData/ctrl.csv", ctrlFrame,header = true,append=true)
# now save it as JLD2
@save "ctrl.jld2" ctrlFrame
