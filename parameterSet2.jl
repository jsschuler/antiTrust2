###########################################################################################################
#            Parameter Generation Code                                                                    #
#            July 2022                                                                                    #
#            John S. Schuler                                                                              #
#            OECD Version                                                                                 #
#                                                                                                         #
###########################################################################################################

# now select a seed 
println("FLAG")
println(paramVec)
seed1=paramVec[3]
Random.seed!(seed1)
#strSeed=string(seed)
## global parameters
# now, we need the parameters for the Exponential distributions generating the two Beta parameters for each agent 
agtCnt=paramVec[8]
# how many times to run the model?
modRuns=paramVec[13]
# how many ticks for each model run?
modTime::Int64=paramVec[13]
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
privacyVal::Float64=paramVec[5]
privacyBeta::Beta{Float64}=Beta(1.0,privacyVal)
# how close does the offered search result have to be before the agent accepts it?
searchResolution::Float64=paramVec[6]
# we need a Poisson process for how many agents act exogenously 
switchPct::Float64=paramVec[7]
poissonDist::Poisson{Float64}=Poisson(switchPct*agtCnt)
# and a probability distribution for how much agents search 
searchCountDist::NegativeBinomial{Float64}=NegativeBinomial(1.0,.1)
# set the Graph structure
pctConnected=paramVec[9]
expDegree=floor(Int64,.2*agtCnt)
β=paramVec[11]
agtGraph=watts_strogatz(agtCnt, expDegree, β)
# Finally, we need a Poisson parameter to how much agents search
searchQty=Poisson{Float64}(paramVec[12])
# now set ticks
ordering=paramVec[14]
# generate ticks at random 
tickList=sort(rand(DiscreteUniform(1,modTime),length(ordering)))
# set these to -10 by default so they don't fire 

key=paramVec[4]
seed2=paramVec[3]
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
duckTick=paramVec[16]
vpnTick=paramVec[17]
deletionTick=paramVec[18]
sharingTick=paramVec[19]
# We need an agent memory parameter 
# that is, for how many ticks after an agent tries an action will it refuse to try again?
agtMemory::Int64=10

#println("Event Ticks")
#println(duckTick)
#println(vpnTick)
#println(deletionTick)
#println(sharingTick)



:paramGen