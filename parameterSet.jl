###########################################################################################################
#            Parameter Generation Code                                                                    #
#            July 2022                                                                                    #
#            John S. Schuler                                                                              #
#            OECD Version                                                                                 #
#                                                                                                         #
###########################################################################################################

# now select a seed 

Random.seed!(paramVec[2])
#strSeed=string(seed)
## global parameters
# now, we need the parameters for the Exponential distributions generating the two Beta parameters for each agent 
agtCnt=paramVec[8]
# how many times to run the model?
modRuns=100
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
:paramGen