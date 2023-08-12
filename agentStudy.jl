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

Random.seed!(684049)
#strSeed=string(seed)
## global parameters
# now, we need the parameters for the Exponential distributions generating the two Beta parameters for each agent 
agtCnt=100
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
privacyVal::Float64=2.0
privacyBeta::Beta{Float64}=Beta(1.0,privacyVal)
# how close does the offered search result have to be before the agent accepts it?
searchResolution::Float64=.05
# we need a Poisson process for how many agents act exogenously 
switchPct::Float64=.05
poissonDist::Poisson{Float64}=Poisson(switchPct*agtCnt)
# and a probability distribution for how much agents search 
searchCountDist::NegativeBinomial{Float64}=NegativeBinomial(1.0,.1)
# set the Graph structure
pctConnected=.1
expDegree=floor(Int64,.2*agtCnt)
β=.2
agtGraph=watts_strogatz(agtCnt, expDegree, β)
# Finally, we need a Poisson parameter to how much agents search
searchQty=Poisson{Float64}(100)
# now set ticks
#ordering=paramVec[14]
# generate ticks at random 
#tickList=sort(rand(DiscreteUniform(1,modTime),length(ordering)))
# set these to -10 by default so they don't fire 
#duckTick=-10
#vpnTick=-10
#deletionTick=-10
#sharingTick=-10


key="JTEST"
seed2=257
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
duckTick=-10
vpnTick=-10
deletionTick=-10
sharingTick=-10


#println("Event Ticks")
#println(duckTick)
#println(vpnTick)
#println(deletionTick)
#println(sharingTick)

# include files
include("objects.jl")
include("initFunctions.jl")
googleGen()
include("searchFunctions.jl")
include("agentGen.jl")
include("modelFunctions.jl")

# test code
expUnifArray=[]
blissPoints=[]
expSubjArray=[]
privArray=[]
for agt in agtList
    push!(expUnifArray,agt.expUnif)
    push!(blissPoints,agt.blissPoint)
    push!(expSubjArray,agt.expSubj)
    push!(privArray,agt.privacy)
end
histogram(blissPoints, bins = 20)


agtComp=DataFrame(uniform=expUnifArray , bliss=blissPoints, subjective=expSubjArray,privacy=privArray)

scatter(agtComp.privacy,agtComp.subjective)

quantile(agtComp.uniform,[0,.05,.25,.5,.75,.95,1])
quantile(agtComp.subjective,[0,.05,.25,.5,.75,.95,1])

# relate uniform to subjective
scatter(agtComp.uniform,agtComp.subjective)

# now, for each agent, let's see how its search performance improves with search time 


# then, let's see how agent wait times compare between Google and Duck Duck Go after K steps of learning 

function learnK(k::Int64)
    global agtList
    # reset agents 
    Random.seed!(684049)
    agtList=agent[]
    genAgents()
    # for each agent, search k times 
    agtDict=Dict()

    for agt in agtList
        searchOut=search(agt,k)
        
        allTicks=[]
        for res in searchOut
            push!(allTicks,res[3])
        end
        #println(allTicks)
        agtDict[agt]=mean(allTicks)
    end
return agtDict
end
google10=learnK(10)
google20=learnK(20)
google30=learnK(30)
google40=learnK(40)
google50=learnK(50)
google100=learnK(100)
google200=learnK(200)
google300=learnK(300)
google400=learnK(400)
google500=learnK(500)
google1000=learnK(1000)


googleArray10=[]
googleArray20=[]
googleArray30=[]
googleArray40=[]
googleArray50=[]
googleArray100=[]
googleArray200=[]
googleArray300=[]
googleArray400=[]
googleArray500=[]
googleArray1000=[]

for i in 1:length(agtList)
    push!(googleArray10,0)
    push!(googleArray20,0)
    push!(googleArray30,0)
    push!(googleArray40,0)
    push!(googleArray50,0)
    push!(googleArray100,0)
    push!(googleArray200,0)
    push!(googleArray300,0)
    push!(googleArray400,0)
    push!(googleArray500,0)
    push!(googleArray1000,0)

end

for agt in keys(google10)
    googleArray10[agt.agtNum]=google10[agt]
end
for agt in keys(google20)
    googleArray20[agt.agtNum]=google20[agt]
end
for agt in keys(google30)    
    googleArray30[agt.agtNum]=google30[agt]
end
for agt in keys(google40)
    googleArray40[agt.agtNum]=google40[agt]
end
for agt in keys(google50)    
    googleArray50[agt.agtNum]=google50[agt]
end
for agt in keys(google100)
    googleArray100[agt.agtNum]=google100[agt]
end
for agt in keys(google200)
    googleArray200[agt.agtNum]=google200[agt]
end
for agt in keys(google300)    
    googleArray300[agt.agtNum]=google300[agt]
end
for agt in keys(google400)
    googleArray400[agt.agtNum]=google400[agt]
end
for agt in keys(google500)    
    googleArray500[agt.agtNum]=google500[agt]
end
for agt in keys(google1000)    
    googleArray1000[agt.agtNum]=google1000[agt]
end

googFrame=DataFrame(

gLearn10 =googleArray10,
gLearn20 =googleArray20,
gLearn30 =googleArray30,
gLearn40 =googleArray40,
gLearn50 =googleArray50,
gLearn100=googleArray100,
gLearn200=googleArray200,
gLearn300=googleArray300,
gLearn400=googleArray400,
gLearn500=googleArray500,
gLearn1000=googleArray1000
)


# now examine them for Duck Duck Go
function learnKDuck(k::Int64)
    global agtList
    global engineList
    # reset agents 
    Random.seed!(684049)
    agtList=agent[]
    genAgents()
    duckGen()
    for agt in agtList
        agt.currEngine=engineList[2]
    end
    # for each agent, search k times 
    agtDict=Dict()

    for agt in agtList
        searchOut=search(agt,k)
        allTicks=[]
        for res in searchOut
            push!(allTicks,res[3])
        end
        #println(allTicks)
        agtDict[agt]=mean(allTicks)
    end
return agtDict
end


duck10 =learnKDuck(10)
duck20 =learnKDuck(20)
duck30 =learnKDuck(30)
duck40 =learnKDuck(40)
duck50 =learnKDuck(50)
duck100=learnKDuck(100)
duck200=learnKDuck(200)
duck300=learnKDuck(300)
duck400=learnKDuck(400)
duck500=learnKDuck(500)
duck1000=learnKDuck(1000)

# now prepare plots of ticks 

duckArray10 =[]
duckArray20 =[]
duckArray30 =[]
duckArray40 =[]
duckArray50 =[]
duckArray100=[]
duckArray200=[]
duckArray300=[]
duckArray400=[]
duckArray500=[]
duckArray1000=[]

for i in 1:length(agtList)
    push!(duckArray10,0)
    push!(duckArray20,0)
    push!(duckArray30,0)
    push!(duckArray40,0)
    push!(duckArray50,0)
    push!(duckArray100,0)
    push!(duckArray200,0)
    push!(duckArray300,0)
    push!(duckArray400,0)
    push!(duckArray500,0)
    push!(duckArray1000,0)
end
for agt in keys(duck10)
    duckArray10[agt.agtNum]=duck10[agt]
end
for agt in keys(duck20)    
    duckArray20[agt.agtNum]=duck20[agt]
end
for agt in keys(duck30)
    duckArray30[agt.agtNum]=duck30[agt]
end
for agt in keys(duck40)    
    duckArray40[agt.agtNum]=duck40[agt]
end
for agt in keys(duck50)    
    duckArray50[agt.agtNum]=duck50[agt]
end
for agt in keys(duck100)
    duckArray100[agt.agtNum]=duck100[agt]
end
for agt in keys(duck200)    
    duckArray200[agt.agtNum]=duck200[agt]
end
for agt in keys(duck300)
    duckArray300[agt.agtNum]=duck300[agt]
end
for agt in keys(duck400)    
    duckArray400[agt.agtNum]=duck400[agt]
end
for agt in keys(duck500)    
    duckArray500[agt.agtNum]=duck500[agt]
end
for agt in keys(duck1000)    
    duckArray1000[agt.agtNum]=duck1000[agt]
end

duckFrame=DataFrame(
dLearn10 =duckArray10,
dLearn20 =duckArray20,
dLearn30 =duckArray30,
dLearn40 =duckArray40,
dLearn50 =duckArray50,
dLearn100=duckArray100,
dLearn200=duckArray200,
dLearn300=duckArray300,
dLearn400=duckArray400,
dLearn500=duckArray500,
dLearn1000=duckArray1000)

sideFrame=hcat(googFrame,duckFrame)

# now, let's determine the utility agents get from each 
utilArray=[]
for k in 1:size(sideFrame)[2]
    agtArray=[]
    for agt in agtList
        push!(agtArray,util(agt,sideFrame[agt.agtNum,k]))
    end
    push!(utilArray,agtArray)
end

utilNames=Symbol.(cat("gUtil".*string.(10:10:50),"gUtil".*string.(100:100:500),"gUtil1000","dUtil".*string.(10:10:50),"dUtil".*string.(100:100:500),"dUtil1000",dims=1))

allUtils=DataFrame(utilArray,utilNames)

# now, show that forcing all agents to use a vpn puts Google and Duck Duck Go on an equal footing 

function learnKvpn(k::Int64)
    global agtList
    # reset agents 
    Random.seed!(684049)
    agtList=agent[]
    genAgents()

    for agt in agtList
        agt.mask.optOut=true
    end
    # for each agent, search k times 
    agtDict=Dict()

    for agt in agtList
        searchOut=search(agt,k)
        
        allTicks=[]
        for res in searchOut
            push!(allTicks,res[3])
        end
        #println(allTicks)
        agtDict[agt]=mean(allTicks)
    end
return agtDict
end

googleVpn10=learnKvpn(10)
googleVpn20=learnKvpn(20)
googleVpn30=learnKvpn(30)
googleVpn40=learnKvpn(40)
googleVpn50=learnKvpn(50)
googleVpn100=learnKvpn(100)
googleVpn200=learnKvpn(200)
googleVpn300=learnKvpn(300)
googleVpn400=learnKvpn(400)
googleVpn500=learnKvpn(500)
googleVpn1000=learnKvpn(1000)


googleArrayVpn10=[]
googleArrayVpn20=[]
googleArrayVpn30=[]
googleArrayVpn40=[]
googleArrayVpn50=[]
googleArrayVpn100=[]
googleArrayVpn200=[]
googleArrayVpn300=[]
googleArrayVpn400=[]
googleArrayVpn500=[]
googleArrayVpn1000=[]

for i in 1:length(agtList)
    push!(googleArrayVpn10,0)
    push!(googleArrayVpn20,0)
    push!(googleArrayVpn30,0)
    push!(googleArrayVpn40,0)
    push!(googleArrayVpn50,0)
    push!(googleArrayVpn100,0)
    push!(googleArrayVpn200,0)
    push!(googleArrayVpn300,0)
    push!(googleArrayVpn400,0)
    push!(googleArrayVpn500,0)
    push!(googleArrayVpn1000,0)

end

for agt in keys(googleVpn10)
    googleArrayVpn10[agt.agtNum]=googleVpn10[agt]
end
for agt in keys(googleVpn20)
    googleArrayVpn20[agt.agtNum]=googleVpn20[agt]
end
for agt in keys(googleVpn30)    
    googleArrayVpn30[agt.agtNum]=googleVpn30[agt]
end
for agt in keys(googleVpn40)
    googleArrayVpn40[agt.agtNum]=googleVpn40[agt]
end
for agt in keys(googleVpn50)    
    googleArrayVpn50[agt.agtNum]=googleVpn50[agt]
end
for agt in keys(googleVpn100)
    googleArrayVpn100[agt.agtNum]=googleVpn100[agt]
end
for agt in keys(googleVpn200)
    googleArrayVpn200[agt.agtNum]=googleVpn200[agt]
end
for agt in keys(googleVpn300)    
    googleArrayVpn300[agt.agtNum]=googleVpn300[agt]
end
for agt in keys(googleVpn400)
    googleArrayVpn400[agt.agtNum]=googleVpn400[agt]
end
for agt in keys(googleVpn500)    
    googleArrayVpn500[agt.agtNum]=googleVpn500[agt]
end
for agt in keys(googleVpn1000)    
    googleArrayVpn1000[agt.agtNum]=googleVpn1000[agt]
end

googFrameVpn=DataFrame(

gLearnVpn10 =googleArrayVpn10,
gLearnVpn20 =googleArrayVpn20,
gLearnVpn30 =googleArrayVpn30,
gLearnVpn40 =googleArrayVpn40,
gLearnVpn50 =googleArrayVpn50,
gLearnVpn100=googleArrayVpn100,
gLearnVpn200=googleArrayVpn200,
gLearnVpn300=googleArrayVpn300,
gLearnVpn400=googleArrayVpn400,
gLearnVpn500=googleArrayVpn500,
gLearnVpn1000=googleArrayVpn1000
)

# now do Duck Duck Go with VPN 

function learnKDuckVpn(k::Int64)
    global agtList
    global engineList
    # reset agents 
    Random.seed!(684049)
    agtList=agent[]
    genAgents()
    duckGen()
    for agt in agtList
        agt.currEngine=engineList[2]
        agt.mask.optOut=true
    end
    # for each agent, search k times 
    agtDict=Dict()

    for agt in agtList
        searchOut=search(agt,k)
        allTicks=[]
        for res in searchOut
            push!(allTicks,res[3])
        end
        #println(allTicks)
        agtDict[agt]=mean(allTicks)
    end
return agtDict
end


duckVpn10 =learnKDuckVpn(10)
duckVpn20 =learnKDuckVpn(20)
duckVpn30 =learnKDuckVpn(30)
duckVpn40 =learnKDuckVpn(40)
duckVpn50 =learnKDuckVpn(50)
duckVpn100=learnKDuckVpn(100)
duckVpn200=learnKDuckVpn(200)
duckVpn300=learnKDuckVpn(300)
duckVpn400=learnKDuckVpn(400)
duckVpn500=learnKDuckVpn(500)
duckVpn1000=learnKDuckVpn(1000)

# now prepare plots of ticks 

duckArrayVpn10 =[]
duckArrayVpn20 =[]
duckArrayVpn30 =[]
duckArrayVpn40 =[]
duckArrayVpn50 =[]
duckArrayVpn100=[]
duckArrayVpn200=[]
duckArrayVpn300=[]
duckArrayVpn400=[]
duckArrayVpn500=[]
duckArrayVpn1000=[]

for i in 1:length(agtList)
    push!(duckArrayVpn10,0)
    push!(duckArrayVpn20,0)
    push!(duckArrayVpn30,0)
    push!(duckArrayVpn40,0)
    push!(duckArrayVpn50,0)
    push!(duckArrayVpn100,0)
    push!(duckArrayVpn200,0)
    push!(duckArrayVpn300,0)
    push!(duckArrayVpn400,0)
    push!(duckArrayVpn500,0)
    push!(duckArrayVpn1000,0)
end
for agt in keys(duckVpn10)
    duckArrayVpn10[agt.agtNum]=duckVpn10[agt]
end
for agt in keys(duckVpn20)    
    duckArrayVpn20[agt.agtNum]=duckVpn20[agt]
end
for agt in keys(duckVpn30)
    duckArrayVpn30[agt.agtNum]=duckVpn30[agt]
end
for agt in keys(duckVpn40)    
    duckArrayVpn40[agt.agtNum]=duckVpn40[agt]
end
for agt in keys(duckVpn50)    
    duckArrayVpn50[agt.agtNum]=duckVpn50[agt]
end
for agt in keys(duckVpn100)
    duckArrayVpn100[agt.agtNum]=duckVpn100[agt]
end
for agt in keys(duckVpn200)    
    duckArrayVpn200[agt.agtNum]=duckVpn200[agt]
end
for agt in keys(duckVpn300)
    duckArrayVpn300[agt.agtNum]=duckVpn300[agt]
end
for agt in keys(duckVpn400)    
    duckArrayVpn400[agt.agtNum]=duckVpn400[agt]
end
for agt in keys(duckVpn500)    
    duckArrayVpn500[agt.agtNum]=duckVpn500[agt]
end
for agt in keys(duckVpn1000)    
    duckArrayVpn1000[agt.agtNum]=duckVpn1000[agt]
end

duckFrameVpn=DataFrame(
dLearnVpn10 =duckArrayVpn10,
dLearnVpn20 =duckArrayVpn20,
dLearnVpn30 =duckArrayVpn30,
dLearnVpn40 =duckArrayVpn40,
dLearnVpn50 =duckArrayVpn50,
dLearnVpn100=duckArrayVpn100,
dLearnVpn200=duckArrayVpn200,
dLearnVpn300=duckArrayVpn300,
dLearnVpn400=duckArrayVpn400,
dLearnVpn500=duckArrayVpn500,
dLearnVpn1000=duckArrayVpn1000)

sideFrameVpn=hcat(googFrameVpn,duckFrameVpn)


# run model? 



#include("modelMain.jl")