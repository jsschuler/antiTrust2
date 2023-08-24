#############################################################################################################################
#                         Debugging Code                                                                                    #
#                         This code does not run as part of the model                                                       #
#                                                                                                                           #
#                                                                                                                           #
#                                                                                                                           #
#############################################################################################################################


function agtSearchData(agt::agent)
    res=searchRes=search(agtList[1],100)
    tickVec=[]
    for i in 1:length(res)
        push!(tickVec,res[i][3])
    end
    return mean(tickVec)
end

function agtSearchStats(agt::agent)
    perfVec=[]
    for k in 1:1000
        push!(perfVec,agtSearchData(agtList[1]))
    end

    # now, how often does it exceed the bliss point?
    excBliss=perfVec .> agt.blissPoint
    excUnif=perfVec .> agt.expUnif
    excSelf=perfVec .> agt.expSubj

    # how often will the agent get more utility from the uniform expectation than the realization?
    function currUtil(x)
        return util(agt,x)
    end

    realDelta=currUtil.(perfVec) .< currUtil(agt.expUnif) 

    statDict=Dict()
    statDict["Mean"]=mean(perfVec)
    statDict["Variance"]=var(perfVec)
    statDict["Min"]=minimum(perfVec)
    statDict["q5"]=quantile(perfVec,.05)
    statDict["q25"]=quantile(perfVec,.25)
    statDict["Median"]=quantile(perfVec,.5)
    statDict["q75"]=quantile(perfVec,.75)
    statDict["q95"]=quantile(perfVec,.95)
    statDict["Max"]=maximum(perfVec)
    statDict["excBliss"]=mean(excBliss)
    statDict["excUnif"]=mean(excUnif)
    statDict["excSelf"]=mean(excSelf)
    statDict["realDelta"]=mean(realDelta)

    return statDict
end

# now, sort agents by their bliss point 

agtNumVec=[]
blissVec=[]
for agt in agtList
    push!(agtNumVec,agt.agtNum)
    push!(blissVec,agt.blissPoint)
end

sortFrame=DataFrame(agtnum=agtNumVec,bliss=blissVec)
sort!(sortFrame,:bliss)

# now get the real Delta for all agents 

deltaVec=[]
for idx in sortFrame.agtnum
    stats=agtSearchStats(agtList[idx])
    push!(deltaVec,stats["realDelta"])
end

# now, let's compare 100 runs of the same age Google vs Duck Duck Go

agt1=agtList[84]

googPerf1=agtSearchStats(agt1)

agt1.currEngine=engineList[2]
agt1.prevEngine=engineList[1]

duckPerf1=agtSearchStats(agt1)

agt2=agtList[22]
agt2.currEngine=engineList[1]
agt2.prevEngine=nothing
googPerf2=agtSearchStats(agt2)

agt2.currEngine=engineList[2]
agt2.prevEngine=nothing
duckPerf2=agtSearchStats(agt2)

# let's examine the preference Generation
prefVec=[]
for t in 1:100
    push!(prefVec,preferenceGen())
end

function densityPlot(distrib)
    x0=minimum(distrib)
    x1=maximum(distrib)
    if isinf(x0)
        x0=quantile(distrib,.0000001)
    end
    if isinf(x1)
        x1=quantile(distrib,1-.0000001)
    end

    xVals=range(x0,x1,length=1000)
    densityVals=pdf.(distrib,xVals)
    plot!(xVals, densityVals, label="Density", xlabel="x", ylabel="Density", title="Density Plot")
end

for el in prefVec
    densityPlot(el)
    sleep(3)
end

# now, consider a the same agent 

currAgt=agtList[46]


googleBetter=[]

for t in 1:1000
    currAgt.currEngine=engineList[1]
    googleSearch=search(currAgt,1000)
    currAgt.currEngine=engineList[2]
    duckSearch=search(currAgt,1000)

    push!(googleBetter,util(currAgt,googleSearch[3]) > util(currAgt,duckSearch[3]))

end

