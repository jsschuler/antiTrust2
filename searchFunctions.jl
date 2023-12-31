###########################################################################################################
#            Antitrust Model Search Functions                                                             #
#            April 2022                                                                                   #
#            John S. Schuler                                                                              #
#            OECD Version                                                                                 #
#                                                                                                         #
###########################################################################################################

# now, we need functions whereby the search engine updates its data 
# Google records
function update(result::Float64,mask::alias,engine::google)
    push!(engine.aliasData[mask],result)
end
# Duck Duck Go does not
function update(result::Float64,mask::alias,engine::duckDuckGo)
end


function subsearch(agt::agent,engine::google,searchResolution::Float64)
    # set globals 
    # first, fit the agent's history 
    #println("Agent")
    #println(agt.agtNum)
    bestDist::probType=Uniform()
    U::Uniform=Uniform()
    #println(engine.aliasData)
    # now, check if the search engine has seen this mask before
    if !haskey(engine.aliasData,agt.mask)
        engine.aliasData[agt.mask]=[]
    end

    if length(engine.aliasData[agt.mask]) >= 30 && !agt.mask.optOut
        #println("Fitting")
        bestDist=fit(Beta,engine.aliasData[agt.mask])
    else
        bestDist=Uniform()
    end
    # now, begin the search process 
    # generate actual desired result
    result::Float64=rand(agt.betaObj,1)[1]
    # now prepare the loop
    tick::Int64=0
    cum::Float64=0.0
    newRevenue::Int64=0
    finGuess::Float64=0.0

    maxGuess::Float64=1.0
    minGuess::Float64=0.0

    while true
        tick=tick+1
        guess::Float64=rand(bestDist,1)[1]
        #println("Tick")
        #println(tick)
        #println("Target")
        #println(result)
        #println("Guess")
        #println(guess)
        #println("Tick\n"*string(tick)*"\nTarget\n"*string(result)*"\nGuess\n"*string(guess))
        if abs(guess-result) <= searchResolution
            # add this to the agent's history 
            finGuess=guess
            #println("Flag")
            break
        else
            hiArray=Float64[1]
            loArray=Float64[0]
            if guess > result
                # if the guess is too high then we replace the upper bound with the guess 
                #maxGuess=guess
                push!(hiArray,guess)
            else
                # if the guess is too low, we replace the upper bound with the guess 
                #minGuess=guess
                push!(loArray,guess)
            end
            # the max of the range should be the smallest value that was too high 
            # parallel for min of the range
            maxGuess=minimum(hiArray)
            minGuess=maximum(loArray)
            # find out the quantile of the guess for the assumed distribution
            loGuess=cdf(bestDist,minGuess)
            hiGuess=cdf(bestDist,maxGuess)
            guess=quantile(bestDist,rand(U,1)[1]*(hiGuess-loGuess)+loGuess)
        end
    end

    update(finGuess,agt.mask,agt.currEngine)

    return Any[agt.mask,agt.agtNum,tick,finGuess]
end



function subsearch(agt::agent,engine::duckDuckGo,searchResolution::Float64)
      # set globals 
    # first, fit the agent's history 
    #println("Agent")
    #println(agt.agtNum)
    bestDist::probType=Uniform()
    U::Uniform=Uniform()
        # now, check if the search engine has seen this mask before
    if !haskey(engine.aliasData,agt.mask)
        engine.aliasData[agt.mask]=[]
    end
    if length(engine.aliasData[agt.mask]) >= 30 && !agt.mask.optOut
        #println("Fitting")
        bestDist=fit(Beta,engine.aliasData[agt.mask])
    else
        bestDist=Uniform()
    end
    # now, begin the search process 
    # generate actual desired result
    result::Float64=rand(agt.betaObj,1)[1]
    # now prepare the loop
    tick::Int64=0
    cum::Float64=0.0
    newRevenue::Int64=0
    finGuess::Float64=0.0

    maxGuess::Float64=1.0
    minGuess::Float64=0.0

    while true
        tick=tick+1
        guess::Float64=rand(bestDist,1)[1]
        #println("Tick")
        #println(tick)
        #println("Target")
        #println(result)
        #println("Guess")
        #println(guess)
        #println("Tick\n"*string(tick)*"\nTarget\n"*string(result)*"\nGuess\n"*string(guess))
        if abs(guess-result) <= searchResolution
            # add this to the agent's history 
            finGuess=guess
            #println("Flag")
            break
        else
            hiArray=Float64[1]
            loArray=Float64[0]
            if guess > result
                # if the guess is too high then we replace the upper bound with the guess 
                #maxGuess=guess
                push!(hiArray,guess)
            else
                # if the guess is too low, we replace the upper bound with the guess 
                #minGuess=guess
                push!(loArray,guess)
            end
            # the max of the range should be the smallest value that was too high 
            # parallel for min of the range
            maxGuess=minimum(hiArray)
            minGuess=maximum(loArray)
            # find out the quantile of the guess for the assumed distribution
            loGuess=cdf(bestDist,minGuess)
            hiGuess=cdf(bestDist,maxGuess)
            guess=quantile(bestDist,rand(U,1)[1]*(hiGuess-loGuess)+loGuess)
        end
    end
    update(finGuess,agt.mask,agt.currEngine)
    return Any[agt.mask,agt.agtNum,tick,finGuess]
end

function search(agt::agent,searchCnt::Int64)
    global searchResolution
    global key
    global tick
    currCSV="../antiTrustData/search"*key*".csv"
    results=[]
    for n in 1:searchCnt
        searchOut=subsearch(agt,agt.currEngine,searchResolution)
        push!(results,searchOut)
    end
    tickHist=[]
    for currRes in results
        push!(tickHist,currRes[3])
    end
    vecOut=DataFrame(KeyCol=key,TickCol=tick,agtCol=agt.agtNum,agtEngine=typeof(agt.currEngine),optOut=agt.mask.optOut,waitTime=mean(tickHist),utils=util(agt,mean(tickHist)))
    CSV.write(currCSV, vecOut,header = false,append=true)
    return results
end
:searchFuncs