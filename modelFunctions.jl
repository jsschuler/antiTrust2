# now, the takeAction function

function takeAction(agt::agent)
    global currentActDict
    global scheduleActDict

    if isnothing(currentActDict[agt])
        #println("Not Acting")
        return false
    else
        #println("Acting")
        currAct=currentActDict[agt]
        #println(typeof(currAct.law))
        #println(typeof(currAct.engine))
        beforeAct(agt,currAct)
        # now, schedule actions for neighbors 
        global agtGraph
        neighborNums=collect(neighbors(agtGraph,agt.agtNum))
        agtVec=agent[]
        for n in neighborNums
            push!(agtVec,agtList[n])
        end
        # now, if the agent was previously scheduled to act next time
        # we do not overwrite this assignment 
        # also, if the agent has undertaken the same action within memory, we do not schedule it 
        global tick
        global agtMemory
        for vAgt in agtVec
            # now, we check that it has been at least a given number of ticks since the agent 
            # last undertook this action
            ### CHECK THIS LOGIC ###
            if !isnothing(actionHistory[vAgt][currentActDict[agt]])
                cutOffTick=actionHistory[vAgt][currentActDict[agt]]+agtMemory
            else
                cutOffTick=0
            end
            if isnothing(scheduleActDict[vAgt]) & tick > cutOffTick
                scheduleActDict[vAgt]=currAct
            end
        end
        return true
    end
end

function exogenousActs()
    global actionList
    # pick a random number of agents to act
    if length(actionList)==0
        return false
    else
        #println("Exogenous!")
        global poissonDist
        exogCnt=rand(poissonDist,1)[1]
        exogAgts=sample(agtList,min(exogCnt,length(agtList)),replace=false)
        #println("Exogenous Poisson")
        #println(exogCnt)
        #println("Action Count")
        #println(length(exogAgts))
        #println(typeof.(actionList))
        # now, assign these agents actions if they do not already have them
        global actionList
        global scheduleActDict
        #println(agtNumber.(keys(scheduleActDict)))
        for agt in exogAgts
            if isnothing(scheduleActDict[agt])
                #println("Act Assigned")
                newAct=sample(actionList,1)[1]
                scheduleActDict[agt]=newAct
                #actTarget(newAct)
            end
        end
        return true
    end
end

function allSearches(tick)
    global agtCnt
    # have each agent search 50 times
    #searchCnt=rand(searchQty,agtCnt)
    searchCnt=repeat([50],agtCnt)
    # randomize agent ordering
    searchOrder=sample(1:agtCnt,agtCnt,replace=false)
    for i in searchOrder
        # we need an array for how long it took
        searchWait=Int64[]
        #println(searchCnt[i])
        #println("searching")
        #println(agtList[i].agtNum)
        #println(typeof(agtList[i].currEngine))
        #println(typeof(agtList[i].prevEngine))
        searchRes=search(agtList[i],searchCnt[i])
        # now for each agent, we need to know the final target of the search result 
        for res in searchRes
        # update search engine records for the alias with the search target
            update(res[4],agtList[i].mask,agtList[i].currEngine)
            push!(searchWait,res[3])
        end
        # now update agent's history
        agtList[i].history[tick]=mean(searchWait)
    end
end

function reverseDecision(agt::agent)
    if !isnothing(agt.lastAct)
        #println("Comparison")
        global tick
        #println(typeof(agt.lastAct))
        #println("old util")
        #println(util(agt,agt.history[tick]))
        #println("new util")
        #println(util(agt,agt.history[tick-1]))
        result=util(agt,agt.history[tick]) > util(agt,agt.history[tick-1])
        if result
            #println("Behavior Change")
            #println(typeof(agt.currEngine))
            #println(typeof(agt.prevEngine))
        end
        afterAct(agt,result,agt.lastAct)
    end
end

function resetSchedule()
    global currentActDict
    global scheduleActDict
    global agtList
    #println("New Debug")
    for agt in agtList
        
        #println(typeof(currentActDict[agt]))
        #println(typeof(scheduleActDict[agt]))
        # if the agent has no current action, make its current action its scheduled action
        if isnothing(currentActDict[agt])
            currentActDict[agt]=scheduleActDict[agt]
            scheduleActDict[agt]=nothing
        else
            # otherwise, reset its current action 
            currentActDict[agt]=nothing
        end

    end
end
:modelFuncs