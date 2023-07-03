currentActDict=Dict{agent,Union{Nothing,Null,action}}()
scheduleActDict=Dict{agent,Union{Nothing,Null,action}}()
deletionDict=Dict{agent,Bool}()
sharingDict=Dict{agent,Bool}()


# initialize all agents actions to nothing
for agt in agtList
    currentActDict[agt]=nothing
    scheduleActDict[agt]=nothing
    deletionDict[agt]=false
    sharingDict[agt]=false
end
# we need an array to store the already generated structs to avoid redefining them 
structTuples=Set([])

# order of introdudction, 

# Duck Duck Go must be introduced before data sharing 




tick=0
for ticker in 1:modRuns
    # principle 1: agents search no matter what 
    global tick
    tick=tick + 1
    # Step 0: new laws or search engines are introduced.
     # first, introduce any new laws or search engines 
     if tick==30
        #println("DuckDuckGo In")
        duckGen()
        @actionGen()
    end
    #println("Tick")
    #println(tick)
    # now introduce new laws if applicable 
    if tick==15
        #println("VPN In")
        vpnGen(tick)
        #@actionGen()
    end
    #println("Tick")
    #println(tick)
    if tick==20
        #println("Deletion In")
        deletionGen(tick)
        @actionGen()
    end
    #println("Tick")
    #println(tick)
    if tick==31
        #println("Sharing In")
        sharingGen(tick)
        @actionGen()
    end
    #println("Action List")
    #println(length(actionList))
    
    # Step 1: agents previously scheduled to act take action
    # some of these agents were chosen exogenously, 
    # some because they were neighbors of agents that did act 
    for agt in agtList
        takeAction(agt)
    end

    # now, some agents are chosen exogenously to act next time
    exogenousActs()
    # Step 2: agents search 

    allSearches(tick)

    # Step 3: agents decide to reverse the action or not
    for agt in agtList
        reverseDecision(agt)
    end
    # now, make the next tick's dictionary the current one
    #schedulePrint(currentActDict)
    #schedulePrint(scheduleActDict)
    resetSchedule()
    # now plot data
    #svgGen(tick)
end
#println("Deletion")
#for k in keys(deletionDict)
#    println(deletionDict[k])
#end
#println("Sharing")
#for k in keys(sharingDict)
#    println(sharingDict[k])
#end

:complete