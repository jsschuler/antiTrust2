###########################################################################################################
#            Antitrust Model Main Code                                                                    #
#            July 2022                                                                                    #
#            John S. Schuler                                                                              #
#            OECD Version                                                                                 #
#                                                                                                         #
###########################################################################################################
using Distributed
using Combinatorics
@everywhere using CSV
@everywhere using DataFrames
@everywhere using Distributions
@everywhere using InteractiveUtils
@everywhere using Graphs 
@everywhere using Random
@everywhere using JLD2
@everywhere using Dates

# load the control file 
@load "ctrl.jld2"
cores=16
# get the number of cores / rows of data 
remainingFrame=ctrlFrame[.!ctrlFrame.initialized,:]
remainingCount=min(cores-1,size(remainingFrame)[1])
workingFrame=remainingFrame[1:remainingCount,:]

initialVec=ctrlFrame.initialized
t=0
for el in initialVec
    global t
    if el==true
        t=t+1
    end
end
for k in (t+1):(t+1 + remainingCount)
    initialVec[k]=true
end
ctrlFrame.initialized=initialVec

@save "ctrl.jld2" ctrlFrame


@everywhere paramVec=[]

counter=0

function firstRow()
    global workingFrame
    global counter
    counter=counter+1
    return workingFrame[counter,:]
end

# two things left:
    # write the function that updates the control frame
    # and write the code that parses the order vector and assigns entry ticks

function markComplete(currKey)
    global finVec
    return filter!(x -> (x != currKey),finVec)
end

@everywhere begin 
    function pullFirst()
        
        global paramVec
        paramVec=fetch(@spawnat 1 firstRow())
        println("Check")
        println(paramVec)
        return :rowLoad
    end
end    

# if I alter the above function to throw off a complete symbol, then I can solve the breakage problem

@everywhere key=""
# first division, either the process is done or it isn,t
coreDict=Dict()
resultDict=Dict()
for j in 2:cores
    coreDict[j]=nothing
    resultDict[j]=nothing
end
t=0
while true
    killLoop=true
    for ky in keys(coreDict)
        if coreDict[ky] != :end
            killLoop=false
        end
    end
    if killLoop
        break
    end

    # write a function to allow isReady to execute on a symbol

    function isReady(arg::Future)
        return isready(arg)
    end

    function isReady(arg::Symbol)
        return false
    end
    function isReady(arg::Nothing)
        return false
    end

    for c in 2:cores
        if isnothing(coreDict[c])
            # if the core dictionary is nothing, we send it the parameters
            #println("Sending Parameters")
            println("core")
            println(c)
            coreDict[c]=@spawnat c pullFirst()
            #println(resultDict==:complete)
        elseif isReady(coreDict[c])
            #println("Ready")
            resultDict[c]=fetch(coreDict[c])
            #println("Checking")
            #println(resultDict[c])
            if resultDict[c]==:rowLoad
                println("rowLoaded")
                coreDict[c]=@spawnat c include("parameterSet2.jl")
            elseif resultDict[c]==:paramGen
                coreDict[c]=@spawnat c include("objects2.jl")
            elseif resultDict[c]==:objects
                coreDict[c]=@spawnat c include("initFunctions.jl")
            elseif resultDict[c]==:initGen
                println("initials Generated")
                coreDict[c]=@spawnat c googleGen()
            elseif resultDict[c]==:Google
                println("Google Generated")
                coreDict[c]=@spawnat c include("searchFunctions.jl")
            elseif resultDict[c]==:searchFuncs
                println("Search Funcs Generated")
                coreDict[c]=@spawnat c include("agentGen.jl")
            elseif resultDict[c]==:agentGen
                println("Agents Generated")
                coreDict[c]=@spawnat c include("modelFunctions.jl")
            elseif resultDict[c]==:modelFuncs
                println("SVG functions generated")
                coreDict[c]=@spawnat c include("NetPlot.jl")
            elseif resultDict[c]==:svg
                println("Running Model")
                coreDict[c]=@spawnat c include("modelMain.jl")
            elseif resultDict[c]==:complete
                println("Marking Complete")
                coreDict[c]=:end
            end
        end
        #println("Condition")
        #println(maximum(ctrlFrame.complete.==false)==true)
        #println(ctrlFrame.complete)
    end
end

