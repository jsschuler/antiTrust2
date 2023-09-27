################################################################################
#              Model Data Analysis Code                                        #
#               September 2023                                                 #
#               John S. Schuler                                                #
#                                                                              #
#                                                                              #
################################################################################

# libraries
library(ggplot2)
library(dplyr)
library(rlist)

# functions

mLogit <-function(arg){
  return(log((arg+.01)/(1-arg+.01)))
}

invLogit <- function(arg){
  return((.01+exp(arg))/(1+exp(arg)))
}

# we need a function that gives the bliss range 

blissRangeFunc <- function(blissData){
  return(seq(min(blissData),max(blissData),by=0.05))
}

dataLoad <- function(path){
  ctrlPath <- paste0(path,"/ctrl.csv")
  read.csv(ctrlPath,sep=",") -> control
  list.files(path) -> allFi
  
  outList <- list()
  searchList <- list()
  beforeList <- list()
  afterList <- list()
  agentList <- list()
  for (fi in allFi){
    paste0(path,"/",fi) -> fiPath
    if (grepl("output",fi)){read.csv(fiPath,header = FALSE)-> outList[[length(outList)+1]]  }
    if (grepl("before",fi)){read.csv(fiPath,header = FALSE)-> beforeList[[length(beforeList)+1]]  }
    if (grepl("search",fi)){read.csv(fiPath,header = FALSE)-> searchList[[length(searchList)+1]]  }
    if (grepl("after",fi)){read.csv(fiPath,header = FALSE)-> afterList[[length(afterList)+1]]  }
    if (grepl("agent",fi)){read.csv(fiPath,header = FALSE)-> agentList[[length(agentList)+1]]  }
  }
  list.rbind(outList) -> outDat
  names(outDat) <- c("key","tick","agent","currEngine","duckTick","vpnTick","delTick","shareTick","optOut")
  
  list.rbind(beforeList) -> beforeDat
  names(beforeDat) <- c("key","tick","agent","act","engine")
  
  list.rbind(searchList) -> searchDat
  names(searchDat) <- c("key","tick","agent","searchEngine","optOut","waitTime","utility")
  
  list.rbind(afterList) -> afterDat
  names(afterDat) <- c("key","tick","agent","act","engine","result")
  
  list.rbind(agentList) -> agentDat
  names(agentDat) <- c("key","agent","blissPoint","selfExp","unifExp")
  
  return(list(control,outDat,beforeDat,searchDat,afterDat,agentDat))
}

dataLoad("~/ResearchCode/antiTrustData") -> dList

# now test the function
data <- dList[[2]]
agtDat <- dList[[6]]
yVarb <- "googPct"
firstEvent <- "shareTick"
secondEvent <- "duckTick"
lag <- 25
mainX <- "blissPoint"




modFunc <- function(data,agtDat,yVarb,firstEvent,secondEvent,lag,mainX){
  
  # what are the other events?
  setdiff(c("duckTick","vpnTick","delTick","shareTick") ,c(firstEvent,secondEvent)) -> otherEvents
  
  
  data[data$tick >= (data[,secondEvent] + lag) & data[,firstEvent] <= data[,secondEvent] & data[,firstEvent] !=-10 & data[,secondEvent]!=-10 & data[otherEvents[[1]]]==-10 & data[otherEvents[[2]]]==-10,] -> subData
  
  subData$delta <- subData[,firstEvent] - subData[,secondEvent]
  
  subData %>% transform(googBool=(currEngine=="google"),vpnBool=(optOut=="true")) %>% group_by(key,delta,agent) %>% summarise(googPct=mean(googBool),vpnPct=mean(vpnBool)) -> agtSmry
  
  merge(agtSmry,agtDat,by=c("agent","key")) -> agtData
  xCategory <- agtData$delta
  yLogit <- mLogit(agtData[,yVarb])
  
  X1 <- agtData[,mainX]
  # now generate the x-categories
  unique(xCategory) -> allCats 
  allCats[2:length(allCats)] -> allCats 
  boolList <- list()
  for (cat in allCats){
    (agtData[,"delta"]==cat) -> boolList[[length(boolList)+1]]
  }
  boolDat <- list.cbind(boolList)
  # now assemble data set
  
  data.frame(yLogit,X1,boolDat) -> modDat 
  names(modDat)[3:ncol(modDat)] <- paste0("Z",1:(ncol(modDat)-2))
  
  stringForm <- paste0("yLogit~",paste0(names(modDat)[2:ncol(modDat)],collapse="+"))
  objForm <- as.formula(stringForm)
  # now fit model
  lm(objForm,data=modDat) -> mod
  
  # now get range of X1
  blissRangeFunc(X1) -> xRng
  # now form a list of matrices for each category
  
  
  matList <- list(cbind(rep(1,length(xRng)),xRng,matrix(0,nrow=length(xRng),ncol=length(allCats))))
  for (j in 1:length(allCats)){
    holdMat <-  matrix(0,nrow=length(xRng),ncol=length(allCats))
    holdMat[,j] <- 1
    
    matList[[length(matList)+1]] <- cbind(rep(1,length(xRng)),xRng,holdMat)
  }
  
  # now make predictions
  yPred <- c()
  for (mat in matList){
    c(yPred <- invLogit(mat %*% mod$coefficients)) -> yPred
  }
  # now, get the category
  categoryVec <- c()
  for (val in unique(xCategory)){
    categoryVec <- c(categoryVec,rep(val,length(xRng)))
  }
  
  data.frame(cbind(yPred,rep(xRng,length(unique(xCategory))),categoryVec)) -> finDat
  names(finDat) <- c("y","x","category")
  
  
  return(list(agtData,finDat))
  
}

Scenario1 <- modFunc(dList[[2]],dList[[6]], "googPct","shareTick","duckTick",5,"blissPoint")

baseData <- Scenario1[[1]]
finalDat <- Scenario1[[2]]

ggplot() + geom_point(aes(x=baseData$blissPoint,y=baseData$googPct),alpha=.1) + geom_line(aes(x=finalDat$x,y=finalDat$y,color=finalDat$category)) +  labs(color = "Delta") + xlab("Bliss Point") + ylab("Google %")

# let's try the VPN
dataLoad("~/ResearchCode/antiTrustDataVPN") -> dList
data <- dList[[2]]
agtDat <- dList[[6]]
yVarb <- "googPct"
firstEvent <- "vpnTick"
secondEvent <- "duckTick"
lag <- 0
mainX <- "blissPoint"

Scenario1 <- modFunc(dList[[2]],dList[[6]], "googPct","shareTick","duckTick",5,"blissPoint")

baseData <- Scenario1[[1]]
finalDat <- Scenario1[[2]]

ggplot() + geom_point(aes(x=baseData$blissPoint,y=baseData$googPct),alpha=.1) + geom_line(aes(x=finalDat$x,y=finalDat$y,color=as.factor(finalDat$category))) +  labs(color = "Delta") + xlab("Bliss Point") + ylab("Google %")

