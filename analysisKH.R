################################################################################
#             Final Analysis Code                                              #
#             Anti-Trust Model - Side Payments                                 #
#             June 2024                                                        #
#             John S. Schuler                                                  #
#                                                                              #
################################################################################

#### BASIC RESULTS #######

library(ggplot2)
library(ggExtra)
library(ggpubr)
library(dplyr)
library(rlist)

# template
googColor <- "#34a853"
#basePoint <- "white"
#bgFill <- "black"
basePoint <- "black"
bgFill <- "white"
vpnTrue <- "#4285f4"
vpnFalse <- "#ea4335"
hiOrange <- "#E37151"
shareColor <- "#ffa700"
#vpnColor <- "vpnColor"
vpnColor <- "purple"

setwd("~/ResearchCode/antiTrustData")
read.csv("ctrl.csv",sep=",") -> control
list.files("~/ResearchCode/antiTrustData") -> allFi

outList <- list()
searchList <- list()
beforeList <- list()
afterList <- list()
agentList <- list()
for (fi in allFi){
  if (grepl("output",fi)){read.csv(fi,header = FALSE)-> outList[[length(outList)+1]]  }
  if (grepl("before",fi)){read.csv(fi,header = FALSE)-> beforeList[[length(beforeList)+1]]  }
  if (grepl("search",fi)){read.csv(fi,header = FALSE)-> searchList[[length(searchList)+1]]  }
  if (grepl("after",fi)){read.csv(fi,header = FALSE)-> afterList[[length(afterList)+1]]  }
  if (grepl("agent",fi)){read.csv(fi,header = FALSE)-> agentList[[length(agentList)+1]]  }
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

read.csv("ctrl.csv") -> control

# now get all unique runs

data.frame(unique(outDat[,c("key")])) -> allRuns
names(allRuns) <- c("key")
allRuns$ran <- TRUE
merge(control,allRuns,by="key",all.x=TRUE) -> joint
joint[is.na(joint$ran),"ran"] <- FALSE

table(joint$ran)
joint$ran -> bool
(1:nrow(joint))[!bool] -> missing
missing[2:length(missing)]-missing[1:(length(missing)-1)]
# now classify the runs as control runs, VPN runs, sharing runs, deletion runs

control$VPN <- control$vpnTick !=-10
control$deletion <- control$deletionTick !=-10
control$sharing <- control$sharingTick != -10
control$runType <- control$VPN + 2*control$deletion + 4*control$sharing

control$VPN <- NULL
control$deletion <- NULL
control$sharing <- NULL

control$fullSeed <- paste0(control$seed1,"-",control$seed2)



outDat %>% group_by(key) %>% summarize(tick=max(tick))

# remove all data before tick 60
outDat <- outDat[outDat$tick >= 200,]

outDat %>% transform(googBool=(currEngine=="google")) %>% group_by(key) %>% summarize(searchCnt=n(),googCnt=sum(googBool)) -> keySmry
keySmry$duckCnt <- keySmry$searchCnt-keySmry$googCnt

# now cut down control to only have the relevant information
controlMerge <- control[,c("key","seed1","seed2","runType")]

finDat <- merge(controlMerge,keySmry,by="key")
finDat$googPct <- finDat$googCnt/finDat$searchCnt
baseDat <- finDat[finDat$runType==0,]
vpnDat <- finDat[finDat$runType==1,]
delDat <- finDat[finDat$runType==2,]
shareDat <- finDat[finDat$runType==4,]




ggplot()+ geom_histogram(aes(x=finDat$googPct,fill=as.factor(finDat$runType)),alpha=.4) + xlab("% of Searches on Google") + ylab("Count")
cbind(c("Base","VPN","Deletion","Share"),rbind(
quantile(baseDat$googPct,c(.01,.05,.25,.5,.75,.95,.99)),
quantile(vpnDat$googPct,c(.01,.05,.25,.5,.75,.95,.99)),
quantile(delDat$googPct,c(.01,.05,.25,.5,.75,.95,.99)),
quantile(shareDat$googPct,c(.01,.05,.25,.5,.75,.95,.99))))




names(baseDat)[5:8] <- paste0(names(baseDat)[5:8],"Base")
names(vpnDat)[5:8] <- paste0(names(baseDat)[5:8],"Vpn")
names(delDat)[5:8] <- paste0(names(baseDat)[5:8],"Del")
names(shareDat)[5:8] <- paste0(names(baseDat)[5:8],"Share")

merge(baseDat,vpnDat,by=c("seed1","seed2")) -> vpnBase
merge(baseDat,delDat,by=c("seed1","seed2")) -> delBase
merge(baseDat,shareDat,by=c("seed1","seed2")) -> shareBase

vpnBase$delta <- vpnBase$googPctBaseVpn-vpnBase$googPctBase
delBase$delta <- delBase$googPctBaseDel - delBase$googPctBase
shareBase$delta <- shareBase$googPctBaseShare - shareBase$googPctBase

ggplot() + geom_histogram(aes(x=vpnBase$delta)) + ggtitle("VPN") + xlab("VPN - Base")
ggplot() + geom_histogram(aes(x=delBase$delta)) + ggtitle("Deletion") + xlab("Deletion - Base")
ggplot() + geom_histogram(aes(x=shareBase$delta)) + ggtitle("Sharing") + xlab("Sharing - Base")

# follow up
# study these gaps as a function of privacy parameter of the population. This will require additonal model runs. 

