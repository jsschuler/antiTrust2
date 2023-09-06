library(ggplot2)
library(dplyr)
library(rlist)

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
names(beforeDat) <- c("key","tick","agent","beforeAct","beforeActEngine")

list.rbind(searchList) -> searchDat
names(searchDat) <- c("key","tick","agent","searchEngine","optOut","waitTime","utility")

list.rbind(afterList) -> afterDat
names(afterDat) <- c("key","tick","agent","afterAct","afterActEngine","result")

list.rbind(agentList) -> agentDat
names(agentDat) <- c("key","agent","blissPoint","selfExp","unifExp")

# step one, consider only cases where there is no vpn
# and only consider data where Duck Duck Go has been available
outDat[outDat$tick >= (outDat$duckTick+50) & outDat$vpnTick==-10 & outDat$delTick==-10 & outDat$shareTick==-10,] -> afterDuck

# now, get each agent's Google usage

afterDuck %>% transform(googBool=(currEngine=="google")) %>% group_by(key,agent) %>% summarise(googPct=mean(googBool)) -> agtUsage

merge(agtUsage,agentDat,by=c("agent","key")) -> agtPropUsage


cor(agtPropUsage$googPct,agtPropUsage$blissPoint)

log((agtPropUsage$googPct+.01)/(1-agtPropUsage$googPct+.01)) -> logitGoog
lm(logitGoog~agtPropUsage$blissPoint) -> centerMod
predict(centerMod,newdata = data.frame(seq(min(agtPropUsage$blissPoint),max(agtPropUsage$blissPoint),by=0.05))) -> yHat

# now transform y back


xRng <- seq(min(agtPropUsage$blissPoint),max(agtPropUsage$blissPoint),by=0.05)
data.frame(xRng) -> xDat
names(xDat) <- c("blissPoint")
cbind(rep(1,length(xRng)),xRng) %*% matrix(centerMod$coefficients,ncol=1)-> yHat
yHatE <- (.01+exp(yHat))/(1+exp(yHat))

ggplot() + geom_point(aes(x=agtPropUsage$blissPoint,y=agtPropUsage$googPct)) + geom_line(aes(x=xRng,y=yHatE),color="red") + xlab("Bliss Point") + ylab("Google Usage")

# actually, try a quick and dirty copula

ecdf(agtPropUsage$googPct) -> Fgoog
ecdf(agtPropUsage$blissPoint) -> Fbliss

uGoog <- Fgoog(agtPropUsage$googPct)
uBliss <- Fbliss(agtPropUsage$blissPoint)

ggplot() + geom_point(aes(x=uBliss,y=uGoog))

# now, consider cases where vpns are available 

outDat[outDat$tick >= (outDat$duckTick+50) & outDat$vpnTick!=-10 & outDat$delTick==-10 & outDat$shareTick==-10,] -> afterDuckVPN

afterDuckVPN %>% transform(googBool=(currEngine=="google")) %>% group_by(key,agent,optOut) %>% summarise(googPct=mean(googBool)) -> agtUsageVPN
merge(agtUsageVPN,agentDat,by=c("agent","key")) -> agtPropUsageVPN
ggplot() + geom_point(aes(x=agtPropUsageVPN$blissPoint,y=agtPropUsageVPN$googPct,color=agtPropUsageVPN$optOut),alpha=.3) 

# next, study the agent's vpn usage behavior. 

searchDat %>% arrange(key,agent,tick) -> searchDat

merge(searchDat,agentDat[,c("key","agent","blissPoint")],by=c("key","agent")) %>% arrange(key,agent,tick) -> agtSearch 
