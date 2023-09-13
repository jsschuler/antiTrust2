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
names(beforeDat) <- c("key","tick","agent","act","engine")

list.rbind(searchList) -> searchDat
names(searchDat) <- c("key","tick","agent","searchEngine","optOut","waitTime","utility")

list.rbind(afterList) -> afterDat
names(afterDat) <- c("key","tick","agent","act","engine","result")

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

# now transform y back


xRng <- seq(min(agtPropUsage$blissPoint),max(agtPropUsage$blissPoint),by=0.05)
data.frame(xRng) -> xDat
names(xDat) <- c("blissPoint")
cbind(rep(1,length(xRng)),xRng) %*% matrix(centerMod$coefficients,ncol=1)-> yHat
yHatE <- (.01+exp(yHat))/(1+exp(yHat))

ggplot() + geom_point(aes(x=agtPropUsage$blissPoint,y=agtPropUsage$googPct)) + geom_line(aes(x=xRng,y=yHatE),color="red") + xlab("Bliss Point") + ylab("Google Usage")

# now, consider cases where vpns are available 

outDat[outDat$tick >= (outDat$duckTick+50) & outDat$vpnTick!=-10 & outDat$delTick==-10 & outDat$shareTick==-10,] -> afterDuckVPN

afterDuckVPN %>% transform(googBool=(currEngine=="google")) %>% group_by(key,agent,optOut) %>% summarise(googPct=mean(googBool)) -> agtUsageVPN
merge(agtUsageVPN,agentDat,by=c("agent","key")) -> agtPropUsageVPN

log((agtPropUsageVPN$googPct+.01)/(1-agtPropUsageVPN$googPct+.01)) -> logitGoogVPN
lm(logitGoogVPN~agtPropUsageVPN$blissPoint + agtUsageVPN$optOut) -> centerModVPN

blissRange <- seq(min(agtPropUsageVPN$blissPoint),max(agtPropUsageVPN$blissPoint),by=0.05)
noVPNFrame <- data.frame(blissRange,rep(0,length(blissRange)))
VPNFrame <- data.frame(blissRange,rep(1,length(blissRange)))

predict(centerModVPN,newdata = noVPNFrame) -> yHatnoVPN
predict(centerModVPN,newdata = VPNFrame) -> yHatVPN

cbind(rep(1,length(blissRange)),blissRange,rep(0,length(xRng))) %*% matrix(centerModVPN$coefficients,ncol=1)-> yHatnoVPN
cbind(rep(1,length(blissRange)),xRng,rep(1,length(blissRange))) %*% matrix(centerModVPN$coefficients,ncol=1)-> yHatVPN
yHatEnoVPN <- (.01+exp(yHatnoVPN))/(1+exp(yHatnoVPN))
yHatEVPN <- (.01+exp(yHatVPN))/(1+exp(yHatVPN))

ggplot() + geom_point(aes(x=agtPropUsageVPN$blissPoint,y=agtPropUsageVPN$googPct,color=agtPropUsageVPN$optOut),alpha=.3) + geom_line(aes(x=blissRange,y=yHatEnoVPN),color="red") + geom_line(aes(x=blissRange,y=yHatEVPN),color="blue")

ggplot() + geom_point(aes(x=agtPropUsageVPN$blissPoint,y=agtPropUsageVPN$googPct,color=agtPropUsageVPN$optOut),alpha=.3) + geom_line(aes(x=blissRange,y=yHatEnoVPN),color="red") + geom_line(aes(x=blissRange,y=yHatEVPN),color="blue") + ylim(.45,.8)

# plot VPN usage against bliss point 

outDat[outDat$tick >= (outDat$vpnTick+50) & outDat$vpnTick!=-10 & outDat$delTick==-10 & outDat$shareTick==-10,] -> afterVPN

afterVPN %>% transform(optOut=optOut=="true") %>%  group_by(key,agent) %>% summarise(optPct=mean(optOut),googPct=mean(currEngine=="google")) -> vpnAgent

# now get agent bliss point 

merge(vpnAgent,agentDat,by=c("key","agent")) -> vpnBliss

log((vpnBliss$optPct+.01)/(1-vpnBliss$optPct+.01)) -> logitVPN
lm(logitVPN~vpnBliss$blissPoint) -> centerVPNMod

blissRange <- seq(min(vpnBliss$blissPoint),max(vpnBliss$blissPoint),by=0.05)
cbind(rep(1,length(blissRange)),blissRange) %*% matrix(centerVPNMod$coefficients,ncol=1)-> vpnY

vpnYE <- (.01+exp(vpnY))/(1+exp(vpnY))

ggplot() + geom_point(aes(x=vpnBliss$blissPoint,y=vpnBliss$optPct,color=vpnBliss$googPct),alpha=.2) + geom_line(aes(x=blissRange,y=vpnYE),color="red")

# now, get vpn usage by search engine

afterVPN %>% transform(optOut=optOut=="true") %>%  group_by(key,agent,currEngine) %>% summarise(optPct=mean(optOut),googPct=mean(currEngine=="google")) -> vpnAgentEngine

merge(vpnAgentEngine,agentDat,by=c("key","agent")) -> vpnEngineBliss

vpnEngineBliss$googBool <- vpnEngineBliss$currEngine=="google"

log((vpnEngineBliss$optPct+.01)/(1-vpnEngineBliss$optPct+.01)) -> logitVPNEngine
lm(logitVPNEngine~vpnEngineBliss$blissPoint+vpnEngineBliss$googBool) -> vpnEngineMod

blissRange <- seq(min(vpnEngineBliss$blissPoint),max(vpnEngineBliss$blissPoint),by=0.05)
cbind(rep(1,length(blissRange)),blissRange,rep(0,length(blissRange))) %*% matrix(vpnEngineMod$coefficients,ncol=1)-> duckVPN
cbind(rep(1,length(blissRange)),blissRange,rep(1,length(blissRange))) %*% matrix(vpnEngineMod$coefficients,ncol=1)-> googVPN

duckVPNE <- (.01+exp(duckVPN))/(1+exp(duckVPN))
googVPNE <- (.01+exp(googVPN))/(1+exp(googVPN))


ggplot() + geom_point(aes(x=vpnEngineBliss$blissPoint,y=vpnEngineBliss$optPct,color=vpnEngineBliss$googBool),alpha=.2) + geom_line(aes(x=blissRange,y=duckVPNE),color="red")  + geom_line(aes(x=blissRange,y=googVPNE),color="green")

# now, consider usage before and after vpns 

# consider cases where Duck Duck Go entered before there was a VPN available

outDat[outDat$vpnTick!=-10 & outDat$duckTick!=-10,] -> duckVPN


quantile(duckVPN$duckTick-duckVPN$vpnTick,c(0,.05,.25,.5,.75,.95,1))

duckVPN$tickDelta <- duckVPN$duckTick-duckVPN$vpnTick

# Let's examine whether the vpn being introduced before Duck Duck Go cuts into market share 

duckVPN[duckVPN$vpnTick < duckVPN$duckTick,] -> afterVPN

afterVPN %>% transform(googBool=currEngine=="google") %>% group_by(key,agent,tickDelta) %>% summarise(googPct=mean(googBool)) -> afterVPNSmry

merge(afterVPNSmry,agentDat,by=c("key","agent")) -> afterVPNSmryAgt

ggplot() + geom_point(aes(x=afterVPNSmryAgt$blissPoint,y=afterVPNSmryAgt$googPct,color=afterVPNSmryAgt$tickDelta),alpha=.3)

# now, build the several logit models 
# how many categories do we have? 
unique(afterVPNSmryAgt$tickDelta) -> allDeltas

afterVPNSmryAgt$dummy50 <- afterVPNSmryAgt$tickDelta==50
afterVPNSmryAgt$dummy95 <- afterVPNSmryAgt$tickDelta==95

blissRange <- seq(min(afterVPNSmryAgt$blissPoint),max(afterVPNSmryAgt$blissPoint),by=0.05)

log((afterVPNSmryAgt$googPct+.01)/(1-afterVPNSmryAgt$googPct+.01)) -> logitGoogDelta

lm(logitGoogDelta~afterVPNSmryAgt$blissPoint+afterVPNSmryAgt$dummy50+afterVPNSmryAgt$dummy95) -> deltaMod
cbind(rep(1,length(blissRange)),blissRange,rep(0,length(blissRange)),rep(0,length(blissRange))) -> X1
cbind(rep(1,length(blissRange)),blissRange,rep(1,length(blissRange)),rep(0,length(blissRange))) -> X2
cbind(rep(1,length(blissRange)),blissRange,rep(0,length(blissRange)),rep(1,length(blissRange))) -> X3

Y1 <- X1 %*% matrix(deltaMod$coefficients,ncol=1)
Y2 <- X2 %*% matrix(deltaMod$coefficients,ncol=1)
Y3 <- X3 %*% matrix(deltaMod$coefficients,ncol=1)

eY1 <- (.01+exp(Y1))/(1+exp(Y1))
eY2 <- (.01+exp(Y2))/(1+exp(Y2))
eY3 <- (.01+exp(Y3))/(1+exp(Y3))

ggplot() + geom_point(aes(x=afterVPNSmryAgt$blissPoint,y=afterVPNSmryAgt$googPct,color=afterVPNSmryAgt$tickDelta),alpha=.3) + geom_line(aes(x=blissRange,y=eY1),color="black") +
  geom_line(aes(x=blissRange,y=eY2),color="red") + geom_line(aes(x=blissRange,y=eY3),color="green")


# now, let's do similar analysis for when Duck Duck Go is introduced before the VPN. 

duckVPN[duckVPN$vpnTick > duckVPN$duckTick,] -> afterDuck2
unique(afterDuck2$tickDelta)

afterDuck2$dummy40 <- afterDuck2$tickDelta==-40
afterDuck2$dummy100 <- afterDuck2$tickDelta==-100

afterDuck2 %>% transform(optOut=(optOut=="true"),googBool=(currEngine=="google")) %>% group_by(key,agent,tickDelta) %>% summarise(vpnPct=mean(optOut),dummy40=max(dummy40),dummy100=max(dummy100),googPct=mean(googBool)) -> afterDuck2VPN

merge(afterDuck2VPN,agentDat,by=c("key","agent")) -> afterDuck2VPNAgt

blissRange <- seq(min(afterDuck2VPNAgt$blissPoint),max(afterDuck2VPNAgt$blissPoint),by=0.05)

log((afterDuck2VPNAgt$vpnPct+.01)/(1-afterDuck2VPNAgt$vpnPct+.01)) -> logitVPNDelta
# now fit linear model 

lm(logitVPNDelta~afterDuck2VPNAgt$blissPoint+afterDuck2VPNAgt$dummy40+afterDuck2VPNAgt$dummy100) -> vpnTickMod

cbind(rep(1,length(blissRange)),blissRange,rep(0,length(blissRange)),rep(0,length(blissRange))) -> X1
cbind(rep(1,length(blissRange)),blissRange,rep(1,length(blissRange)),rep(0,length(blissRange))) -> X2
cbind(rep(1,length(blissRange)),blissRange,rep(0,length(blissRange)),rep(1,length(blissRange))) -> X3

Y1 <- X1 %*% matrix(vpnTickMod$coefficients,ncol=1)
Y2 <- X2 %*% matrix(vpnTickMod$coefficients,ncol=1)
Y3 <- X3 %*% matrix(vpnTickMod$coefficients,ncol=1)

eY1 <- (.01+exp(Y1))/(1+exp(Y1))
eY2 <- (.01+exp(Y2))/(1+exp(Y2))
eY3 <- (.01+exp(Y3))/(1+exp(Y3))

ggplot() + geom_point(aes(x=afterDuck2VPNAgt$blissPoint,y=afterDuck2VPNAgt$vpnPct,color=afterDuck2VPNAgt$googPct),alpha=.3) + geom_line(aes(x=blissRange,y=eY1),color="black") +
  geom_line(aes(x=blissRange,y=eY2),color="red") + geom_line(aes(x=blissRange,y=eY3),color="green")

# now, let's examine the net flow of agents from Google to Duck Duck Go over time 

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

# determine when we get the deletion law
