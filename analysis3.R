library(ggplot2)
library(ggExtra)
library(ggpubr)
library(dplyr)
library(rlist)

setwd("~/ResearchCode/antiTrustDataVPN")
read.csv("ctrl.csv",sep=",") -> control
list.files("~/ResearchCode/antiTrustDataVPN") -> allFi

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

# THESE PLOTS ESTABLISH THE BASIC RESULT, HIGHER BLISS POINT AGENTS SHY AWAY FROM GOOGLE
# NOTE THESE AGENTS DO NOT HAVE VPN ACCESS

ggplot() + geom_point(aes(x=agtPropUsage$blissPoint,y=agtPropUsage$googPct),alpha=.1) + geom_line(aes(x=xRng,y=yHatE),color="red") + xlab("Bliss Point") + ylab("Google Usage") + ggtitle("Modified Logit") -> plt1


window <- .5
seq(min(agtPropUsage$blissPoint),max(agtPropUsage$blissPoint),by=window) -> rng

blissX <- c()
mnGoog <- c()
for (k in 1:(length(rng)-1)){
  blissX <- c(blissX,(rng[k+1]+rng[k])/2)
  mnGoog <- c(mnGoog,mean(agtPropUsage[agtPropUsage$blissPoint >= rng[k] & agtPropUsage$blissPoint < rng[k+1] ,"googPct"]))
}

ggplot() + geom_point(aes(x=agtPropUsage$blissPoint,y=agtPropUsage$googPct),alpha=.1) + geom_line(aes(x=blissX,y=mnGoog),color="red") + xlab("Bliss Point") + ylab("% Google Usage") + ggtitle("Moving Average") -> plt2
ggarrange(plt1,plt2,nrow=1,ncol=2)


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

cbind(rep(1,length(blissRange)),blissRange,rep(0,length(blissRange))) %*% matrix(centerModVPN$coefficients,ncol=1)-> yHatnoVPN
cbind(rep(1,length(blissRange)),blissRange,rep(1,length(blissRange))) %*% matrix(centerModVPN$coefficients,ncol=1)-> yHatVPN
yHatEnoVPN <- (.01+exp(yHatnoVPN))/(1+exp(yHatnoVPN))
yHatEVPN <- (.01+exp(yHatVPN))/(1+exp(yHatVPN))

ggplot() + geom_point(aes(x=agtPropUsageVPN$blissPoint,y=agtPropUsageVPN$googPct,color=agtPropUsageVPN$optOut),alpha=.1) + geom_line(aes(x=blissRange,y=yHatEnoVPN),color="red") + geom_line(aes(x=blissRange,y=yHatEVPN),color="blue") + xlab("Bliss Point") + ylab("Google Usage") + labs(color = "VPN") + ggtitle("Modified Logit") -> plt1

#ggplot() + geom_point(aes(x=agtPropUsageVPN$blissPoint,y=agtPropUsageVPN$googPct,color=agtPropUsageVPN$optOut),alpha=.1) + geom_line(aes(x=blissRange,y=yHatEnoVPN),color="red") + geom_line(aes(x=blissRange,y=yHatEVPN),color="blue") + ylim(.45,.8)


window <- .5
seq(min(agtPropUsageVPN$blissPoint),max(agtPropUsageVPN$blissPoint),by=window) -> rng

blissX <- c()
mnNoVPN <- c()
mnVPN <- c()

for (k in 1:(length(rng)-1)){
  blissX <- c(blissX,(rng[k+1]+rng[k])/2)
  mnNoVPN <- c(mnNoVPN,mean(agtPropUsageVPN[agtPropUsageVPN$optOut=="false" & agtPropUsageVPN$blissPoint >= rng[k] & agtPropUsageVPN$blissPoint < rng[k+1] ,"googPct"]))
  mnVPN <- c(mnVPN,mean(agtPropUsageVPN[agtPropUsageVPN$optOut=="true" & agtPropUsageVPN$blissPoint >= rng[k] & agtPropUsageVPN$blissPoint < rng[k+1],"googPct"]))
  
}

ggplot() + geom_point(aes(x=agtPropUsageVPN$blissPoint,y=agtPropUsageVPN$googPct,color=agtPropUsageVPN$optOut),alpha=.1) + geom_line(aes(x=blissX,y=mnNoVPN),color="red") + geom_line(aes(x=blissX,y=mnVPN),color="blue") + xlab("Bliss Point") + ylab("% Google Usage") + ggtitle("Moving Average") + labs(color = "VPN")-> plt2
ggarrange(plt1,plt2,nrow=1,ncol=2,common.legend = TRUE, legend = "bottom")

# NOW STUDY AGENT'S TENDENCY TO USE VPNS

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

ggplot() + geom_point(aes(x=vpnBliss$blissPoint,y=vpnBliss$optPct,color=vpnBliss$googPct),alpha=.1) + geom_line(aes(x=blissRange,y=vpnYE),color="red") + xlab("Bliss Point") + ylab("% VPN Usage") + labs(color="Google %") + ggtitle("Modified Logit") -> plt1

window <- .5
seq(min(vpnBliss$blissPoint),max(vpnBliss$blissPoint),by=window) -> rng

blissX <- c()
mnNoVPN <- c()
mnVPN <- c()

for (k in 1:(length(rng)-1)){
  blissX <- c(blissX,(rng[k+1]+rng[k])/2)
  
  mnVPN <- c(mnVPN,mean(vpnBliss[vpnBliss$blissPoint >= rng[k] & vpnBliss$blissPoint < rng[k+1],"optPct"]))
  
}

ggplot() + geom_point(aes(x=vpnBliss$blissPoint,y=vpnBliss$optPct,color=vpnBliss$googPct),alpha=.1) + geom_line(aes(x=blissX,y=mnVPN),color="red") + xlab("Bliss Point") + ylab("% VPN Usage") + ggtitle("Moving Average") + labs(color = "Google %")-> plt2
ggarrange(plt1,plt2,nrow=1,ncol=2,common.legend = TRUE, legend = "bottom")



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


ggplot() + geom_point(aes(x=vpnEngineBliss$blissPoint,y=vpnEngineBliss$optPct,color=vpnEngineBliss$googBool),alpha=.1) + geom_line(aes(x=blissRange,y=duckVPNE),color="red")  + geom_line(aes(x=blissRange,y=googVPNE),color="green") + xlab("Bliss Point") + ylab("VPN Usage %") + labs(color="Google User") + ggtitle("Modified Logit") -> plt1

window <- .5
seq(min(vpnEngineBliss$blissPoint),max(vpnEngineBliss$blissPoint),by=window) -> rng

blissX <- c()
mnVPNGoog <- c()
mnVPNDuck <- c()

for (k in 1:(length(rng)-1)){
  blissX <- c(blissX,(rng[k+1]+rng[k])/2)
  
  mnVPNGoog <- c(mnVPNGoog,mean(vpnEngineBliss[vpnEngineBliss$currEngine=="google" & vpnEngineBliss$blissPoint >= rng[k] & vpnEngineBliss$blissPoint < rng[k+1],"optPct"]))
  mnVPNDuck <- c(mnVPNDuck,mean(vpnEngineBliss[vpnEngineBliss$currEngine=="duckDuckGo" & vpnEngineBliss$blissPoint >= rng[k] & vpnEngineBliss$blissPoint < rng[k+1],"optPct"]))
}

ggplot() + geom_point(aes(x=vpnEngineBliss$blissPoint,y=vpnEngineBliss$optPct,color=vpnEngineBliss$googBool),alpha=.1) + geom_line(aes(x=blissX,y=mnVPNDuck),color="red") + geom_line(aes(x=blissX,y=mnVPNGoog),color="green")  + xlab("Bliss Point") + ylab("% VPN Usage") + ggtitle("Moving Average") + labs(color = "Google %")-> plt2

# THIS PLOT ESTABLISHES VPN BEHAVIOR IS DIFFERENT BY SEARCH ENGINE CHOICE AND BLISS POINT

ggarrange(plt1,plt2,nrow=1,ncol=2,common.legend = TRUE, legend = "bottom")




# now, consider usage before and after vpns 

# consider cases where Duck Duck Go entered after there was a VPN available

outDat[outDat$vpnTick!=-10 & outDat$duckTick!=-10 & outDat$vpnTick < outDat$duckTick & outDat$tick >= (outDat$duckTick+25),] -> duckVPN


quantile(duckVPN$duckTick-duckVPN$vpnTick,c(0,.05,.25,.5,.75,.95,1))

duckVPN$tickDelta <- duckVPN$duckTick-duckVPN$vpnTick

# Let's examine whether the vpn being introduced before Duck Duck Go cuts into market share 
# Make sure though that we are comparing apples to apples; that is, make sure we are only comparing situations where Duck Duck Go was available the whole time

duckVPN %>% transform(googBool=currEngine=="google") %>% group_by(key,agent,tickDelta) %>% summarise(googPct=mean(googBool)) -> afterVPNSmry

merge(afterVPNSmry,agentDat,by=c("key","agent")) -> afterVPNSmryAgt



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

ggplot() + geom_point(aes(x=afterVPNSmryAgt$blissPoint,y=afterVPNSmryAgt$googPct,color=as.factor(afterVPNSmryAgt$tickDelta)),alpha=.1) + xlab("Bliss Point") + ylab("Google Usage %") + labs(color="VPN Lag")  + geom_line(aes(x=blissRange,y=eY1),color="black") +
  geom_line(aes(x=blissRange,y=eY2),color="red") + geom_line(aes(x=blissRange,y=eY3),color="green") + ggtitle("Modified Logit") -> plt1

window <- .5
seq(min(afterVPNSmryAgt$blissPoint),max(afterVPNSmryAgt$blissPoint),by=window) -> rng

blissX <- c()
mnVPN5 <- c()
mnVPN50 <- c()
mnVPN95 <- c()

for (k in 1:(length(rng)-1)){
  blissX <- c(blissX,(rng[k+1]+rng[k])/2)
  
  mnVPN5 <- c(mnVPN5,mean(afterVPNSmryAgt[afterVPNSmryAgt$tickDelta==5 & afterVPNSmryAgt$blissPoint >= rng[k] & afterVPNSmryAgt$blissPoint < rng[k+1],"googPct"]))
  mnVPN50 <- c(mnVPN50,mean(afterVPNSmryAgt[afterVPNSmryAgt$tickDelta==50 & afterVPNSmryAgt$blissPoint >= rng[k] & afterVPNSmryAgt$blissPoint < rng[k+1],"googPct"]))
  mnVPN95 <- c(mnVPN95,mean(afterVPNSmryAgt[afterVPNSmryAgt$tickDelta==95 & afterVPNSmryAgt$blissPoint >= rng[k] & afterVPNSmryAgt$blissPoint < rng[k+1],"googPct"]))
}

ggplot() + geom_point(aes(x=afterVPNSmryAgt$blissPoint,y=afterVPNSmryAgt$googPct,color=as.factor(afterVPNSmryAgt$tickDelta)),alpha=.1) + xlab("Bliss Point") + ylab("Google Usage %") + labs(color="VPN Lag") + geom_line(aes(x=blissX,y=mnVPN5),color="black") +
  geom_line(aes(x=blissX,y=mnVPN50),color="red") + geom_line(aes(x=blissX,y=mnVPN95),color="green") + ggtitle("Moving Average")-> plt2

ggarrange(plt1,plt2,nrow=1,ncol=2,common.legend = TRUE, legend = "bottom")


### HERE I STOPPED ##

# now, let's do similar analysis for when Duck Duck Go is introduced before the VPN. 
outDat[outDat$vpnTick!=-10 & outDat$duckTick!=-10 & outDat$vpnTick > outDat$duckTick & outDat$tick >= (outDat$vpnTick+25),] -> afterDuck2
afterDuck2$tickDelta <- afterDuck2$vpnTick-afterDuck2$duckTick
unique(afterDuck2$tickDelta)

afterDuck2$dummy40 <- afterDuck2$tickDelta==40
afterDuck2$dummy100 <- afterDuck2$tickDelta==100

afterDuck2 %>% transform(optOut=(optOut=="true"),googBool=(currEngine=="google")) %>% group_by(key,agent,tickDelta) %>% summarise(vpnPct=mean(optOut),dummy40=max(dummy40),dummy100=max(dummy100),googPct=mean(googBool)) -> afterDuck2VPN

merge(afterDuck2VPN,agentDat,by=c("key","agent")) -> afterDuck2VPNAgt

blissRange <- seq(min(afterDuck2VPNAgt$blissPoint),max(afterDuck2VPNAgt$blissPoint),by=0.05)

log((afterDuck2VPNAgt$googPct+.01)/(1-afterDuck2VPNAgt$googPct+.01)) -> logitGoog
# now fit linear model 

lm(logitGoog~afterDuck2VPNAgt$blissPoint+afterDuck2VPNAgt$dummy40+afterDuck2VPNAgt$dummy100) -> vpnTickMod

cbind(rep(1,length(blissRange)),blissRange,rep(0,length(blissRange)),rep(0,length(blissRange))) -> X1
cbind(rep(1,length(blissRange)),blissRange,rep(1,length(blissRange)),rep(0,length(blissRange))) -> X2
cbind(rep(1,length(blissRange)),blissRange,rep(0,length(blissRange)),rep(1,length(blissRange))) -> X3

Y1 <- X1 %*% matrix(vpnTickMod$coefficients,ncol=1)
Y2 <- X2 %*% matrix(vpnTickMod$coefficients,ncol=1)
Y3 <- X3 %*% matrix(vpnTickMod$coefficients,ncol=1)

eY1 <- (.01+exp(Y1))/(1+exp(Y1))
eY2 <- (.01+exp(Y2))/(1+exp(Y2))
eY3 <- (.01+exp(Y3))/(1+exp(Y3))

ggplot() + geom_point(aes(x=afterDuck2VPNAgt$blissPoint,y=afterDuck2VPNAgt$googPct,color=as.factor(afterDuck2VPNAgt$tickDelta)),alpha=.1) + xlab("Bliss Point") + ylab("Google Usage %") + labs(color="Duck Duck Go Lag")  + geom_line(aes(x=blissRange,y=eY1),color="black") +
  geom_line(aes(x=blissRange,y=eY2),color="red") + geom_line(aes(x=blissRange,y=eY3),color="green") + ggtitle("Modified Logit") -> plt1

window <- .5
seq(min(afterVPNSmryAgt$blissPoint),max(afterVPNSmryAgt$blissPoint),by=window) -> rng

blissX <- c()
lag10 <- c()
lag40 <- c()
lag100 <- c()

for (k in 1:(length(rng)-1)){
  blissX <- c(blissX,(rng[k+1]+rng[k])/2)
  
  lag10 <- c(lag10,mean(afterDuck2VPNAgt[afterDuck2VPNAgt$tickDelta==10 & afterDuck2VPNAgt$blissPoint >= rng[k] & afterDuck2VPNAgt$blissPoint < rng[k+1],"vpnPct"]))
  lag40 <- c(lag40,mean(afterDuck2VPNAgt[afterDuck2VPNAgt$tickDelta==40 & afterDuck2VPNAgt$blissPoint >= rng[k] & afterDuck2VPNAgt$blissPoint < rng[k+1],"vpnPct"]))
  lag100 <- c(lag100,mean(afterDuck2VPNAgt[afterDuck2VPNAgt$tickDelta==100 & afterDuck2VPNAgt$blissPoint >= rng[k] & afterDuck2VPNAgt$blissPoint < rng[k+1],"vpnPct"]))
}

ggplot() + geom_point(aes(x=afterDuck2VPNAgt$blissPoint,y=afterDuck2VPNAgt$googPct,color=as.factor(afterDuck2VPNAgt$tickDelta)),alpha=.1) + xlab("Bliss Point") + ylab("Google Usage %") + labs(color="Duck Duck Go Lag") + geom_line(aes(x=blissX,y=lag10),color="black") +
  geom_line(aes(x=blissX,y=lag40),color="red") + geom_line(aes(x=blissX,y=lag100),color="green") + ggtitle("Moving Average") -> plt2


ggarrange(plt1,plt2,nrow=1,ncol=2,common.legend = TRUE, legend = "bottom")


##### SHARING RULES #####

setwd("~/ResearchCode/antiTrustDataShare")
read.csv("ctrl.csv",sep=",") -> control
list.files("~/ResearchCode/antiTrustDataShare") -> allFi

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

# first, let's find out how a sharing rule affects Google's usage
outDat[outDat$tick >= outDat$shareTick + 25,] -> afterShare
afterShare$shareAvail <- (afterShare$shareTick !=-10)

afterShare %>% transform(googBool=(currEngine=="google")) %>% group_by(key,shareAvail,agent) %>% summarise(googPct=mean(googBool)) -> shareSmry

merge(shareSmry,agentDat,by=c("key","agent")) -> agtShareSmry

# we need a modified logit function

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

unique(agtShareSmry$shareAvail)
lm(mLogit(agtShareSmry$googPct)~agtShareSmry$blissPoint +  (agtShareSmry$shareAvail)) -> lMod

blissRangeFunc(agtShareSmry$blissPoint) -> blissRng
cbind(rep(1,length(blissRng)),blissRng,rep(0,length(blissRng))) -> deltaNoShare
cbind(rep(1,length(blissRng)),blissRng,rep(1,length(blissRng))) -> deltaDeltaShare


deltaNoShare %*% lMod$coefficients -> yHatNo
deltaDeltaShare %*% lMod$coefficients -> yHatYes


yHatENo <- (.01+exp(yHatNo))/(1+exp(yHatNo))
yHatEYes <- (.01+exp(yHatYes))/(1+exp(yHatYes))



ggplot() + geom_point(aes(x=agtShareSmry$blissPoint,y=agtShareSmry$googPct),alpha=.1) + geom_line(aes(x=blissRng,y=yHatENo),color="blue") + geom_line(aes(x=blissRng,y=yHatEYes),color="red")  + ylab("% Google Usage") + xlab("Bliss Point") + labs(color="Duck Duck Go Lag") + ggtitle("Modified Logit") -> plt1

# now get the smoothing

window <- .5
seq(min(agtShareSmry$blissPoint),max(agtShareSmry$blissPoint),by=window) -> rng

blissX <- c()
noSharing <- c()
Sharing <- c()


for (k in 1:(length(rng)-1)){
  blissX <- c(blissX,(rng[k+1]+rng[k])/2)
  
  noSharing <- c(noSharing,mean(agtShareSmry[!agtShareSmry$shareAvail & agtShareSmry$blissPoint >= rng[k] & agtShareSmry$blissPoint < rng[k+1],"googPct"]))
  Sharing <- c(Sharing,mean(agtShareSmry[agtShareSmry$shareAvail & agtShareSmry$blissPoint >= rng[k] & agtShareSmry$blissPoint < rng[k+1],"googPct"]))
}

ggplot() + geom_point(aes(x=agtShareSmry$blissPoint,y=agtShareSmry$googPct),alpha=.1) + xlab("Bliss Point") + ylab("Google Usage %") + labs(color="Duck Duck Go Lag") + geom_line(aes(x=blissX,y=noSharing),color="blue")+ geom_line(aes(x=blissX,y=Sharing),color="red")   + ggtitle("Moving Average") -> plt2

ggarrange(plt1,plt2,nrow=1,ncol=2,common.legend = TRUE, legend = "bottom")





# now, let's look at Google usage when the sharing rule is introduced first and we wait 25 ticks

outDat[(outDat$duckTick >= outDat$shareTick) & outDat$shareTick!=-10 & outDat$tick >= outDat$duckTick + 25,] -> afterShare
afterShare$duckDelta <- afterShare$duckTick - afterShare$shareTick

afterShare %>% transform(googBool=(currEngine=="google")) %>% group_by(key,duckDelta,agent) %>% summarise(googPct=mean(googBool)) -> shareSmry

merge(shareSmry,agentDat,by=c("key","agent")) -> agtShareSmry

# we need a modified logit function

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

unique(agtShareSmry$duckDelta)
lm(mLogit(agtShareSmry$googPct)~agtShareSmry$blissPoint +  (agtShareSmry$duckDelta==50) + (agtShareSmry$duckDelta==95)) -> lMod

blissRangeFunc(agtShareSmry$blissPoint) -> blissRng
cbind(rep(1,length(blissRng)),blissRng,rep(0,length(blissRng)),rep(0,length(blissRng))) -> delta5
cbind(rep(1,length(blissRng)),blissRng,rep(1,length(blissRng)),rep(0,length(blissRng))) -> delta50
cbind(rep(1,length(blissRng)),blissRng,rep(0,length(blissRng)),rep(1,length(blissRng))) -> delta95


delta5 %*% lMod$coefficients -> yHat5
delta50 %*% lMod$coefficients -> yHat50
delta95 %*% lMod$coefficients -> yHat95



yHatE5 <- (.01+exp(yHat5))/(1+exp(yHat5))
yHatE50 <- (.01+exp(yHat50))/(1+exp(yHat50))
yHatE95 <- (.01+exp(yHat95))/(1+exp(yHat95))


ggplot() + geom_point(aes(x=agtShareSmry$blissPoint,y=agtShareSmry$googPct,color=as.factor(agtShareSmry$duckDelta)),alpha=.1) + geom_line(aes(x=blissRng,y=yHatE5),color="black") + geom_line(aes(x=blissRng,y=yHatE50),color="red") + geom_line(aes(x=blissRng,y=yHatE95),color="blue") + ylab("% Google Usage") + xlab("Bliss Point") + labs(color="Duck Duck Go Lag") + ggtitle("Modified Logit") -> plt1

# now get the smoothing

window <- .5
seq(min(agtShareSmry$blissPoint),max(agtShareSmry$blissPoint),by=window) -> rng

blissX <- c()
lag5 <- c()
lag50 <- c()
lag95 <- c()

for (k in 1:(length(rng)-1)){
  blissX <- c(blissX,(rng[k+1]+rng[k])/2)
  
  lag5 <- c(lag5,mean(agtShareSmry[agtShareSmry$duckDelta==5 & agtShareSmry$blissPoint >= rng[k] & agtShareSmry$blissPoint < rng[k+1],"googPct"]))
  lag50 <- c(lag50,mean(agtShareSmry[agtShareSmry$duckDelta==50 & agtShareSmry$blissPoint >= rng[k] & agtShareSmry$blissPoint < rng[k+1],"googPct"]))
  lag95 <- c(lag95,mean(agtShareSmry[agtShareSmry$duckDelta==95 & agtShareSmry$blissPoint >= rng[k] & agtShareSmry$blissPoint < rng[k+1],"googPct"]))

}

ggplot() + geom_point(aes(x=agtShareSmry$blissPoint,y=agtShareSmry$googPct,color=as.factor(agtShareSmry$duckDelta)),alpha=.1) + xlab("Bliss Point") + ylab("Google Usage %") + labs(color="Duck Duck Go Lag") + geom_line(aes(x=blissX,y=lag5),color="black") + geom_line(aes(x=blissX,y=lag95),color="red") +
  geom_line(aes(x=blissX,y=lag95),color="blue")  + ggtitle("Moving Average") -> plt2

ggarrange(plt1,plt2,nrow=1,ncol=2,common.legend = TRUE, legend = "bottom")

# now, consider the case where Duck Duck Go is available before hand, again, we wait 25 ticks 


outDat[(outDat$duckTick < outDat$shareTick) & outDat$shareTick!=-10 & outDat$tick >= outDat$shareTick + 25,] -> afterShare
afterShare$duckDelta <- -(afterShare$duckTick - afterShare$shareTick)

afterShare %>% transform(googBool=(currEngine=="google")) %>% group_by(key,duckDelta,agent) %>% summarise(googPct=mean(googBool)) -> shareSmry

merge(shareSmry,agentDat,by=c("key","agent")) -> agtShareSmry

# we need a modified logit function

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

unique(agtShareSmry$duckDelta)
lm(mLogit(agtShareSmry$googPct)~agtShareSmry$blissPoint +  (agtShareSmry$duckDelta==40) + (agtShareSmry$duckDelta==100)) -> lMod

blissRangeFunc(agtShareSmry$blissPoint) -> blissRng
cbind(rep(1,length(blissRng)),blissRng,rep(0,length(blissRng)),rep(0,length(blissRng))) -> delta5
cbind(rep(1,length(blissRng)),blissRng,rep(1,length(blissRng)),rep(0,length(blissRng))) -> delta50
cbind(rep(1,length(blissRng)),blissRng,rep(0,length(blissRng)),rep(1,length(blissRng))) -> delta95


delta5 %*% lMod$coefficients -> yHat5
delta50 %*% lMod$coefficients -> yHat50
delta95 %*% lMod$coefficients -> yHat95



yHatE5 <- (.01+exp(yHat5))/(1+exp(yHat5))
yHatE50 <- (.01+exp(yHat50))/(1+exp(yHat50))
yHatE95 <- (.01+exp(yHat95))/(1+exp(yHat95))


ggplot() + geom_point(aes(x=agtShareSmry$blissPoint,y=agtShareSmry$googPct,color=as.factor(agtShareSmry$duckDelta)),alpha=.1) + geom_line(aes(x=blissRng,y=yHatE5),color="black") + geom_line(aes(x=blissRng,y=yHatE50),color="red") + geom_line(aes(x=blissRng,y=yHatE95),color="blue") + ylab("% Google Usage") + xlab("Bliss Point") + labs(color="Sharing Lag") + ggtitle("Modified Logit") -> plt1

# now get the smoothing

window <- .5
seq(min(agtShareSmry$blissPoint),max(agtShareSmry$blissPoint),by=window) -> rng

blissX <- c()
lag5 <- c()
lag50 <- c()
lag95 <- c()

for (k in 1:(length(rng)-1)){
  blissX <- c(blissX,(rng[k+1]+rng[k])/2)
  
  lag5 <- c(lag5,mean(agtShareSmry[agtShareSmry$duckDelta==10 & agtShareSmry$blissPoint >= rng[k] & agtShareSmry$blissPoint < rng[k+1],"googPct"]))
  lag50 <- c(lag50,mean(agtShareSmry[agtShareSmry$duckDelta==40 & agtShareSmry$blissPoint >= rng[k] & agtShareSmry$blissPoint < rng[k+1],"googPct"]))
  lag95 <- c(lag95,mean(agtShareSmry[agtShareSmry$duckDelta==100 & agtShareSmry$blissPoint >= rng[k] & agtShareSmry$blissPoint < rng[k+1],"googPct"]))
  
}

ggplot() + geom_point(aes(x=agtShareSmry$blissPoint,y=agtShareSmry$googPct,color=as.factor(agtShareSmry$duckDelta)),alpha=.1) + xlab("Bliss Point") + ylab("Google Usage %") + labs(color="Sharing Lag") + geom_line(aes(x=blissX,y=lag5),color="black") + geom_line(aes(x=blissX,y=lag95),color="red") +
  geom_line(aes(x=blissX,y=lag95),color="blue")  + ggtitle("Moving Average") -> plt2

ggarrange(plt1,plt2,nrow=1,ncol=2,common.legend = TRUE, legend = "bottom")


###### DATA DELETION RULES ########


setwd("~/ResearchCode/antiTrustDataDel")
read.csv("ctrl.csv",sep=",") -> control
list.files("~/ResearchCode/antiTrustDataDel") -> allFi

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

# step 1, let's consider in general how the deletion rule affects Google 

outDat[outDat$tick > (outDat$delTick + 25),] -> delDat

delDat$delBool <- (delDat$delTick!=-10)

delDat %>% transform(googBool=(currEngine=="google")) %>% group_by(key,delBool,agent) %>% summarise(googPct=mean(googBool)) -> delSmry

merge(delSmry,agentDat,by=c("key","agent")) -> agtDelSmry

# now, prepare models and plots


lm(mLogit(agtDelSmry$googPct)~agtDelSmry$blissPoint +  (agtDelSmry$delBool)) -> lMod

blissRangeFunc(agtDelSmry$blissPoint) -> blissRng
cbind(rep(1,length(blissRng)),blissRng,rep(0,length(blissRng))) -> noDel
cbind(rep(1,length(blissRng)),blissRng,rep(1,length(blissRng))) -> yesDel


noDel %*% lMod$coefficients -> yHatNoDel
yesDel %*% lMod$coefficients -> yHatDel


yHatNoDel <- (.01+exp(yHatNoDel))/(1+exp(yHatNoDel))
yHatDel <- (.01+exp(yHatDel))/(1+exp(yHatDel))



ggplot() + geom_point(aes(x=agtDelSmry$blissPoint,y=agtDelSmry$googPct),alpha=.1) + geom_line(aes(x=blissRng,y=yHatNoDel),color="blue") + geom_line(aes(x=blissRng,y=yHatDel),color="red")  + ylab("% Google Usage") + xlab("Bliss Point")  + ggtitle("Modified Logit") -> plt1

# now get the smoothing

window <- .5
seq(min(agtDelSmry$blissPoint),max(agtDelSmry$blissPoint),by=window) -> rng

blissX <- c()
noDelSeries <- c()
delSeries <- c()


for (k in 1:(length(rng)-1)){
  blissX <- c(blissX,(rng[k+1]+rng[k])/2)
  noDelSeries <- c(noDelSeries,mean(agtDelSmry[!agtDelSmry$delBool & agtDelSmry$blissPoint >= rng[k] & agtDelSmry$blissPoint < rng[k+1],"googPct"]))
  delSeries <- c(delSeries,mean(agtDelSmry[agtDelSmry$delBool & agtDelSmry$blissPoint >= rng[k] & agtDelSmry$blissPoint < rng[k+1],"googPct"]))
}

ggplot() + geom_point(aes(x=agtDelSmry$blissPoint,y=agtDelSmry$googPct),alpha=.1) + xlab("Bliss Point") + ylab("Google Usage %")  + geom_line(aes(x=blissX,y=noDelSeries),color="blue") + geom_line(aes(x=blissX,y=delSeries),color="red") + ggtitle("Moving Average") -> plt2

ggarrange(plt1,plt2,nrow=1,ncol=2,common.legend = TRUE, legend = "bottom")


