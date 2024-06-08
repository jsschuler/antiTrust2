################################################################################
#             Final Analysis Code                                              #
#             Anti-Trust Model                                                 #
#             June 2024 Update                                                 #
#             John S. Schuler                                                  #
#             This version uses privacy index rather than bliss point          #
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

# now, from the agent data, calculate their privacy index
agentDat$privDex <- (agentDat$blissPoint/agentDat$unifExp)


# step one, consider only cases where there is no vpn
# and only consider data where Duck Duck Go has been available
outDat[outDat$tick >= (outDat$duckTick+50) & outDat$vpnTick==-10 & outDat$delTick==-10 & outDat$shareTick==-10,] -> afterDuck

# now, get each agent's Google usage

afterDuck %>% transform(googBool=(currEngine=="google")) %>% group_by(key,agent) %>% summarise(googPct=mean(googBool)) -> agtUsage

merge(agtUsage,agentDat,by=c("agent","key")) -> agtPropUsage




log((agtPropUsage$googPct+.01)/(1-agtPropUsage$googPct+.01)) -> logitGoog
lm(logitGoog~agtPropUsage$privDex) -> centerMod
summary(centerMod)

# now transform y back


xRng <- seq(min(agtPropUsage$privDex),max(agtPropUsage$privDex),by=0.05)
data.frame(xRng) -> xDat
names(xDat) <- c("Privacy Index")
cbind(rep(1,length(xRng)),xRng) %*% matrix(centerMod$coefficients,ncol=1)-> yHat
yHatE <- (.01+exp(yHat))/(1+exp(yHat))

# THESE PLOTS ESTABLISH THE BASIC RESULT, HIGHER BLISS POINT AGENTS SHY AWAY FROM GOOGLE
# NOTE THESE AGENTS DO NOT HAVE VPN ACCESS




ggplot() + geom_point(aes(x=agtPropUsage$privDex,y=agtPropUsage$googPct),alpha=.1,color=basePoint) + geom_line(aes(x=xRng,y=yHatE),color=googColor,size=1) + xlab("Privacy Index") + ylab("Google Usage") + ggtitle("Modified Logit") + theme(
  panel.background = element_rect(fill = bgFill),
  plot.title = element_text(color =basePoint,hjust = 0.5),
  plot.background = element_rect(fill = bgFill),
  panel.grid = element_blank(),
  axis.text = element_text(color =basePoint),
  axis.title = element_text(color =basePoint),
  legend.background = element_rect(fill = bgFill),
  legend.text = element_text(color =basePoint)) -> plt1


window <- .05
seq(min(agtPropUsage$privDex),max(agtPropUsage$privDex),by=window) -> rng

blissX <- c()
mnGoog <- c()
for (k in 1:(length(rng)-1)){
  blissX <- c(blissX,(rng[k+1]+rng[k])/2)
  mnGoog <- c(mnGoog,mean(agtPropUsage[agtPropUsage$privDex >= rng[k] & agtPropUsage$privDex < rng[k+1] ,"googPct"]))
}

ggplot() + geom_point(aes(x=agtPropUsage$privDex,y=agtPropUsage$googPct),alpha=.1,color=basePoint) + geom_line(aes(x=blissX,y=mnGoog),color=googColor,size=1) + xlab("Privacy Index") + ylab("% Google Usage") + ggtitle("Moving Average") + theme(
  panel.background = element_rect(fill = bgFill),
  plot.title = element_text(color =basePoint,hjust = 0.5),
  plot.background = element_rect(fill = bgFill),
  panel.grid = element_blank(),
  axis.text = element_text(color =basePoint),
  axis.title = element_text(color =basePoint),
  legend.background = element_rect(fill = bgFill),
  legend.text = element_text(color =basePoint)) -> plt2
ggarrange(plt1,plt2,nrow=1,ncol=2)
ggsave(filename = "~/ResearchCode/antiTrustImages/basic2.png",width=7,height=7,bg=bgFill)


#### VPN ACCESS 


# now, consider cases where vpns are available 

outDat[outDat$tick >= (outDat$duckTick+50) & outDat$vpnTick!=-10 & outDat$delTick==-10 & outDat$shareTick==-10,] -> afterDuckVPN

afterDuckVPN %>% transform(googBool=(currEngine=="google")) %>% group_by(key,agent,optOut) %>% summarise(googPct=mean(googBool)) -> agtUsageVPN
merge(agtUsageVPN,agentDat,by=c("agent","key")) -> agtPropUsageVPN

log((agtPropUsageVPN$googPct+.01)/(1-agtPropUsageVPN$googPct+.01)) -> logitGoogVPN
lm(logitGoogVPN~agtPropUsageVPN$privDex + agtUsageVPN$optOut) -> centerModVPN

blissRange <- seq(min(agtPropUsageVPN$privDex),max(agtPropUsageVPN$privDex),by=0.05)
noVPNFrame <- data.frame(blissRange,rep(0,length(blissRange)))
VPNFrame <- data.frame(blissRange,rep(1,length(blissRange)))

predict(centerModVPN,newdata = noVPNFrame) -> yHatnoVPN
predict(centerModVPN,newdata = VPNFrame) -> yHatVPN

cbind(rep(1,length(blissRange)),blissRange,rep(0,length(blissRange))) %*% matrix(centerModVPN$coefficients,ncol=1)-> yHatnoVPN
cbind(rep(1,length(blissRange)),blissRange,rep(1,length(blissRange))) %*% matrix(centerModVPN$coefficients,ncol=1)-> yHatVPN
yHatEnoVPN <- (.01+exp(yHatnoVPN))/(1+exp(yHatnoVPN))
yHatEVPN <- (.01+exp(yHatVPN))/(1+exp(yHatVPN))

data.frame(c(blissRange,blissRange),c(yHatEnoVPN,yHatEVPN),c(rep("False",length(blissRange)),rep("True",length(blissRange)))) -> ggFrame1
names(ggFrame1) <- c("blissRng","googPct","vpn")
#ggFrame1$vpn <- as.factor(ggFrame1$vpn)
substr(agtPropUsageVPN$optOut,1,1) <- toupper(substr(agtPropUsageVPN$optOut,1,1))


ggplot() + geom_point(aes(x=agtPropUsageVPN$privDex,y=agtPropUsageVPN$googPct,color=agtPropUsageVPN$optOut),alpha=.1)  + geom_line(aes(x=ggFrame1$blissRng,y=ggFrame1$googPct,color=ggFrame1$vpn),size=1) + 
  scale_color_manual(values = c("False" = vpnTrue, "True" = vpnFalse)) + xlab("Privacy Index") + ylab("Google Usage") + labs(color = "VPN") + ggtitle("Modified Logit") + theme(
  panel.background = element_rect(fill = bgFill),
  plot.title = element_text(color =basePoint,hjust = 0.5),
  plot.background = element_rect(fill = bgFill),
  panel.grid = element_blank(),
  axis.text = element_text(color =basePoint),
  axis.title = element_text(color =basePoint),
  legend.background = element_rect(fill = bgFill,color=bgFill),
  legend.key = element_rect(fill = bgFill, colour = bgFill),
  legend.text = element_text(color =basePoint),
  legend.title = element_text(color =basePoint)) -> plt1


window <- .05
seq(0,1,by=window) -> rng

blissX <- c()
mnNoVPN <- c()
mnVPN <- c()

for (k in 1:(length(rng)-1)){
  blissX <- c(blissX,(rng[k+1]+rng[k])/2)
  mnNoVPN <- c(mnNoVPN,mean(agtPropUsageVPN[agtPropUsageVPN$optOut=="False" & agtPropUsageVPN$privDex >= rng[k] & agtPropUsageVPN$privDex < rng[k+1] ,"googPct"]))
  mnVPN <- c(mnVPN,mean(agtPropUsageVPN[agtPropUsageVPN$optOut=="True" & agtPropUsageVPN$privDex >= rng[k] & agtPropUsageVPN$privDex < rng[k+1],"googPct"]))
  
}

data.frame(c(blissX,blissX),c(mnNoVPN,mnVPN),c(rep("False",length(blissX)),rep("True",length(blissX)))) -> ggFrame2
names(ggFrame2) <- c("blissRng","googPct","vpn")
#ggFrame2$vpn <- as.factor(ggFrame2$vpn)

ggplot() + geom_point(aes(x=agtPropUsageVPN$privDex,y=agtPropUsageVPN$googPct,color=agtPropUsageVPN$optOut),alpha=.1) + 
  geom_line(aes(x=ggFrame2$blissRng,y=ggFrame2$googPct,color=ggFrame2$vpn),size=1) + scale_color_manual(values = c("False" = vpnTrue, "True" = vpnFalse)) + 
  xlab("Privacy Index") + ylab("% Google Usage") + ggtitle("Moving Average") + labs(color = "VPN") + theme(
    panel.background = element_rect(fill = bgFill),
    plot.title = element_text(color =basePoint,hjust = 0.5),
    plot.background = element_rect(fill = bgFill),
    panel.grid = element_blank(),
    axis.text = element_text(color =basePoint),
    axis.title = element_text(color =basePoint),
    legend.background = element_rect(fill = bgFill,color=bgFill),
    legend.key = element_rect(fill = bgFill, colour = bgFill),
    legend.text = element_text(color =basePoint),
    legend.title = element_text(color =basePoint)) -> plt2
ggarrange(plt1,plt2,nrow=1,ncol=2,common.legend = TRUE, legend = "bottom") 

ggsave(filename = "~/ResearchCode/antiTrustImages/vpn2.png",width=7,height=7,bg=bgFill)


# NOW STUDY AGENT'S TENDENCY TO USE VPNS

# plot VPN usage against bliss point 

outDat[outDat$tick >= (outDat$vpnTick+50) & outDat$vpnTick!=-10 & outDat$delTick==-10 & outDat$shareTick==-10,] -> afterVPN

afterVPN %>% transform(optOut=optOut=="true") %>%  group_by(key,agent) %>% summarise(optPct=mean(optOut),googPct=mean(currEngine=="google")) -> vpnAgent

# now get agent bliss point 

merge(vpnAgent,agentDat,by=c("key","agent")) -> vpnBliss

log((vpnBliss$optPct+.01)/(1-vpnBliss$optPct+.01)) -> logitVPN
lm(logitVPN~vpnBliss$privDex) -> centerVPNMod

blissRange <- seq(0,1,by=0.05)
cbind(rep(1,length(blissRange)),blissRange) %*% matrix(centerVPNMod$coefficients,ncol=1)-> vpnY

vpnYE <- (.01+exp(vpnY))/(1+exp(vpnY))

ggplot() + geom_point(aes(x=vpnBliss$privDex,y=vpnBliss$optPct,color=100*vpnBliss$googPct),alpha=.1) + scale_color_gradient(low = hiOrange, high = googColor)  + geom_line(aes(x=blissRange,y=vpnYE),color=vpnColor,size=1) + 
  xlab("Privacy Index") + ylab("% VPN Usage") + labs(color="Google %") + ggtitle("Modified Logit") + theme(
    panel.background = element_rect(fill = bgFill),
    plot.title = element_text(color =basePoint,hjust = 0.5),
    plot.background = element_rect(fill = bgFill),
    panel.grid = element_blank(),
    axis.text = element_text(color =basePoint),
    axis.title = element_text(color =basePoint),
    legend.background = element_rect(fill = bgFill,color=bgFill),
    legend.key = element_rect(fill = bgFill, colour = bgFill),
    legend.text = element_text(color =basePoint),
    legend.title = element_text(color =basePoint)) -> plt1

window <- .05
seq(0,1,by=window) -> rng

blissX <- c()
mnNoVPN <- c()
mnVPN <- c()

for (k in 1:(length(rng)-1)){
  blissX <- c(blissX,(rng[k+1]+rng[k])/2)
  
  mnVPN <- c(mnVPN,mean(vpnBliss[vpnBliss$privDex >= rng[k] & vpnBliss$privDex < rng[k+1],"optPct"]))
  
}

ggplot() + geom_point(aes(x=vpnBliss$privDex,y=vpnBliss$optPct,color=100*vpnBliss$googPct),alpha=.1) + scale_color_gradient(low = hiOrange, high = googColor) + geom_line(aes(x=blissX,y=mnVPN),color=vpnColor,size=1) + 
  xlab("Privacy Index") + ylab("% VPN Usage") + ggtitle("Moving Average") + labs(color = "Google %") + theme(
    panel.background = element_rect(fill = bgFill),
    plot.title = element_text(color =basePoint,hjust = 0.5),
    plot.background = element_rect(fill = bgFill),
    panel.grid = element_blank(),
    axis.text = element_text(color =basePoint),
    axis.title = element_text(color =basePoint),
    legend.background = element_rect(fill = bgFill,color=bgFill),
    legend.key = element_rect(fill = bgFill, colour = bgFill),
    legend.text = element_text(color =basePoint),
    legend.title = element_text(color =basePoint)) -> plt2
ggarrange(plt1,plt2,nrow=1,ncol=2,common.legend = TRUE, legend = "bottom")
ggsave(filename = "~/ResearchCode/antiTrustImages/vpnUsage2.png",width=7,height=7,bg=bgFill)


#### DELETION LAW #####


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
# now, from the agent data, calculate their privacy index
agentDat$privDex <- (agentDat$blissPoint/agentDat$unifExp)
# step 1, let's consider in general how the deletion rule affects Google 

outDat[outDat$tick > (outDat$delTick + 25),] -> delDat

delDat$delBool <- (delDat$delTick!=-10)

delDat %>% transform(googBool=(currEngine=="google")) %>% group_by(key,delBool,agent) %>% summarise(googPct=mean(googBool)) -> delSmry

merge(delSmry,agentDat,by=c("key","agent")) -> agtDelSmry

# now, prepare models and plots


lm(mLogit(agtDelSmry$googPct)~agtDelSmry$privDex +  (agtDelSmry$delBool)) -> lMod

blissRangeFunc(agtDelSmry$privDex) -> blissRng
cbind(rep(1,length(blissRng)),blissRng,rep(0,length(blissRng))) -> noDel
cbind(rep(1,length(blissRng)),blissRng,rep(1,length(blissRng))) -> yesDel


noDel %*% lMod$coefficients -> yHatNoDel
yesDel %*% lMod$coefficients -> yHatDel


yHatNoDel <- (.01+exp(yHatNoDel))/(1+exp(yHatNoDel))
yHatDel <- (.01+exp(yHatDel))/(1+exp(yHatDel))


data.frame(c(blissRng,blissRng),c(yHatNoDel,yHatDel),c(rep("False",length(blissRng)),rep("True",length(blissRng)))) -> ggFrame1
names(ggFrame1) <-  c("blissRng","googPct","deletion")

ggFrame1$deletion <- as.factor(ggFrame1$deletion)



ggplot() + geom_point(aes(x=agtDelSmry$privDex,y=agtDelSmry$googPct),alpha=.1,color=basePoint) + geom_line(aes(x=ggFrame1$blissRng,y=ggFrame1$googPct,color=ggFrame1$deletion),size=1) + 
  scale_color_manual(values = c("False" = vpnTrue, "True" = vpnFalse))  + ylab("% Google Usage") + xlab("Privacy Index") + labs(color="Deletion Law") + ggtitle("Modified Logit") + theme(
    panel.background = element_rect(fill = bgFill),
    plot.title = element_text(color =basePoint,hjust = 0.5),
    plot.background = element_rect(fill = bgFill),
    panel.grid = element_blank(),
    axis.text = element_text(color =basePoint),
    axis.title = element_text(color =basePoint),
    legend.background = element_rect(fill = bgFill,color=bgFill),
    legend.key = element_rect(fill = bgFill, colour = bgFill),
    legend.text = element_text(color =basePoint),
    legend.title = element_text(color =basePoint)) -> plt1

# now get the smoothing

window <- .05
seq(0,1,by=window) -> rng

blissX <- c()
noDelSeries <- c()
delSeries <- c()


for (k in 1:(length(rng)-1)){
  blissX <- c(blissX,(rng[k+1]+rng[k])/2)
  noDelSeries <- c(noDelSeries,mean(agtDelSmry[!agtDelSmry$delBool & agtDelSmry$privDex >= rng[k] & agtDelSmry$privDex < rng[k+1],"googPct"]))
  delSeries <- c(delSeries,mean(agtDelSmry[agtDelSmry$delBool & agtDelSmry$privDex >= rng[k] & agtDelSmry$privDex < rng[k+1],"googPct"]))
}

data.frame(c(blissX,blissX),c(noDelSeries,delSeries),c(rep("False",length(blissX)),rep("True",length(blissX)))) -> ggFrame2
names(ggFrame2) <-  c("blissRng","googPct","deletion")

ggFrame2$deletion <- as.factor(ggFrame2$deletion)



ggplot() + geom_point(aes(x=agtDelSmry$privDex,y=agtDelSmry$googPct),alpha=.1,color=basePoint) + geom_line(aes(x=ggFrame2$blissRng,y=ggFrame2$googPct,color=ggFrame2$deletion),size=1) + 
  scale_color_manual(values = c("False" = vpnTrue, "True" = vpnFalse))  + ylab("% Google Usage") + xlab("Privacy Index") + labs(color="Deletion Law") + 
  ggtitle("Moving Average") + theme(
    panel.background = element_rect(fill = bgFill),
    plot.title = element_text(color =basePoint,hjust = 0.5),
    plot.background = element_rect(fill = bgFill),
    panel.grid = element_blank(),
    axis.text = element_text(color =basePoint),
    axis.title = element_text(color =basePoint),
    legend.background = element_rect(fill = bgFill,color=bgFill),
    legend.key = element_rect(fill = bgFill, colour = bgFill),
    legend.text = element_text(color =basePoint),
    legend.title = element_text(color =basePoint)) -> plt2

ggarrange(plt1,plt2,nrow=1,ncol=2,common.legend = TRUE, legend = "bottom")
ggsave(filename = "~/ResearchCode/antiTrustImages/deletion2.png",width=7,height=7,bg=bgFill)
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
agentDat$privDex <- (agentDat$blissPoint/agentDat$unifExp)
# first, let's find out how a sharing rule affects Google's usage
outDat[outDat$tick >= outDat$shareTick + 25,] -> afterShare
afterShare$shareAvail <- (afterShare$shareTick !=-10)

afterShare %>% transform(googBool=(currEngine=="google")) %>% group_by(key,shareAvail,agent) %>% summarise(googPct=mean(googBool)) -> shareSmry

merge(shareSmry,agentDat,by=c("key","agent")) -> agtShareSmry



unique(agtShareSmry$shareAvail)
lm(mLogit(agtShareSmry$googPct)~agtShareSmry$privDex +  (agtShareSmry$shareAvail)) -> lMod

blissRangeFunc(agtShareSmry$privDex) -> blissRng
cbind(rep(1,length(blissRng)),blissRng,rep(0,length(blissRng))) -> deltaNoShare
cbind(rep(1,length(blissRng)),blissRng,rep(1,length(blissRng))) -> deltaDeltaShare


deltaNoShare %*% lMod$coefficients -> yHatNo
deltaDeltaShare %*% lMod$coefficients -> yHatYes


yHatENo <- (.01+exp(yHatNo))/(1+exp(yHatNo))
yHatEYes <- (.01+exp(yHatYes))/(1+exp(yHatYes))

data.frame(c(blissRng,blissRng),c(yHatENo,yHatEYes),c(rep("False",length(blissRng)),rep("True",length(blissRng)))) -> ggFrame1
names(ggFrame1) <-  c("blissRng","googPct","sharing")

ggFrame1$sharing <- as.factor(ggFrame1$sharing)



ggplot() + geom_point(aes(x=agtShareSmry$privDex,y=agtShareSmry$googPct),alpha=.1,color=basePoint) + geom_line(aes(x=ggFrame1$blissRng,y=ggFrame1$googPct,color=ggFrame1$sharing),size=1) + 
  scale_color_manual(values = c("False" = vpnTrue, "True" = vpnFalse))  + ylab("% Google Usage") + xlab("Privacy Index") + labs(color="Sharing Law") + ggtitle("Modified Logit")+ theme(
    panel.background = element_rect(fill = bgFill),
    plot.title = element_text(color =basePoint,hjust = 0.5),
    plot.background = element_rect(fill = bgFill),
    panel.grid = element_blank(),
    axis.text = element_text(color =basePoint),
    axis.title = element_text(color =basePoint),
    legend.background = element_rect(fill = bgFill,color=bgFill),
    legend.key = element_rect(fill = bgFill, colour = bgFill),
    legend.text = element_text(color =basePoint),
    legend.title = element_text(color =basePoint))  -> plt1

# now get the smoothing

window <- .05
seq(0,1,by=window) -> rng

blissX <- c()
noSharing <- c()
Sharing <- c()


for (k in 1:(length(rng)-1)){
  blissX <- c(blissX,(rng[k+1]+rng[k])/2)
  
  noSharing <- c(noSharing,mean(agtShareSmry[!agtShareSmry$shareAvail & agtShareSmry$privDex >= rng[k] & agtShareSmry$privDex < rng[k+1],"googPct"]))
  Sharing <- c(Sharing,mean(agtShareSmry[agtShareSmry$shareAvail & agtShareSmry$privDex >= rng[k] & agtShareSmry$privDex < rng[k+1],"googPct"]))
}

data.frame(c(blissX,blissX),c(noSharing,Sharing),c(rep("False",length(blissX)),rep("True",length(blissX)))) -> ggFrame2
names(ggFrame2) <-  c("blissRng","googPct","sharing")

ggFrame2$sharing <- as.factor(ggFrame2$sharing)



ggplot() + geom_point(aes(x=agtShareSmry$privDex,y=agtShareSmry$googPct),alpha=.1,color=basePoint) + xlab("Privacy Index") + ylab("Google Usage %") + labs(color="Sharing") + 
  geom_line(aes(x=ggFrame2$blissRng,y=ggFrame2$googPct,color=ggFrame2$sharing),size=1) + scale_color_manual(values = c("False" = vpnTrue, "True" = vpnFalse))   + ggtitle("Moving Average") + theme(
    panel.background = element_rect(fill = bgFill),
    plot.title = element_text(color =basePoint,hjust = 0.5),
    plot.background = element_rect(fill = bgFill),
    panel.grid = element_blank(),
    axis.text = element_text(color =basePoint),
    axis.title = element_text(color =basePoint),
    legend.background = element_rect(fill = bgFill,color=bgFill),
    legend.key = element_rect(fill = bgFill, colour = bgFill),
    legend.text = element_text(color =basePoint),
    legend.title = element_text(color =basePoint)) -> plt2

ggarrange(plt1,plt2,nrow=1,ncol=2,common.legend = TRUE, legend = "bottom")
ggsave(filename = "~/ResearchCode/antiTrustImages/sharing2.png",width=7,height=7)

# now test importance of deletion law vs sharing law order




setwd("/home/jsschuler/ResearchCode/antiTrustDataExp")
read.csv("ctrl.csv",sep=",") -> control
list.files("/home/jsschuler/ResearchCode/antiTrustDataExp") -> allFi

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
agentDat$privDex <- (agentDat$blissPoint/agentDat$unifExp)
# first, let's find out how the order of the sharing and deletion rules affects 
# Google's share.
# step 1: the ssame time
outDat$lastTick <- max(outDat$delTick,outDat$shareTick)

# now, get a variable tracking which came first 
outDat$orderCat <- as.factor((outDat$shareTick==outDat$delTick) + 2*(outDat$shareTick>outDat$delTick)+4*(outDat$shareTick<outDat$delTick))



outDat[outDat$tick >= outDat$lastTick + 25 ,] -> afterLast
#afterShareDel1$shareAvail <- (afterShareDel1$shareTick !=-10)

afterLast %>% transform(googBool=(currEngine=="google")) %>% group_by(key,agent,orderCat) %>% summarise(googPct=mean(googBool)) -> shareSmry

merge(shareSmry,agentDat,by=c("key","agent")) -> agtShareSmry



unique(agtShareSmry$shareAvail)
lm(mLogit(agtShareSmry$googPct)~agtShareSmry$privDex +  agtShareSmry$orderCat) -> lMod

blissRangeFunc(agtShareSmry$privDex) -> blissRng
cbind(rep(1,length(blissRng)), blissRng,rep(0,length(blissRng)),rep(0,length(blissRng))) -> deltaSimulatenous
cbind(rep(1,length(blissRng)), blissRng,rep(1,length(blissRng)),rep(0,length(blissRng))) -> deltaDelFirst
cbind(rep(1,length(blissRng)), blissRng,rep(0,length(blissRng)),rep(1,length(blissRng))) -> deltaShareFirst



deltaSimulatenous %*% lMod$coefficients -> yHatSim
deltaDelFirst  %*% lMod$coefficients -> yHatDelFirst
deltaShareFirst  %*% lMod$coefficients -> yHatShareFirst
 

yHatESim <- (.01+exp(yHatSim))/(1+exp(yHatSim))
yHatEDelFirst <- (.01+exp(yHatDelFirst))/(1+exp(yHatDelFirst))
yHatEShareFirst <- (.01+exp(yHatShareFirst))/(1+exp(yHatShareFirst))

data.frame(c(blissRng,blissRng,blissRng),c(yHatESim,yHatEDelFirst,yHatEShareFirst),c(rep("Simultaneous",length(blissRng)),rep("Deletion First",length(blissRng)),rep("Sharing First",length(blissRng)))) -> ggFrame1
names(ggFrame1) <-  c("blissRng","googPct","order")

#ggFrame1$sharing <- as.factor(ggFrame1$order)



ggplot() + geom_point(aes(x=agtShareSmry$privDex,y=agtShareSmry$googPct),alpha=.1,color=basePoint) + geom_line(aes(x=ggFrame1$blissRng,y=ggFrame1$googPct,color=ggFrame1$order),size=1) + 
  scale_color_manual(values = c("Simultaneous" = vpnTrue, "Deletion First" = vpnFalse,"Sharing First"=shareColor))  + ylab("% Google Usage") + xlab("Privacy index") + labs(color="Order") + ggtitle("Modified Logit")+ theme(
    panel.background = element_rect(fill = bgFill),
    plot.title = element_text(color =basePoint,hjust = 0.5),
    plot.background = element_rect(fill = bgFill),
    panel.grid = element_blank(),
    axis.text = element_text(color =basePoint),
    axis.title = element_text(color =basePoint),
    legend.background = element_rect(fill = bgFill,color=bgFill),
    legend.key = element_rect(fill = bgFill, colour = bgFill),
    legend.text = element_text(color =basePoint),
    legend.title = element_text(color =basePoint))  -> plt1

# now get the smoothing

window <- .05
seq(0,1,by=window) -> rng

blissX <- c()
simultaneous <- c()
sharingFirst <- c()
deletionFirst <- c()

for (k in 1:(length(rng)-1)){
  blissX <- c(blissX,(rng[k+1]+rng[k])/2)
  
  simultaneous <- c(simultaneous,mean(agtShareSmry[agtShareSmry$orderCat==1 & agtShareSmry$privDex >= rng[k] & agtShareSmry$privDex < rng[k+1],"googPct"]))
  sharingFirst <- c(sharingFirst,mean(agtShareSmry[agtShareSmry$orderCat==2 & agtShareSmry$privDex >= rng[k] & agtShareSmry$privDex < rng[k+1],"googPct"]))
  deletionFirst <- c(deletionFirst,mean(agtShareSmry[agtShareSmry$orderCat==4 & agtShareSmry$privDex >= rng[k] & agtShareSmry$privDex < rng[k+1],"googPct"]))
}

data.frame(c(blissX,blissX,blissX),c(simultaneous,sharingFirst,deletionFirst),c(rep("Simultaneous",length(blissX)),rep("Deletion First",length(blissX)),rep("Sharing First",length(blissX)))) -> ggFrame2
names(ggFrame2) <-  c("blissRng","googPct","order")

#ggFrame2$sharing <- as.factor(ggFrame2$sharing)

ctrl <- control[,c("key","deletionTick","sharingTick","privacyVal")]
ctrl$shareFirst <- ctrl$deletionTick < ctrl$sharingTick
ctrl$delFirst <- ctrl$deletionTick > ctrl$sharingTick

merge(agtShareSmry,ctrl,by="key") -> modOrder
modChk <- lm(modOrder$googPct ~ modOrder$privDex + modOrder$shareFirst + modOrder$delFirst )
summary(modChk)
modChk1 <- lm(mLogit(modOrder$googPct) ~ modOrder$privDex + modOrder$shareFirst + modOrder$delFirst )
summary(modChk1)




ggplot() + geom_point(aes(x=agtShareSmry$privDex,y=agtShareSmry$googPct),alpha=.1,color=basePoint) + geom_line(aes(x=ggFrame2$blissRng,y=ggFrame2$googPct,color=ggFrame2$order),size=1) + 
scale_color_manual(values = c("Simultaneous" = vpnTrue, "Deletion First" = vpnFalse,"Sharing First"=shareColor))  + ylab("% Google Usage") + xlab("Privacy Index") +
  
labs(color="Order") + ggtitle("Moving Average")+ theme(
    panel.background = element_rect(fill = bgFill),
    plot.title = element_text(color =basePoint,hjust = 0.5),
    plot.background = element_rect(fill = bgFill),
    panel.grid = element_blank(),
    axis.text = element_text(color =basePoint),
    axis.title = element_text(color =basePoint),
    legend.background = element_rect(fill = bgFill,color=bgFill),
    legend.key = element_rect(fill = bgFill, colour = bgFill),
    legend.text = element_text(color =basePoint),
    legend.title = element_text(color =basePoint))  -> plt2

ggarrange(plt1,plt2,nrow=1,ncol=2,common.legend = TRUE, legend = "bottom")
ggsave(filename = "~/ResearchCode/antiTrustImages/sharingDeletion2.png",width=7,height=7)



