################################################################################
#             Final Analysis Code                                              #
#             Anti-Trust Model                                                 #
#             September 2023                                                   #
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

# step one, consider only cases where there is no vpn
# and only consider data where Duck Duck Go has been available
outDat[outDat$tick >= (outDat$duckTick+50) & outDat$vpnTick==-10 & outDat$delTick==-10 & outDat$shareTick==-10,] -> afterDuck

# now, get each agent's Google usage

afterDuck %>% transform(googBool=(currEngine=="google")) %>% group_by(key,agent) %>% summarise(googPct=mean(googBool)) -> agtUsage

merge(agtUsage,agentDat,by=c("agent","key")) -> agtPropUsage




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




ggplot() + geom_point(aes(x=agtPropUsage$blissPoint,y=agtPropUsage$googPct),alpha=.1,color=basePoint) + geom_line(aes(x=xRng,y=yHatE),color=googColor,size=1) + xlab("Bliss Point") + ylab("Google Usage") + ggtitle("Modified Logit") + theme(
  panel.background = element_rect(fill = bgFill),
  plot.title = element_text(color =basePoint,hjust = 0.5),
  plot.background = element_rect(fill = bgFill),
  panel.grid = element_blank(),
  axis.text = element_text(color =basePoint),
  axis.title = element_text(color =basePoint),
  legend.background = element_rect(fill = bgFill),
  legend.text = element_text(color =basePoint)) -> plt1


window <- .5
seq(min(agtPropUsage$blissPoint),max(agtPropUsage$blissPoint),by=window) -> rng

blissX <- c()
mnGoog <- c()
for (k in 1:(length(rng)-1)){
  blissX <- c(blissX,(rng[k+1]+rng[k])/2)
  mnGoog <- c(mnGoog,mean(agtPropUsage[agtPropUsage$blissPoint >= rng[k] & agtPropUsage$blissPoint < rng[k+1] ,"googPct"]))
}

ggplot() + geom_point(aes(x=agtPropUsage$blissPoint,y=agtPropUsage$googPct),alpha=.1,color=basePoint) + geom_line(aes(x=blissX,y=mnGoog),color=googColor,size=1) + xlab("Bliss Point") + ylab("% Google Usage") + ggtitle("Moving Average") + theme(
  panel.background = element_rect(fill = bgFill),
  plot.title = element_text(color =basePoint,hjust = 0.5),
  plot.background = element_rect(fill = bgFill),
  panel.grid = element_blank(),
  axis.text = element_text(color =basePoint),
  axis.title = element_text(color =basePoint),
  legend.background = element_rect(fill = bgFill),
  legend.text = element_text(color =basePoint)) -> plt2
ggarrange(plt1,plt2,nrow=1,ncol=2)
ggsave(filename = "~/ResearchCode/antiTrustImages/basic.png",width=7,height=7,bg=bgFill)

# now examine the mixing speed 



#### VPN ACCESS 


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

data.frame(c(blissRange,blissRange),c(yHatEnoVPN,yHatEVPN),c(rep("False",length(blissRange)),rep("True",length(blissRange)))) -> ggFrame1
names(ggFrame1) <- c("blissRng","googPct","vpn")
#ggFrame1$vpn <- as.factor(ggFrame1$vpn)
substr(agtPropUsageVPN$optOut,1,1) <- toupper(substr(agtPropUsageVPN$optOut,1,1))


ggplot() + geom_point(aes(x=agtPropUsageVPN$blissPoint,y=agtPropUsageVPN$googPct,color=agtPropUsageVPN$optOut),alpha=.1)  + geom_line(aes(x=ggFrame1$blissRng,y=ggFrame1$googPct,color=ggFrame1$vpn),size=1) + 
  scale_color_manual(values = c("False" = vpnTrue, "True" = vpnFalse)) + xlab("Bliss Point") + ylab("Google Usage") + labs(color = "VPN") + ggtitle("Modified Logit") + theme(
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


window <- .5
seq(min(agtPropUsageVPN$blissPoint),max(agtPropUsageVPN$blissPoint),by=window) -> rng

blissX <- c()
mnNoVPN <- c()
mnVPN <- c()

for (k in 1:(length(rng)-1)){
  blissX <- c(blissX,(rng[k+1]+rng[k])/2)
  mnNoVPN <- c(mnNoVPN,mean(agtPropUsageVPN[agtPropUsageVPN$optOut=="False" & agtPropUsageVPN$blissPoint >= rng[k] & agtPropUsageVPN$blissPoint < rng[k+1] ,"googPct"]))
  mnVPN <- c(mnVPN,mean(agtPropUsageVPN[agtPropUsageVPN$optOut=="True" & agtPropUsageVPN$blissPoint >= rng[k] & agtPropUsageVPN$blissPoint < rng[k+1],"googPct"]))
  
}

data.frame(c(blissX,blissX),c(mnNoVPN,mnVPN),c(rep("False",length(blissX)),rep("True",length(blissX)))) -> ggFrame2
names(ggFrame2) <- c("blissRng","googPct","vpn")
#ggFrame2$vpn <- as.factor(ggFrame2$vpn)

ggplot() + geom_point(aes(x=agtPropUsageVPN$blissPoint,y=agtPropUsageVPN$googPct,color=agtPropUsageVPN$optOut),alpha=.1) + 
  geom_line(aes(x=ggFrame2$blissRng,y=ggFrame2$googPct,color=ggFrame2$vpn),size=1) + scale_color_manual(values = c("False" = vpnTrue, "True" = vpnFalse)) + 
  xlab("Bliss Point") + ylab("% Google Usage") + ggtitle("Moving Average") + labs(color = "VPN") + theme(
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

ggsave(filename = "~/ResearchCode/antiTrustImages/vpn.png",width=7,height=7,bg=bgFill)


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

ggplot() + geom_point(aes(x=vpnBliss$blissPoint,y=vpnBliss$optPct,color=100*vpnBliss$googPct),alpha=.1) + scale_color_gradient(low = hiOrange, high = googColor)  + geom_line(aes(x=blissRange,y=vpnYE),color=vpnColor,size=1) + 
  xlab("Bliss Point") + ylab("% VPN Usage") + labs(color="Google %") + ggtitle("Modified Logit") + theme(
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

window <- .5
seq(min(vpnBliss$blissPoint),max(vpnBliss$blissPoint),by=window) -> rng

blissX <- c()
mnNoVPN <- c()
mnVPN <- c()

for (k in 1:(length(rng)-1)){
  blissX <- c(blissX,(rng[k+1]+rng[k])/2)
  
  mnVPN <- c(mnVPN,mean(vpnBliss[vpnBliss$blissPoint >= rng[k] & vpnBliss$blissPoint < rng[k+1],"optPct"]))
  
}

ggplot() + geom_point(aes(x=vpnBliss$blissPoint,y=vpnBliss$optPct,color=100*vpnBliss$googPct),alpha=.1) + scale_color_gradient(low = hiOrange, high = googColor) + geom_line(aes(x=blissX,y=mnVPN),color=vpnColor,size=1) + 
  xlab("Bliss Point") + ylab("% VPN Usage") + ggtitle("Moving Average") + labs(color = "Google %") + theme(
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
ggsave(filename = "~/ResearchCode/antiTrustImages/vpnUsage.png",width=7,height=7,bg=bgFill)


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


data.frame(c(blissRng,blissRng),c(yHatNoDel,yHatDel),c(rep("False",length(blissRng)),rep("True",length(blissRng)))) -> ggFrame1
names(ggFrame1) <-  c("blissRng","googPct","deletion")

ggFrame1$deletion <- as.factor(ggFrame1$deletion)



ggplot() + geom_point(aes(x=agtDelSmry$blissPoint,y=agtDelSmry$googPct),alpha=.1,color=basePoint) + geom_line(aes(x=ggFrame1$blissRng,y=ggFrame1$googPct,color=ggFrame1$deletion),size=1) + 
  scale_color_manual(values = c("False" = vpnTrue, "True" = vpnFalse))  + ylab("% Google Usage") + xlab("Bliss Point") + labs(color="Deletion Law") + ggtitle("Modified Logit") + theme(
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

data.frame(c(blissX,blissX),c(noDelSeries,delSeries),c(rep("False",length(blissX)),rep("True",length(blissX)))) -> ggFrame2
names(ggFrame2) <-  c("blissRng","googPct","deletion")

ggFrame2$deletion <- as.factor(ggFrame2$deletion)



ggplot() + geom_point(aes(x=agtDelSmry$blissPoint,y=agtDelSmry$googPct),alpha=.1,color=basePoint) + geom_line(aes(x=ggFrame2$blissRng,y=ggFrame2$googPct,color=ggFrame2$deletion),size=1) + 
  scale_color_manual(values = c("False" = vpnTrue, "True" = vpnFalse))  + ylab("% Google Usage") + xlab("Bliss Point") + labs(color="Deletion Law") + 
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
ggsave(filename = "~/ResearchCode/antiTrustImages/deletion.png",width=7,height=7,bg=bgFill)
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



unique(agtShareSmry$shareAvail)
lm(mLogit(agtShareSmry$googPct)~agtShareSmry$blissPoint +  (agtShareSmry$shareAvail)) -> lMod

blissRangeFunc(agtShareSmry$blissPoint) -> blissRng
cbind(rep(1,length(blissRng)),blissRng,rep(0,length(blissRng))) -> deltaNoShare
cbind(rep(1,length(blissRng)),blissRng,rep(1,length(blissRng))) -> deltaDeltaShare


deltaNoShare %*% lMod$coefficients -> yHatNo
deltaDeltaShare %*% lMod$coefficients -> yHatYes


yHatENo <- (.01+exp(yHatNo))/(1+exp(yHatNo))
yHatEYes <- (.01+exp(yHatYes))/(1+exp(yHatYes))

data.frame(c(blissRng,blissRng),c(yHatENo,yHatEYes),c(rep("False",length(blissRng)),rep("True",length(blissRng)))) -> ggFrame1
names(ggFrame1) <-  c("blissRng","googPct","sharing")

ggFrame1$sharing <- as.factor(ggFrame1$sharing)



ggplot() + geom_point(aes(x=agtShareSmry$blissPoint,y=agtShareSmry$googPct),alpha=.1,color=basePoint) + geom_line(aes(x=ggFrame1$blissRng,y=ggFrame1$googPct,color=ggFrame1$sharing),size=1) + 
  scale_color_manual(values = c("False" = vpnTrue, "True" = vpnFalse))  + ylab("% Google Usage") + xlab("Bliss Point") + labs(color="Sharing Law") + ggtitle("Modified Logit")+ theme(
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

data.frame(c(blissX,blissX),c(noSharing,Sharing),c(rep("False",length(blissX)),rep("True",length(blissX)))) -> ggFrame2
names(ggFrame2) <-  c("blissRng","googPct","sharing")

ggFrame2$sharing <- as.factor(ggFrame2$sharing)



ggplot() + geom_point(aes(x=agtShareSmry$blissPoint,y=agtShareSmry$googPct),alpha=.1,color=basePoint) + xlab("Bliss Point") + ylab("Google Usage %") + labs(color="Sharing") + 
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
ggsave(filename = "~/ResearchCode/antiTrustImages/sharing.png",width=7,height=7)

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
lm(mLogit(agtShareSmry$googPct)~agtShareSmry$blissPoint +  agtShareSmry$orderCat) -> lMod

blissRangeFunc(agtShareSmry$blissPoint) -> blissRng
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



ggplot() + geom_point(aes(x=agtShareSmry$blissPoint,y=agtShareSmry$googPct),alpha=.1,color=basePoint) + geom_line(aes(x=ggFrame1$blissRng,y=ggFrame1$googPct,color=ggFrame1$order),size=1) + 
  scale_color_manual(values = c("Simultaneous" = vpnTrue, "Deletion First" = vpnFalse,"Sharing First"=shareColor))  + ylab("% Google Usage") + xlab("Bliss Point") + labs(color="Order") + ggtitle("Modified Logit")+ theme(
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

window <- .5
seq(min(agtShareSmry$blissPoint),max(agtShareSmry$blissPoint),by=window) -> rng

blissX <- c()
simultaneous <- c()
sharingFirst <- c()
deletionFirst <- c()

for (k in 1:(length(rng)-1)){
  blissX <- c(blissX,(rng[k+1]+rng[k])/2)
  
  simultaneous <- c(simultaneous,mean(agtShareSmry[agtShareSmry$orderCat==1 & agtShareSmry$blissPoint >= rng[k] & agtShareSmry$blissPoint < rng[k+1],"googPct"]))
  sharingFirst <- c(sharingFirst,mean(agtShareSmry[agtShareSmry$orderCat==2 & agtShareSmry$blissPoint >= rng[k] & agtShareSmry$blissPoint < rng[k+1],"googPct"]))
  deletionFirst <- c(deletionFirst,mean(agtShareSmry[agtShareSmry$orderCat==4 & agtShareSmry$blissPoint >= rng[k] & agtShareSmry$blissPoint < rng[k+1],"googPct"]))
}

data.frame(c(blissX,blissX,blissX),c(simultaneous,sharingFirst,deletionFirst),c(rep("Simultaneous",length(blissX)),rep("Deletion First",length(blissX)),rep("Sharing First",length(blissX)))) -> ggFrame2
names(ggFrame2) <-  c("blissRng","googPct","order")

#ggFrame2$sharing <- as.factor(ggFrame2$sharing)




ggplot() + geom_point(aes(x=agtShareSmry$blissPoint,y=agtShareSmry$googPct),alpha=.1,color=basePoint) + geom_line(aes(x=ggFrame2$blissRng,y=ggFrame2$googPct,color=ggFrame2$order),size=1) + 
  scale_color_manual(values = c("Simultaneous" = vpnTrue, "Deletion First" = vpnFalse,"Sharing First"=shareColor))  + ylab("% Google Usage") + xlab("Bliss Point") + labs(color="Order") + ggtitle("Moving Average")+ theme(
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
ggsave(filename = "~/ResearchCode/antiTrustImages/sharingDeletion.png",width=7,height=7)



