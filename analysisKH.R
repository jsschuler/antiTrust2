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

setwd("~/ResearchCode/antiTrustDataKH")
read.csv("ctrl.csv",sep=",") -> control
list.files("~/ResearchCode/antiTrustDataKH") -> allFi

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
control$privacyPop <- 1-control$privacyVal/max(control$privacyVal)
controlMerge <- control[,c("key","seed1","seed2","runType","privacyPop")]

finDat <- merge(controlMerge,keySmry,by="key")
finDat$googPct <- finDat$googCnt/finDat$searchCnt
baseDat <- finDat[finDat$runType==0,]
vpnDat <- finDat[finDat$runType==1,]
delDat <- finDat[finDat$runType==2,]
shareDat <- finDat[finDat$runType==4,]

vpnBaseDat  <- finDat[finDat$runType==0 | finDat$runType==1 ,]
delBaseDat  <- finDat[finDat$runType==0 | finDat$runType==2 ,]
shareBaseDat  <- finDat[finDat$runType==0 | finDat$runType==4 ,]

boolForm <- function(val){if (val){return("True")} else {return("False")}}
BoolForm <- Vectorize(boolForm,"val")

ggplot() + geom_point(aes(x=100*baseDat$privacyPop,y=100*baseDat$googPct)) + xlab("Population Privacy Index") + 
  ylab("Aggregate % Google Usage") + ggtitle("Aggregate Google Usage") +
  theme(
    panel.background = element_rect(fill = bgFill),
    plot.title = element_text(color =basePoint,hjust = 0.5),
    plot.background = element_rect(fill = bgFill),
    panel.grid = element_line(color = basePoint,
                              size = 0.25,
                              linetype = 2),
    axis.text = element_text(color =basePoint),
    axis.title = element_text(color =basePoint),
    legend.background = element_rect(fill = bgFill,color=bgFill),
    legend.key = element_rect(fill = bgFill, colour = bgFill),
    legend.text = element_text(color =basePoint),
    legend.title = element_text(color =basePoint)) 

ggsave(filename = "~/ResearchCode/antiTrustImages/KHoverall.png",width=7,height=7)

  
  
ggplot() + geom_point(aes(x=100*vpnBaseDat$privacyPop,y=100*vpnBaseDat$googPct,color=BoolForm(vpnBaseDat$runType==1))) +
  xlab("Population Privacy Index") + ylab("Aggregate % Google Usage") + ggtitle("Aggregate VPN Effect") + labs(color="VPN Available") +
  scale_color_manual(values = c("True" = vpnTrue, "False" = vpnFalse)) +
    theme(
    panel.background = element_rect(fill = bgFill),
    plot.title = element_text(color =basePoint,hjust = 0.5),
    plot.background = element_rect(fill = bgFill),
    panel.grid = element_line(color = basePoint,
                              size = 0.25,
                              linetype = 2),
    axis.text = element_text(color =basePoint),
    axis.title = element_text(color =basePoint),
    legend.background = element_rect(fill = bgFill,color=bgFill),
    legend.key = element_rect(fill = bgFill, colour = bgFill),
    legend.text = element_text(color =basePoint),
    legend.title = element_text(color =basePoint))  -> khVPN1


ggplot() + geom_point(aes(x=100*delBaseDat$privacyPop,y=100*delBaseDat$googPct,color=BoolForm(delBaseDat$runType==2))) + xlab("Population Privacy Index") + ylab("Aggregate % Google Usage") + ggtitle("Aggregate Deletion Law Effect") + labs(color="Deletion Law Available") +
  scale_color_manual(values = c("True" = vpnTrue, "False" = vpnFalse)) +
  theme(
    panel.background = element_rect(fill = bgFill),
    plot.title = element_text(color =basePoint,hjust = 0.5),
    plot.background = element_rect(fill = bgFill),
    panel.grid = element_line(color = basePoint,
                              size = 0.25,
                              linetype = 2),
    axis.text = element_text(color =basePoint),
    axis.title = element_text(color =basePoint),
    legend.background = element_rect(fill = bgFill,color=bgFill),
    legend.key = element_rect(fill = bgFill, colour = bgFill),
    legend.text = element_text(color =basePoint),
    legend.title = element_text(color =basePoint))  -> khDel1

ggsave(filename = "~/ResearchCode/antiTrustImages/KHDel.png",width=7,height=7)

ggplot() + geom_point(aes(x=100*shareBaseDat$privacyPop,y=100*shareBaseDat$googPct,color=BoolForm(shareBaseDat$runType==4))) + xlab("Population Privacy Index") + ylab("Aggregate % Google Usage") + ggtitle("Aggregate Sharing Law Effect") + labs(color="Sharing Law Available") +
  scale_color_manual(values = c("True" = vpnTrue, "False" = vpnFalse)) +
  theme(
    panel.background = element_rect(fill = bgFill),
    plot.title = element_text(color =basePoint,hjust = 0.5),
    plot.background = element_rect(fill = bgFill),
    panel.grid = element_line(color = basePoint,
                              size = 0.25,
                              linetype = 2),
    axis.text = element_text(color =basePoint),
    axis.title = element_text(color =basePoint),
    legend.background = element_rect(fill = bgFill,color=bgFill),
    legend.key = element_rect(fill = bgFill, colour = bgFill),
    legend.text = element_text(color =basePoint),
    legend.title = element_text(color =basePoint)) -> khShar1
ggsave(filename = "~/ResearchCode/antiTrustImages/KHsharing.png",width=7,height=7)




# now, examine wealth transfers
baseDat[,c("seed1","seed2","googPct","privacyPop")] -> baseDat
names(baseDat)[[3]] <- "googPctBase"
vpnDat[,c("seed1","seed2","googPct")] -> vpnDat
names(vpnDat)[[3]] <- "googPctVpn"
delDat[,c("seed1","seed2","googPct")] -> delDat
names(delDat)[[3]] <- "googPctDel"
shareDat[,c("seed1","seed2","googPct")] -> shareDat
names(shareDat)[[3]] <- "googPctShar"
merge(baseDat,vpnDat,by=c("seed1","seed2")) -> vpnSideDat
merge(baseDat,delDat,by=c("seed1","seed2")) -> delSideDat
merge(baseDat,shareDat,by=c("seed1","seed2")) -> sharSideDat
vpnSideDat$delta <- vpnSideDat$googPctVpn-vpnSideDat$googPctBase
delSideDat$delta <- delSideDat$googPctDel-delSideDat$googPctBase
sharSideDat$delta <- sharSideDat$googPctShar-sharSideDat$googPctBase

#vpnTrue <- "#4285f4"
#vpnFalse <- "#ea4335"

# we need a function that maps true and false into favoring Google vs Duck Duck Go 
favorFunc <- function(val){
  if (val){return("DuckDuckGo")} else {return("Google")}
}

FavorFunc <- Vectorize(favorFunc,"val")

vpnSideDat$deltaSign <- as.character(vpnSideDat$delta < 0)

ggplot() + scale_color_manual(values = c("DuckDuckGo" = hiOrange, "Google" = googColor)) + 
  geom_point(aes(x=(100*vpnSideDat$privacyPop),y=100*vpnSideDat$delta,color=FavorFunc(vpnSideDat$deltaSign))) + xlab("Population Privacy Index") + 
  ylab("Additional Google Usage Over Base") + ggtitle("VPN Access") + labs(color="Favorable To")  + theme(
  panel.background = element_rect(fill = bgFill),
  plot.title = element_text(color =basePoint,hjust = 0.5),
  plot.background = element_rect(fill = bgFill),
  panel.grid = element_line(color = basePoint,
  size = 0.25,
  linetype = 2),
  axis.text = element_text(color =basePoint),
  axis.title = element_text(color =basePoint),
  #legend.background = element_rect(fill = bgFill),
  #legend.text = element_text(color =basePoint)
  #legend.position = "none"
  ) + geom_hline(yintercept = 0,color="black") -> khVPN2



delSideDat$deltaSign <- as.character(delSideDat$delta < 0)
ggplot() + scale_color_manual(values = c("DuckDuckGo" = hiOrange, "Google" = googColor)) + geom_point(aes(x=(100*delSideDat$privacyPop),y=100*delSideDat$delta,color=FavorFunc(delSideDat$deltaSign))) + 
  ggtitle("Deletion Law") + xlab("Population Privacy Index") + ylab("Additional Google Usage Over Base") + labs(color="Favorable To") + theme(
  panel.background = element_rect(fill = bgFill),
  plot.title = element_text(color =basePoint,hjust = 0.5),
  plot.background = element_rect(fill = bgFill),
  panel.grid = element_line(color = basePoint,
                            size = 0.25,
                            linetype = 2),
  axis.text = element_text(color =basePoint),
  axis.title = element_text(color =basePoint),
  #legend.background = element_rect(fill = bgFill),
  #legend.text = element_text(color =basePoint)
  #legend.position = "none"
  ) + geom_hline(yintercept = 0,color="black") -> khDel2



sharSideDat$deltaSign <- as.character(sharSideDat$delta < 0)



ggplot() + scale_color_manual(values = c("DuckDuckGo" = hiOrange, "Google" = googColor)) + geom_point(aes(x=(100*sharSideDat$privacyPop),y=100*sharSideDat$delta,color=FavorFunc(sharSideDat$deltaSign))) + 
  xlab("Population Privacy Index") + ylab("Additional Google Usage Over Base") + ggtitle("Sharing Law") + labs(color="Favorable To") + theme(
  panel.background = element_rect(fill = bgFill),
  plot.title = element_text(color =basePoint,hjust = 0.5),
  plot.background = element_rect(fill = bgFill),
  panel.grid = element_line(color = basePoint,
                            size = 0.25,
                            linetype = 2),
  axis.text = element_text(color =basePoint),
  axis.title = element_text(color =basePoint),
  #legend.background = element_rect(fill = bgFill),
  #legend.text = element_text(color =basePoint)
  #legend.position = "none"
  ) + geom_hline(yintercept = 0,color="black") -> khShar2




ggarrange(khVPN1,khVPN2,nrow=1,ncol=2,common.legend = FALSE, legend = "bottom")

ggsave(filename = "~/ResearchCode/antiTrustImages/KHVPN.png",width=7,height=7)

ggarrange(khDel1,khDel2,nrow=1,ncol=2,common.legend = FALSE, legend = "bottom")

ggsave(filename = "~/ResearchCode/antiTrustImages/KHDel.png",width=7,height=7)

ggarrange(khShar1,khShar2,nrow=1,ncol=2,common.legend = FALSE, legend = "bottom")

ggsave(filename = "~/ResearchCode/antiTrustImages/KHsharing.png",width=7,height=7)

lm(delSideDat$delta~delSideDat$privacyPop) -> dMod
summary(dMod)


### Rule Order ###

setwd("~/ResearchCode/antiTrustDataOrder2")
read.csv("ctrl.csv",sep=",") -> control
list.files("~/ResearchCode/antiTrustDataOrder2") -> allFi

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

control$runType <- (control$deletionTick < control$sharingTick) + 2*(control$deletionTick==control$sharingTick) + 4*(control$deletionTick > control$sharingTick)

outDat <- outDat[outDat$tick >= 150,]

outDat %>% transform(googBool=(currEngine=="google")) %>% group_by(key) %>% 
  summarize(searchCnt=n(),googCnt=sum(googBool)) %>% transform(googPct=googCnt/searchCnt)-> keySmry
ctrl <- control[,c("key","seed1","seed2","privacyVal","runType","pctConnected")]
merge(keySmry,ctrl,by="key") -> orderSmry



orderSmry %>% group_by(runType,privacyVal,pctConnected) %>% summarise(googMn=mean(googPct),goog75=quantile(googPct,c(.75))) -> privSmry

runFunc <- function(val){
  if (val==1){return("Deletion First")}
  if (val==2){return("Simultaneous")}
  if (val==4){return("Sharing First")}
}

RunFunc <- Vectorize(runFunc,"val")

ggplot() + geom_point(aes(x=100*orderSmry$privacyVal,y=100*orderSmry$googPct,color=RunFunc(orderSmry$runType)),alpha=.2) +
  geom_line(aes(x=100*privSmry$privacyVal,y=100*privSmry$googMn,color=RunFunc(privSmry$runType))) +
  scale_color_manual(values = c("Simultaneous" = vpnTrue, "Deletion First" = vpnFalse,"Sharing First"=shareColor)) +
  ylab("% Google Usage") + xlab("Population Privacy index") + labs(color="Order") + ggtitle("Order of Rule Introduction") +
  theme(
    panel.background = element_rect(fill = bgFill),
    plot.title = element_text(color =basePoint,hjust = 0.5),
    plot.background = element_rect(fill = bgFill),
    panel.grid = element_line(color = basePoint,
                              size = 0.25,
                              linetype = 2),
    axis.text = element_text(color =basePoint),
    axis.title = element_text(color =basePoint),
    #legend.background = element_rect(fill = bgFill,color=bgFill),
    legend.key = element_rect(fill = bgFill, colour = bgFill),
    #legend.text = element_text(color =basePoint),
    legend.title = element_text(color =basePoint))
  
ggsave(filename = "~/ResearchCode/antiTrustImages/KHruleOrder.png",width=7,height=7)
