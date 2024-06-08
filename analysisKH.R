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

ggplot() + geom_point(aes(x=baseDat$privacyPop,y=baseDat$googPct)) + xlab("Population Privacy Index") + 
  ylab("Aggregate % Google Usage") + ggtitle("Aggregate Google Usage") +
  theme(
    panel.background = element_rect(fill = bgFill),
    plot.title = element_text(color =basePoint,hjust = 0.5),
    plot.background = element_rect(fill = bgFill),
    panel.grid = element_blank(),
    axis.text = element_text(color =basePoint),
    axis.title = element_text(color =basePoint),
    legend.background = element_rect(fill = bgFill,color=bgFill),
    legend.key = element_rect(fill = bgFill, colour = bgFill),
    legend.text = element_text(color =basePoint),
    legend.title = element_text(color =basePoint))

  
  
ggplot() + geom_point(aes(x=vpnBaseDat$privacyPop,y=vpnBaseDat$googPct,color=BoolForm(vpnBaseDat$runType==1))) +
  xlab("Population Privacy Index") + ylab("Aggregate % Google Usage") + ggtitle("Aggregate VPN Effect") + labs(color="VPN Available") +
  scale_color_manual(values = c("True" = vpnTrue, "False" = vpnFalse)) +
    theme(
    panel.background = element_rect(fill = bgFill),
    plot.title = element_text(color =basePoint,hjust = 0.5),
    plot.background = element_rect(fill = bgFill),
    panel.grid = element_blank(),
    axis.text = element_text(color =basePoint),
    axis.title = element_text(color =basePoint),
    legend.background = element_rect(fill = bgFill,color=bgFill),
    legend.key = element_rect(fill = bgFill, colour = bgFill),
    legend.text = element_text(color =basePoint),
    legend.title = element_text(color =basePoint))


ggplot() + geom_point(aes(x=delBaseDat$privacyPop,y=delBaseDat$googPct,color=BoolForm(delBaseDat$runType==2))) + xlab("Population Privacy Index") + ylab("Aggregate % Google Usage") + ggtitle("Aggregate Deletion Law Effect") + labs(color="Deletion Law Available") +
  scale_color_manual(values = c("True" = vpnTrue, "False" = vpnFalse)) +
  theme(
    panel.background = element_rect(fill = bgFill),
    plot.title = element_text(color =basePoint,hjust = 0.5),
    plot.background = element_rect(fill = bgFill),
    panel.grid = element_blank(),
    axis.text = element_text(color =basePoint),
    axis.title = element_text(color =basePoint),
    legend.background = element_rect(fill = bgFill,color=bgFill),
    legend.key = element_rect(fill = bgFill, colour = bgFill),
    legend.text = element_text(color =basePoint),
    legend.title = element_text(color =basePoint))


ggplot() + geom_point(aes(x=shareBaseDat$privacyPop,y=shareBaseDat$googPct,color=BoolForm(shareBaseDat$runType==4))) + xlab("Population Privacy Index") + ylab("Aggregate % Google Usage") + ggtitle("Aggregate Sharing Law Effect") + labs(color="Sharing Law Available") +
  scale_color_manual(values = c("True" = vpnTrue, "False" = vpnFalse)) +
  theme(
    panel.background = element_rect(fill = bgFill),
    plot.title = element_text(color =basePoint,hjust = 0.5),
    plot.background = element_rect(fill = bgFill),
    panel.grid = element_blank(),
    axis.text = element_text(color =basePoint),
    axis.title = element_text(color =basePoint),
    legend.background = element_rect(fill = bgFill,color=bgFill),
    legend.key = element_rect(fill = bgFill, colour = bgFill),
    legend.text = element_text(color =basePoint),
    legend.title = element_text(color =basePoint))

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

# 
vpnSideDat$deltaSign <- as.character(vpnSideDat$delta >= 0)

ggplot() + scale_color_manual(values = c("FALSE" = hiOrange, "TRUE" = googColor)) + 
  geom_point(aes(x=(1-vpnSideDat$privacyPop),y=vpnSideDat$delta,color=vpnSideDat$deltaSign)) + xlab("Population Privacy Index") + ylab("Additional Google Usage Over Base") + ggtitle("VPN Access") + theme(
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
  legend.position = "none")
delSideDat$deltaSign <- as.character(delSideDat$delta >= 0)
ggplot() + scale_color_manual(values = c("FALSE" = hiOrange, "TRUE" = googColor)) + geom_point(aes(x=(delSideDat$privacyPop),y=delSideDat$delta,color=delSideDat$deltaSign)) + 
  ggtitle("Deletion Law") + xlab("Population Privacy Index") + ylab("Additional Google Usage Over Base") +theme(
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
  legend.position = "none")

sharSideDat$deltaSign <- as.character(sharSideDat$delta >= 0)
ggplot() + scale_color_manual(values = c("FALSE" = hiOrange, "TRUE" = googColor)) + geom_point(aes(x=(sharSideDat$privacyPop),y=sharSideDat$delta,color=sharSideDat$deltaSign)) + 
  xlab("Population Privacy Index") + ylab("Additional Google Usage Over Base") + ggtitle("Sharing Law") + theme(
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
  legend.position = "none")

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

ggplot() + geom_point(aes(x=orderSmry$privacyVal,y=orderSmry$googPct,color=as.factor(orderSmry$runType)),alpha=.2) +
  geom_line(aes(x=privSmry$privacyVal,y=privSmry$googMn,color=as.character(privSmry$runType)))

ggplot() + geom_point(aes(x=orderSmry$privacyVal,y=orderSmry$googPct,color=as.factor(orderSmry$runType)),alpha=.2) +
  geom_line(aes(x=privSmry$privacyVal,y=privSmry$goog95,color=as.character(privSmry$runType)))

# now compare deletion first to sharing first

orderSmyNonSimul <- orderSmry[orderSmry$runType!=2,]
delFirst <- orderSmyNonSimul[orderSmyNonSimul$runType==1,c("key","seed1","seed2","privacyVal","pctConnected","googPct")]
names(delFirst)[[1]] <- "key1"
names(delFirst)[[6]] <- "googPctDelFirst"
sharFirst <- orderSmyNonSimul[orderSmyNonSimul$runType==4,c("key","seed1","seed2","googPct")]
names(sharFirst)[[1]] <- "key2"
names(sharFirst)[[4]] <- "googPctsharFirst"

merge(delFirst,sharFirst,by=c("seed1","seed2")) -> orderJoint
orderJoint$delta <- orderJoint$googPctDelFirst-orderJoint$googPctsharFirst

ggplot() + geom_point(aes(x=orderJoint$privacyVal,y=orderJoint$delta))

ggplot() + geom_point(aes(x=orderJoint$pctConnected,y=orderJoint$delta))

lm(orderJoint$delta~orderJoint$privacyVal + orderJoint$pctConnected) -> mod1
lm(orderJoint$delta~orderJoint$pctConnected) -> mod2
table(orderJoint$delta >= 0)[["TRUE"]]/nrow(orderJoint)

orderJoint %>% group_by(privacyVal) %>% summarize(cnt=n())

# now, sample 100 times with replacement stratified by privacy value
proport <- c()
for (i in 1:100){
  sampOrder <- c()
  for (j in 1:15){sampOrder <- c(sampOrder,sample(1:10,replace = TRUE))}
  c(proport,table(orderJoint[sampOrder,"delta"] >= 0)[["TRUE"]]/nrow(orderJoint)) -> proport
}

# boostrap without stratification
proport2 <- c()
for (i in 1:100){
  c(proport2,table(orderJoint[sample(1:nrow(orderJoint),replace=TRUE),"delta"] >= 0)[["TRUE"]]/nrow(orderJoint)) -> proport2
  
}
