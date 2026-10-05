################################################################################
#             Final Analysis Code                                              #
#             Anti-Trust Model                                                 #
#             Nov 2025 Update                                                  #
#             John S. Schuler                                                  #
#             This version uses privacy index rather than bliss point          #
################################################################################

#### BASIC RESULTS #######

library(ggplot2)
library(ggExtra)
library(ggpattern)
library(ggpubr)
library(dplyr)
library(rlist)
library(tidyr)
library(cowplot)
library(data.table)
library(parallel)
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

# we want to build a table of effect sizes, mid 95% of the sampling distribution and the p-value
labl <- c()
effSize <- c()
loQ <- c()
hiQ <- c()
pVals <- c()
pValsApprox <- c()
pValsPlus1 <- c()
pFloor <- c()
nPerms <- c()

perm_pvals <- function(plotDat){
  obsVal <- plotDat$value[plotDat$label=="observed"][1]
  nullVals <- plotDat$value[plotDat$label=="permute"]
  B <- length(nullVals)
  extremeCnt <- sum(abs(nullVals) >= abs(obsVal))
  pApprox <- extremeCnt / B
  pPlus1 <- (extremeCnt + 1) / (B + 1)
  if (pApprox == 0){
    floorTxt <- paste0("< ", signif(1 / B, 3))
  } else {
    floorTxt <- paste0("= ", sprintf("%.3f", pApprox))
  }
  return(list(
    B = B,
    p_approx = pApprox,
    p_plus1 = pPlus1,
    p_floor = floorTxt
  ))
}



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

#setwd("~/ResearchCode/antiTrustData")
setwd("~/Dropbox/Data\ Archive/antiTrustData")
read.csv("~/Dropbox/Data\ Archive/antiTrustData/ctrl.csv",sep=",") -> control
list.files("~/Dropbox/Data\ Archive/antiTrustData") -> allFi

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

# we need several different visualizations
# Firstly, let's look at model level differences in Hoogle usage with vs without each option
# plotted against the model level privacy parameter 

outDat %>% filter(tick >= 50) %>% group_by(key,currEngine) %>% summarise(cnt=n()) %>%
  pivot_wider(names_from = currEngine, values_from = cnt, values_fill = 0) %>%
  transform(total=google+duckDuckGo) %>% transform(googPct=100*google/total) %>% 
  select(key,googPct) -> outSmry
control$privacyVal <- as.numeric(control$privacyVal)
control$privalVal <- if_else(control$privacyVal == 1.0, "Low", "High")
control$privalVal <- factor(control$privalVal, levels = c("Low", "High"))

merge(control,outSmry,by="key") -> controlSmry


### VPN ###
controlSmry %>% transform(category=1*(vpnTick==50)+2*(deletionTick==50)+4*(sharingTick==50)) %>% 
  group_by(seed1,privalVal,category) %>% filter(category %in% c(0,1)) %>% transform(category=as.factor(category)) -> vpnDat

x0 <- min(vpnDat$googPct)
x1 <- max(vpnDat$googPct)

# what is the difference in means with respect to vpn use
vpnDat %>% filter(privalVal=="Low") %>% transform(category=if_else(category==0,"False","True")) %>% 
  group_by(category) %>% summarise(mn=mean(googPct)) %>% 
  pivot_wider(names_from = category, values_from = c(mn)) %>% transform(delta=True-False) -> xBar

# now calculate the sampling distribution of the difference under independence by permuting the label
# set a seed to make deterministic
set.seed(1234)
vpnDat %>% filter(privalVal=="Low") %>% transform(category=if_else(category==0,"False","True")) -> vpnLong
nullMeanList <- c()
for (i in 1:nrow(vpnLong)){
  vpnLong$permCat <- sample(vpnLong$category,nrow(vpnLong),replace=FALSE)
  vpnLong %>% group_by(permCat) %>% summarise(mn=mean(googPct)) %>% 
    pivot_wider(names_from = permCat, values_from = c(mn)) %>% transform(delta=True-False) -> tmp
  nullMeanList <- c(nullMeanList,tmp$delta)
}
ggplot() + geom_density(aes(x=nullMeanList),fill="gray",alpha=.5) +
  geom_vline(xintercept = xBar$delta,color="red",size=1.2) +
  labs(x="Difference in % Hoogle Usage (VPN - No VPN)",y="Density",title="Sampling Distribution under Null (Lower Privacy Preference)") +
  theme(
    panel.background = element_rect(fill = bgFill),
    plot.title = element_text(color =basePoint,hjust = 0.5),
    plot.background = element_rect(fill = bgFill),
    panel.grid.major = element_line(color = rgb(0, 0, 0, alpha = 0.3)),
    panel.grid.minor = element_line(color = rgb(0, 0, 0, alpha = 0.3)),
    axis.text = element_text(color =basePoint),
    axis.title = element_text(color =basePoint),
    legend.background = element_rect(fill = bgFill),
    legend.text = element_text(color =basePoint))

c(rep("permute",length(nullMeanList)),"observed") -> labVec
c(nullMeanList,xBar$delta) -> valVec
data.frame(label=labVec,value=valVec) -> plotDat

plotDat %>% arrange(value) %>% transform(idx=1:nrow(plotDat)) -> plotDat
# now calculate the p-value
(1:nrow(plotDat))[plotDat$label=="observed"] -> obsIdx
# what percentage of the data is smaller
pVal <- min(obsIdx/nrow(plotDat),1 - obsIdx/nrow(plotDat))
# now save data
labl <- c(labl,"low privacy - VPN")
loQ <- c(loQ,quantile(nullMeanList,.025))
hiQ <- c(hiQ,quantile(nullMeanList,.975))
effSize <- c(effSize,xBar$delta)
pTmp <- perm_pvals(plotDat)

pVals <- c(pVals,pVal)
pValsApprox <- c(pValsApprox,pTmp$p_approx)
pValsPlus1 <- c(pValsPlus1,pTmp$p_plus1)
pFloor <- c(pFloor,pTmp$p_floor)
nPerms <- c(nPerms,pTmp$B)

# now repeat the analysis for high privacy
vpnDat %>% filter(privalVal=="High") %>% transform(category=if_else(category==0,"False","True")) %>% 
  group_by(category) %>% summarise(mn=mean(googPct)) %>% 
  pivot_wider(names_from = category, values_from = c(mn)) %>% transform(delta=True-False) -> xBar


set.seed(67869)
vpnDat %>% filter(privalVal=="High") %>% transform(category=if_else(category==0,"False","True")) -> vpnLong
nullMeanList <- c()
for (i in 1:nrow(vpnLong)){
  vpnLong$permCat <- sample(vpnLong$category,nrow(vpnLong),replace=FALSE)
  vpnLong %>% group_by(permCat) %>% summarise(mn=mean(googPct)) %>% 
    pivot_wider(names_from = permCat, values_from = c(mn)) %>% transform(delta=True-False) -> tmp
  nullMeanList <- c(nullMeanList,tmp$delta)
}
ggplot() + geom_density(aes(x=nullMeanList),fill="gray",alpha=.5) +
  geom_vline(xintercept = xBar$delta,color="red",size=1.2) +
  labs(x="Difference in % Hoogle Usage (VPN - No VPN)",y="Density",title="Sampling Distribution under Null (Lower Privacy Preference)") +
  theme(
    panel.background = element_rect(fill = bgFill),
    plot.title = element_text(color =basePoint,hjust = 0.5),
    plot.background = element_rect(fill = bgFill),
    panel.grid.major = element_line(color = rgb(0, 0, 0, alpha = 0.3)),
    panel.grid.minor = element_line(color = rgb(0, 0, 0, alpha = 0.3)),
    axis.text = element_text(color =basePoint),
    axis.title = element_text(color =basePoint),
    legend.background = element_rect(fill = bgFill),
    legend.text = element_text(color =basePoint))

c(rep("permute",length(nullMeanList)),"observed") -> labVec
c(nullMeanList,xBar$delta) -> valVec
data.frame(label=labVec,value=valVec) -> plotDat

plotDat %>% arrange(value) %>% transform(idx=1:nrow(plotDat)) -> plotDat
# now calculate the p-value
(1:nrow(plotDat))[plotDat$label=="observed"] -> obsIdx
# what percentage of the data is smaller
pVal <- min(obsIdx/nrow(plotDat),1 - obsIdx/nrow(plotDat))

labl <- c(labl,"high privacy - VPN")
loQ <- c(loQ,quantile(nullMeanList,.025))
hiQ <- c(hiQ,quantile(nullMeanList,.975))
effSize <- c(effSize,xBar$delta)
pTmp <- perm_pvals(plotDat)

pVals <- c(pVals,pVal)
pValsApprox <- c(pValsApprox,pTmp$p_approx)
pValsPlus1 <- c(pValsPlus1,pTmp$p_plus1)
pFloor <- c(pFloor,pTmp$p_floor)
nPerms <- c(nPerms,pTmp$B)


vpnDat %>% filter(privalVal=="Low") %>% transform(category=if_else(category==0,"False","True")) %>%
  ggplot() + geom_density(aes(x=googPct, linetype=category),alpha=.2) +
  xlim(x0,x1) + labs(x="% Hoogle Usage",y="Density",title=paste0("Lower Privacy Preference (d=",round(effSize[[1]],1),"pp, p ",pFloor[[1]],")"),linetype="VPN") +
  theme(
    panel.background = element_rect(fill = bgFill),
    plot.title = element_text(color =basePoint,hjust = 0.5),
    plot.background = element_rect(fill = bgFill),
    panel.grid.major = element_line(color = rgb(0, 0, 0, alpha = 0.3)),
    panel.grid.minor = element_line(color = rgb(0, 0, 0, alpha = 0.3)),
    axis.text = element_text(color =basePoint),
    axis.title = element_text(color =basePoint),
    legend.background = element_rect(fill = bgFill),
    legend.text = element_text(color =basePoint))-> loVPN

vpnDat%>% filter(privalVal=="High") %>% transform(category=if_else(category==0,"False","True")) %>% 
  ggplot() + geom_density(aes(x=googPct, linetype=category),alpha=.2) + 
  xlim(x0,x1) + labs(x="% Hoogle Usage",y="Density",title=paste0("Higher Privacy Preference (d=",round(effSize[[2]],1),"pp, p ",pFloor[[2]],")"),linetype="VPN") +
  theme(
    panel.background = element_rect(fill = bgFill),
    plot.title = element_text(color =basePoint,hjust = 0.5),
    plot.background = element_rect(fill = bgFill),
    panel.grid.major = element_line(color = rgb(0, 0, 0, alpha = 0.3)),
    panel.grid.minor = element_line(color = rgb(0, 0, 0, alpha = 0.3)),
    axis.text = element_text(color =basePoint),
    axis.title = element_text(color =basePoint),
    legend.background = element_rect(fill = bgFill),
    legend.text = element_text(color =basePoint))-> hiVPN

plot_grid(loVPN, hiVPN, align = "v", axis = "lr", ncol = 1)
ggsave("~/ResearchCode/antiTrustImages/vpnDist.png",width=7,height=7,bg=bgFill)
### Deletion ###

controlSmry %>% transform(category=1*(vpnTick==50)+2*(deletionTick==50)+4*(sharingTick==50)) %>% 
  group_by(seed1,privalVal,category) %>% filter(category %in% c(0,2)) %>% transform(category=as.factor(category)) -> deletionDat


# now let's run the significance tests
deletionDat %>% filter(privalVal=="Low") %>% transform(category=if_else(category==0,"False","True")) %>% 
  group_by(category) %>% summarise(mn=mean(googPct)) %>% 
  pivot_wider(names_from = category, values_from = c(mn)) %>% transform(delta=True-False) -> xBar
set.seed(2468)
deletionDat %>% filter(privalVal=="Low") %>% transform(category=if_else(category==0,"False","True")) -> delLong
nullMeanList <- c()
for (i in 1:nrow(delLong)){
  delLong$permCat <- sample(delLong$category,nrow(delLong),replace=FALSE)
  delLong %>% group_by(permCat) %>% summarise(mn=mean(googPct)) %>% 
    pivot_wider(names_from = permCat, values_from = c(mn)) %>% transform(delta=True-False) -> tmp
  nullMeanList <- c(nullMeanList,tmp$delta)
}
ggplot() + geom_density(aes(x=nullMeanList),fill="gray",alpha=.5) +
  geom_vline(xintercept = xBar$delta,color="red",size=1.2) +
  labs(x="Difference in % Hoogle Usage (Deletion - No Deletion)",y="Density",title="Sampling Distribution under Null (Lower Privacy Preference)") +
  theme(
    panel.background = element_rect(fill = bgFill),
    plot.title = element_text(color =basePoint,hjust = 0.5),
    plot.background = element_rect(fill = bgFill),
    panel.grid.major = element_line(color = rgb(0, 0, 0, alpha = 0.3)),
    panel.grid.minor = element_line(color = rgb(0, 0, 0, alpha = 0.3)),
    axis.text = element_text(color =basePoint),
    axis.title = element_text(color =basePoint),
    legend.background = element_rect(fill = bgFill),
    legend.text = element_text(color =basePoint))

c(rep("permute",length(nullMeanList)),"observed") -> labVec
c(nullMeanList,xBar$delta) -> valVec
data.frame(label=labVec,value=valVec) -> plotDat
plotDat %>% arrange(value) %>% transform(idx=1:nrow(plotDat)) -> plotDat
# now calculate the p-value
(1:nrow(plotDat))[plotDat$label=="observed"] -> obsIdx
# what percentage of the data is smaller
pVal <- min(obsIdx/nrow(plotDat),1 - obsIdx/nrow(plotDat))

labl <- c(labl,"low privacy - Deletion")
loQ <- c(loQ,quantile(nullMeanList,.025))
hiQ <- c(hiQ,quantile(nullMeanList,.975))
effSize <- c(effSize,xBar$delta)
pTmp <- perm_pvals(plotDat)

pVals <- c(pVals,pVal)
pValsApprox <- c(pValsApprox,pTmp$p_approx)
pValsPlus1 <- c(pValsPlus1,pTmp$p_plus1)
pFloor <- c(pFloor,pTmp$p_floor)
nPerms <- c(nPerms,pTmp$B)


# now do the same thing for higher privacy preference

deletionDat %>% filter(privalVal=="High") %>% transform(category=if_else(category==0,"False","True")) %>% 
  group_by(category) %>% summarise(mn=mean(googPct)) %>% 
  pivot_wider(names_from = category, values_from = c(mn)) %>% transform(delta=True-False) -> xBar
set.seed(243)
deletionDat %>% filter(privalVal=="High") %>% transform(category=if_else(category==0,"False","True")) -> delLong
nullMeanList <- c()
for (i in 1:nrow(delLong)){
  delLong$permCat <- sample(delLong$category,nrow(delLong),replace=FALSE)
  delLong %>% group_by(permCat) %>% summarise(mn=mean(googPct)) %>% 
    pivot_wider(names_from = permCat, values_from = c(mn)) %>% transform(delta=True-False) -> tmp
  nullMeanList <- c(nullMeanList,tmp$delta)
}
ggplot() + geom_density(aes(x=nullMeanList),fill="gray",alpha=.5) +
  geom_vline(xintercept = xBar$delta,color="red",size=1.2) +
  labs(x="Difference in % Hoogle Usage (Deletion - No Deletion)",y="Density",title="Sampling Distribution under Null (Higher Privacy Preference)") +
  theme(
    panel.background = element_rect(fill = bgFill),
    plot.title = element_text(color =basePoint,hjust = 0.5),
    plot.background = element_rect(fill = bgFill),
    panel.grid.major = element_line(color = rgb(0, 0, 0, alpha = 0.3)),
    panel.grid.minor = element_line(color = rgb(0, 0, 0, alpha = 0.3)),
    axis.text = element_text(color =basePoint),
    axis.title = element_text(color =basePoint),
    legend.background = element_rect(fill = bgFill),
    legend.text = element_text(color =basePoint))

c(rep("permute",length(nullMeanList)),"observed") -> labVec
c(nullMeanList,xBar$delta) -> valVec
data.frame(label=labVec,value=valVec) -> plotDat
plotDat %>% arrange(value) %>% transform(idx=1:nrow(plotDat)) -> plotDat
# now calculate the p-value
(1:nrow(plotDat))[plotDat$label=="observed"] -> obsIdx
# what percentage of the data is smaller
pVal <- min(obsIdx/nrow(plotDat),1 - obsIdx/nrow(plotDat))

labl <- c(labl,"high privacy - Deletion")
loQ <- c(loQ,quantile(nullMeanList,.025))
hiQ <- c(hiQ,quantile(nullMeanList,.975))
effSize <- c(effSize,xBar$delta)
pTmp <- perm_pvals(plotDat)

pVals <- c(pVals,pVal)
pValsApprox <- c(pValsApprox,pTmp$p_approx)
pValsPlus1 <- c(pValsPlus1,pTmp$p_plus1)
pFloor <- c(pFloor,pTmp$p_floor)
nPerms <- c(nPerms,pTmp$B)



x0 <- min(deletionDat$googPct)
x1 <- max(deletionDat$googPct)
deletionDat %>% filter(privalVal=="Low") %>% transform(category=if_else(category==0,"False","True")) %>%
  ggplot() + geom_density(aes(x=googPct, linetype=category),alpha=.2) +
  xlim(x0,x1) + labs(x="% Hoogle Usage",y="Density",title=paste0("Lower Privacy Preference (d=",round(effSize[[3]],1),"pp, p ",pFloor[[3]],")"),linetype="Deletion") +
  theme(
    panel.background = element_rect(fill = bgFill),
    plot.title = element_text(color =basePoint,hjust = 0.5),
    plot.background = element_rect(fill = bgFill),
    panel.grid.major = element_line(color = rgb(0, 0, 0, alpha = 0.3)),
    panel.grid.minor = element_line(color = rgb(0, 0, 0, alpha = 0.3)),
    axis.text = element_text(color =basePoint),
    axis.title = element_text(color =basePoint),
    legend.background = element_rect(fill = bgFill),
    legend.text = element_text(color =basePoint))-> loDel

deletionDat%>% filter(privalVal=="High") %>% transform(category=if_else(category==0,"False","True")) %>% 
  ggplot() + geom_density(aes(x=googPct, linetype=category),alpha=.2) + 
  xlim(x0,x1) + labs(x="% Hoogle Usage",y="Density",title=paste0("Higher Privacy Preference (d=",round(effSize[[4]],1),"pp, p ",pFloor[[4]],")"),linetype="Deletion") +
  theme(
    panel.background = element_rect(fill = bgFill),
    plot.title = element_text(color =basePoint,hjust = 0.5),
    plot.background = element_rect(fill = bgFill),
    panel.grid.major = element_line(color = rgb(0, 0, 0, alpha = 0.3)),
    panel.grid.minor = element_line(color = rgb(0, 0, 0, alpha = 0.3)),
    axis.text = element_text(color =basePoint),
    axis.title = element_text(color =basePoint),
    legend.background = element_rect(fill = bgFill),
    legend.text = element_text(color =basePoint))-> hiDel

plot_grid(loDel, hiDel, align = "v", axis = "lr", ncol = 1)
ggsave("~/ResearchCode/antiTrustImages/deletionDist.png",width=7,height=7,bg=bgFill)
### Sharing ###
# significance testing




controlSmry %>% transform(category=1*(vpnTick==50)+2*(deletionTick==50)+4*(sharingTick==50)) %>% 
  group_by(seed1,privalVal,category) %>% filter(category %in% c(0,4)) %>% transform(category=as.factor(category)) -> sharingDat

sharingDat %>% filter(privalVal=="Low") %>% transform(category=if_else(category==0,"False","True")) %>% 
  group_by(category) %>% summarise(mn=mean(googPct)) %>% 
  pivot_wider(names_from = category, values_from = c(mn)) %>% transform(delta=True-False) -> xBar

set.seed(1357)
sharingDat %>% filter(privalVal=="Low") %>% transform(category=if_else(category==0,"False","True")) -> shareLong
nullMeanList <- c()
for (i in 1:nrow(shareLong)){
  shareLong$permCat <- sample(shareLong$category,nrow(shareLong),replace=FALSE)
  shareLong %>% group_by(permCat) %>% summarise(mn=mean(googPct)) %>% 
    pivot_wider(names_from = permCat, values_from = c(mn)) %>% transform(delta=True-False) -> tmp
  nullMeanList <- c(nullMeanList,tmp$delta)
}
ggplot() + geom_density(aes(x=nullMeanList),fill="gray",alpha=.5) +
  geom_vline(xintercept = xBar$delta,color="red",size=1.2) +
  labs(x="Difference in % Hoogle Usage (Sharing - No Sharing)",y="Density",title="Sampling Distribution under Null (Lower Privacy Preference)") +
  theme(
    panel.background = element_rect(fill = bgFill),
    plot.title = element_text(color =basePoint,hjust = 0.5),
    plot.background = element_rect(fill = bgFill),
    panel.grid.major = element_line(color = rgb(0, 0, 0, alpha = 0.3)),
    panel.grid.minor = element_line(color = rgb(0, 0, 0, alpha = 0.3)),
    axis.text = element_text(color =basePoint),
    axis.title = element_text(color =basePoint),
    legend.background = element_rect(fill = bgFill),
    legend.text = element_text(color =basePoint))
c(rep("permute",length(nullMeanList)),"observed") -> labVec
c(nullMeanList,xBar$delta) -> valVec
data.frame(label=labVec,value=valVec) -> plotDat
plotDat %>% arrange(value) %>% transform(idx=1:nrow(plotDat)) -> plotDat
# now calculate the p-value
(1:nrow(plotDat))[plotDat$label=="observed"] -> obsIdx
# what percentage of the data is smaller
pVal <- min(obsIdx/nrow(plotDat),1 - obsIdx/nrow(plotDat))

labl <- c(labl,"low privacy - Sharing")
loQ <- c(loQ,quantile(nullMeanList,.025))
hiQ <- c(hiQ,quantile(nullMeanList,.975))
effSize <- c(effSize,xBar$delta)
pTmp <- perm_pvals(plotDat)

pVals <- c(pVals,pVal)
pValsApprox <- c(pValsApprox,pTmp$p_approx)
pValsPlus1 <- c(pValsPlus1,pTmp$p_plus1)
pFloor <- c(pFloor,pTmp$p_floor)
nPerms <- c(nPerms,pTmp$B)



# now for high privacy preference
sharingDat %>% filter(privalVal=="High") %>% transform(category=if_else(category==0,"False","True")) %>% 
  group_by(category) %>% summarise(mn=mean(googPct)) %>% 
  pivot_wider(names_from = category, values_from = c(mn)) %>% transform(delta=True-False) -> xBar

set.seed(1357)
sharingDat %>% filter(privalVal=="High") %>% transform(category=if_else(category==0,"False","True")) -> shareLong
nullMeanList <- c()
for (i in 1:nrow(shareLong)){
  shareLong$permCat <- sample(shareLong$category,nrow(shareLong),replace=FALSE)
  shareLong %>% group_by(permCat) %>% summarise(mn=mean(googPct)) %>% 
    pivot_wider(names_from = permCat, values_from = c(mn)) %>% transform(delta=True-False) -> tmp
  nullMeanList <- c(nullMeanList,tmp$delta)
}
ggplot() + geom_density(aes(x=nullMeanList),fill="gray",alpha=.5) +
  geom_vline(xintercept = xBar$delta,color="red",size=1.2) +
  labs(x="Difference in % Hoogle Usage (Sharing - No Sharing)",y="Density",title="Sampling Distribution under Null (Higher Privacy Preference)") +
  theme(
    panel.background = element_rect(fill = bgFill),
    plot.title = element_text(color =basePoint,hjust = 0.5),
    plot.background = element_rect(fill = bgFill),
    panel.grid.major = element_line(color = rgb(0, 0, 0, alpha = 0.3)),
    panel.grid.minor = element_line(color = rgb(0, 0, 0, alpha = 0.3)),
    axis.text = element_text(color =basePoint),
    axis.title = element_text(color =basePoint),
    legend.background = element_rect(fill = bgFill),
    legend.text = element_text(color =basePoint))
c(rep("permute",length(nullMeanList)),"observed") -> labVec
c(nullMeanList,xBar$delta) -> valVec
data.frame(label=labVec,value=valVec) -> plotDat
plotDat %>% arrange(value) %>% transform(idx=1:nrow(plotDat)) -> plotDat
# now calculate the p-value
(1:nrow(plotDat))[plotDat$label=="observed"] -> obsIdx
# what percentage of the data is smaller
pVal <- min(obsIdx/nrow(plotDat),1 - obsIdx/nrow(plotDat))

labl <- c(labl,"high privacy - Sharing")
loQ <- c(loQ,quantile(nullMeanList,.025))
hiQ <- c(hiQ,quantile(nullMeanList,.975))
effSize <- c(effSize,xBar$delta)
pTmp <- perm_pvals(plotDat)

pVals <- c(pVals,pVal)
pValsApprox <- c(pValsApprox,pTmp$p_approx)
pValsPlus1 <- c(pValsPlus1,pTmp$p_plus1)
pFloor <- c(pFloor,pTmp$p_floor)
nPerms <- c(nPerms,pTmp$B)


# now, we need to caclculate the effect size for low privacy value
sharingDat %>% filter(privalVal=="Low") %>% transform(category=if_else(category==0,"False","True")) %>% 
  group_by(category) %>% summarise(mn=mean(googPct)) %>% 
  pivot_wider(names_from = category, values_from = c(mn)) %>% transform(delta=True-False) -> xBar

set.seed(1357)
sharingDat %>% filter(privalVal=="Low") %>% transform(category=if_else(category==0,"False","True")) -> shareLong
nullMeanList <- c()
for (i in 1:nrow(shareLong)){
  shareLong$permCat <- sample(shareLong$category,nrow(shareLong),replace=FALSE)
  shareLong %>% group_by(permCat) %>% summarise(mn=mean(googPct)) %>% 
    pivot_wider(names_from = permCat, values_from = c(mn)) %>% transform(delta=True-False) -> tmp
  nullMeanList <- c(nullMeanList,tmp$delta)
}

x0 <- min(sharingDat$googPct)
x1 <- max(sharingDat$googPct)
sharingDat %>% filter(privalVal=="Low") %>% transform(category=if_else(category==0,"False","True")) %>%
  ggplot() + geom_density(aes(x=googPct, linetype=category),alpha=.2) +
  xlim(x0,x1) + labs(x="% Hoogle Usage",y="Density",title=paste0("Lower Privacy Preference (d=",round(effSize[[5]],1),"pp, p ",pFloor[[5]],")"),linetype="Sharing") +
  theme(
    panel.background = element_rect(fill = bgFill),
    plot.title = element_text(color =basePoint,hjust = 0.5),
    plot.background = element_rect(fill = bgFill),
    panel.grid.major = element_line(color = rgb(0, 0, 0, alpha = 0.3)),
    panel.grid.minor = element_line(color = rgb(0, 0, 0, alpha = 0.3)),
    axis.text = element_text(color =basePoint),
    axis.title = element_text(color =basePoint),
    legend.background = element_rect(fill = bgFill),
    legend.text = element_text(color =basePoint))-> loSharing

sharingDat%>% filter(privalVal=="High") %>% transform(category=if_else(category==0,"False","True")) %>% 
  ggplot() + geom_density(aes(x=googPct, linetype=category),alpha=.2) + 
  xlim(x0,x1) + labs(x="% Hoogle Usage",y="Density",title=paste0("Higher Privacy Preference (d=",round(effSize[[6]],1),"pp, p ",pFloor[[6]],")"),linetype="Sharing") +
  theme(
    panel.background = element_rect(fill = bgFill),
    plot.title = element_text(color =basePoint,hjust = 0.5),
    plot.background = element_rect(fill = bgFill),
    panel.grid.major = element_line(color = rgb(0, 0, 0, alpha = 0.3)),
    panel.grid.minor = element_line(color = rgb(0, 0, 0, alpha = 0.3)),
    axis.text = element_text(color =basePoint),
    axis.title = element_text(color =basePoint),
    legend.background = element_rect(fill = bgFill),
    legend.text = element_text(color =basePoint))-> hiSharing

plot_grid(loSharing, hiSharing, align = "v", axis = "lr", ncol = 1)
ggsave("~/ResearchCode/antiTrustImages/sharingDist.png",width=7,height=7,bg=bgFill)

### now agent level analysis ###

outDat %>% filter(tick >= 50) %>% group_by(key,agent,currEngine) %>% summarise(cnt=n()) %>%
  pivot_wider(names_from = currEngine, values_from = cnt, values_fill = 0) %>%
  transform(total=google+duckDuckGo) %>% transform(googPct=100*google/total) -> agtSmry
merge(agentDat,agtSmry,by=c("key","agent")) -> agtSmry
merge(control,agtSmry,by="key") -> agtSmry

### VPN ###

agtSmry %>% transform(category=1*(vpnTick==50)+2*(deletionTick==50)+4*(sharingTick==50)) %>%
  filter(category %in% c(0,1)) -> agtVPN
agtVPN$privDex <- (agtVPN$blissPoint/agtVPN$unifExp)



# now, get a moving average and quantiles

# first pool the 

window <- 50

datList <- list()

for (t in (window/2):(10000-window/2)){
  loRng <- (t - window/(2))/10000
  hiRng <- (t + window/(2))/10000
  agtVPN %>% filter(privDex >= loRng & privDex <= hiRng) %>% group_by(category) %>%
    summarize(
      q05 = quantile(googPct, 0.05),
      q25 = quantile(googPct, 0.25),
      q50 = quantile(googPct, 0.50),
      mn = mean(googPct),
      q75 = quantile(googPct, 0.75),
      q95 = quantile(googPct, 0.95)) -> tmp
  tmp$privDex <- t/10000
  datList[[length(datList) + 1]] <- tmp
}

rbindlist(datList) -> smryFrame

# now significance testing
# we first calculate the integral of the absolute difference between the two curves

smryFrame %>% filter(category %in% c(0,1)) %>% select(privDex,category,mn) %>%
  pivot_wider(names_from = category, values_from = mn) %>%
  transform(absDiff=abs(`0` - `1`)) -> diffFrame

# set a seed for montecarlo integration
set.seed(123443)
nSamps <- 10000
# get X range
xLo <- min(diffFrame$privDex)
xHi <- max(diffFrame$privDex)
# get Y range
yLo <- 0
yHi <- max(diffFrame$absDiff)
# sample points
runif(nSamps, xLo, xHi) -> xSamps
runif(nSamps, yLo, yHi) -> ySamps
# now see how many are under the curve
sum(sapply(1:nSamps, function(i){
  # get the curve value at this x
  curveY <- diffFrame$absDiff[which.min(abs(diffFrame$privDex -
                                               xSamps[i]))]
  if (ySamps[i] <= curveY){
    return (1)
  } else {
    return (0)
  }
})) -> nUnder
# now estimate the integral
areaRect <- (xHi - xLo) * (yHi - yLo)

integralEst <- areaRect * (nUnder / nSamps)

# now let's bootstrap the sampling distribution of this integral under the null hypothesis
nullIntList <- c()
set.seed(98765)
# now, we permute the category labels within each key and 
# rebuild the moving average curves
# and then integrate the difference
vpnTest <- function(i){
 agtVPN %>% group_by(key) %>% summarise(category=max(category)) -> allCats
allCats$permCat <- sample(allCats$category,nrow(allCats),replace=FALSE)
merge(agtVPN,allCats[,c("key","permCat")],by="key") -> permAgt
# now, get a moving average and quantiles
window <- 50
datList <- list()
for (t in (window/2):(10000-window/2)){
  loRng <- (t - window/(2))/10000
  hiRng <- (t + window/(2))/10000
  permAgt %>% filter(privDex >= loRng & privDex <= hiRng) %>% group_by(permCat) %>%
    summarize(
      mn = mean(googPct)) -> tmp
  tmp$privDex <- t/10000
  datList[[length(datList) + 1]] <- tmp
}
rbindlist(datList) -> permSmryFrame
permSmryFrame %>% filter(permCat %in% c(0,1)) %>% select(privDex,permCat,mn) %>%
  pivot_wider(names_from = permCat, values_from = mn) %>%
  transform(absDiff=abs(`0` - `1`)) -> permDiffFrame
# set a seed for montecarlo integration
set.seed(123443 + i)
nSamps <- 10000
# get X range
xLo <- min(permDiffFrame$privDex)
xHi <- max(permDiffFrame$privDex)
# get Y range
yLo <- 0
yHi <- max(permDiffFrame$absDiff)
# sample points
runif(nSamps, xLo, xHi) -> xSamps
runif(nSamps, yLo, yHi) -> ySamps
# now see how many are under the curve
sum(sapply(1:nSamps, function(i){
  # get the curve value at this x
  curveY <- permDiffFrame$absDiff[which.min(abs(permDiffFrame$privDex -
                                               xSamps[i]))]
  if (ySamps[i] <= curveY){
    return (1)
  } else {
    return (0)
  }
})) -> nUnder
# now estimate the integral
areaRect <- (xHi - xLo) * (yHi - yLo)
nullInt <- areaRect * (nUnder / nSamps)
return(nullInt)
}
unlist(mclapply(1:1000,vpnTest,mc.cores=16)) -> nullIntList
c(rep("permute",length(nullIntList)),"observed") -> labVec
c(nullIntList,integralEst) -> valVec
data.frame(label=labVec,value=valVec) -> plotDat
plotDat %>% arrange(value) %>% transform(idx=1:nrow(plotDat)) -> plotDat
# now calculate the p-value
(1:nrow(plotDat))[plotDat$label=="observed"] -> obsIdx
# what percentage of the data is smaller
pVal <- min(obsIdx/nrow(plotDat),1 - obsIdx/nrow(plotDat))


labl <- c(labl,"Privacy - VPN")
loQ <- c(loQ,quantile(nullIntList,.025))
hiQ <- c(hiQ,quantile(nullIntList,.975))
effSize <- c(effSize,integralEst)
pTmp <- perm_pvals(plotDat)

pVals <- c(pVals,pVal)
pValsApprox <- c(pValsApprox,pTmp$p_approx)
pValsPlus1 <- c(pValsPlus1,pTmp$p_plus1)
pFloor <- c(pFloor,pTmp$p_floor)
nPerms <- c(nPerms,pTmp$B)


smryFrame %>% filter(category == 0) %>%
  ggplot() + geom_line(aes(x=privDex,y=mn),size=1.2) +
  geom_line(aes(x=privDex,y=q05),alpha=.8,linetype="dotted",size=1) +
  geom_line(aes(x=privDex,y=q95),alpha=.8,linetype="dotted",size=1) +
  geom_ribbon(aes(x=privDex,ymin=q25,ymax=q75),alpha=.4) +
  geom_line(aes(x=privDex,y=q50),linetype="dotted",size=1.2) +
  theme(
    panel.background = element_rect(fill = bgFill),
    plot.title = element_text(color =basePoint,hjust = 0.5),
    plot.background = element_rect(fill = bgFill),
    panel.grid.major = element_line(color = rgb(0, 0, 0, alpha = 0.3)),
    panel.grid.minor = element_line(color = rgb(0, 0, 0, alpha = 0.3)),
    axis.text = element_text(color =basePoint),
    axis.title = element_text(color =basePoint),
    legend.background = element_rect(fill = bgFill),
    legend.text = element_text(color =basePoint)) +
  labs(x="Agent Privacy Index",y="% Hoogle Usage",title="Hoogle Usage With Respect to Agent Privacy Preference")  + ylim(0,100)
ggsave("~/ResearchCode/antiTrustImages/base.png",width=12,height=6,bg=bgFill)


smryFrame %>% filter(category == 0) %>%
  ggplot() + geom_line(aes(x=privDex,y=mn),size=1.2) +
  geom_line(aes(x=privDex,y=q05),alpha=.8,linetype="dotted",size=1) +
  geom_line(aes(x=privDex,y=q95),alpha=.8,linetype="dotted",size=1) +
  geom_ribbon(aes(x=privDex,ymin=q25,ymax=q75),alpha=.4) +
  geom_line(aes(x=privDex,y=q50),linetype="dotted",size=1.2) +
  theme(
    panel.background = element_rect(fill = bgFill),
    plot.title = element_text(color =basePoint,hjust = 0.5),
    plot.background = element_rect(fill = bgFill),
    panel.grid.major = element_line(color = rgb(0, 0, 0, alpha = 0.3)),
    panel.grid.minor = element_line(color = rgb(0, 0, 0, alpha = 0.3)),
    axis.text = element_text(color =basePoint),
    axis.title = element_text(color =basePoint),
    legend.background = element_rect(fill = bgFill),
    legend.text = element_text(color =basePoint)) +
  labs(x="Agent Privacy Index",y="% Hoogle Usage",title="No VPN")  + ylim(0,100)-> noVpnPlot

smryFrame %>% filter(category == 1) %>%
  ggplot() + geom_line(aes(x=privDex,y=mn),size=1.2) +
  geom_line(aes(x=privDex,y=q05),alpha=.8,linetype="dotted",size=1) +
  geom_line(aes(x=privDex,y=q95),alpha=.8,linetype="dotted",size=1) +
  geom_ribbon(aes(x=privDex,ymin=q25,ymax=q75),alpha=.4) +
  geom_line(aes(x=privDex,y=q50),linetype="dotted",size=1.2) +
  theme(
    panel.background = element_rect(fill = bgFill),
    plot.title = element_text(color =basePoint,hjust = 0.5),
    plot.background = element_rect(fill = bgFill),
    panel.grid.major = element_line(color = rgb(0, 0, 0, alpha = 0.3)),
    panel.grid.minor = element_line(color = rgb(0, 0, 0, alpha = 0.3)),
    axis.text = element_text(color =basePoint),
    axis.title = element_text(color =basePoint),
    legend.background = element_rect(fill = bgFill),
    legend.text = element_text(color =basePoint)) +
  labs(x="Agent Privacy Index",y="% Hoogle Usage",title="VPN") + ylim(0,100) -> vpnPlot

plot_grid(noVpnPlot, vpnPlot, align = "h", axis = "lr", ncol = 2) -> grid


title <- ggdraw() +
  draw_label(
    paste0("Hoogle Usage (d=",round(effSize[[7]],3),", p ",pFloor[[7]],")"),
    fontface = "bold",
    size = 14,
    hjust = 0.5
  )

plot_grid(
  title,
  grid,
  ncol = 1,
  rel_heights = c(0.12, 1)
)
ggsave("~/ResearchCode/antiTrustImages/vpnAgent.png",width=12,height=6,bg=bgFill)


#plot(diffFrame$privDex,diffFrame$absDiff,type="l",xlab="Agent Privacy Index",ylab="Absolute Difference in % Hoogle Usage",main="Absolute Difference between VPN and No VPN")


### Deletion ###
agtSmry %>% transform(category=1*(vpnTick==50)+2*(deletionTick==50)+4*(sharingTick==50)) %>%
  filter(category %in% c(0,2)) -> agtDel
agtDel$privDex <- (agtDel$blissPoint/agtDel$unifExp)
# now, let's repeat the analysis here



# now, get a moving average and quantiles
window <- 50

datList <- list()

for (t in (window/2):(10000-window/2)){
  loRng <- (t - window/(2))/10000
  hiRng <- (t + window/(2))/10000
  agtDel %>% filter(privDex >= loRng & privDex <= hiRng) %>% group_by(category) %>%
    summarize(
      q05 = quantile(googPct, 0.05),
      q25 = quantile(googPct, 0.25),
      q50 = quantile(googPct, 0.50),
      mn = mean(googPct),
      q75 = quantile(googPct, 0.75),
      q95 = quantile(googPct, 0.95)) -> tmp
  tmp$privDex <- t/10000
  datList[[length(datList) + 1]] <- tmp
}

rbindlist(datList) -> smryFrame


# now get the observed difference between the two moving averages
smryFrame %>% filter(category %in% c(0,2)) %>% select(privDex,category,mn) %>%
  pivot_wider(names_from = category, values_from = mn) %>%
  transform(absDiff=abs(`0` - `2`)) -> diffFrame

# now set a seed for montecarlo integration
set.seed(56789)
nSamps <- 10000
# get X range
xLo <- min(diffFrame$privDex)
xHi <- max(diffFrame$privDex)
# get Y range
yLo <- 0
yHi <- max(diffFrame$absDiff)
# sample points
runif(nSamps, xLo, xHi) -> xSamps
runif(nSamps, yLo, yHi) -> ySamps
# now see how many are under the curve
sum(sapply(1:nSamps, function(i){
  # get the curve value at this x
  curveY <- diffFrame$absDiff[which.min(abs(diffFrame$privDex -
                                               xSamps[i]))]
  if (ySamps[i] <= curveY){
    return (1)
  } else {
    return (0)
  }
})) -> nUnder
# now estimate the integral
areaRect <- (xHi - xLo) * (yHi - yLo)
integralEst <- areaRect * (nUnder / nSamps)

delTest <- function(i){
  agtDel %>% group_by(key) %>% summarise(category=max(category)) -> allCats
  allCats$permCat <- sample(allCats$category,nrow(allCats),replace=FALSE)
  merge(agtDel,allCats[,c("key","permCat")],by="key") -> permAgt
  # now, get a moving average and quantiles
  window <- 50
  datList <- list()
  for (t in (window/2):(10000-window/2)){
    loRng <- (t - window/(2))/10000
    hiRng <- (t + window/(2))/10000
    permAgt %>% filter(privDex >= loRng & privDex <= hiRng) %>% group_by(permCat) %>%
      summarize(
        mn = mean(googPct)) -> tmp
    tmp$privDex <- t/10000
    datList[[length(datList) + 1]] <- tmp
  }
  rbindlist(datList) -> permSmryFrame
  permSmryFrame %>% filter(permCat %in% c(0,2)) %>% select(privDex,permCat,mn) %>%
    pivot_wider(names_from = permCat, values_from = mn) %>%
    transform(absDiff=abs(`0` - `2`)) -> permDiffFrame
  # set a seed for montecarlo integration
  set.seed(15333443 + i)
  nSamps <- 10000
  # get X range
  xLo <- min(permDiffFrame$privDex)
  xHi <- max(permDiffFrame$privDex)
  # get Y range
  yLo <- 0
  yHi <- max(permDiffFrame$absDiff)
  # sample points
  runif(nSamps, xLo, xHi) -> xSamps
  runif(nSamps, yLo, yHi) -> ySamps
  # now see how many are under the curve
  sum(sapply(1:nSamps, function(i){
    # get the curve value at this x
    curveY <- permDiffFrame$absDiff[which.min(abs(permDiffFrame$privDex -
                                                    xSamps[i]))]
    if (ySamps[i] <= curveY){
      return (1)
    } else {
      return (0)
    }
  })) -> nUnder
  # now estimate the integral
  areaRect <- (xHi - xLo) * (yHi - yLo)
  nullInt <- areaRect * (nUnder / nSamps)
  return(nullInt)
}
unlist(mclapply(1:1000,delTest,mc.cores=16)) -> nullIntList
c(rep("permute",length(nullIntList)),"observed") -> labVec
c(nullIntList,integralEst) -> valVec
data.frame(label=labVec,value=valVec) -> plotDat
plotDat %>% arrange(value) %>% transform(idx=1:nrow(plotDat)) -> plotDat
# now calculate the p-value
(1:nrow(plotDat))[plotDat$label=="observed"] -> obsIdx
# what percentage of the data is smaller
pVal <- min(obsIdx/nrow(plotDat),1 - obsIdx/nrow(plotDat))

labl <- c(labl,"Privacy - Deletion")
loQ <- c(loQ,quantile(nullIntList,.025))
hiQ <- c(hiQ,quantile(nullIntList,.975))
effSize <- c(effSize,integralEst)
pTmp <- perm_pvals(plotDat)

pVals <- c(pVals,pVal)
pValsApprox <- c(pValsApprox,pTmp$p_approx)
pValsPlus1 <- c(pValsPlus1,pTmp$p_plus1)
pFloor <- c(pFloor,pTmp$p_floor)
nPerms <- c(nPerms,pTmp$B)

smryFrame %>% filter(category == 0) %>%
  ggplot() + geom_line(aes(x=privDex,y=mn),size=1.2) +
  geom_line(aes(x=privDex,y=q05),alpha=.8,linetype="dotted",size=1) +
  geom_line(aes(x=privDex,y=q95),alpha=.8,linetype="dotted",size=1) +
  geom_ribbon(aes(x=privDex,ymin=q25,ymax=q75),alpha=.4) +
  geom_line(aes(x=privDex,y=q50),linetype="dotted",size=1.2) +
  theme(
    panel.background = element_rect(fill = bgFill),
    plot.title = element_text(color =basePoint,hjust = 0.5),
    plot.background = element_rect(fill = bgFill),
    panel.grid.major = element_line(color = rgb(0, 0, 0, alpha = 0.3)),
    panel.grid.minor = element_line(color = rgb(0, 0, 0, alpha = 0.3)),
    axis.text = element_text(color =basePoint),
    axis.title = element_text(color =basePoint),
    legend.background = element_rect(fill = bgFill),
    legend.text = element_text(color =basePoint)) +
  labs(x="Agent Privacy Index",y="% Hoogle Usage",title="No Deletion")  + ylim(0,100)-> noDelPlot

smryFrame %>% filter(category == 2) %>%
  ggplot() + geom_line(aes(x=privDex,y=mn),size=1.2) +
  geom_line(aes(x=privDex,y=q05),alpha=.8,linetype="dotted",size=1) +
  geom_line(aes(x=privDex,y=q95),alpha=.8,linetype="dotted",size=1) +
  geom_ribbon(aes(x=privDex,ymin=q25,ymax=q75),alpha=.4) +
  geom_line(aes(x=privDex,y=q50),linetype="dotted",size=1.2) +
  theme(
    panel.background = element_rect(fill = bgFill),
    plot.title = element_text(color =basePoint,hjust = 0.5),
    plot.background = element_rect(fill = bgFill),
    panel.grid.major = element_line(color = rgb(0, 0, 0, alpha = 0.3)),
    panel.grid.minor = element_line(color = rgb(0, 0, 0, alpha = 0.3)),
    axis.text = element_text(color =basePoint),
    axis.title = element_text(color =basePoint),
    legend.background = element_rect(fill = bgFill),
    legend.text = element_text(color =basePoint)) +
  labs(x="Agent Privacy Index",y="% Hoogle Usage",title="Deletion") + ylim(0,100) -> delPlot

plot_grid(noDelPlot, delPlot, align = "h", axis = "lr", ncol = 2) -> grid


title <- ggdraw() +
  draw_label(
    paste0("Hoogle Usage (d=",round(effSize[[8]],3),", p ",pFloor[[8]],")"),
    fontface = "bold",
    size = 14,
    hjust = 0.5
  )

plot_grid(
  title,
  grid,
  ncol = 1,
  rel_heights = c(0.12, 1)
)
ggsave("~/ResearchCode/antiTrustImages/deletionAgent.png",width=12,height=6,bg=bgFill)


### Sharing ###
agtSmry %>% transform(category=1*(vpnTick==50)+2*(deletionTick==50)+4*(sharingTick==50)) %>%
  filter(category %in% c(0,4)) -> agtSharing
agtSharing$privDex <- (agtSharing$blissPoint/agtSharing$unifExp)



# now, get a moving average and quantiles
window <- 50

datList <- list()

for (t in (window/2):(10000-window/2)){
  loRng <- (t - window/(2))/10000
  hiRng <- (t + window/(2))/10000
  agtSharing %>% filter(privDex >= loRng & privDex <= hiRng) %>% group_by(category) %>%
    summarize(
      q05 = quantile(googPct, 0.05),
      q25 = quantile(googPct, 0.25),
      q50 = quantile(googPct, 0.50),
      mn = mean(googPct),
      q75 = quantile(googPct, 0.75),
      q95 = quantile(googPct, 0.95)) -> tmp
  tmp$privDex <- t/10000
  datList[[length(datList) + 1]] <- tmp
}

rbindlist(datList) -> smryFrame
# now get the observed difference between the two moving averages
smryFrame %>% filter(category %in% c(0,4)) %>% select(privDex,category,mn) %>%
  pivot_wider(names_from = category, values_from = mn) %>%
  transform(absDiff=abs(`0` - `4`)) -> diffFrame
# now set a seed for montecarlo integration
set.seed(2468032)
nSamps <- 10000
# get X range
xLo <- min(diffFrame$privDex)
xHi <- max(diffFrame$privDex)
# get Y range
yLo <- 0
yHi <- max(diffFrame$absDiff)
# sample points
runif(nSamps, xLo, xHi) -> xSamps
runif(nSamps, yLo, yHi) -> ySamps
# now see how many are under the curve
sum(sapply(1:nSamps, function(i){
  # get the curve value at this x
  curveY <- diffFrame$absDiff[which.min(abs(diffFrame$privDex -
                                               xSamps[i]))]
  if (ySamps[i] <= curveY){
    return (1)
  } else {
    return (0)  
  }
})) -> nUnder
# now estimate the integral
areaRect <- (xHi - xLo) * (yHi - yLo)
integralEst <- areaRect * (nUnder / nSamps)

# now we need to bootstrap the null distribution
shareTest <- function(i){
  agtSharing %>% group_by(key) %>% summarise(category=max(category)) -> allCats
  allCats$permCat <- sample(allCats$category,nrow(allCats),replace=FALSE)
  merge(agtSharing,allCats[,c("key","permCat")],by="key") -> permAgt
  # now, get a moving average and quantiles
  window <- 50
  datList <- list()
  for (t in (window/2):(10000-window/2)){
    loRng <- (t - window/(2))/10000
    hiRng <- (t + window/(2))/10000
    permAgt %>% filter(privDex >= loRng & privDex <= hiRng) %>% group_by(permCat) %>%
      summarize(
        mn = mean(googPct)) -> tmp
    tmp$privDex <- t/10000
    datList[[length(datList) + 1]] <- tmp
  }
  rbindlist(datList) -> permSmryFrame
  permSmryFrame %>% filter(permCat %in% c(0,4)) %>% select(privDex,permCat,mn) %>%
    pivot_wider(names_from = permCat, values_from = mn) %>%
    transform(absDiff=abs(`0` - `4`)) -> permDiffFrame
  # set a seed for montecarlo integration
  set.seed(3443 + i)
  nSamps <- 10000
  # get X range
  xLo <- min(permDiffFrame$privDex)
  xHi <- max(permDiffFrame$privDex)
  # get Y range
  yLo <- 0
  yHi <- max(permDiffFrame$absDiff)
  # sample points
  runif(nSamps, xLo, xHi) -> xSamps
  runif(nSamps, yLo, yHi) -> ySamps
  # now see how many are under the curve
  sum(sapply(1:nSamps, function(i){
    # get the curve value at this x
    curveY <- permDiffFrame$absDiff[which.min(abs(permDiffFrame$
privDex -
                                                    xSamps[i]))]
    if (ySamps[i] <= curveY){
      return (1)
    } else {
      return (0)
    }
  })) -> nUnder
  # now estimate the integral
  areaRect <- (xHi - xLo) * (yHi - yLo)
  nullInt <- areaRect * (nUnder / nSamps)
  return(nullInt)
}
unlist(mclapply(1:1000,shareTest,mc.cores=16)) -> nullIntList
c(rep("permute",length(nullIntList)),"observed") -> labVec
c(nullIntList,integralEst) -> valVec
data.frame(label=labVec,value=valVec) -> plotDat
plotDat %>% arrange(value) %>% transform(idx=1:nrow(plotDat)) -> plotDat
# now calculate the p-value
(1:nrow(plotDat))[plotDat$label=="observed"] -> obsIdx
# what percentage of the data is smaller
pVal <- min(obsIdx/nrow(plotDat),1 - obsIdx/nrow(plotDat))

labl <- c(labl,"Privacy - Sharing")
loQ <- c(loQ,quantile(nullIntList,.025))
hiQ <- c(hiQ,quantile(nullIntList,.975))
effSize <- c(effSize,integralEst)
pTmp <- perm_pvals(plotDat)

pVals <- c(pVals,pVal)
pValsApprox <- c(pValsApprox,pTmp$p_approx)
pValsPlus1 <- c(pValsPlus1,pTmp$p_plus1)
pFloor <- c(pFloor,pTmp$p_floor)
nPerms <- c(nPerms,pTmp$B)



smryFrame %>% filter(category == 0) %>%
  ggplot() + geom_line(aes(x=privDex,y=mn),size=1.2) +
  geom_line(aes(x=privDex,y=q05),alpha=.8,linetype="dotted",size=1) +
  geom_line(aes(x=privDex,y=q95),alpha=.8,linetype="dotted",size=1) +
  geom_ribbon(aes(x=privDex,ymin=q25,ymax=q75),alpha=.4) +
  geom_line(aes(x=privDex,y=q50),linetype="dotted",size=1.2) +
  theme(
    panel.background = element_rect(fill = bgFill),
    plot.title = element_text(color =basePoint,hjust = 0.5),
    plot.background = element_rect(fill = bgFill),
    panel.grid.major = element_line(color = rgb(0, 0, 0, alpha = 0.3)),
    panel.grid.minor = element_line(color = rgb(0, 0, 0, alpha = 0.3)),
    axis.text = element_text(color =basePoint),
    axis.title = element_text(color =basePoint),
    legend.background = element_rect(fill = bgFill),
    legend.text = element_text(color =basePoint)) +
  labs(x="Agent Privacy Index",y="% Hoogle Usage",title="No Sharing")  + ylim(0,100)-> noSharePlot

smryFrame %>% filter(category == 4) %>%
  ggplot() + geom_line(aes(x=privDex,y=mn),size=1.2) +
  geom_line(aes(x=privDex,y=q05),alpha=.8,linetype="dotted",size=1) +
  geom_line(aes(x=privDex,y=q95),alpha=.8,linetype="dotted",size=1) +
  geom_ribbon(aes(x=privDex,ymin=q25,ymax=q75),alpha=.4) +
  geom_line(aes(x=privDex,y=q50),linetype="dotted",size=1) +
  theme(
    panel.background = element_rect(fill = bgFill),
    plot.title = element_text(color =basePoint,hjust = 0.5),
    plot.background = element_rect(fill = bgFill),
    panel.grid.major = element_line(color = rgb(0, 0, 0, alpha = 0.3)),
    panel.grid.minor = element_line(color = rgb(0, 0, 0, alpha = 0.3)),
    axis.text = element_text(color =basePoint),
    axis.title = element_text(color =basePoint),
    legend.background = element_rect(fill = bgFill),
    legend.text = element_text(color =basePoint)) +
  labs(x="Agent Privacy Index",y="% Hoogle Usage",title="Sharing") + ylim(0,100) -> sharePlot


plot_grid(noSharePlot, sharePlot, align = "h", axis = "lr", ncol = 2) -> grid

title <- ggdraw() +
  draw_label(
    paste0("Hoogle Usage (d=",round(effSize[[9]],3),", p ",pFloor[[9]],")"),
    fontface = "bold",
    size = 14,
    hjust = 0.5
  )

plot_grid(
  title,
  grid,
  ncol = 1,
  rel_heights = c(0.12, 1)
)

ggsave("~/ResearchCode/antiTrustImages/sharingAgent.png",width=12,height=6,bg=bgFill)




### NOW TIME TRENDS ###
# let's build a single function that generates plots and analytics based on one variable change
"~/Dropbox/Data\ Archive/MayDataArchive/antiTrustDataVPN" -> loc
variable <- "vpnTick"
varLab <- "VPN"

setwd(loc)
read.csv("~/Dropbox/Data\ Archive/MayDataArchive/antiTrustDataVPN/ctrl.csv",sep=",") -> control
list.files() -> allFi

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

control <- control[,c("dateTime","seed1","seed2","key","privacyVal")]
merge(control,outDat,by="key") -> jointDat

jointDat$category <- if_else(jointDat[,variable]==50,TRUE,FALSE)

jointDat %>% group_by(tick,category,currEngine) %>% summarise(cnt=n(), .groups="drop") %>%
  tidyr::complete(tidyr::nesting(tick,category), currEngine=union(currEngine,"google"),
                  fill=list(cnt=0)) -> byCat

jointDat %>% group_by(tick,category) %>% summarise(total=n()) -> denom

merge(byCat,denom,by=c("tick","category")) -> jointDenom
jointDenom$googPCt <- jointDenom$cnt / jointDenom$total

jointDenom %>% arrange(tick,category) %>% filter(currEngine=="google") -> jointDenom
ggplot() + geom_line(data=jointDenom, aes(x=tick,y=googPCt,color=category))

# now, let's get quantiles 
# Retain zero-Google runs for every observed run/tick/category combination.
jointDat %>% group_by(key,tick,category,currEngine) %>% summarise(cnt=n(), .groups="drop") %>%
  tidyr::complete(tidyr::nesting(key,tick,category), currEngine=union(currEngine,"google"),
                  fill=list(cnt=0)) -> modSmry

jointDat %>% group_by(key,tick,category) %>% summarise(total=n()) -> modDenom
merge(modSmry,modDenom,by=c("key","tick","category")) -> modJoint
modJoint %>% arrange(key,tick,category) %>% filter(currEngine=="google") %>%
  transform(googPct=100*cnt/total) %>% group_by(tick,category) %>%
  summarize(q05=quantile(googPct,.05),median=quantile(googPct,.5),q95=quantile(googPct,.95)) -> newData

newData %>% transform(category=if_else(category,"VPN","No VPN")) -> newData



# first get the difference between the two curves
newData %>% filter(category %in% c("VPN","No VPN")) %>% select(tick,category,median) %>%
  pivot_wider(names_from = category, values_from = median) %>%
  transform(absDiff=abs(`VPN` - `No VPN`)) -> diffFrame

# now integrate the area under the curve
# set a seed for montecarlo integration
set.seed(1357922)
nSamps <- 10000
# get X range
xLo <- min(diffFrame$tick)
xHi <- max(diffFrame$tick)
# get Y range
yLo <- 0
yHi <- max(diffFrame$absDiff)
# sample points
runif(nSamps, xLo, xHi) -> xSamps
runif(nSamps, yLo, yHi) -> ySamps
# now see how many are under the curve
sum(sapply(1:nSamps, function(i){
  # get the curve value at this x
  curveY <- diffFrame$absDiff[which.min(abs(diffFrame$tick -
                                               xSamps[i]))]
  if (ySamps[i] <= curveY){
    return (1)
  } else {
    return (0)
  }
})) -> nUnder
# now estimate the integral
areaRect <- (xHi - xLo) * (yHi - yLo)
integralEst <- areaRect * (nUnder / nSamps)

# now let's write a function to bootstrap the null distribution
vpnTest <- function(i){
  modJoint %>% arrange(key,tick,category) %>% filter(currEngine=="google") %>%
    transform(googPct=100*cnt/total) 
  # now get the category per key
  modJoint %>% group_by(key) %>% summarise(category=max(category)) -> allCats
  allCats$permCat <- sample(allCats$category,nrow(allCats),replace=FALSE)
  merge(modJoint,allCats[,c("key","permCat")],by="key") -> permMod
  permMod %>% arrange(key,tick,permCat) %>% filter(currEngine=="google") %>%
    transform(googPct=100*cnt/total) -> permModPct
  permModPct %>% group_by(tick,permCat) %>% 
    summarize(q05=quantile(googPct,.05),median=quantile(googPct,.5),q95=quantile(googPct,.95)) -> permData
  permData %>% filter(permCat %in% c(1,0)) %>% select(tick,permCat,median) %>%
    pivot_wider(names_from = permCat, values_from = median) %>%
    transform(absDiff=abs(`1` - `0`)) -> permDiffFrame
  # set a seed for montecarlo integration
  set.seed(24680 + i)
  nSamps <- 10000
  # get X range
  xLo <- min(permDiffFrame$tick)
  xHi <- max(permDiffFrame$tick)
  # get Y range
  yLo <- 0
  yHi <- max(permDiffFrame$absDiff)
  # sample points
  runif(nSamps, xLo, xHi) -> xSamps
  runif(nSamps, yLo, yHi) -> ySamps
  # now see how many are under the curve
  sum(sapply(1:nSamps, function(i){
    # get the curve value at this x
    curveY <- permDiffFrame$absDiff[which.min(abs(permDiffFrame$tick -
                                                    xSamps[i]))]
    if (ySamps[i] <= curveY){
      return (1)
    } else {
      return (0)
  
 }
  })) -> nUnder
  # now estimate the integral
  areaRect <- (xHi - xLo) * (yHi - yLo)
  nullInt <- areaRect * (nUnder / nSamps)
  return(nullInt)
}
unlist(lapply(1:1000,vpnTest))  -> nullIntList
#unlist(mclapply(1:1000,vpnTest,mc.cores=16)) -> nullIntList
c(rep("permute",length(nullIntList)),"observed") -> labVec
c(nullIntList,integralEst) -> valVec
data.frame(label=labVec,value=valVec) -> plotDat
plotDat %>% arrange(value) %>% transform(idx=1:nrow(plotDat)) -> plotDat
# now calculate the p-value
(1:nrow(plotDat))[plotDat$label=="observed"] -> obsIdx
# what percentage of the data is smaller
pVal <- min(obsIdx/nrow(plotDat),1 - obsIdx/nrow(plotDat))

labl <- c(labl,"Time - VPN")
loQ <- c(loQ,quantile(nullIntList,.025))
hiQ <- c(hiQ,quantile(nullIntList,.975))
effSize <- c(effSize,integralEst)
pTmp <- perm_pvals(plotDat)

pVals <- c(pVals,pVal)
pValsApprox <- c(pValsApprox,pTmp$p_approx)
pValsPlus1 <- c(pValsPlus1,pTmp$p_plus1)
pFloor <- c(pFloor,pTmp$p_floor)
nPerms <- c(nPerms,pTmp$B)


ggplot(data=newData) + geom_line(aes(x=tick,y=median,color="black",linetype = category)) +
  geom_ribbon(aes(x=tick,ymin=q05,ymax=q95,linetype=category),alpha=.2) +
  geom_vline(xintercept=30,color="black",alpha=.4) +
  geom_vline(xintercept=50,color="black",alpha=.4) +
  scale_pattern_manual(values = c("VPN" = "stripe", "No VPN" = "crosshatch")) +
  theme(
    panel.background = element_rect(fill = bgFill),
    plot.title = element_text(color =basePoint,hjust = 0.5),
    plot.background = element_rect(fill = bgFill),
    panel.grid.major = element_line(color = rgb(0, 0, 0, alpha = 0.3)),
    panel.grid.minor = element_line(color = rgb(0, 0, 0, alpha = 0.3)),
    axis.text = element_text(color =basePoint),
    axis.title = element_text(color =basePoint),
    legend.background = element_rect(fill = bgFill),
    legend.text = element_text(color =basePoint)) + 
  labs(x="Time",y="% Hoogle Usage",title=paste0("Hoogle Usage Over Time (d=",round(effSize[[10]],3),", p ",pFloor[[10]],")"),linetype="VPN Access",fill="VPN Access") +
  scale_color_manual(values = c("VPN" = vpnTrue, "No VPN" = vpnFalse)) +
  scale_fill_manual(values = c("VPN" = vpnTrue, "No VPN" = vpnFalse)) +
  scale_linetype_manual(values = c("VPN" = "solid", "No VPN" = "dotted")) +
  
  geom_text(label="Duck Duck Go",x=30,y=25,angle=90,vjust=-.2,color="black", size=4,family = "Helvetica",   
            fontface = "plain") +
  geom_text(label="VPN",x=50,y=75,angle=90,vjust=-.2,color="black", size=4,family = "Helvetica",   
            fontface = "plain")

ggsave(filename = "~/ResearchCode/antiTrustImages/vpnTime.png",width=7,height=7,bg=bgFill)



"~/ResearchCode/MayDataArchive/antiTrustDataDel" -> loc
variable <- "delTick"
varLab <- "Deletion"


setwd(loc)
read.csv("ctrl.csv",sep=",") -> control
list.files() -> allFi

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

control <- control[,c("dateTime","seed1","seed2","key","privacyVal")]
merge(control,outDat,by="key") -> jointDat

jointDat$category <- if_else(jointDat[,variable]==50,TRUE,FALSE)

jointDat %>% group_by(tick,category,currEngine) %>% summarise(cnt=n(), .groups="drop") %>%
  tidyr::complete(tidyr::nesting(tick,category), currEngine=union(currEngine,"google"),
                  fill=list(cnt=0)) -> byCat

jointDat %>% group_by(tick,category) %>% summarise(total=n()) -> denom

merge(byCat,denom,by=c("tick","category")) -> jointDenom
jointDenom$googPCt <- jointDenom$cnt / jointDenom$total

jointDenom %>% arrange(tick,category) %>% filter(currEngine=="google") -> jointDenom
ggplot() + geom_line(data=jointDenom, aes(x=tick,y=googPCt,color=category))

# now, let's get quantiles 
# Retain zero-Google runs for every observed run/tick/category combination.
jointDat %>% group_by(key,tick,category,currEngine) %>% summarise(cnt=n(), .groups="drop") %>%
  tidyr::complete(tidyr::nesting(key,tick,category), currEngine=union(currEngine,"google"),
                  fill=list(cnt=0)) -> modSmry

jointDat %>% group_by(key,tick,category) %>% summarise(total=n()) -> modDenom
merge(modSmry,modDenom,by=c("key","tick","category")) -> modJoint
modJoint %>% arrange(key,tick,category) %>% filter(currEngine=="google") %>%
  transform(googPct=100*cnt/total) %>% group_by(tick,category) %>%
  summarize(q05=quantile(googPct,.05),median=quantile(googPct,.5),q95=quantile(googPct,.95)) -> newData

newData %>% transform(category=if_else(category,"Deletion","No Deletion")) -> newData


newData %>% filter(category %in% c("Deletion","No Deletion")) %>% select(tick,category,median) %>%
  pivot_wider(names_from = category, values_from = median) %>%
  transform(absDiff=abs(`Deletion` - `No Deletion`)) -> diffFrame

# now integrate the area under the curve
# set a seed for montecarlo integration
set.seed(1357926567)
nSamps <- 10000
# get X range
xLo <- min(diffFrame$tick)
xHi <- max(diffFrame$tick)
# get Y range
yLo <- 0
yHi <- max(diffFrame$absDiff)
# sample points
runif(nSamps, xLo, xHi) -> xSamps
runif(nSamps, yLo, yHi) -> ySamps
# now see how many are under the curve
sum(sapply(1:nSamps, function(i){
  # get the curve value at this x
  curveY <- diffFrame$absDiff[which.min(abs(diffFrame$tick -
                                              xSamps[i]))]
  if (ySamps[i] <= curveY){
    return (1)
  } else {
    return (0)
  }
})) -> nUnder
# now estimate the integral
areaRect <- (xHi - xLo) * (yHi - yLo)
integralEst <- areaRect * (nUnder / nSamps)

# now let's write a function to bootstrap the null distribution
delTest <- function(i){
  modJoint %>% arrange(key,tick,category) %>% filter(currEngine=="google") %>%
    transform(googPct=100*cnt/total) 
  # now get the category per key
  modJoint %>% group_by(key) %>% summarise(category=max(category)) -> allCats
  allCats$permCat <- sample(allCats$category,nrow(allCats),replace=FALSE)
  merge(modJoint,allCats[,c("key","permCat")],by="key") -> permMod
  permMod %>% arrange(key,tick,permCat) %>% filter(currEngine=="google") %>%
    transform(googPct=100*cnt/total) -> permModPct
  permModPct %>% group_by(tick,permCat) %>% 
    summarize(q05=quantile(googPct,.05),median=quantile(googPct,.5),q95=quantile(googPct,.95)) -> permData
  permData %>% filter(permCat %in% c(1,0)) %>% select(tick,permCat,median) %>%
    pivot_wider(names_from = permCat, values_from = median) %>%
    transform(absDiff=abs(`1` - `0`)) -> permDiffFrame
  # set a seed for montecarlo integration
  set.seed(24680 + i)
  nSamps <- 10000
  # get X range
  xLo <- min(permDiffFrame$tick)
  xHi <- max(permDiffFrame$tick)
  # get Y range
  yLo <- 0
  yHi <- max(permDiffFrame$absDiff)
  # sample points
  runif(nSamps, xLo, xHi) -> xSamps
  runif(nSamps, yLo, yHi) -> ySamps
  # now see how many are under the curve
  sum(sapply(1:nSamps, function(i){
    # get the curve value at this x
    curveY <- permDiffFrame$absDiff[which.min(abs(permDiffFrame$tick -
                                                    xSamps[i]))]
    if (ySamps[i] <= curveY){
      return (1)
    } else {
      return (0)
      
    }
  })) -> nUnder
  # now estimate the integral
  areaRect <- (xHi - xLo) * (yHi - yLo)
  nullInt <- areaRect * (nUnder / nSamps)
  return(nullInt)
}
unlist(lapply(1:1000,delTest)) -> nullIntList
#unlist(mclapply(1:10,delTest,mc.cores=16)) -> nullIntList
c(rep("permute",length(nullIntList)),"observed") -> labVec
c(nullIntList,integralEst) -> valVec
data.frame(label=labVec,value=valVec) -> plotDat
plotDat %>% arrange(value) %>% transform(idx=1:nrow(plotDat)) -> plotDat
# now calculate the p-value
(1:nrow(plotDat))[plotDat$label=="observed"] -> obsIdx
# what percentage of the data is smaller
pVal <- min(obsIdx/nrow(plotDat),1 - obsIdx/nrow(plotDat))

labl <- c(labl,"Time - Deletion")
loQ <- c(loQ,quantile(nullIntList,.025))
hiQ <- c(hiQ,quantile(nullIntList,.975))
effSize <- c(effSize,integralEst)
pTmp <- perm_pvals(plotDat)

pVals <- c(pVals,pVal)
pValsApprox <- c(pValsApprox,pTmp$p_approx)
pValsPlus1 <- c(pValsPlus1,pTmp$p_plus1)
pFloor <- c(pFloor,pTmp$p_floor)
nPerms <- c(nPerms,pTmp$B)



ggplot(data=newData) + geom_line(aes(x=tick,y=median,linetype=category)) +
  geom_ribbon(aes(x=tick,ymin=q05,ymax=q95,linetype=category),alpha=.2) +
  geom_vline(xintercept=30,color="black",alpha=.4) +
  geom_vline(xintercept=50,color="black",alpha=.4) +
  theme(
    panel.background = element_rect(fill = bgFill),
    plot.title = element_text(color =basePoint,hjust = 0.5),
    plot.background = element_rect(fill = bgFill),
    panel.grid.major = element_line(color = rgb(0, 0, 0, alpha = 0.3)),
    panel.grid.minor = element_line(color = rgb(0, 0, 0, alpha = 0.3)),
    axis.text = element_text(color =basePoint),
    axis.title = element_text(color =basePoint),
    legend.background = element_rect(fill = bgFill),
    legend.text = element_text(color =basePoint)) + 
  labs(x="Time",y="% Hoogle Usage",title=paste0("Hoogle Usage Over Time (d=",round(effSize[[11]],3),", p ",pFloor[[11]],")"),linetype="Deletion Access",fill="Deletion Access") +
  #scale_color_manual(values = c("Deletion" = vpnTrue, "No Deletion" = vpnFalse)) +
  #scale_fill_manual(values = c("Deletion" = vpnTrue, "No Deletion" = vpnFalse)) +
  scale_linetype_manual(values = c("Deletion" = "solid", "No Deletion" = "dotted")) +
  geom_text(label="Duck Duck Go",x=30,y=25,angle=90,vjust=-.2,color="black", size=4,family = "Helvetica",   
            fontface = "plain") +
  geom_text(label="Deletion",x=50,y=75,angle=90,vjust=-.2,color="black", size=4,family = "Helvetica",   
            fontface = "plain")
ggsave(filename = "~/ResearchCode/antiTrustImages/DeletionTime.png",width=7,height=7,bg=bgFill)



"~/ResearchCode/MayDataArchive/antiTrustDataShare" -> loc
variable <- "shareTick"
varLab <- "Sharing"


setwd(loc)
read.csv("ctrl.csv",sep=",") -> control
list.files() -> allFi

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

control <- control[,c("dateTime","seed1","seed2","key","privacyVal")]
merge(control,outDat,by="key") -> jointDat

jointDat$category <- if_else(jointDat[,variable]==50,TRUE,FALSE)

jointDat %>% group_by(tick,category,currEngine) %>% summarise(cnt=n(), .groups="drop") %>%
  tidyr::complete(tidyr::nesting(tick,category), currEngine=union(currEngine,"google"),
                  fill=list(cnt=0)) -> byCat

jointDat %>% group_by(tick,category) %>% summarise(total=n()) -> denom

merge(byCat,denom,by=c("tick","category")) -> jointDenom
jointDenom$googPCt <- jointDenom$cnt / jointDenom$total

jointDenom %>% arrange(tick,category) %>% filter(currEngine=="google") -> jointDenom
ggplot() + geom_line(data=jointDenom, aes(x=tick,y=googPCt,color=category))

# now, let's get quantiles 
# Retain zero-Google runs for every observed run/tick/category combination.
jointDat %>% group_by(key,tick,category,currEngine) %>% summarise(cnt=n(), .groups="drop") %>%
  tidyr::complete(tidyr::nesting(key,tick,category), currEngine=union(currEngine,"google"),
                  fill=list(cnt=0)) -> modSmry

jointDat %>% group_by(key,tick,category) %>% summarise(total=n()) -> modDenom
merge(modSmry,modDenom,by=c("key","tick","category")) -> modJoint
modJoint %>% arrange(key,tick,category) %>% filter(currEngine=="google") %>%
  transform(googPct=100*cnt/total) %>% group_by(tick,category) %>%
  summarize(q05=quantile(googPct,.05),median=quantile(googPct,.5),q95=quantile(googPct,.95)) -> newData

newData %>% transform(category=if_else(category,"Sharing","No Sharing")) -> newData


newData %>% filter(category %in% c("Sharing","No Sharing")) %>% select(tick,category,median) %>%
  pivot_wider(names_from = category, values_from = median) %>%
  transform(absDiff=abs(`Sharing` - `No Sharing`)) -> diffFrame

# now integrate the area under the curve
# set a seed for montecarlo integration
set.seed(1357567)
nSamps <- 10000
# get X range
xLo <- min(diffFrame$tick)
xHi <- max(diffFrame$tick)
# get Y range
yLo <- 0
yHi <- max(diffFrame$absDiff)
# sample points
runif(nSamps, xLo, xHi) -> xSamps
runif(nSamps, yLo, yHi) -> ySamps
# now see how many are under the curve
sum(sapply(1:nSamps, function(i){
  # get the curve value at this x
  curveY <- diffFrame$absDiff[which.min(abs(diffFrame$tick -
                                              xSamps[i]))]
  if (ySamps[i] <= curveY){
    return (1)
  } else {
    return (0)
  }
})) -> nUnder
# now estimate the integral
areaRect <- (xHi - xLo) * (yHi - yLo)
integralEst <- areaRect * (nUnder / nSamps)

# now let's write a function to bootstrap the null distribution
sharTest <- function(i){
  modJoint %>% arrange(key,tick,category) %>% filter(currEngine=="google") %>%
    transform(googPct=100*cnt/total) 
  # now get the category per key
  modJoint %>% group_by(key) %>% summarise(category=max(category)) -> allCats
  allCats$permCat <- sample(allCats$category,nrow(allCats),replace=FALSE)
  merge(modJoint,allCats[,c("key","permCat")],by="key") -> permMod
  permMod %>% arrange(key,tick,permCat) %>% filter(currEngine=="google") %>%
    transform(googPct=100*cnt/total) -> permModPct
  permModPct %>% group_by(tick,permCat) %>% 
    summarize(q05=quantile(googPct,.05),median=quantile(googPct,.5),q95=quantile(googPct,.95)) -> permData
  permData %>% filter(permCat %in% c(1,0)) %>% select(tick,permCat,median) %>%
    pivot_wider(names_from = permCat, values_from = median) %>%
    transform(absDiff=abs(`1` - `0`)) -> permDiffFrame
  # set a seed for montecarlo integration
  set.seed(680 + i)
  nSamps <- 10000
  # get X range
  xLo <- min(permDiffFrame$tick)
  xHi <- max(permDiffFrame$tick)
  # get Y range
  yLo <- 0
  yHi <- max(permDiffFrame$absDiff)
  # sample points
  runif(nSamps, xLo, xHi) -> xSamps
  runif(nSamps, yLo, yHi) -> ySamps
  # now see how many are under the curve
  sum(sapply(1:nSamps, function(i){
    # get the curve value at this x
    curveY <- permDiffFrame$absDiff[which.min(abs(permDiffFrame$tick -
                                                    xSamps[i]))]
    if (ySamps[i] <= curveY){
      return (1)
    } else {
      return (0)
      
    }
  })) -> nUnder
  # now estimate the integral
  areaRect <- (xHi - xLo) * (yHi - yLo)
  nullInt <- areaRect * (nUnder / nSamps)
  return(nullInt)
}
unlist(lapply(1:1000,sharTest)) -> nullIntList
c(rep("permute",length(nullIntList)),"observed") -> labVec
c(nullIntList,integralEst) -> valVec
data.frame(label=labVec,value=valVec) -> plotDat
plotDat %>% arrange(value) %>% transform(idx=1:nrow(plotDat)) -> plotDat
# now calculate the p-value
(1:nrow(plotDat))[plotDat$label=="observed"] -> obsIdx
# what percentage of the data is smaller
pVal <- min(obsIdx/nrow(plotDat),1 - obsIdx/nrow(plotDat))
labl <- c(labl,"Time - Sharing")
loQ <- c(loQ,quantile(nullIntList,.025))
hiQ <- c(hiQ,quantile(nullIntList,.975))
effSize <- c(effSize,integralEst)
pTmp <- perm_pvals(plotDat)

pVals <- c(pVals,pVal)
pValsApprox <- c(pValsApprox,pTmp$p_approx)
pValsPlus1 <- c(pValsPlus1,pTmp$p_plus1)
pFloor <- c(pFloor,pTmp$p_floor)
nPerms <- c(nPerms,pTmp$B)
ggplot(data=newData) + geom_line(aes(x=tick,y=median,linetype=category)) +
  geom_ribbon(aes(x=tick,ymin=q05,ymax=q95,linetype =category),alpha=.2) +
  geom_vline(xintercept=30,color="black",alpha=.4) +
  geom_vline(xintercept=50,color="black",alpha=.4) +
  theme(
    panel.background = element_rect(fill = bgFill),
    plot.title = element_text(color =basePoint,hjust = 0.5),
    plot.background = element_rect(fill = bgFill),
    panel.grid.major = element_line(color = rgb(0, 0, 0, alpha = 0.3)),
    panel.grid.minor = element_line(color = rgb(0, 0, 0, alpha = 0.3)),
    axis.text = element_text(color =basePoint),
    axis.title = element_text(color =basePoint),
    legend.background = element_rect(fill = bgFill),
    legend.text = element_text(color =basePoint)) + 
  labs(x="Time",y="% Hoogle Usage",title=paste0("Hoogle Usage Over Time (d=",round(effSize[[12]],3),", p ",pFloor[[12]],")"),color="Sharing Access",fill="Sharing Access",linetype="Sharing Access") +
  #scale_color_manual(values = c("Sharing" = vpnTrue, "No Sharing" = vpnFalse)) +
  #scale_fill_manual(values = c("Sharing" = vpnTrue, "No Sharing" = vpnFalse)) +
  scale_linetype_manual(values = c("Sharing" = "solid", "No Sharing" = "dotted")) +
  geom_text(label="Duck Duck Go",x=30,y=25,angle=90,vjust=-.2,color="black", size=4,family = "Helvetica",   
            fontface = "plain") +
  geom_text(label="Sharing",x=50,y=75,angle=90,vjust=-.2,color="black", size=4,family = "Helvetica",   
            fontface = "plain")
ggsave(filename = "~/ResearchCode/antiTrustImages/SharingTime.png",width=7,height=7,bg=bgFill)

# final effect-size and p-value table for manuscript reporting
effectTable <- data.frame(
  label = labl,
  effect_size = effSize,
  null_q025 = loQ,
  null_q975 = hiQ,
  p_rank = pVals,
  p_approx = pValsApprox,
  p_plus1 = pValsPlus1,
  p_report = pFloor,
  n_permutations = nPerms
)

print(effectTable)
write.csv(effectTable, "effect_size_summary.csv", row.names = FALSE)

# Persist plot-ready artifacts so plots can be edited without rerunning analysis.
artifactDir <- file.path(getwd(), "analysis_artifacts", format(Sys.time(), "%Y%m%d_%H%M%S"))
dir.create(artifactDir, recursive = TRUE, showWarnings = FALSE)

save_if_exists <- function(obj_name, out_dir, as_csv = FALSE){
  if (exists(obj_name, inherits = TRUE)){
    obj <- get(obj_name, inherits = TRUE)
    saveRDS(obj, file.path(out_dir, paste0(obj_name, ".rds")))
    if (as_csv && is.data.frame(obj)){
      write.csv(obj, file.path(out_dir, paste0(obj_name, ".csv")), row.names = FALSE)
    }
    return(TRUE)
  }
  return(FALSE)
}

# Tables and core computed vectors
save_if_exists("effectTable", artifactDir, as_csv = TRUE)
save_if_exists("labl", artifactDir)
save_if_exists("effSize", artifactDir)
save_if_exists("loQ", artifactDir)
save_if_exists("hiQ", artifactDir)
save_if_exists("pVals", artifactDir)
save_if_exists("pValsApprox", artifactDir)
save_if_exists("pValsPlus1", artifactDir)
save_if_exists("pFloor", artifactDir)
save_if_exists("nPerms", artifactDir)

# Data commonly reused for plot editing
save_if_exists("newData", artifactDir, as_csv = TRUE)
save_if_exists("vpnDat", artifactDir, as_csv = TRUE)
save_if_exists("deletionDat", artifactDir, as_csv = TRUE)
save_if_exists("sharingDat", artifactDir, as_csv = TRUE)
save_if_exists("controlSmry", artifactDir, as_csv = TRUE)
save_if_exists("jointDat", artifactDir, as_csv = TRUE)
save_if_exists("modJoint", artifactDir, as_csv = TRUE)

# Plot objects (can be re-opened and restyled with ggplot_build/ggplotGrob)
save_if_exists("loVPN", artifactDir)
save_if_exists("hiVPN", artifactDir)
save_if_exists("loDel", artifactDir)
save_if_exists("hiDel", artifactDir)
save_if_exists("loSharing", artifactDir)
save_if_exists("hiSharing", artifactDir)

writeLines(artifactDir, con = file.path(getwd(), "analysis_artifacts", "LATEST_ARTIFACT_DIR.txt"))
message(paste0("Artifacts written to: ", artifactDir))


