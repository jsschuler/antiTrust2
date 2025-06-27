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
library(tidyr)
library(cowplot)
library(data.table)
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

# we need several different visualizations
# Firstly, let's look at model level differences in Google usage with vs without each option
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
vpnDat %>% filter(privalVal=="Low") %>% transform(category=if_else(category==0,"False","True")) %>%
  ggplot() + geom_density(aes(x=googPct, linetype=category),alpha=.2) +
  xlim(x0,x1) + labs(x="% Google Usage",y="Density",title="Lower Privacy Preference",linetype="VPN") +
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
  xlim(x0,x1) + labs(x="% Google Usage",y="Density",title="Higher Privacy Preference",linetype="VPN") +
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
ggsave("../antiTrustImages/vpnDist.png",width=7,height=7,bg=bgFill)
### Deletion ###

controlSmry %>% transform(category=1*(vpnTick==50)+2*(deletionTick==50)+4*(sharingTick==50)) %>% 
  group_by(seed1,privalVal,category) %>% filter(category %in% c(0,2)) %>% transform(category=as.factor(category)) -> deletionDat

x0 <- min(deletionDat$googPct)
x1 <- max(deletionDat$googPct)
deletionDat %>% filter(privalVal=="Low") %>% transform(category=if_else(category==0,"False","True")) %>%
  ggplot() + geom_density(aes(x=googPct, linetype=category),alpha=.2) +
  xlim(x0,x1) + labs(x="% Google Usage",y="Density",title="Lower Privacy Preference",linetype="Deletion") +
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
  xlim(x0,x1) + labs(x="% Google Usage",y="Density",title="Higher Privacy Preference",linetype="Deletion") +
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
ggsave("../antiTrustImages/deletionDist.png",width=7,height=7,bg=bgFill)
### Sharing ###


controlSmry %>% transform(category=1*(vpnTick==50)+2*(deletionTick==50)+4*(sharingTick==50)) %>% 
  group_by(seed1,privalVal,category) %>% filter(category %in% c(0,4)) %>% transform(category=as.factor(category)) -> sharingDat

x0 <- min(sharingDat$googPct)
x1 <- max(sharingDat$googPct)
sharingDat %>% filter(privalVal=="Low") %>% transform(category=if_else(category==0,"False","True")) %>%
  ggplot() + geom_density(aes(x=googPct, linetype=category),alpha=.2) +
  xlim(x0,x1) + labs(x="% Google Usage",y="Density",title="Lower Privacy Preference",linetype="Sharing") +
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
  xlim(x0,x1) + labs(x="% Google Usage",y="Density",title="Higher Privacy Preference",linetype="Sharing") +
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
ggsave("../antiTrustImages/sharingDist.png",width=7,height=7,bg=bgFill)

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
window <- 50

datList <- list()

for (t in (window/2):(10000-window/2)){
  loRng <- (t - window/(2))/10000
  hiRng <- (t + window/(2)/10000)
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
  labs(x="Agent Privacy Index",y="% Google Usage",title="Google Usage without VPN")  + ylim(0,100)-> noVpnPlot

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
  labs(x="Agent Privacy Index",y="% Google Usage",title="Google Usage with VPN") + ylim(0,100) -> vpnPlot

plot_grid(noVpnPlot, vpnPlot, align = "h", axis = "lr", ncol = 2)
ggsave("../antiTrustImages/vpnAgent.png",width=12,height=6,bg=bgFill)

### Deletion ###
agtSmry %>% transform(category=1*(vpnTick==50)+2*(deletionTick==50)+4*(sharingTick==50)) %>%
  filter(category %in% c(0,2)) -> agtDel
agtDel$privDex <- (agtDel$blissPoint/agtDel$unifExp)



# now, get a moving average and quantiles
window <- 50

datList <- list()

for (t in (window/2):(10000-window/2)){
  loRng <- (t - window/(2))/10000
  hiRng <- (t + window/(2)/10000)
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
  labs(x="Agent Privacy Index",y="% Google Usage",title="Google Usage without Deletion")  + ylim(0,100)-> noDelPlot

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
  labs(x="Agent Privacy Index",y="% Google Usage",title="Google Usage with Deletion") + ylim(0,100) -> delPlot

plot_grid(noDelPlot, delPlot, align = "h", axis = "lr", ncol = 2)
ggsave("../antiTrustImages/deletionAgent.png",width=12,height=6,bg=bgFill)

### Sharing ###
agtSmry %>% transform(category=1*(vpnTick==50)+2*(deletionTick==50)+4*(sharingTick==50)) %>%
  filter(category %in% c(0,4)) -> agtSharing
agtSharing$privDex <- (agtSharing$blissPoint/agtSharing$unifExp)



# now, get a moving average and quantiles
window <- 50

datList <- list()

for (t in (window/2):(10000-window/2)){
  loRng <- (t - window/(2))/10000
  hiRng <- (t + window/(2)/10000)
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
  labs(x="Agent Privacy Index",y="% Google Usage",title="Google Usage without Sharing")  + ylim(0,100)-> noSharePlot

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
  labs(x="Agent Privacy Index",y="% Google Usage",title="Google Usage with Sharing") + ylim(0,100) -> sharePlot

plot_grid(noSharePlot, sharePlot, align = "h", axis = "lr", ncol = 2)
ggsave("../antiTrustImages/sharingAgent.png",width=12,height=6,bg=bgFill)
