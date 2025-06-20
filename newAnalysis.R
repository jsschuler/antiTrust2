library(rlist)
library(tidyverse)
library(ggplot2)
library(ggpattern)
library(dplyr)
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

# let's build a single function that generates plots and analytics based on one variable change
"~/ResearchCode/MayDataArchive/antiTrustDataVPN" -> loc
variable <- "vpnTick"
varLab <- "VPN"

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
  
 jointDat %>% group_by(tick,category,currEngine) %>% summarise(cnt=n()) -> byCat 
 
 jointDat %>% group_by(tick,category) %>% summarise(total=n()) -> denom
 
 merge(byCat,denom,by=c("tick","category")) -> jointDenom
  jointDenom$googPCt <- jointDenom$cnt / jointDenom$total
  
  jointDenom %>% arrange(tick,category) %>% filter(currEngine=="google") -> jointDenom
ggplot() + geom_line(data=jointDenom, aes(x=tick,y=googPCt,color=category))
  
# now, let's get quantiles 
jointDat %>% group_by(key,tick,category,currEngine) %>% summarise(cnt=n()) -> modSmry

jointDat %>% group_by(key,tick,category) %>% summarise(total=n()) -> modDenom
merge(modSmry,modDenom,by=c("key","tick","category")) -> modJoint
modJoint %>% arrange(key,tick,category) %>% filter(currEngine=="google") %>%
  transform(googPct=100*cnt/total) %>% group_by(tick,category) %>%
  summarize(q05=quantile(googPct,.05),median=quantile(googPct,.5),q95=quantile(googPct,.95)) -> newData

newData %>% transform(category=if_else(category,"VPN","No VPN")) -> newData



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
  labs(x="Time",y="% Google Usage",title="Google Usage Over Time (VPN Introduced)",linetype="VPN Access",fill="VPN Access") +
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

jointDat %>% group_by(tick,category,currEngine) %>% summarise(cnt=n()) -> byCat 

jointDat %>% group_by(tick,category) %>% summarise(total=n()) -> denom

merge(byCat,denom,by=c("tick","category")) -> jointDenom
jointDenom$googPCt <- jointDenom$cnt / jointDenom$total

jointDenom %>% arrange(tick,category) %>% filter(currEngine=="google") -> jointDenom
ggplot() + geom_line(data=jointDenom, aes(x=tick,y=googPCt,color=category))

# now, let's get quantiles 
jointDat %>% group_by(key,tick,category,currEngine) %>% summarise(cnt=n()) -> modSmry

jointDat %>% group_by(key,tick,category) %>% summarise(total=n()) -> modDenom
merge(modSmry,modDenom,by=c("key","tick","category")) -> modJoint
modJoint %>% arrange(key,tick,category) %>% filter(currEngine=="google") %>%
  transform(googPct=100*cnt/total) %>% group_by(tick,category) %>%
  summarize(q05=quantile(googPct,.05),median=quantile(googPct,.5),q95=quantile(googPct,.95)) -> newData

newData %>% transform(category=if_else(category,"Deletion","No Deletion")) -> newData

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
  labs(x="Time",y="% Google Usage",title="Google Usage Over Time (Deletion Introduced)",linetype="Deletion Access",fill="Deletion Access") +
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

jointDat %>% group_by(tick,category,currEngine) %>% summarise(cnt=n()) -> byCat 

jointDat %>% group_by(tick,category) %>% summarise(total=n()) -> denom

merge(byCat,denom,by=c("tick","category")) -> jointDenom
jointDenom$googPCt <- jointDenom$cnt / jointDenom$total

jointDenom %>% arrange(tick,category) %>% filter(currEngine=="google") -> jointDenom
ggplot() + geom_line(data=jointDenom, aes(x=tick,y=googPCt,color=category))

# now, let's get quantiles 
jointDat %>% group_by(key,tick,category,currEngine) %>% summarise(cnt=n()) -> modSmry

jointDat %>% group_by(key,tick,category) %>% summarise(total=n()) -> modDenom
merge(modSmry,modDenom,by=c("key","tick","category")) -> modJoint
modJoint %>% arrange(key,tick,category) %>% filter(currEngine=="google") %>%
  transform(googPct=100*cnt/total) %>% group_by(tick,category) %>%
  summarize(q05=quantile(googPct,.05),median=quantile(googPct,.5),q95=quantile(googPct,.95)) -> newData

newData %>% transform(category=if_else(category,"Sharing","No Sharing")) -> newData

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
  labs(x="Time",y="% Google Usage",title="Google Usage Over Time (Sharing Introduced)",color="Sharing Access",fill="Sharing Access") +
  #scale_color_manual(values = c("Sharing" = vpnTrue, "No Sharing" = vpnFalse)) +
  #scale_fill_manual(values = c("Sharing" = vpnTrue, "No Sharing" = vpnFalse)) +
  scale_linetype_manual(values = c("Sharing" = "solid", "No Sharing" = "dotted")) +
  geom_text(label="Duck Duck Go",x=30,y=25,angle=90,vjust=-.2,color="black", size=4,family = "Helvetica",   
            fontface = "plain") +
  geom_text(label="Sharing",x=50,y=75,angle=90,vjust=-.2,color="black", size=4,family = "Helvetica",   
            fontface = "plain")
ggsave(filename = "~/ResearchCode/antiTrustImages/SharingTime.png",width=7,height=7,bg=bgFill)


