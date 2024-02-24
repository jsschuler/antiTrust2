
setwd("~/ResearchCode/antiTrustNetworkData")
read.csv("~/ResearchCode/antiTrustNetworkData/ctrl.csv",sep=",") -> control
list.files("~/ResearchCode/antiTrustNetworkData") -> allFi

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

read.csv()

# step one, consider only cases where there is no vpn
# and only consider data where Duck Duck Go has been available
outDat[outDat$tick >= (outDat$duckTick) & outDat$vpnTick==-10 & outDat$delTick==-10 & outDat$shareTick==-10,] -> afterDuckImm
afterDuckImm$tickSince <- afterDuckImm$tick-afterDuckImm$duckTick


control[,c("key","expDegree")] -> paramFi

afterDuckImm %>% transform(googBool=(currEngine=="google")) %>% group_by(key,tickSince) %>% summarize(googPct=100*mean(googBool)) %>% group_by(tickSince) %>% summarize(goog75=quantile(googPct,.75),
                                                                                                                                                                    #googMn=mean(googPct),
                                                                                                                                                                    goog50=quantile(googPct,.5),
                                                                                                                                                                    goog25=quantile(googPct,.25)) %>% pivot_longer(
                                                                                                                                                                      cols = c(goog75,goog50,goog25),
                                                                                                                                                                      names_to = "Variable",
                                                                                                                                                                      values_to = "Value"
                                                                                                                                                                    )-> mixSmry
afterDuckImm %>% transform(googBool=(currEngine=="google")) %>% group_by(key,tickSince) %>% summarize(googPct=100*mean(googBool)) -> series
series %>% group_by(key) %>% summarise(finGoog=min(googPct)) -> serMin
merge(series,serMin,by="key") -> seriesJoint

seriesJoint$minFlag <- abs(seriesJoint$googPct-seriesJoint$finGoog)<=5 
seriesJoint %>% group_by(key,minFlag) %>% summarise(minTick=min(tickSince)) -> deltaTick
deltaTick[deltaTick$minFlag,] -> timeSince

merge(paramFi,timeSince,by="key") -> paramTime

ggplot() + geom_point(aes(x=paramTime$expDegree,paramTime$minTick),color=as.numeric(paramTime$minTick))

lm(paramTime$minTick~paramTime$expDegree)

labelShift <- function(lab){
  if (lab=="goog25"){outLab="25th"}
  if (lab=="goog50"){outLab="50th"}
  if (lab=="goog75"){outLab="75th"}
  
  return(outLab)
}

LabelShift <- Vectorize(labelShift,"lab")

ggplot() + geom_line(aes(x=mixSmry$tickSince,y=mixSmry$Value,color=LabelShift(mixSmry$Variable)))  + theme(
  panel.background = element_rect(fill = bgFill),
  plot.title = element_text(color =basePoint,hjust = 0.5),
  plot.background = element_rect(fill = bgFill),
  panel.grid = element_blank(),
  axis.text = element_text(color =basePoint),
  axis.title = element_text(color =basePoint),
  legend.background = element_rect(fill = bgFill),
  legend.text = element_text(color =basePoint)) + xlab("Time Since Duck Duck Go Enters") + ylab("% Google Usage") + labs(color = "Google Usage Series")
