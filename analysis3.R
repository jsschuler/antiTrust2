library(ggplot2)
library(dplyr)
library(rlist)

setwd("~/ResearchCode/antiTrustDataArchive1")
read.csv("ctrl.csv",sep=",") -> control
list.files("~/ResearchCode/antiTrustDataArchive1") -> allFi

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
names(beforeDat) <- c("key","tick","agent","beforeAct","beforeActEngine")

list.rbind(searchList) -> searchDat
names(searchDat) <- c("key","tick","agent","searchEngine","optOut","waitTime","utility")

list.rbind(afterList) -> afterDat
names(afterDat) <- c("key","tick","agent","afterAct","afterActEngine","result")

list.rbind(agentList) -> agentDat
names(agentDat) <- c("key","agent","blissPoint","selfExp","unifExp")

# merge the search data with the control data 

merge(control,searchDat,by="key") -> searchCtrl

searchCtrl %>% filter(tick >= duckTick & tick <= (duckTick+50)) %>% transform(googBool=(searchEngine=="google")) %>% group_by(privacyVal,key,duckTick) %>% summarize(googPct=mean(googBool)) %>% arrange(duckTick,privacyVal) -> report

# now get the summary statistics 

report  %>% group_by(duckTick,privacyVal) %>% summarise(g0=min(googPct),g25=quantile(googPct,.25),g50=quantile(googPct,.5),g75=quantile(googPct,.75),g100=max(googPct),gMean=exp(mean(log(googPct)))) -> repSummary

report %>% arrange()

smry <- repSummary[repSummary$duckTick==100,]
smry2 <- repSummary[repSummary$duckTick==10,]

  # transform duck 100
  
  smry$privacyVal <- 3 +smry$privacyVal
  
  ggplot()  +xlab("Privacy Indifference") + ylab("Google Search Share") +xlim(0,6)+ ylim(39.5,80.5) +
    geom_hline(yintercept =40,linetype="dashed",alpha=.3) + 
    geom_hline(yintercept =45,linetype="dotted",alpha=.3) +
    geom_hline(yintercept =50,linetype="dashed",alpha=.3) +
    geom_hline(yintercept =55,linetype="dotted",alpha=.3) +
    geom_hline(yintercept =60,linetype="dashed",alpha=.3) +
    geom_hline(yintercept =65,linetype="dotted",alpha=.3) +
    geom_hline(yintercept =70,linetype="dashed",alpha=.3) +
    geom_hline(yintercept =75,linetype="dotted",alpha=.3) +
    geom_hline(yintercept =80,linetype="dashed",alpha=.3) +
    geom_vline(xintercept = 0,linetype="solid",alpha=.3) +
    geom_vline(xintercept = .5,linetype="dotted",alpha=.3) +
    geom_vline(xintercept = 1,linetype="dashed",alpha=.3) +
    geom_vline(xintercept = 1.5,linetype="dotted",alpha=.3) +
    geom_vline(xintercept = 2,linetype="dashed",alpha=.3) +
    geom_vline(xintercept = 2.5,linetype="dotted",alpha=.3) +
    geom_vline(xintercept = 3,linetype="solid",alpha=.3) +    
    geom_vline(xintercept = 3.5,linetype="dotted",alpha=.3) +
    geom_vline(xintercept = 4,linetype="dashed",alpha=.3) +
    geom_vline(xintercept = 4.5,linetype="dotted",alpha=.3) +
    geom_vline(xintercept = 5,linetype="dashed",alpha=.3) +
    geom_vline(xintercept = 5.5,linetype="dotted",alpha=.3) +
    geom_vline(xintercept = 6,linetype="solid",alpha=.3) +    
    geom_line(aes(x=smry$privacyVal,y=100*smry$g0),color="#FF0000") +
    geom_line(aes(x=smry$privacyVal,y=100*smry$g25),color="#990066") +
    geom_line(aes(x=smry$privacyVal,y=100*smry$g50),color="#660099") +
    geom_line(aes(x=smry$privacyVal,y=100*smry$g75),color="#3300CC") +
    geom_line(aes(x=smry$privacyVal,y=100*smry$g100),color="#0000FF") +
    geom_line(aes(x=smry$privacyVal,y=100*smry$gMean),color="#228C22") +
  
    annotate(geom="text",color="#FF0000",x=smry$privacyVal[1],y=100*smry$g0[1],label="Min",hjust=1,vjust=.95) +
    annotate(geom="text",color="#990066",x=smry$privacyVal[1],y=100*smry$g25[1],label="25%",hjust=1,vjust=.95) +
    annotate(geom="text",color="#660099",x=smry$privacyVal[1],y=100*smry$g50[1],label="Med",hjust=1,vjust=.95) +
    annotate(geom="text",color="#3300CC",x=smry$privacyVal[1],y=100*smry$g75[1],label="75%",hjust=1,vjust=.95) +
    annotate(geom="text",color="#0000FF",x=smry$privacyVal[1],y=100*smry$g100[1],label="Max",hjust=1,vjust=.95) +
    annotate(geom="text",color="#228C22",x=smry$privacyVal[nrow(smry)],y=100*smry$gMean[nrow(smry)],label="Mean",hjust=0,vjust=.95) +
    geom_line(aes(x=smry2$privacyVal,y=100*smry2$g0),color="#FF0000") +
    geom_line(aes(x=smry2$privacyVal,y=100*smry2$g25),color="#990066") +
    geom_line(aes(x=smry2$privacyVal,y=100*smry2$g50),color="#660099") +
    geom_line(aes(x=smry2$privacyVal,y=100*smry2$g75),color="#3300CC") +
    geom_line(aes(x=smry2$privacyVal,y=100*smry2$g100),color="#0000FF")  +
    geom_line(aes(x=smry2$privacyVal,y=100*smry2$gMean),color="#228C22") +xlab("Privacy Indifference") + ylab("Google Search Share") +
    annotate(geom="text",color="#FF0000",x=smry2$privacyVal[1],y=100*smry2$g0[1],label="Min",hjust=1,vjust=.95) +
    annotate(geom="text",color="#990066",x=smry2$privacyVal[1],y=100*smry2$g25[1],label="25%",hjust=1,vjust=.95) +
    annotate(geom="text",color="#660099",x=smry2$privacyVal[1],y=100*smry2$g50[1],label="Med",hjust=1,vjust=.95) +
    annotate(geom="text",color="#3300CC",x=smry2$privacyVal[1],y=100*smry2$g75[1],label="75%",hjust=1,vjust=.95) +
    annotate(geom="text",color="#0000FF",x=smry2$privacyVal[1],y=100*smry2$g100[1],label="Max",hjust=1,vjust=.95) + 
    annotate(geom="text",color="#228C22",x=smry2$privacyVal[nrow(smry2)],y=100*smry2$gMean[nrow(smry2)],label="Mean",hjust=0,vjust=.95) + ggtitle("Google Market Share") + theme(
      axis.ticks.x = element_blank(),
      axis.text.x  = element_blank(),
      panel.background = element_blank(),
      plot.title.position = "plot",
      plot.title = element_text(hjust = 0.5)
    ) + annotate(geom="text",color="#E37151",x=.5,y=80,label="Duck Duck Go Enters at Tick 10",hjust=0,vjust=1.5) +
    annotate(geom="text",color="#E37151",x=3.5,y=80,label="Duck Duck Go Enters at Tick 100",hjust=0,vjust=1.5) 
    
  
  # now let's examine behavior over time 
  
  searchCtrl %>% transform(googBool=(searchEngine=="google"),afterTick=tick-duckTick) %>% filter(afterTick <=400 & afterTick >=0) %>% group_by(afterTick,key,duckTick) %>% summarize(googPct=mean(googBool)) %>% arrange(duckTick,afterTick) -> timeReport
  timeReport$googPct <- 100*timeReport$googPct
  timeReport[timeReport$duckTick==10,] -> timeSer1
  timeReport[timeReport$duckTick==100,] -> timeSer2

  timeSer1 %>% group_by(afterTick) %>% summarise(g0=min(googPct),g25=quantile(googPct,.25),g50=quantile(googPct,.5),g75=quantile(googPct,.75),g100=max(googPct),gMean=exp(mean(log(googPct)))) -> timeSmry1
  timeSer2 %>% group_by(afterTick) %>% summarise(g0=min(googPct),g25=quantile(googPct,.25),g50=quantile(googPct,.5),g75=quantile(googPct,.75),g100=max(googPct),gMean=exp(mean(log(googPct)))) -> timeSmry2
  
  # now transform time axis for second series
  timeSmry2$afterTick <- 450+ timeSmry2$afterTick
  
  ggplot()  +xlab("Time") + ylab("Google Search Share") +xlim(0,850)+ ylim(0,100) +
    geom_hline(yintercept =0, linetype="solid",alpha=.3) + 
    geom_hline(yintercept =10, linetype="dashed",alpha=.3) +
    geom_hline(yintercept =20, linetype="dashed",alpha=.3) +
    geom_hline(yintercept =30, linetype="dashed",alpha=.3) +
    geom_hline(yintercept =40, linetype="dashed",alpha=.3) +
    geom_hline(yintercept =50, linetype="dashed",alpha=.3) +
    geom_hline(yintercept =60, linetype="dashed",alpha=.3) +
    geom_hline(yintercept =70, linetype="dashed",alpha=.3) +
    geom_hline(yintercept =80, linetype="dashed",alpha=.3) +
    geom_hline(yintercept =90, linetype="dashed",alpha=.3) +
    geom_hline(yintercept =100, linetype="solid",alpha=.3) +
    
    geom_vline(xintercept =0, linetype="solid",alpha=.3) +
    geom_vline(xintercept =50, linetype="dashed",alpha=.3) +
    geom_vline(xintercept =100, linetype="dashed",alpha=.3) +
    geom_vline(xintercept =150, linetype="dashed",alpha=.3) +
    geom_vline(xintercept =200, linetype="dashed",alpha=.3) +
    geom_vline(xintercept =250, linetype="dashed",alpha=.3) +
    geom_vline(xintercept =300, linetype="dashed",alpha=.3) +
    geom_vline(xintercept =350, linetype="dashed",alpha=.3) +
    geom_vline(xintercept =400, linetype="solid",alpha=.3) +
    
    geom_line(aes(x=timeSmry1$afterTick,y=timeSmry1$g0),color="#FF0000") +
    geom_line(aes(x=timeSmry1$afterTick,y=timeSmry1$g25),color="#990066") +
    geom_line(aes(x=timeSmry1$afterTick,y=timeSmry1$g50),color="#660099") +
    geom_line(aes(x=timeSmry1$afterTick,y=timeSmry1$g75),color="#3300CC") +
    geom_line(aes(x=timeSmry1$afterTick,y=timeSmry1$g100),color="#0000FF")  +
    geom_line(aes(x=timeSmry1$afterTick,y=timeSmry1$gMean),color="#228C22") +
    
    annotate(geom="text",color="#FF0000",x=400,y=timeSmry1$g0[[400]],label="Min",hjust=0,vjust=.95) +
    annotate(geom="text",color="#990066",x=400,y=timeSmry1$g25[400],label="25%",hjust=0,vjust=1) +
    annotate(geom="text",color="#660099",x=400,y=timeSmry1$g50[400],label="Med",hjust=0,vjust=.95) +
    annotate(geom="text",color="#3300CC",x=400,y=timeSmry1$g75[400],label="75%",hjust=0,vjust=-1) +
    annotate(geom="text",color="#0000FF",x=400,y=timeSmry1$g100[400],label="Max",hjust=0,vjust=.95) +
    annotate(geom="text",color="#228C22",x=400,y=timeSmry1$gMean[400],label="Mean",hjust=0,vjust=-.5) +
    
    geom_vline(xintercept =450, linetype="solid",alpha=.3) +
    geom_vline(xintercept =500, linetype="dashed",alpha=.3) +
    geom_vline(xintercept =550, linetype="dashed",alpha=.3) +
    geom_vline(xintercept =600, linetype="dashed",alpha=.3) +
    geom_vline(xintercept =650, linetype="dashed",alpha=.3) +
    geom_vline(xintercept =700, linetype="dashed",alpha=.3) +
    geom_vline(xintercept =750, linetype="dashed",alpha=.3) +
    geom_vline(xintercept =800, linetype="dashed",alpha=.3) +
    geom_vline(xintercept =850, linetype="solid",alpha=.3) +
    
    geom_line(aes(x=timeSmry2$afterTick,y=timeSmry2$g0),color="#FF0000") +
    geom_line(aes(x=timeSmry2$afterTick,y=timeSmry2$g25),color="#990066") +
    geom_line(aes(x=timeSmry2$afterTick,y=timeSmry2$g50),color="#660099") +
    geom_line(aes(x=timeSmry2$afterTick,y=timeSmry2$g75),color="#3300CC") +
    geom_line(aes(x=timeSmry2$afterTick,y=timeSmry2$g100),color="#0000FF")  +
    geom_line(aes(x=timeSmry2$afterTick,y=timeSmry2$gMean),color="#228C22") +

    annotate(geom="text",color="#FF0000",x=850,y=timeSmry2$g0[[400]],label="Min",hjust=0,vjust=.95) +
    annotate(geom="text",color="#990066",x=850,y=timeSmry2$g25[400],label="25%",hjust=0,vjust=1) +
    annotate(geom="text",color="#660099",x=850,y=timeSmry2$g50[400],label="Med",hjust=0,vjust=.95) +
    annotate(geom="text",color="#3300CC",x=850,y=timeSmry2$g75[400],label="75%",hjust=0,vjust=-1) +
    annotate(geom="text",color="#0000FF",x=850,y=timeSmry2$g100[400],label="Max",hjust=0,vjust=.95) +
    annotate(geom="text",color="#228C22",x=850,y=timeSmry2$gMean[400],label="Mean",hjust=0,vjust=-.5) +
   
    ggtitle("Google Market Share") + 
    theme(
      axis.ticks.x = element_blank(),
      axis.text.x  = element_blank(),
      panel.background = element_blank(),
      plot.title.position = "plot",
      plot.title = element_text(hjust = 0.5)
    ) + 
    annotate(geom="text",color="#E37151",x=200,y=95,label="Duck Duck Go Enters at Tick 10",hjust=0,vjust=1.5) +
    annotate(geom="text",color="#E37151",x=500,y=95,label="Duck Duck Go Enters at Tick 100",hjust=0,vjust=1.5) 
  
  # now, consider the means
  
  # reverse transformation
  timeSmry2$afterTick <- timeSmry2$afterTick-450
  
  paste0(names(timeSmry1)[2:7],"_10") -> names(timeSmry1)[2:7]
  paste0(names(timeSmry2)[2:7],"_100") -> names(timeSmry2)[2:7]
  
  merge(timeSmry1,timeSmry2,by="afterTick") -> jointTime
  
  jointTime$meanDelta <- jointTime$gMean_100-jointTime$gMean_10
  jointTime$delta0 <- jointTime$g0_100-jointTime$g0_10
  jointTime$delta25 <- jointTime$g25_100-jointTime$g25_10
  jointTime$delta50 <- jointTime$g50_100-jointTime$g50_10
  jointTime$delta75 <- jointTime$g75_100-jointTime$g75_10
  jointTime$delta100 <- jointTime$g100_100-jointTime$g100_10
  
  
plot(jointTime$afterTick,jointTime$meanDelta)  
ggplot() + 
  #geom_line(aes(x=jointTime$afterTick,y=jointTime$delta0),color="#FF0000") +
  #geom_line(aes(x=jointTime$afterTick,y=jointTime$delta25),color="#990066") +
  #geom_line(aes(x=jointTime$afterTick,y=jointTime$delta50),color="#660099") +
  #geom_line(aes(x=jointTime$afterTick,y=jointTime$delta75),color="#3300CC") +
  #geom_line(aes(x=jointTime$afterTick,y=jointTime$delta100),color="#0000FF") +
  geom_line(aes(x=jointTime$afterTick,y=jointTime$meanDelta),color="#228C22") +
  xlab("Time Since Duck Duck Go Entered") + ylab("Difference of Mean Google Share")


# let's split this out by privacy preference 

searchCtrl %>% transform(googBool=(searchEngine=="google"),afterTick=tick-duckTick) %>% filter(afterTick <=400 & afterTick >=0) %>% group_by(afterTick,key,duckTick,privacyVal) %>% summarize(googPct=mean(googBool)) %>% arrange(privacyVal,duckTick,afterTick) -> timeReportPriv
timeReportPriv$googPct <- 100*timeReport$googPct
timeReportPriv[timeReportPriv$duckTick==10,] -> timeSerPriv1
timeReportPriv[timeReportPriv$duckTick==100,] -> timeSerPriv2

timeSerPriv1 %>% group_by(privacyVal,afterTick) %>% summarise(g0=min(googPct),g25=quantile(googPct,.25),g50=quantile(googPct,.5),g75=quantile(googPct,.75),g100=max(googPct),gMean=exp(mean(log(googPct)))) -> timeSmryPriv1
timeSerPriv2 %>% group_by(privacyVal,afterTick) %>% summarise(g0=min(googPct),g25=quantile(googPct,.25),g50=quantile(googPct,.5),g75=quantile(googPct,.75),g100=max(googPct),gMean=exp(mean(log(googPct)))) -> timeSmryPriv2

paste0(names(timeSmryPriv1)[3:8],"_10") -> names(timeSmryPriv1)[3:8]
paste0(names(timeSmryPriv2)[3:8],"_100") -> names(timeSmryPriv2)[3:8]

merge(timeSmryPriv1,timeSmryPriv2,by=c("privacyVal","afterTick")) -> jointTimePriv

jointTimePriv$meanDelta <- jointTimePriv$gMean_100-jointTimePriv$gMean_10

unique(jointTimePriv$privacyVal)
table(jointTimePriv$privacyVal)

# now, plot all of these 

ggplot() + geom_line(aes(x=jointTimePriv$afterTick,y=jointTimePriv$meanDelta,color=jointTimePriv$privacyVal))
