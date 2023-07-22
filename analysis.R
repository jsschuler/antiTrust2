################################################################################
#           Data Analysis Code for Antitrust                                   #
#           July 2023                                                          #
#           John S. Schuler                                                    #
#                                                                              #
################################################################################
library(sqldf)
library(dplyr)
library(rlist)
library(ggplot)
library(ggridges)

# read in mod runs
setwd("~/Research\ Code/antiTrustData")
read.csv("ctrl.csv",sep=",") -> control
list.files("~/Research\ Code/antiTrustData") -> allFi
allFrame <- list()
for (fi in allFi){
  if (fi!="ctrl.csv"){
    read.csv(fi,header=FALSE,sep=",") -> allFrame[[length(allFrame)+1]]
  }
}

data.frame(list.rbind(allFrame)) -> allDat
names(allDat) <- c("key","tick","agt","engine","duckTick","vpnTick","delTick","sharTick")
table(allDat$engine)
sort(unique(allDat$key)) -> allKey
sort(unique(control$key)) -> allCTRL

# now, get the final Duck Duck Go Market Share

allDat[allDat$tick==100,] -> lastTick
lastTick$googBool <- lastTick$engine=="google"
lastTick %>% group_by(key) %>% summarise(googPct=mean(googBool)) -> googSmry

# now read in ctrl Frame 
read.csv("ctrl.csv") -> control

merge(control,googSmry,by="key") -> joint
duckOnly <- joint[joint$order=="Any[1]",]
library(ggplot2)
ggplot()+geom_point(aes(x=duckOnly$privacyVal,y=duckOnly$googPct))

ggplot()+geom_point(aes(x=joint$privacyVal,y=joint$googPct))
ggplot(joint, aes(xgoogPct=, y=privacyVal)) + 
  geom_boxplot(color="red", fill="orange", alpha=0.2)

ggplot() + geom_point(aes(x=joint$pctConnected,y=joint$googPct))

# bin percent connected
round(as.numeric(joint$pctConnected )/ .05,0)*.05 -> joint$pctBinned

ggplot(joint, aes(x = googPct, y =as.character(pctBinned) , fill = googPct)) +
  geom_density_ridges(alpha=0.6, stat="binline", bins=20) +
  theme_ridges() + 
  theme(legend.position = "none")
                               