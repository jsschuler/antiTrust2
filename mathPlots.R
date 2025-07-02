library(ggplot2)
library(latex2exp)
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

X <- seq(0,1,.01)
Y <- dbeta(X,5,5)
setwd("~/Research/antiTrust/")
# get quantiles
qbeta(c(.05,.25,.5,.75,.95),5,5) -> qs

ggplot() + geom_line(aes(x=X,y=Y)) +  geom_segment(aes(x=0,y=0,xend=1,yend=0), linetype = "solid", color = "black") + xlab("Support") + ylab("Density") +
  geom_segment(aes(x=qs[1],y=dbeta(qs[1],5,5),xend=qs[1],yend=0),linetype="dashed",color="black") +
  geom_segment(aes(x=qs[2],y=dbeta(qs[2],5,5),xend=qs[2],yend=0),linetype="dashed",color="black") +
  geom_segment(aes(x=qs[3],y=dbeta(qs[3],5,5),xend=qs[3],yend=0),linetype="dashed",color="black") +
  geom_segment(aes(x=qs[4],y=dbeta(qs[4],5,5),xend=qs[4],yend=0),linetype="dashed",color="black") +
  geom_segment(aes(x=qs[5],y=dbeta(qs[5],5,5),xend=qs[5],yend=0),linetype="dashed",color="black") +
  geom_text(aes(x = qs[1]-.02, y = dbeta(qs[1],5,5)/2), label = TeX("$5\\%$"), parse = TRUE) + 
  geom_text(aes(x = qs[2]-.02, y = dbeta(qs[1],5,5)/2), label = TeX("$25\\%$"), parse = TRUE) + 
  geom_text(aes(x = qs[3]-.02, y = dbeta(qs[1],5,5)/2), label = TeX("$50\\%$"), parse = TRUE) + 
  geom_text(aes(x = qs[4]-.02, y = dbeta(qs[1],5,5)/2), label = TeX("$75\\%$"), parse = TRUE) + 
  geom_text(aes(x = qs[5]-.02, y = dbeta(qs[1],5,5)/2), label = TeX("$95\\%$"), parse = TRUE) + 
  theme(
  panel.background = element_rect(fill = bgFill),
  plot.title = element_text(color =basePoint,hjust = 0.5),
  plot.background = element_rect(fill = bgFill),
  panel.grid = element_blank(),
  axis.text = element_text(color =basePoint),
  axis.title = element_text(color =basePoint),
  legend.background = element_rect(fill = bgFill),
  legend.text = element_text(color =basePoint)) -> modePlot

ggsave("modeDist.png",modePlot)

X <- seq(0,1,.01)
Y <- dexp(X,5)


qexp(c(.05,.25,.5,.75,.95),5,5) -> qs

ggplot() + geom_line(aes(x=X,y=Y)) +  geom_segment(aes(x=0,y=0,xend=1,yend=0), linetype = "solid", color = "black") + xlab("Support") + ylab("Density") +
  geom_segment(aes(x=qs[1],y=dexp(qs[1],5),xend=qs[1],yend=0),linetype="dashed",color="black") +
  geom_segment(aes(x=qs[2],y=dexp(qs[2],5),xend=qs[2],yend=0),linetype="dashed",color="black") +
  geom_segment(aes(x=qs[3],y=dexp(qs[3],5),xend=qs[3],yend=0),linetype="dashed",color="black") +
  geom_segment(aes(x=qs[4],y=dexp(qs[4],5),xend=qs[4],yend=0),linetype="dashed",color="black") +
  geom_segment(aes(x=qs[5],y=dexp(qs[5],5),xend=qs[5],yend=0),linetype="dashed",color="black") +
  geom_text(aes(x = qs[1]+.03, y = .1), label = TeX("$5\\%$"), parse = TRUE) + 
  geom_text(aes(x = qs[2]+.03, y = .1), label = TeX("$25\\%$"), parse = TRUE) + 
  geom_text(aes(x = qs[3]+.03, y = .1), label = TeX("$50\\%$"), parse = TRUE) + 
  geom_text(aes(x = qs[4]+.03, y = .1), label = TeX("$75\\%$"), parse = TRUE) + 
  geom_text(aes(x = qs[5]+.03, y = .1), label = TeX("$95\\%$"), parse = TRUE) + 
  theme(
    panel.background = element_rect(fill = bgFill),
    plot.title = element_text(color =basePoint,hjust = 0.5),
    plot.background = element_rect(fill = bgFill),
    panel.grid = element_blank(),
    axis.text = element_text(color =basePoint),
    axis.title = element_text(color =basePoint),
    legend.background = element_rect(fill = bgFill),
    legend.text = element_text(color =basePoint)) -> expPlot

ggsave("IntDist.png",expPlot)

# now generate a sample for 5 different agents 

rbeta(5,5,5) -> modeSamp
rexp(5,5) -> paramSamp 
factor <- as.factor(1:5)
# get alpha from mode and beta
alphaFunc <- function(mode,beta){

    alpha <- (1+beta*mode-2*mode)/(1-mode)
    return(alpha)
}

colorPal <- c("#000f70","#eda200","#ff0000","#ba096e","#016b01")

ggplot() + geom_point(aes(x=modeSamp,y=paramSamp,color=factor)) + xlab("Mode Value") + ylab("Parameter Value")  + scale_color_manual(values=colorPal) + theme(
  panel.background = element_rect(fill = bgFill),
  plot.title = element_text(color =basePoint,hjust = 0.5),
  plot.background = element_rect(fill = bgFill),
  panel.grid = element_blank(),
  axis.text = element_text(color =basePoint),
  axis.title = element_text(color =basePoint),
  legend.background = element_rect(fill = bgFill),
  legend.text = element_text(color =basePoint),
  legend.position = "none")

X <- seq(0,1,.01)
Y <- dbeta(X,5,5)

ggplot() + geom_line(aes(x=X,y=Y)) +  geom_segment(aes(x=0,y=0,xend=1,yend=0), linetype = "solid",color="black") + xlab("Support") + ylab("Density")  +
  geom_segment(aes(x=modeSamp[1],y=dbeta(modeSamp[1],5,5),xend=modeSamp[1],yend=0, color = colorPal[1]),linetype="dashed") +
  geom_segment(aes(x=modeSamp[2],y=dbeta(modeSamp[2],5,5),xend=modeSamp[2],yend=0, color = colorPal[2]),linetype="dashed") +
  geom_segment(aes(x=modeSamp[3],y=dbeta(modeSamp[3],5,5),xend=modeSamp[3],yend=0, color = colorPal[3]),linetype="dashed") +
  geom_segment(aes(x=modeSamp[4],y=dbeta(modeSamp[4],5,5),xend=modeSamp[4],yend=0, color = colorPal[4]),linetype="dashed") +
  geom_segment(aes(x=modeSamp[5],y=dbeta(modeSamp[5],5,5),xend=modeSamp[5],yend=0, color = colorPal[5]),linetype="dashed") +
  theme(
    panel.background = element_rect(fill = bgFill),
    plot.title = element_text(color =basePoint,hjust = 0.5),
    plot.background = element_rect(fill = bgFill),
    panel.grid = element_blank(),
    axis.text = element_text(color =basePoint),
    axis.title = element_text(color =basePoint),
    legend.background = element_rect(fill = bgFill),
    legend.text = element_text(color =basePoint),
    legend.position = "none")


X <- seq(0,1,.01)
Y <- dexp(X,5)

ggplot() + geom_line(aes(x=X,y=Y)) +  geom_segment(aes(x=0,y=0,xend=1,yend=0), linetype = "solid", color = "black") + xlab("Support") + ylab("Density") +
  geom_segment(aes(x=paramSamp[1],y=dexp(paramSamp[1],5),xend=paramSamp[1],yend=0),linetype="dashed",color=colorPal[1]) +
  geom_segment(aes(x=paramSamp[2],y=dexp(paramSamp[2],5),xend=paramSamp[2],yend=0),linetype="dashed",color=colorPal[2]) +
  geom_segment(aes(x=paramSamp[3],y=dexp(paramSamp[3],5),xend=paramSamp[3],yend=0),linetype="dashed",color=colorPal[3]) +
  geom_segment(aes(x=paramSamp[4],y=dexp(paramSamp[4],5),xend=paramSamp[4],yend=0),linetype="dashed",color=colorPal[4]) +
  geom_segment(aes(x=paramSamp[5],y=dexp(paramSamp[5],5),xend=paramSamp[5],yend=0),linetype="dashed",color=colorPal[5]) +
  theme(
    panel.background = element_rect(fill = bgFill),
    plot.title = element_text(color =basePoint,hjust = 0.5),
    plot.background = element_rect(fill = bgFill),
    panel.grid = element_blank(),
    axis.text = element_text(color =basePoint),
    axis.title = element_text(color =basePoint),
    legend.background = element_rect(fill = bgFill),
    legend.text = element_text(color =basePoint)) 


 
  