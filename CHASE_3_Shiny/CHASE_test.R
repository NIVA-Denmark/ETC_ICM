

library(dplyr)
library(ggplot2)

this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

source('./CHASE.R')

datafile<-"./data/CHASE_example.txt"
df<-read.table(datafile,sep=";",header=TRUE,stringsAsFactors=FALSE,encoding="UTF-8")


#InData<-Assessment(df)    #Individual indicator results
QE<-Assessment(df,3)  #Quality Element results
QEspr<-Assessment(df,2)   #QE 



QE$Waterbody<-as.factor(QE$Waterbody)
ymax=max(QE$ConSum,na.rm=TRUE)
ymax=ceiling(ymax)
if(ymax>5 & ymax<10){ymax=10}
if(ymax>1 & ymax<5){ymax=5}

if (is.null(QE)){return(NULL)}

levels<-data_frame(factor(c("High","Good","Moderate","Poor","Bad"),
                          levels=c("High","Good","Moderate","Poor","Bad")),
                   c(0.0,0.5,1,5,10),
                   c(0.5,1,5,10,ymax))
names(levels)[1] <- 'Status'
names(levels)[2] <- 'ymin'
names(levels)[3] <- 'ymax'

levels2<-levels
levels$x<-0.5
levels2$x<-0.5+max(as.numeric(QE$Waterbody))

levels<-rbind(levels, levels2)

levels<-levels[levels$ymin<=ymax,]
ymax2=max(levels$ymax,na.rm=TRUE)
levels[levels$ymax==ymax2,]$ymax<-ymax    
Palette1=c("#33AA00", "#99FF66", "#FFD5CC","#FF8066","#FF2B00" )

p<-ggplot(data=QE,x=Waterbody,y=ConSum) + theme_bw() +
  geom_point(size=5,data=QE, aes(x=factor(Waterbody), y=ConSum,shape=Matrix, ymin=0)) +
  geom_ribbon(data=levels,aes(x=x,ymin=ymin,ymax=ymax,fill=Status),alpha=0.5) +
  geom_point(size=5,data=QE, aes(x=factor(Waterbody), y=ConSum,shape=Matrix, ymin=0)) +
  scale_fill_manual(name="Status", values=Palette1)+
  xlab('Waterbody')+ylab('Contamination Sum')

QEspr
p