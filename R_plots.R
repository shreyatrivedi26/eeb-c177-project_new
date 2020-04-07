setwd("/home/eebc177student/Developer/Repos/eeb-c177-project/python-scripting")
sit=read.csv("SIT_average.csv",sep = ",",header = T)
library(ggplot2)
time=seq(1,54,1)
sit$anomaly=sit$col1-mean(sit$col1)
St_Dev=sd(sit$anomaly)
St_Dev2=2*sd(sit$anomaly)


ggplot(sit, aes(x = time , y = sit$anomaly)) +
  geom_line() + 
  geom_smooth(method = "lm", size=1,alpha = .2,aes(fill =  St_Dev),level=0.99)+
  geom_smooth(method = "lm", size=1,alpha = .2,aes(fill =  St_Dev2),level=0.99)


tm=seq(as.Date("2002-10-15"), as.Date("2011-09-15"), by="months")
dates=as.data.frame(tm)
dates$month <- months(dates$tm, abbreviate = T)

winter_spring <- subset(dates, month %in% c("May","Jun","Jul","Aug","Sep","Oct"))
spring.months <- subset(dates, month %in% c("Sep","Oct"))
winter.months <- subset(dates, month %in% c("May","Jun","Jul","Aug"))
june.months <- subset(dates, month %in% "Jun")
oct.months <- subset(dates, month %in% "Oct")


ggplot(sit, aes(x = winter_spring$tm, y = sit$anomaly)) +
  geom_line() +
  geom_ribbon( aes(ymin = St_Dev*-1, ymax = St_Dev,fill = St_Dev), alpha = .15) +
  geom_ribbon( aes(ymin = St_Dev2*-1, ymax = St_Dev2,fill = St_Dev2), alpha = .15)+
  geom_smooth(method = "lm", size=1,alpha = .2,level=0.99) + ylim(c(-0.5,0.5)) +xlab("Years")+
  ylab("Anomalies")+ggtitle("SIT Anomalies with mean deviations (2002-2011)")+labs(colur="Standard Deviation")

sit <- sit[-c(55), ]

#dens_sit <- ggplot(data=sit,aes(x=sit$anomaly, fill=winter_spring$month))
#dd_plot=dens_sit+ geom_density(stat="density", alpha=I(0.3)) +
#  xlab("SIT") +  ylab("Density") + ggtitle("Density Curve of SIT for Winter-Spring months(2002-2011)")
#dd_plot

dens_sit <- ggplot(data=sit,aes(x=sit$anomaly, y=winter_spring$month,fill=stat(x)))
dd_plot=dens_sit+ geom_density_ridges_gradient(rel_min_height=0.01)+
  scale_fill_viridis_c(name="SIT (in m)",option="C")+
  xlab("SIT") +  ylab("Months") + ggtitle("Density Estimates: SIT for Winter-Spring(2002-2011)")
dd_plot


