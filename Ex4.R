library(devtools)
library(Diagnoza)
library(weights)
library(radiant.data)
library(Weighted.Desc.Stat)
library(ggplot2)
data("gospodarstwa")

blaptop<-c(gospodarstwa$bf9_13i)
claptop<-c(gospodarstwa$cf9i_16)
dlaptop<-c(gospodarstwa$df11_11i)
elaptop<-c(gospodarstwa$ef9_10i)
flaptop<-c(gospodarstwa$ff9_10c)
glaptop<-c(gospodarstwa$gf18c_10)
hlaptop<-c(gospodarstwa$hf19c_10)

bwaga<-c(gospodarstwa$waga_gd_2003)
cwaga<-c(gospodarstwa$waga_gd_2005)
dwaga<-c(gospodarstwa$waga_gd_2007)
ewaga<-c(gospodarstwa$waga_gd_2009)
fwaga<-c(gospodarstwa$waga_gd_2011)
gwaga<-c(gospodarstwa$waga_gd_2013)
hwaga<-c(gospodarstwa$waga_gd_2015)

#CREATE DATA FRAMES AND REMOVE ALL NA VALUES INDIVIDUALLY FROM EACH FRAME
#IF THERE IS A NA IN ONE FRAME WE DO NOT REMOVE THE ROW FROM OTHERS
bframe<-as.data.frame(cbind(blaptop,bwaga))
bframe<-na.omit(bframe)
cframe<-as.data.frame(cbind(claptop,cwaga))
cframe<-na.omit(cframe)
dframe<-as.data.frame(cbind(dlaptop,dwaga))
dframe<-na.omit(dframe)
eframe<-as.data.frame(cbind(elaptop,ewaga))
eframe<-na.omit(eframe)
fframe<-as.data.frame(cbind(flaptop,fwaga))
fframe<-na.omit(fframe)
gframe<-as.data.frame(cbind(glaptop,gwaga))
gframe<-na.omit(gframe)
hframe<-as.data.frame(cbind(hlaptop,hwaga))
hframe<-na.omit(hframe)

b<-wtd.mean(x=bframe$blaptop, weights = bframe$bwaga)
c<-wtd.mean(x=cframe$claptop, weights = cframe$cwaga)
d<-wtd.mean(x=dframe$dlaptop, weights = dframe$dwaga)
e<-wtd.mean(x=eframe$elaptop, weights = eframe$ewaga)
f<-wtd.mean(x=fframe$flaptop, weights = fframe$fwaga)
g<-wtd.mean(x=gframe$glaptop, weights = gframe$gwaga)
h<-wtd.mean(x=hframe$hlaptop, weights = hframe$hwaga)


l<-c(b,c,d,e,f,g,h)

time<-c(2003,2005,2007,2009,2011,2013,2015)
df<-as.data.frame(cbind(l,time))

plot(df$l~df$time, xlab = "Years", ylab = "Mean amount of laptops",pch=20, main = "Change of mean amount of laptops per household \nin years 2003-2015", ylim=c(1.07,1.3))
text(df$time, df$l, labels=round(df$l,2), cex= 0.7, pos=3)
lines(df$l~df$time)
#CAAGR
meanB<-(df$l[length(df$l)]/df$l[1])^(1/12)
lines(2003:2015, df$l[1]*meanB^(0:12), col='red')
points(2003:2015, df$l[1]*meanB^(0:12), col='red', pch=20)
legend(2003, 1.27, legend=c("Mean amount of laptops", "CAGR line"),
       col=c("black", "red"), lty=1, cex=1)

#SIMPLE INDECES
l1<-df$l[1:length(df$l)]/df$l[1]*100
df1<-as.data.frame(cbind(l1,time))
plot(df1$l1~df1$time,ylim=c(99,121), pch=19, xlab="Years", ylab = "Percentage of growth", main="How the amount of laptops changed \nin comparison with previous years")
text(df1$time, df1$l1, labels=round(df1$l1), cex= 0.7, pos=3)
lines(df1$l1~df1$time)

#CHAIN INDECES
l2<-df$l[2:length(df$l)]/df$l[1:length(df$l)-1]*100
l2<-c(NA,l2)
df2<-as.data.frame(cbind(l2,time))
points(df2$l2~df2$time,col="green", pch=19)
text(df2$time, df2$l2, labels=round(df2$l2), cex= .7, pos=1, col ="green")
lines(df2$l2~df2$time,col="green")
legend(2003, 120, legend=c("Simple indices", "Chain indces"),
       col=c("black", "green"), lty=1, cex=1)

