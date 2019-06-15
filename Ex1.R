library(devtools)
library(Diagnoza)
library(weights)
library(radiant.data)
library(Weighted.Desc.Stat)
library(ggplot2)
data("osoby")
data("osobyDict")

weight<-c(osoby$waga_2011_osoby)

months<-c("January","February","March","April","May","June","July","August","September","October","November","December")

wedding_dates<-c(osoby$fc12)

#frame of non-zero data about wedding dates and weight
f1<-as.data.frame(cbind(wedding_dates, weight))
f1<-na.omit(f1)

#compute weighted mean
wm<-wtd.mean(x = f1$wedding_dates, normwt = TRUE, weights = f1$weight)
wm
#histogram showing number of weddings in each month of the year
ggplot(f1,aes(f1$wedding_dates, weight = f1$weight))+
  geom_histogram(binwidth = 1, color="white", fill="palevioletred1")+scale_x_continuous(breaks=c(1:12), labels = months)+
  labs(x="Months", y="Number of weddings") + ggtitle("Number of weddings in each month")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + geom_vline(xintercept = wm)+
  annotate("text", x=7.5, y=3000, label= "Mean" )

#compute weighted sd
weighted.sd(x=f1$wedding_dates, wt = f1$weight)

#quariles
wtd.quantile(x = f1$wedding_dates, weights = f1$weight) 

#5th percentile
wtd.quantile(x = f1$wedding_dates, weights = f1$weight, probs = .05) 

#95th percentile
wtd.quantile(x = f1$wedding_dates, weights = f1$weight, probs = .95) 

#Skeweness
w.skewness(x=f1$wedding_dates, mu=f1$wedding_dates)
#Is assymetry substantial?
abs(w.skewness(f1$wedding_dates,f1$weight))>
  2*sqrt(6/length(f1$wedding_dates))
#Assymetry is substantial

#Kurtosis
w.kurtosis(x=f1$wedding_dates,mu=f1$weight)
#Is kurtosis substantial?
abs(w.kurtosis(f1$wedding_dates,f1$weight))>
  4*sqrt(6/length(f1$wedding_dates))
#Kurtosis is substantial

#ADDITIONAL VISUALIZATION - PIE CHARTS
st<-100*sum(f1$weight*(f1$wedding_dates==1))/sum(f1$weight)
lu<-100*sum(f1$weight*(f1$wedding_dates==2))/sum(f1$weight)
ma<-100*sum(f1$weight*(f1$wedding_dates==3))/sum(f1$weight)
kw<-100*sum(f1$weight*(f1$wedding_dates==4))/sum(f1$weight)
mj<-100*sum(f1$weight*(f1$wedding_dates==5))/sum(f1$weight)
cz<-100*sum(f1$weight*(f1$wedding_dates==6))/sum(f1$weight)
lip<-100*sum(f1$weight*(f1$wedding_dates==7))/sum(f1$weight)
si<-100*sum(f1$weight*(f1$wedding_dates==8))/sum(f1$weight)
wrz<-100*sum(f1$weight*(f1$wedding_dates==9))/sum(f1$weight)
paz<-100*sum(f1$weight*(f1$wedding_dates==10))/sum(f1$weight)
lis<-100*sum(f1$weight*(f1$wedding_dates==11))/sum(f1$weight)
gru<-100*sum(f1$weight*(f1$wedding_dates==12))/sum(f1$weight)

value<-c(st,lu,ma,kw,mj,cz,lip,si,wrz,paz,lis,gru)
prc <- round(value)
lbl <- paste(months,prc) # add percents to labels
lbl <- paste(lbl,"%",sep="")
pie(value,lbl, col = c("dodgerblue","cornflowerblue","darkolivegreen1","green","greenyellow","gold","goldenrod1",
                       
                       "darkgoldenrod1","chocolate1","chocolate","chocolate4","slateblue"))

winter<-100*(sum(f1$weight*(f1$wedding_dates==12))+sum(f1$weight*(f1$wedding_dates==1))+
               sum(f1$weight*(f1$wedding_dates==2)))/sum(f1$weight)
fall<-100*(sum(f1$weight*(f1$wedding_dates==9))+sum(f1$weight*(f1$wedding_dates==10))+
             sum(f1$weight*(f1$wedding_dates==11)))/sum(f1$weight)
spr<-100*(sum(f1$weight*(f1$wedding_dates==3))+sum(f1$weight*(f1$wedding_dates==4))+
            sum(f1$weight*(f1$wedding_dates==5)))/sum(f1$weight)
summ<-100*(sum(f1$weight*(f1$wedding_dates==6))+sum(f1$weight*(f1$wedding_dates==7))+
             sum(f1$weight*(f1$wedding_dates==8)))/sum(f1$weight)
value1<-c(winter,fall,spr,summ)
pct <- round(value1)
lbls <- paste(c("Winter", "Autumn", "Spring","Summer"),pct) # add percents to labels
lbls <- paste(lbls,"%",sep="")
pie(value1, lbls, col = c("deepskyblue","darkorange","greenyellow","gold1"))

