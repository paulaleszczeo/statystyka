library(devtools)
library(Diagnoza)
library(weights)
library(ggplot2)
library(radiant.data)
data("osoby")
data("osobyDict")


w2<-osoby$waga_2015_osoby
income<-c(osoby$hp65)
height<-c(osoby$hp52)
age<-c(osoby$wiek2015)
gender<-c(osoby$plec_all)
#CREATE A DATA FRAME OF THE VECTORS AND DELETE NAs
f<-as.data.frame(cbind(gender,age,height,income,w2))
f<-na.omit(f)
#LIMIT RESULTS ONLY TO PEOPLE ARE NOT UNDERAGED
f<-f[(f$age>=18),]

p=ggplot(f,aes(f$income,f$height))
p = p + geom_point(aes(colour = f$w2, size = f$w2)) + labs(x="Income [PLN]", y="Height [cm]")+
  scale_colour_gradient("Weight") + scale_size_continuous("Weight")
p
#CHECK THE CORRELATION BETWEEN HEIGHT AND INCOME
wtd.cors(f$income,f$height,f$w2)


ai<-wtd.mean(f$income,weights = f$w2) #AVERAGE INCOME
ah<-wtd.mean(f$height,weights = f$w2) #AVERAGE HEIGHT
sdi<-weighted.sd(x=f$income, wt=f$w2) #SD FOR INCOME
sdh<-weighted.sd(x=f$height,wt=f$w2)  #SD FOR HEIGHT
#LINEAR COEFFICIENTS for SD line
b1<-ah - ai*sdh/sdi
a1<-sdh/sdi

#REGRESSION LINES
regression1<-lm(f$heigh~f$income,weights = f$w2)
regression2 <- lm(f$income~f$height, weights = f$w2)

#COEFFICIENTS FOR SECOND REGRESSION LINE
alpha<-regression2$coefficients[1]
beta<-regression2$coefficients[2]
#PLOT SD LINE AND TWO REGRESSION LINES
p+geom_abline(slope=1/beta, intercept =-alpha/beta, color="green", size = .7,show.legend=TRUE) +
  geom_abline(slope = a1, intercept = b1, colour='#E41A1C', size=.7,show.legend=TRUE)+
  geom_smooth(method = "lm", mapping = aes(weight = f$w2), color = "maroon1", show.legend = FALSE, se = FALSE, size=.7) +
  annotate("text", x=26050, y=206, label= "regression nr 1" ) +
  annotate("text", x=9200, y=199, label= "SD line") +
  annotate("text", x=4000, y=206, label= "regression nr 2")

#PLOT RESIDUALS
plot(weighted.residuals(regression1), pch = 20, col = alpha("black", .1), xlab = "", ylab = "Residuals", main = "Residals for regression line nr 1")
plot(weighted.residuals(regression2), pch = 20, col = alpha("black", .1), xlab = "", ylab = "Residuals", main = "Residuals for regression line nr 2")

#PRINT REGRESSION MODELS
summary(regression1)
summary(regression2)

#PRINT RESIDUALS
summary(weighted.residuals(regression1))
summary(weighted.residuals(regression2))
        