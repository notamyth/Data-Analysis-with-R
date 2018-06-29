install.packages("readstata13")
install.packages("plm")
install.packages("lmtest")
install.packages("sandwich")
library(readstata13)
dat <- read.dta13("guns.dta")
summary(dat)
head(dat)
#write.csv(dat, "C:\\Amit\\Spring 18\\Econometrics\\Project\\gun.csv")
install.packages("corrplot")
library(corrplot)

#subsetting data to remove categorical variables like stateid, shall and year

df = subset(dat, select = -c(year,stateid,shall) )
corel<-cor(df)
corrplot(corel, method="circle", insig = "p-value",sig.level = 0.05)
corrplot(corel, method="number")
install.packages("tidyr")
install.packages("ggplot2")
#library(purrr)
library(tidyr)
library(ggplot2)

dat %>%
  #keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

#We see that most of our variables distribution is skewed to th right, hence we transform them using log 
#Violent crime rate, murder rate, robbery rate, incarceration rate, percent of state population that is 
#black and density variables have been transformed using the logarithmic functions.
ldensity <- log(dat$density)
linc_rate <-log(dat$incarc_rate)
lmur <- log(dat$mur)
lpb1064 <- log(dat$pb1064)
lvio <- log(dat$vio)
lrob <- log(dat$rob)
lpop <- log(dat$pop)
#data$ldensity <- ldensity
data = data.frame(dat$year,dat$avginc, ldensity, linc_rate, lmur,lpb1064, dat$pm1029, lpop, dat$pw1064, lrob,dat$shall, dat$stateid, lvio)

#plotting to see the distribution of transformed variables and their skewness
data %>%
 # keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()


## MODELS FOR VIOLENCE RATE
# MODEL 1: Simple Linear Regression

model1 <- lm(log(vio)~shall, data=dat)
summary(model1)

# MODEL 2: Multiple Regression

model2 <- lm(log(vio)~shall+log(incarc_rate), dat)
summary(model2)

# MODEL 3: Simple Linear Regression
model3 <- lm(log(vio)~log(incarc_rate), dat)
summary(model3)

# MODEL 4: Pooled Model
library(plm)
library(lmtest)
library("sandwich")
library("car")
model4 <- plm(log(vio)~shall+log(incarc_rate)+log(pb1064)+pw1064+log(pop)+pm1029+avginc+log(density),index=c("stateid","year"), model="pooling",data=dat)
summary(model4)

#White corrected Pooled Model
summary(model4, vcov = vcovHC)

#F-test for checking wether avginc is irrelevant 
linearHypothesis(model4, "avginc", vcov = vcovHC)

# MODEL 5: Fixed Effects Model
model5<-plm(log(vio)~shall+log(incarc_rate)+log(pb1064)+pw1064+avginc+pop+log(density)+pm1029,index=c("stateid","year"),model="within",data=dat)
summary(model5)
summary(model5, vcov = vcovHC)

# MODEL 6: Time and Entity Fixed Effects
model6<- plm(log(vio)~shall+log(incarc_rate)+log(pb1064)+pw1064+avginc+pop+log(density)+pm1029+factor(year),data=dat,index=c("stateid","year"), model ="within")
coeftest(model6, vcovHC)
# MODEL 7: Random Effects
model7<- plm(log(vio)~shall+log(incarc_rate)+log(pb1064)+pw1064+avginc+pop+log(density)+pm1029+factor(year),data=dat,index=c("stateid","year"), model ="random")
coeftest(model7, vcovHC)

#Hausman Test for Endogeneity : One model is inconsistent. Since our data is not randomly sampled hence we reject random effects model
phtest(model6, model7)


## MODELS FOR MURDER RATE
# MODEL 2.1: Simple Linear Regression

model2.1 <- lm(log(mur)~shall, data=dat)
summary(model2.1)
# MODEL 2.2

model2.2 <- lm(log(mur)~shall+log(incarc_rate), dat)
summary(model2.2)

# MODEL 2.3: Simple Linear Regression
model2.3 <- lm(log(mur)~log(incarc_rate), dat)
summary(model2.3)

# MODEL 4: Pooled Model
library(plm)
library(lmtest)
library("sandwich")
library("car")
model2.4 <- plm(log(mur)~shall+log(incarc_rate)+log(pb1064)+pw1064+pop+pm1029+avginc+log(density),index=c("stateid","year"), model="pooling",data=dat)
summary(model2.4)

#White corrected Pooled Model
summary(model2.4, vcov = vcovHC)

#F-test for checking whether these variables are irrelevant or is there ommitted variable bias
linearHypothesis(model2.4, c("avginc","pw1064","shall", "pop", "log(density)"), vcov = vcovHC)

# MODEL 2.5: Fixed Effects Model
model2.5<-plm(log(mur)~shall+log(incarc_rate)+log(pb1064)+pw1064+avginc+pop+log(density)+pm1029,index=c("stateid","year"),model="within",data=dat)
summary(model2.5)
summary(model2.5, vcov = vcovHC)

# MODEL 2.6: Time and Entity Fixed Effects
model2.6<- plm(log(mur)~shall+log(incarc_rate)+log(pb1064)+pw1064+avginc+pop+log(density)+pm1029+factor(year),data=dat,index=c("stateid","year"), model ="within")
coeftest(model2.6, vcovHC)
# MODEL 2.7: Random Effects
model2.7<- plm(log(mur)~shall+log(incarc_rate)+log(pb1064)+pw1064+avginc+pop+log(density)+pm1029+factor(year),data=dat,index=c("stateid","year"), model ="random")
coeftest(model2.7, vcovHC)

#Hausman Test for Endogeneity : One model is inconsistent. Since our data is not randomly sampled hence we reject random effects model
phtest(model2.6, model2.7)


## MODELS FOR ROBBERY RATE

# MODEL 1: Simple Linear Regression

model3.1 <- lm(log(rob)~shall, data=dat)
summary(model3.1)

# MODEL 2: Multiple Regression

model3.2 <- lm(log(rob)~shall+log(incarc_rate), dat)
summary(model3.2)

# MODEL 3: Simple Linear Regression
model3.3 <- lm(log(rob)~log(incarc_rate), dat)
summary(model3.3)
install.packages("car")
# MODEL 4: Pooled Model
library("plm")
library("lmtest")
library("sandwich")
library("car")
model3.4 <- plm(log(rob)~shall+log(incarc_rate)+log(pb1064)+pw1064+log(pop)+pm1029+avginc+log(density),index=c("stateid","year"), model="pooling",data=dat)
summary(model3.4)
plot(model3.4)

#F-test for checking whether these variables are irrelevant or is there ommitted variable bias
linearHypothesis(model3.4, c("avginc","pw1064","shall", "log(pop)", "log(density)"), vcov = vcovHC, test="F")

linearHypothesis(model3.4, "shall", vcov = vcovHC, test="F")

linearHypothesis(model3.4, "pw1064", vcov = vcovHC, test="F") # not significant

linearHypothesis(model3.4, "pm1029", vcov = vcovHC, test="F")



#plot
dat$resi <- model3.4$residuals
library(ggplot2)
ggplot(data = dat, aes(y = resi, x = linc_rate)) + geom_point(col = 'blue') + geom_abline(slope = 0)
# heteroskedasicity
lmtest::bptest(model3.4)  # Breusch-Pagan test
#
#	studentized Breusch-Pagan test

#data:  model3.4
#BP = 85.556, df = 8, p-value = 3.698e-15
#


#White corrected Pooled Model
summary(model3.4, vcov = vcovHC)



# MODEL 5: Fixed Effects Model
model3.5<-plm(log(rob)~shall+log(incarc_rate)+log(pb1064)+pw1064+avginc+log(pop)+log(density)+pm1029,index=c("stateid","year"),model="within",data=dat)
summary(model3.5)
lmtest::bptest(model3.5)
summary(model3.5, vcov = vcovHC)

# MODEL 6: Time and Entity Fixed Effects
model3.6<- plm(log(rob)~shall+log(incarc_rate)+log(pb1064)+pw1064+avginc+log(pop)+log(density)+pm1029+factor(year),data=dat,index=c("stateid","year"), model ="within")
summary(model3.6)
lmtest::bptest(model3.6)
coeftest(model3.6, vcovHC)
model3.61<- plm(log(rob)~shall+log(incarc_rate)+log(pb1064)+pw1064+avginc+log(pop)+log(density)+pm1029+factor(year),data=dat,index=c("stateid","year"), model ="within", effect="twoways")
coeftest(model3.61, vcovHC)


# MODEL 7: Random Effects
model3.7<- plm(log(rob)~shall+log(incarc_rate)+log(pb1064)+pw1064+avginc+log(pop)+log(density)+pm1029+factor(year),data=dat,index=c("stateid","year"), model ="random")
lmtest::bptest(model3.7)
coeftest(model3.7, vcovHC)
#Hausman Test for Endogeneity : One model is inconsistent. Since our data is not randomly sampled hence we reject random effects model
phtest(model3.6, model3.7)







