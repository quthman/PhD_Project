setwd("C:/Users/uthma/Dropbox (UFL)/PhD/Lab analysis result")
dat<-read.csv("soil_imd.csv")
dat <- data.frame(dat)
dat$Trial <- factor(dat$Trial)
dat$Soil_depth <- factor(dat$Soil_depth)
dat$Soil_ID <- factor(dat$Soil_ID)
dat$EU <- factor(dat$EU)
dat$Trt <- factor(dat$Trt)
dat$Time <- factor(dat$Time)

library(lme4)
library(Matrix)
library(lattice)

model1 <- lmer(Conc~Trt*Soil_depth+Trt*Time+Soil_depth*Time+(1|Soil_ID), 
				data = dat[dat$Trial=="1",])
summary(model1)
plot(model1, type = c("p", "smooth"))
qqmath(model1, id = 0.05)
anovatab1 <- anova(model1)


model2 <- lmer(Conc~Trt*Soil_depth+Trt*Time+Soil_depth*Time+(1|Soil_ID), 
				data = dat[dat$Trial=="2",])
summary(model2)
plot(model2, type = c("p", "smooth"))
qqmath(model2, id = 0.05)
anovatab2 <- anova(model2)
