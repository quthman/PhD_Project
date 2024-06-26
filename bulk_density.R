setwd("C:/Users/uthma/Dropbox (UFL)/PhD/Lab analysis result/Bulk density")
dat <- read.csv("bd.csv")
str(dat)
dat$Hurricane <- factor(dat$Hurricane)
dat$Depth <- factor(dat$Depth)
hist(dat$BD)
md <- aov(BD~Hurricane*Depth, data=dat)
summary(md)
plot(md)
TukeyHSD(md, "Hurricane")
TukeyHSD(md, "Depth")
