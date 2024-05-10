setwd("C:/Users/uthma/Dropbox (UFL)/PhD/Lab analysis result")
dat <- read.csv("soil_imd.csv")
#dat1 <- dat[imd!=0,]

dat <- data.frame(dat); names(dat);str(dat)

dat$Soil_ID<-factor(dat$Soil_ID); dat$Trt<-factor(dat$Trt);
dat$Time<-factor(dat$Time); dat$Soil_depth <- factor(dat$Soil_depth); 
dat$Trial = factor(dat$Trial); dat$EU <- factor(dat$EU);
dat$id <- factor(dat$id)
dat1 <- dat[dat$Trial==1,]
dat2 <- dat[dat$Trial==2,]

## Replace zeros with NA in a selected columns
## dat1["Conc"][dat1["Conc"] == 0] <- NA
## Quick visualization
library(ggplot2)
ggplot(dat1, aes(x=Time,y=Conc, col=Trt))+
	geom_point(show.legend=F)+
	facet_wrap(~id) + 
	theme_bw()

library(nlme)

## Simple Covariance Structure (Measurements within cats uncorrelated)
fit.sim <- gls(Conc ~ Trt * Soil_depth * Time, data = dat1)
summary(fit.sim)
anova(fit.sim)
AIC(fit.sim)
#getVarCov(fit.sim, individual="1",type="conditional")

## Separate means
library(multcompView)
library(emmeans)
c1 <- lsmeans(fit.sim, ~Trt:Time)
ms1 <- pairs(c1, adjust = "Tukey")
ms1; 
#plot(ms1); confint(ms1)

c2 <- lsmeans(fit.sim, ~Trt:Soil_depth)
ms2 <- pairs(c2, adjust = "Tukey")
ms2

fit.cs <- gls(Conc ~ Trt * Soil_depth * Time,
				corr = corCompSymm( form= ~ 1 | id), data = dat1)
summary(fit.cs)
anova(fit.cs)
AIC(fit.cs)
getVarCov(fit.cs, individual="1",type="conditional")

fit.un <- gls(Conc ~ Trt * Soil_depth * Time,
				corr=corSymm(form = ~ 1 | id),
				weights = varIdent(form = ~ 1 | Time), data = dat1)
summary(fit.un)
anova(fit.un)
AIC(fit.un)
getVarCov(fit.un, individual="1",type="conditional")

fit.ar1 <- gls(Conc ~ Trt * Soil_depth * Time,
				corr = corAR1( form= ~ 1 | id), data =dat1)

summary(fit.ar1)
anova(fit.ar1)
AIC(fit.ar1)

getVarCov(fit.ar1, individual="1",type="conditional")

fit.arh1 <- gls(Conc ~ Trt * Soil_depth * Time,
				corr = corAR1( form = ~ 1 | id),
				weight = varIdent(form = ~ 1 | Time), data=dat1)
summary(fit.arh1)
anova(fit.arh1)
AIC(fit.arh1)
getVarCov(fit.arh1, individual="1",type="conditional")

## Trial 2
## Simple Covariance Structure (Measurements within cats uncorrelated)
fit.sim <- gls(Conc ~ Trt * Soil_depth * Time,data = dat2)
summary(fit.sim)
anova(fit.sim)
AIC(fit.sim)
#getVarCov(fit.sim, individual="1",type="conditional")

## Separate means
c1 <- lsmeans(fit.sim, ~Trt:Time)
ms1 <- pairs(c1, adjust = "Tukey")
ms1; 

fit.cs <- gls(Conc ~ Trt * Soil_depth * Time,
				corr = corCompSymm(form= ~ 1 | id), data = dat2)
summary(fit.cs)
anova(fit.cs)
AIC(fit.cs)
getVarCov(fit.cs, individual="1",type="conditional")

fit.un <- gls(Conc ~ Trt * Soil_depth * Time,
				corr=corSymm(form = ~ 1 | id),
				weights = varIdent(form = ~ 1 | Time), data = dat2)
summary(fit.un)
anova(fit.un)
AIC(fit.un)
getVarCov(fit.un, individual="1",type="conditional")

fit.ar1 <- gls(Conc ~ Trt * Soil_depth * Time,
				corr = corAR1(form= ~ 1 | id), data =dat2)

summary(fit.ar1)
anova(fit.ar1)
AIC(fit.ar1)

getVarCov(fit.ar1, individual="1",type="conditional")

fit.arh1 <- gls(Conc ~ Trt * Soil_depth * Time,
				corr = corAR1(form = ~ 1 | id),
				weight = varIdent(form = ~ 1 | Time), data=dat2)
summary(fit.arh1)
anova(fit.arh1)
AIC(fit.arh1)
getVarCov(fit.arh1, individual="1",type="conditional")

## Visualization
## load the theme
library(extrafont)
library(ggplot2)
#loadfonts(device = "win")
#fonts() ### See the names of fonts available
#names(pdfFonts())
#https://github.com/wch/extrafont/issues/88 (issues to fix fonts)
science_theme <- theme(plot.background = element_rect(fill = "white"), 
						panel.background = element_rect(fill="white"), 
						panel.grid.major = element_blank(),
						panel.grid.minor = element_blank(), 
						axis.line = element_line(linewidth = 0.7, color = "black"), 
						legend.position = c(0.77,0.7), 
						axis.text=element_text(size = 12, 
						family="Microsoft Sans Serif",
								color = "black"),
						text = element_text(size = 12, 
						family="Microsoft Sans Serif",
								color = "black"),
						strip.background.x = element_rect("white"),
						strip.background.y = element_rect("white"))

## Plots
library(ggplot2)
library(lattice) ## panel.grid is from lattice
library(plyr) ## confidence interval by treatment and time

dat1_sum <- ddply(dat1[dat1$Trt!="1",],~Trt+Soil_depth+Time,
					summarise,sum=sum(Conc, na.rm=T),m=mean(Conc, na.rm=T),
					sd=sd(Conc, na.rm=T),se = sd/sqrt(length(Conc)),
					alpha = 0.05,degrees_of_freedom = length(Conc) - 1,
					t_score = qt(p=alpha/2, df=degrees_of_freedom,lower.tail=F),
					LL = m-se*t_score, UL=m+se*t_score)
dat1a_sum <- ddply(dat1[dat1$Trt!="1",],~Trt+Time,
					summarise,sum=sum(Conc, na.rm=T),m=mean(Conc, na.rm=T),
					sd=sd(Conc, na.rm=T),se = sd/sqrt(length(Conc)),
					alpha = 0.05,degrees_of_freedom = length(Conc) - 1,
					t_score = qt(p=alpha/2, df=degrees_of_freedom,lower.tail=F),
					LL = m-se*t_score, UL=m+se*t_score)

t1 <- c(
	'1' = 'Day 39', 
	'2' = 'Day 56', 
	'3' = 'Day 74'
	)
t2 <- c(
	'1' = 'Day 7', 
	'2' = 'Day 14', 
	'3' = 'Day 21'
	)

dep <- c (
	"1"="0−15",
	"2"="15−30",
	"3"="30−45",
	"4"="45−60"
	)
## Since two LL are negative, Conc. cannot be negative, 
#then force it to be zero
dat1_sum$LL[dat1_sum$LL<0] <- 0

imd=ggplot(dat1_sum, aes(x=Trt,y=m, fill=Soil_depth))+
	geom_bar(stat = "identity", position = "dodge")+
	geom_errorbar(aes(ymin=m,ymax=UL),width=.15, 
						position =position_dodge(width=0.9))+
	facet_grid(.~Time,labeller=labeller(Time=t1))+
	scale_x_discrete(name = "Standard Treatment",labels=c('2'='×2','3'='×4'))+
	scale_fill_discrete(name="Depth, cm",labels = dep)+
	theme_bw()+labs(y=bquote('Imidacloprid'~('mg'~'kg'^'−1')))+science_theme
ggsave(imd,file=paste0("soil_imd1.png"), width = 6, height = 4, dpi=500)

## Trt by time
dat1a_sum$LL[dat1a_sum$LL<0] <- 0

Let <- c("a","a","a","b","b","a")
#cbind(dat1a_sum,Let)
imd1a=ggplot(dat1a_sum, aes(x=Trt,y=m, fill=Time))+
	ylim(0,3.047003)+
	geom_bar(stat = "identity", position = "dodge")+
	scale_x_discrete(name = "Standard Treatment",labels=c('2'='×2','3'='×4'))+
	geom_errorbar(aes(ymin=LL,ymax=UL),width=.15,position =position_dodge(width=0.9))+
	geom_text(aes(label=Let, y = UL),position =position_dodge(width=0.9),vjust=-.2)+
	scale_fill_discrete(name="Sampling Time",labels = t1)+
	theme_bw()+labs(y=bquote('Imidacloprid'~('mg'~'kg'^'−1')))+
	science_theme+theme(legend.position=c(0.2,0.8))
ggsave(imd1a,file=paste0("soil_imd1a.png"), width = 6, height = 4, dpi=500)

## Trial 2

dat2_sum <- ddply(dat2[dat2$Trt!="1",],~Trt+Soil_depth+Time,
					summarise,sum=sum(Conc, na.rm=T),m=mean(Conc, na.rm=T),
					sd=sd(Conc, na.rm=T),se = sd/sqrt(length(Conc)),
					alpha = 0.05,degrees_of_freedom = length(Conc) - 1,
					t_score = qt(p=alpha/2, df=degrees_of_freedom,lower.tail=F),
					LL = m-se*t_score, UL=m+se*t_score)

## Since two LL are negative, Conc cannot be negative, then force it to be zero
dat2_sum$LL[dat2_sum$LL<0] <- 0

imd2=ggplot(dat2_sum, aes(x=Trt,y=m, fill=Soil_depth))+
	geom_bar(stat = "identity", position = "dodge")+
	geom_errorbar(aes(ymin=m,ymax=UL),width=.15, 
					position =position_dodge(width=0.9))+
	facet_grid(.~Time,labeller=labeller(Time=t2))+
	scale_x_discrete(name = "Standard Treatment",labels=c('2'='×2','3'='×4'))+
	scale_fill_discrete(name="Depth, cm",labels = dep)+
	theme_bw()+labs(y=bquote('Imidacloprid'~('mg'~'kg'^'−1')))+science_theme
ggsave(imd2,file=paste0("soil_imd2.png"), width = 6, height = 4, dpi=500)

dat2a_sum <- ddply(dat2[dat2$Trt!="1",],~Trt+Time,
					summarise,sum=sum(Conc, na.rm=T),m=mean(Conc, na.rm=T),
					sd=sd(Conc, na.rm=T),se = sd/sqrt(length(Conc)),
					alpha = 0.05,degrees_of_freedom = length(Conc) - 1,
					t_score = qt(p=alpha/2, df=degrees_of_freedom,lower.tail=F),
					LL = m-se*t_score, UL=m+se*t_score)

Let2 <- c("a","a","a","c","bc","ab")

imd2a=ggplot(dat2a_sum, aes(x=Trt,y=m, fill=Time))+
	geom_bar(stat = "identity", position = "dodge")+
	geom_errorbar(aes(ymin=LL,ymax=UL),width=.15,position = position_dodge(width=0.9))+
	geom_text(aes(label=Let2, y = UL),position =position_dodge(width=0.9),vjust=-.2)+
	scale_x_discrete(name = "Standard Treatment",labels=c('2'='×2','3'='×4'))+
	scale_fill_discrete(name="Sampling Time",labels = t2)+
	theme_bw()+labs(y=bquote('Imidacloprid'~('mg'~'kg'^'−1')))+
	science_theme+theme(legend.position=c(0.2,0.8))
ggsave(imd2a,file=paste0("soil_imd2a.png"), width = 6, height = 4, dpi=500)

dat3a_sum <- ddply(dat[dat$Trt=="3",],~Time+Soil_depth,
					summarise,sum=sum(Conc, na.rm=T),m=mean(Conc, na.rm=T),
					sd=sd(Conc, na.rm=T),se = sd/sqrt(length(Conc)),
					alpha = 0.05,degrees_of_freedom = length(Conc) - 1,
					t_score = qt(p=alpha/2, df=degrees_of_freedom,lower.tail=F),
					LL = m-se*t_score, UL=m+se*t_score)

