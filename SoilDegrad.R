setwd("C:/Users/uthma/Dropbox (UFL)/PhD/Lab analysis result/Soil_degradation_analysis")
dat <- read.csv('soil_degradation.csv')
str(dat)
dat$SoilType <- factor(dat$SoilType)
dat$depth <- factor(dat$depth)
dat$Rep <- factor(dat$Rep)

first_order <- function(t, k) (10*exp(-k*t))
#model <- nls(St ~first_order(Time,S0,k) , data = dat[dat$SoilType=="B",], 
#						 start = c(S0=10,k=0.01))
#summary(model)
library(aomisc)

Soil = c("B","C")
depth = c(1,2,3,4)
#S0 = c()
k = c()
#S0LL = c()
#S0UL=c()
kll = c()
kul = c()
R2 = c()
for (i in 1:length(Soil)){
	input=dat[dat$SoilType==Soil[i],]
	for (j in 1:length(depth)){
		input1=input[input$depth==depth[j],]
		model <- nls(St ~first_order(Time,k),data=input1,start = c(k=0.001))
	#	model <- lm(St~Time, data =input1)
	#	S0[j]=coef(model)[1]
		k[j]= coef(model)[[1]]
	#	S0LL[j]=confint(model)[1,1]
	#	S0UL[j]=confint(model)[1,2]
		kll[j]=confint(model)[[1]]
		kul[j]=confint(model)[[2]]
		#R2[j]=format(summary(model)$r.squared, digits = 3)
		R2[j]=R2nls(model)[[1]]
			
	}

	output <- cbind(k=as.numeric(k), kll = as.numeric(kll), kul = as.numeric(kul),R2 = as.numeric(R2))
	#output <- cbind(S0 = as.numeric(S0),S0LL=as.numeric(S0LL),S0UL=as.numeric(S0UL)
	#								, kll = as.numeric(kll), kul = as.numeric(kul),R2 = as.numeric(R2))
	output <- round(output, 3)
	
	writexl::write_xlsx(data.frame(output),path = paste0("output", Soil[i],".xlsx"))
}

