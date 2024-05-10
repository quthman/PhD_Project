setwd("C:/Users/uthma/Dropbox (UFL)/PhD/Lab analysis result/Blk9_CBlk_15-60_Corrected")
dat = read.csv("kinetics_2soils.csv")
library(ggplot2)
library(extrafont)
#loadfonts(device = "win")
#fonts() ### See the names of fonts available
#names(pdfFonts())
#https://github.com/wch/extrafont/issues/88 (issues to fix fonts)
science_theme <- theme(plot.background = element_rect(fill = "white"), 
						panel.background = element_rect(fill="white"), 
						panel.grid.major = element_blank(),
						panel.grid.minor = element_blank(), 
						axis.line = element_line(linewidth = 0.7, color = "black"), 
						legend.position = c(0.8,0.2), 
						axis.text=element_text(size = 12, 
						family="Microsoft Sans Serif",
						color = "black"),
						text = element_text(size = 12, 
						family="Microsoft Sans Serif",
						color = "black"),
						strip.background.x = element_rect("white"),
						strip.background.y = element_rect("white"))

datB=dat[dat$Soil.Type=="B",]
datC=dat[dat$Soil.Type=="C",]
t=seq(0,64,length.out=1000)
osne <- function(k2,kd,t) {
	r=1+kd
	b=1/r
	predRC=1/r + (1-1/r)*exp(-(k2*r)*t)
	return(predRC)
}
library(plyr)
dat_sumB =  ddply(datB,~Depth+Time,summarise,mB=mean(Ce_C0),
					sdB=sd(Ce_C0),seB = sdB/sqrt(3))
library(minpack.lm)
B15=dat_sumB[dat_sumB$Depth=="0-15",]
fit15 = nlsLM(mB ~ 1/(1+k)+(1-(1/(1+k)))*exp(-(a*(1+k))*Time), 
				start = list(a=0.005,k=0.5), data=B15)
B30=dat_sumB[dat_sumB$Depth=="15-30",]
fit30 = nlsLM(mB ~ 1/(1+k)+(1-(1/(1+k)))*exp(-(a*(1+k))*Time), 
				start = list(a=0.005,k=0.5), data=B30)

B45=dat_sumB[dat_sumB$Depth=="30-45",]
fit45 = nlsLM(mB ~ 1/(1+k)+(1-(1/(1+k)))*exp(-(a*(1+k))*Time), 
				start = list(a=0.005,k=0.5), data=B45)

B60=dat_sumB[dat_sumB$Depth=="45-60",]
fit60 = nlsLM(mB ~ 1/(1+k)+(1-(1/(1+k)))*exp(-(a*(1+k))*Time), 
				start = list(a=0.005,k=0.5), data=B60)

pb1 = osne(coefficients(fit15)[1],coefficients(fit15)[2],t)
pb2 = osne(coefficients(fit30)[1],coefficients(fit30)[2],t)
pb3 = osne(coefficients(fit45)[1],coefficients(fit45)[2],t)
pb4 = osne(coefficients(fit60)[1],coefficients(fit60)[2],t)
pb = c(pb1,pb2,pb3,pb4)
d = rep(c("0-15","15-30","30-45","45-60"),each=1000)
t_4 = rep(t,4)
new.dataB = data.frame(cbind(t_4,d,pb))
new.dataB$t_4 = as.numeric(new.dataB$t_4)		
new.dataB$pb = as.numeric(new.dataB$pb)

pltB=ggplot()+
	geom_point(data=dat_sumB, aes(x=Time,y=mB,color=Depth,shape=Depth),show.legend = TRUE)+
	geom_errorbar(data=dat_sumB, aes(x=Time,ymin=mB-seB,ymax=mB+seB,colour=Depth), width=.1)+
	geom_line(data=new.dataB, aes(x=t_4, y=as.numeric(pb), color=d),lwd=1,show.legend = TRUE)+
	labs(y=bquote('C'[e]/'C'[0]),
			x=bquote('Time, hours'))+
	scale_x_continuous(expand = c(0, 0), limits = c(0, 65)) +
	scale_y_continuous(expand = c(0, 0), limits = c(0, 1))+
	scale_color_manual(name="Depth, cm",labels= c("0-15","15-30","30-45","45-60"),
						values = c("red","green","blue","purple"))+
	scale_shape_manual(name="Depth, cm",labels= c("0-15","15-30","30-45","45-60"),
						values = c(15,16,17,18))+
#	labs(color = "Depth, cm")+
	science_theme
ggsave(pltB, file=paste0("plotB_all_kinetics.tiff"), width = 6, height = 4)

dat_sumC =  ddply(datC,~Depth+Time,summarise,mC=mean(Ce_C0),
					sdC=sd(Ce_C0),seC = sdC/sqrt(3))

C15=dat_sumC[dat_sumC$Depth=="0-15",]
Cfit15 = nlsLM(mC ~ 1/(1+k)+(1-(1/(1+k)))*exp(-(a*(1+k))*Time), 
				start = list(a=0.005,k=0.5), data=C15)
C30=dat_sumC[dat_sumC$Depth=="15-30",]
Cfit30 = nlsLM(mC ~ 1/(1+k)+(1-(1/(1+k)))*exp(-(a*(1+k))*Time), 
				start = list(a=0.005,k=0.5), data=C30)

C45=dat_sumC[dat_sumC$Depth=="30-45",]
Cfit45 = nlsLM(mC ~ 1/(1+k)+(1-(1/(1+k)))*exp(-(a*(1+k))*Time), 
				start = list(a=0.005,k=0.5), data=C45)

C60=dat_sumC[dat_sumC$Depth=="45-60",]
Cfit60 = nlsLM(mC ~ 1/(1+k)+(1-(1/(1+k)))*exp(-(a*(1+k))*Time), 
				start = list(a=0.005,k=0.5), data=C60)

pc5 = osne(coefficients(Cfit15)[1],coefficients(Cfit15)[2],t)
pc6 = osne(coefficients(Cfit30)[1],coefficients(Cfit30)[2],t)
pc7 = osne(coefficients(Cfit45)[1],coefficients(Cfit45)[2],t)
pc8 = osne(coefficients(Cfit60)[1],coefficients(Cfit60)[2],t)
pc = c(pc5,pc6,pc7,pc8)
new.dataC = data.frame(cbind(t_4,d,pc))
new.dataC$t_4 = as.numeric(new.dataC$t_4)		
new.dataC$pc = as.numeric(new.dataC$pc)

pltC=ggplot()+
	geom_point(data=dat_sumC, aes(x=Time,y=mC,color=Depth,shape=Depth),show.legend = TRUE)+
	geom_errorbar(data=dat_sumC, aes(x=Time,ymin=mC-seC,ymax=mC+seC,colour=Depth), width=.1)+
	geom_line(data=new.dataC, aes(x=t_4, y=as.numeric(pc), colour=d),lwd=1,show.legend = TRUE)+
	labs(y=bquote('C'[e]/'C'[0]),
			 x=bquote('Time, hours'))+
	scale_x_continuous(expand = c(0, 0), limits = c(0, 65)) +
	scale_y_continuous(expand = c(0, 0), limits = c(0, 1))+
	scale_color_manual(name="Depth, cm",labels= c("0-15","15-30","30-45","45-60"),
						values = c("red","green","blue","purple"))+
	scale_shape_manual(name="Depth, cm",labels= c("0-15","15-30","30-45","45-60"),
						values = c(15,16,17,18))+
#	labs(color = "Depth, cm")+
	science_theme
ggsave(pltC, file=paste0("plotC_all_kinetics.tiff"), width = 6, height = 4)

