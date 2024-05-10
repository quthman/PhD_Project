setwd("C:/Users/uthma/Dropbox (UFL)/PhD/Lab analysis result/Blk9_CBlk_15-60_Corrected")
dat = read.csv("isotherm_all.csv")
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
						legend.position = c(0.1,0.8), 
						axis.text=element_text(size = 12, 
						family="Microsoft Sans Serif",
						color = "black"),
						text = element_text(size = 12, 
						family="Microsoft Sans Serif",
						color = "black"),
						strip.background.x = element_rect("white"),
						strip.background.y = element_rect("white"))
dat$Depth=factor(dat$Depth)
datB=dat[dat$SoilType=="B",]
datC=dat[dat$SoilType=="C",]

library(plyr)
datB_sum <- ddply(datB,~Depth+Rep,summarise,mCeB=mean(Ce),mSeB=mean(Se),
					sdCeB=sd(Ce),sdSeB =sd(Se), seCeB = sdCeB/sqrt(3),
					seSeB = sdSeB/sqrt(3)) # three replicates in total.
									
plotB_isotherm2=ggplot(datB_sum,aes(x = mCeB, y = mSeB, color = Depth, shape=Depth) ) +
	geom_point() +
	geom_errorbar(aes(x = mCeB, y = mSeB, xmin=mCeB-seCeB,xmax=mCeB+seCeB,ymin=mSeB-seSeB,ymax=mSeB+seSeB), width=0.1)+
	geom_smooth(method = "lm", formula = y~0+x,fullrange=T,se = F)+
	labs(y=bquote(S[e] ~~mu~g~g^-1),
			x=bquote(C[e] ~~mu~g~mL^-1))+
	scale_x_continuous(expand = c(0, 0),limits=c(0,6.5)) +
	scale_y_continuous(expand = c(0, 0),limits=c(0,5.5))+
	scale_color_manual(name="Depth, cm",labels= c("0-15","15-30","30-45","45-60"),
						values = c("red","green","blue","purple"))+
	scale_shape_manual(name="Depth, cm",labels= c("0-15","15-30","30-45","45-60"),
						values = c(15,16,17,18))+
#	labs(color = "Depth, cm")+
	science_theme
ggsave(plotB_isotherm2, file=paste0("plotB_isotherm2.tiff"), width = 6, height = 4)


datC_sum <- ddply(datC,~Depth+Rep,summarise,mCeC=mean(Ce),mSeC=mean(Se),
					sdCeC=sd(Ce),sdSeC =sd(Se), seCeC = sdCeC/sqrt(3),
					seSeC = sdSeC/sqrt(3)) # three replicates in total.
plotC_isotherm2=ggplot(datC_sum,aes(x = mCeC, y = mSeC, color = Depth, shape=Depth) ) +
	geom_point() +
	geom_errorbar(aes(x = mCeC, y = mSeC, xmin=mCeC-seCeC,xmax=mCeC+seCeC,
					ymin=mSeC-seSeC,ymax=mSeC+seSeC), width=0.1)+
	geom_smooth(method = "lm", formula = y~0+x,fullrange=T,se = F)+
	labs(y=bquote(S[e] ~~mu~g~g^-1),
			x=bquote(C[e] ~~mu~g~mL^-1))+
	scale_x_continuous(expand = c(0, 0),limits=c(0,6.5)) +
	scale_y_continuous(expand = c(0, 0),limits=c(0,5))+
	scale_color_manual(name="Depth, cm",labels= c("0-15","15-30","30-45","45-60"),
						values = c("red","green","blue","purple"))+
	scale_shape_manual(name="Depth, cm",labels= c("0-15","15-30","30-45","45-60"),
						values = c(15,16,17,18))+
#	labs(color = "Depth, cm")+
	science_theme
ggsave(plotC_isotherm2, file=paste0("plotC_isotherm2.tiff"), width = 6, height = 4)


kv = c(1.14,0.779,0.603,0.465,0.81,0.605,0.451,0.429)
trt = rep(c("B","C"),each=4)
mod = aov(kv~trt)
anova(mod)$`Pr(>F)`[1]
