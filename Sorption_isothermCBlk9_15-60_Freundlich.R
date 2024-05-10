setwd("C:/Users/uthma/Dropbox (UFL)/PhD/Lab analysis result/Blk9_CBlk_15-60_Corrected/Freundlich")
dat = read.csv("cityblk9_isotherm.csv")
#dat = data.frame(dat)
#dat$Time = factor(dat$Time); dat$C0=factor(dat$C0)

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
						axis.line = element_line(size = 0.7, color = "black"), 
						legend.position = c(0.1,0.9), 
						axis.text=element_text(size = 12, 
						family="Microsoft Sans Serif",
						color = "black"),
						text = element_text(size = 12, 
						family="Microsoft Sans Serif",
						color = "black"),
						strip.background.x = element_rect("white"),
						strip.background.y = element_rect("white"))


## Sorption isotherm
soil_type=c("B","C")
depth = c("15-30","30-45","45-60")
Kd = c()
KLL = c()
KUL = c()
R2 = c()
for (j in 1:length(seq(2))){
	data=dat[dat$SoilType==soil_type[j],]
	for (i in 1:length(depth)){
		input=data[data$Depth==depth[i],]
		model = lm(log(Se)~log(Ce),data=input)
		Kd[i] = exp(coefficients(model)[[1]])
		R2[i]=format(summary(model)$r.squared, digits = 3)
		KLL[i]=exp(confint(model)[1,1])
		KUL[i]=exp(confint(model)[1,2])
		eq <- substitute(italic(R)^2~"="~r2, 
									 list(r2 = R2[i]))
		qe <- substitute(italic(K[D])~"="~KD, 
									 list(KD = Kd[i]))
		qed <- substitute(italic(Depth)~"="~depth~"cm", 
										list(depth = depth[i]))
		eq=as.character(as.expression(eq))
		qe=as.character(as.expression(qe))
		qed=as.character(as.expression(qed))
	
		p=ggplot(na.omit(input), aes(x=log(Ce)))+
			geom_point(aes(y=log(Se), color="black"),show.legend = TRUE)+
			geom_line(aes(y=fitted(model), color="red"), lwd=1,show.legend = TRUE)+
			labs(y=bquote('ln(Se),'~ug~g^-1),
				 x=bquote('ln(Ce),'~ug~mL^-1))+
			scale_colour_manual(name="",values = c('black' = 'black','red' = 'red'), 
								labels = c('Measured','Predicted'))+
			geom_text(x=log(2), y=0.96*log(max(input$Se)), label=eq, parse=TRUE)+
			geom_text(x=log(2),y=0.85*log(max(input$Se)),label=qe, parse=TRUE)+
			geom_text(x=log(2),y=0.75*log(max(input$Se)),label=qed, parse=TRUE)+
			science_theme
  	ggsave(p, file=paste0("plot_", soil_type[j], depth[i],".png"), width = 6, height = 4)
	}
	output <- cbind(Kd = as.numeric(Kd),KLL=as.numeric(KLL),KUL=as.numeric(KUL),
					R2 = as.numeric(R2))
	output <- round(output, 3)
	output2 <- cbind(depth=seq(30,60,15),output)
	
	writexl::write_xlsx(data.frame(output2),path = paste0("output",soil_type[j],".xlsx"))
}
## Sorption kinetics

## Python was used