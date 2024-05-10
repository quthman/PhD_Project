setwd("C:/Users/uthma/Dropbox (UFL)/PhD/Wave")
dat <- read.csv("hydrograph4.csv")
attach(dat)
#visualization of Q, Recharge and alpha
#dt_daily_rainfall_v <- cbind(dt_daily_rainfall, Q, Recharge)
#rm(list = ls())
DOY1 <- lubridate::ymd(DOY)

library(ggplot2)

g0 <- ggplot(dat, aes(DOY1, P))+
	geom_rect(aes(xmin=as.Date("2022-08-20"), 
				xmax=as.Date("2022-12-31"),
				ymin=-Inf, ymax=Inf),
				fill="grey95")+
	geom_bar(stat = 'identity', fill = "blue") +
	theme_bw() +
	ylab("P (mm)") +
	labs(tag = "a)",title="") +
	scale_y_reverse()+
#	scale_x_date(date_labels = ("%m/%d"))+
	theme(axis.title.x    = element_blank(),
			axis.text.x = element_blank(),
			axis.ticks.x = element_blank())

g1 <- ggplot(dat, aes(DOY1, IR))+
	geom_rect(aes(xmin=as.Date("2022-08-20"), 
				xmax=as.Date("2022-12-31"),
				ymin=-Inf, ymax=Inf),
				fill="grey95")+
	geom_bar(stat = 'identity', fill = "grey") +
	theme_bw() +
	ylab("Irri (mm)") +
	#ylim(20,0)+
	labs(tag = "b)") +
	scale_y_reverse(limits= c(20,0))+
#	scale_x_date(date_labels = ("%m/%d"))+
	theme(axis.title.x = element_blank(),
			axis.text.x = element_blank(),
			axis.ticks.x = element_blank())

g2 <- ggplot(dat, aes(DOY1, ET))+
	geom_rect(aes(xmin=as.Date("2022-08-20"), 
				xmax=as.Date("2022-12-31"),
				ymin=-Inf, ymax=Inf),
				fill="grey95")+
	geom_bar(stat = 'identity', fill = "red") +
	theme_bw() +
	ylab("ETo (mm)") +
	labs(tag = "c)") +
	scale_x_date(date_labels = ("%m/%d"))+
	theme(axis.title.x    = element_blank(),
			axis.ticks.x    = element_blank())

g3 <- ggplot(dat,aes(DOY1, obs1*100))+
	geom_rect(aes(xmin=as.Date("2022-08-20"), 
					xmax=as.Date("2022-12-31"),
					ymin=-Inf, ymax=Inf),
					fill="grey95")+
	geom_point(color="blue") +
	geom_line(aes(DOY1,sim1*100),color="red")+
#	scale_x_date(date_labels = ("%m/%d"))+
	scale_y_continuous(labels=scaleFUN,limits = c(9, 22))+
	ylab(expression(theta["0-15"] * "(%)"))+
	labs(tag = "d)") +
	theme_bw()+
	theme(axis.title.x = element_blank(),
			axis.text.x = element_blank(),
			axis.ticks.x = element_blank())

scaleFUN <- function(x) sprintf("%.f", x)

g4 <- ggplot(dat,aes(DOY1, obs2*100))+
	geom_rect(aes(xmin=as.Date("2022-08-20"), 
					xmax=as.Date("2022-12-31"),
					ymin=-Inf, ymax=Inf),
						fill="grey95")+
	geom_point(color="blue") +
	geom_line(aes(DOY1,sim2*100),color="red")+
#	scale_x_date(date_labels = ("%m/%d"))+
	scale_y_continuous(labels=scaleFUN,limits = c(9, 22))+
	ylab(expression(theta["15-30"] * "(%)"))+
	labs(tag = "e)") +
	theme_bw()+
	theme(axis.title.x = element_blank(),
				axis.text.x = element_blank(),
				axis.ticks.x = element_blank())

g5 <- ggplot(dat,aes(DOY1, obs3*100))+
		geom_rect(aes(xmin=as.Date("2022-08-20"), 
								xmax=as.Date("2022-12-31"),
								ymin=-Inf, ymax=Inf),
						fill="grey95")+
	geom_point(color="blue") +
	geom_line(aes(DOY1,sim3*100),color="red")+
	scale_y_continuous(labels=scaleFUN,limits = c(9, 22))+
#	scale_x_date(date_labels = ("%m/%d"))+
	ylab(expression(theta["30-45"] * "(%)"))+
	labs(tag = "f)") +
	theme_bw()+
	theme(axis.title.x = element_blank(),
				axis.text.x = element_blank(),
				axis.ticks.x = element_blank())

g6 <- ggplot(dat,aes(DOY1, obs4*100))+
	geom_rect(aes(xmin=as.Date("2022-08-20"), 
					xmax=as.Date("2022-12-31"),
					ymin=-Inf, ymax=Inf),
					fill="grey95")+
	geom_point(color="blue") +
	geom_line(aes(DOY1,sim4*100),color="red")+
	scale_x_date(date_labels = ("%m/%d"))+
	scale_y_continuous(labels=scaleFUN,limits = c(9, 22))+
	ylab(expression(theta["45-60"] * "(%)"))+
	xlab("Date (mm/dd)")+
	labs(tag = "g)") +
	theme_bw()


library(grid)
library(gridExtra)
g00 <- ggplot_gtable(ggplot_build(g0))
g01 <- ggplot_gtable(ggplot_build(g1))
g02 <- ggplot_gtable(ggplot_build(g2))
g03 <- ggplot_gtable(ggplot_build(g3))
g04 <- ggplot_gtable(ggplot_build(g4))
g05 <- ggplot_gtable(ggplot_build(g5))
g06 <- ggplot_gtable(ggplot_build(g6))


maxWidth = unit.pmax(g00$widths[2:3],g01$widths[2:3], g02$widths[2:3], 
						g03$widths[2:3],g04$widths[2:3],g05$widths[2:3],
						g06$widths[2:3])
g00$widths[2:3] <- maxWidth
g01$widths[2:3] <- maxWidth
g02$widths[2:3] <- maxWidth
g03$widths[2:3] <- maxWidth
g04$widths[2:3] <- maxWidth
g05$widths[2:3] <- maxWidth
g06$widths[2:3] <- maxWidth
hydro = grid.arrange(g00,g01, g02, g03,g04,g05, g06, ncol = 1, 
						 heights = c(2, 2, 2, 2, 2, 2, 2))
ggsave(hydro,file=paste0("hydroDR4.png"), width = 8, height = 12, dpi=700)
ggsave("hydroDR4.pdf", hydro)
