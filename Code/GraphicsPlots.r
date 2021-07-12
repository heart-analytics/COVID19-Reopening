rm(list=ls())

setwd("C:/Users/ukm/Box/COVID19/Papers/safeOpen/Revision/Simulation Code/SimulationOutput")


for(i in 1:100){
	file = paste(i,"_OUTPUT_Adaptive_2_Tests.txt",sep="")


	d = read.table(file, sep = ",")

	if(i == 1){
		S = as.numeric(rownames(d))
		T = d$Recovered
	}else{
		S = cbind(S, as.numeric(rownames(d)))
		T = cbind(T, d$Recovered)
		
	}

}

summ = function(x){

	x = sort(x)
	return(c(x[2],x[50],x[98]))	
}

St = t(apply(S, 1, FUN = summ))
Tt = t(apply(T, 1, FUN = summ))

colnames(St) = c("SLower", "SMean", "SUpper")
colnames(Tt) = c("TLower", "TMean", "TUpper")
	
d1 = as.data.frame(cbind(Time = seq(1, dim(d)[1]), St, Tt))

library(ggplot2)

d1 = d1[-1,]

g = ggplot(data = d1, aes(x=Time)) + 
	geom_line(aes(y=TMean), color = "red", size=1.6, alpha = 0.7) +
	geom_line(aes(y=SMean/3), color = "blue", size=1.6, alpha = 0.7) + 
	scale_y_continuous(
		name = "Daily Test",
		sec.axis = sec_axis(~.*3, name = "Daily Tests")
	) + xlab("Time (Days)") +
	theme(
    axis.title.y = element_text(size=17, color = "red", family="serif"),
    axis.title.y.right = element_text(size=17, color = "blue", family="serif"),
	axis.title.x = element_text(size=17, family="serif"),
	axis.text.x = element_text(size=15, family="serif"),
	axis.text.y = element_text(color="red", size=15, family="serif"),
	axis.text.y.right = element_text(color="blue", size=15, family="serif")
  )

plot(g)


