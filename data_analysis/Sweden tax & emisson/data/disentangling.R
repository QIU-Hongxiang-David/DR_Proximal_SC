###################################################
### Load file: disentangling_data.dta
###################################################

# Note: see the Stata do-file disentangling_regression.do for information on how the data set was created

library(foreign)
disentangling <- read.dta(file.choose())
attach(disentangling)

disentangling[1:46, 1:6]

###################################################
### Figure 13: Disentangling the Carbon Tax and VAT
###################################################

plot(year[11:46], CarbonTaxandVAT[11:46], type="l", lwd=2, col="black", ylim=c(0,3.5), xlab="Year", ylab="Metric tons per capita (CO2 from transport)", xaxs="i",yaxs="i" )
abline(v=1990,lty="dotted",lwd=2)
legend("bottomright",legend=c("No Carbon Tax, No VAT","No Carbon Tax, With VAT", "Carbon Tax and VAT"),
lty=c(5,4,1),col=c("black","black", "black"),lwd=c(2,2),cex=0.8)
lines(year[31:46], NoCarbonTaxWithVAT[31:46], lty="dotdash" , lwd=2, col="black")
lines(year[29:46], NoCarbonTaxNoVAT[29:46], lty="longdash" , lwd=2, col="black")

###################################################
### Figure 14: Gap in per capita CO2 Emissions from Transport: Synthetic Control vs. Simulation
###################################################

plot(year, CO2_reductions_simulation, type="l", lwd=2, col="gray50", ylim=c(-0.85,0.50), xlab="Year" , ylab="Gap in metric tons per capita (CO2 from transport)", xaxs="i",yaxs="i")
rect(2000, -0.845, 2004.9, 0.495,
     border = "gray95", col = "gray95")
lines(year, CO2_reductions_simulation, lty="solid", lwd=2, col="gray50")
abline(v=1990,lty="dotted",lwd=2)
abline(h = 0, col = "black", lty = "dashed", lwd = 2)
legend("bottomleft",legend=c("Synthetic Control result" ,"Simulation result"),
lty=c(1,1),col=c("black","gray50"),lwd=c(2,2),cex=0.8)
lines(year, CO2_reductions_synth, lty="solid", lwd=2, col="black")
arrows(1987,0.3,1989,0.3,col="black",length=.1)	
Cex.set <- 1
text(1981,0.3,"VAT + Carbon tax",cex=Cex.set) 

###################################################