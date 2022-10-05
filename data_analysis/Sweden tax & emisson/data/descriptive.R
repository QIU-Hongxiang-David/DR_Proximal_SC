###################################################
### Load file: descriptive_data.dta 
###################################################

library(foreign)
descriptive <- read.dta(file.choose())
attach(descriptive)

descriptive[1:46, 1:14]

###################################################
### Figure 1: Gasoline Price Components in Sweden 1960-2005
###################################################


# Figure 1 (a): Plotting Gasoline price components (VAT, carbon tax, etc.)
plot(year[1:46], Real_Gasoline_Price[1:46], type="l", lwd=2, col="black", ylim=c(0,13), xlab="Year", ylab="Real price (SEK/litre)", xaxs="i",yaxs="i" )
abline(v=1990,lty="dotted",lwd=2)
legend("topleft",legend=c("Gasoline price","Energy tax", "VAT", "Carbon tax"),
lty=c(1,1,4,2),col=c("black","gray50", "black","black"),lwd=c(2,2),cex=0.8)
lines(year[1:46], Real_Carbontax[1:46], lty="dashed" , lwd=2, col="black")
lines(year[1:46], Real_VAT[1:46], lty="dotdash" , lwd=2, col="black")
lines(year[1:46], Real_Energytax[1:46], lty="solid" , lwd=2, col="gray50")

# Figure 1(b): Plotting the Gasoline price and the total tax component
plot(year[1:46], Real_Gasoline_Price[1:46], type="l", lwd=2, col="black", ylim=c(0,13), xlab="Year", ylab="Real price (SEK/litre)", xaxs="i",yaxs="i" )
abline(v=1990,lty="dotted",lwd=2)
legend("topleft",legend=c("Gasoline price", "Total tax"),
lty=c(1,2),col=c("black","black"),lwd=c(2,2),cex=.8)
lines(year[1:46], Real_total_tax[1:46], lty="dashed" , lwd=2, col="black")

###################################################
### Figure 2: Road Sector Fuel Consumption Per Capita in Sweden 1960-2005
###################################################

plot(year[1:46], gas_cons, type="l", lwd=2, col="black", ylim=c(0,600), xlab="Year", ylab="Road sector fuel consumption per capita (kg of oil equivalent)", xaxs="i",yaxs="i" )
abline(v=1990,lty="dotted",lwd=2)
legend("bottomright",legend=c("Gasoline","Diesel"),
lty=c(1:2),col=c("black","black"),lwd=c(2,2),cex=.8)
lines(year, diesel_cons, lty="dashed" , lwd=2, col="black")
arrows(1987,100,1989,100,col="black",length=.1)	
Cex.set <- 1
text(1981,100,"VAT + Carbon tax",cex=Cex.set)

###################################################
### Figure 3: Path Plot of per capita CO2 Emissions from Transport: Sweden vs. the OECD Average (14 donor countries)
###################################################

plot(year[1:46], CO2_Sweden, type="l", lwd=2, col="black", ylim=c(0,3), xlab="Year", ylab="Metric tons per capita (CO2 from transport)", xaxs="i",yaxs="i" )
abline(v=1990,lty="dotted",lwd=2)
legend("bottomright",legend=c("Sweden","OECD sample"),
lty=c(1:2),col=c("black","black"),lwd=c(2,2),cex=.8)
lines(year, CO2_OECD, lty="dashed" , lwd=2, col="black")
abline(v=1990,lty="dotted",lwd=2)
arrows(1987,1.0,1989,1.0,col="black",length=.1)	
Cex.set <- 1
text(1981,1.0,"VAT + Carbon tax",cex=Cex.set)

###################################################
### Figure 11: GDP per capita: Sweden vs. Synthetic Sweden
###################################################

plot(year[1:46], GDP_Sweden, type="l", lwd=2, col="black", ylim=c(0,35000), xlab="Year", ylab="GDP per capita (PPP, 2005 USD)", xaxs="i",yaxs="i" )
abline(v=1990,lty="dotted",lwd=2)
legend("bottomright",legend=c("Sweden","synthetic Sweden"),
lty=c(1:2),col=c("black","black"),lwd=c(2,2),cex=.8)
lines(year, GDP_Synthetic_Sweden, lty="dashed" , lwd=2, col="black")
abline(v=1990,lty="dotted",lwd=2)
arrows(1987,10000,1989,10000,col="black",length=.1)	
Cex.set <- 1
text(1981,10000,"VAT + Carbon tax",cex=Cex.set)

#Adding shaded areas for the two main recessions
plot(year[1:46], GDP_Sweden, type="l", lwd=2, col="white", ylim=c(0,35000), xlab="Year", ylab="GDP per capita (PPP, 2005 USD)", xaxs="i",yaxs="i" )
rect(1991, 100, 1993, 34900,
     border = "gray95", col = "gray95")
rect(1976, 100, 1978, 34900,
     border = "gray95", col = "gray95")
abline(v=1990,lty="dotted",lwd=2)
lines(year, GDP_Sweden, lty="solid", lwd=2, col="black")
legend("bottomright",legend=c("Sweden","synthetic Sweden"),
lty=c(1:2),col=c("black","black"),lwd=c(2,2),cex=.8)
lines(year, GDP_Synthetic_Sweden, lty="dashed" , lwd=2, col="black")
abline(v=1990,lty="dotted",lwd=2)
arrows(1987,10000,1989,10000,col="black",length=.1)	
Cex.set <- 1
text(1981,10000,"VAT + Carbon tax",cex=Cex.set)

###################################################
### Figure 12: Gap in GDP per capita and CO2 Emissions per capita from Transport between Sweden and Synthetic Sweden
###################################################

par(mar = c(5, 4, 4, 4))
plot(year, gap_CO2_emissions_transp, type="l", lwd=2, col="black", ylim=c(-0.50,0.50), xlab="Year" , ylab="Gap in metric tons per capita (CO2 from transport)", xaxs="i",yaxs="i")
abline(v=1990,lty="dotted",lwd=1.5)
abline(h = 0, col = "black", lty = "dashed", lwd = 1)
par(new = TRUE)
plot(year, gap_GDP, type="l" , lwd=2, col="gray50", ylim=c(-2500,2500),
 xlab="", ylab="", xaxt="n", yaxt="n", xaxs="i", yaxs="i")
 axis(side=4)
 mtext("Gap in GDP per capita (PPP, 2005 USD)", side = 4, line = 2.8) 
legend("bottomleft",legend=c("CO2 Emissions (left y-axis)" ,"GDP per capita (right y-axis)"),
lty=c(1,1),col=c("black","gray50"),lwd=c(2,2),cex=0.8)
arrows(1987,1500,1989,1500,col="black",length=.1)	
Cex.set <- 1
text(1980,1500,"VAT + Carbon tax",cex=Cex.set) 

#Adding shaded areas for the two main recessions
par(mar = c(5, 4, 4, 4))
plot(year, gap_CO2_emissions_transp, type="l", lwd=2, col="white", ylim=c(-0.50,0.50), xlab="Year" , ylab="Gap in metric tons per capita (CO2 from transport)", xaxs="i",yaxs="i")
rect(1991, -0.496, 1993, 0.496,
     border = "gray95", col = "gray95")
rect(1976, -0.496, 1978, 0.496,
     border = "gray95", col = "gray95")
abline(v=1990,lty="dotted",lwd=1.5)
abline(h = 0, col = "black", lty = "dashed", lwd = 1)
lines(year, gap_CO2_emissions_transp, lty="solid", lwd=2, col="black")
par(new = TRUE)
plot(year, gap_GDP, type="l" , lwd=2, col="gray50", ylim=c(-2500,2500),
 xlab="", ylab="", xaxt="n", yaxt="n", xaxs="i", yaxs="i")
 axis(side=4)
 mtext("Gap in GDP per capita (PPP, 2005 USD)", side = 4, line = 2.8) 
legend("bottomleft",legend=c("CO2 Emissions (left y-axis)" ,"GDP per capita (right y-axis)"),
lty=c(1,1),col=c("black","gray50"),lwd=c(2,2),cex=0.8)
arrows(1987,1500,1989,1500,col="black",length=.1)	
Cex.set <- 1
text(1980,1500,"VAT + Carbon tax",cex=Cex.set) 