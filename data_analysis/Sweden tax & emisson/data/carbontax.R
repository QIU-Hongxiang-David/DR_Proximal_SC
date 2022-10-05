###################################################
### Load file: carbontax_data.dta 
###################################################
 library("Synth")
 library(foreign)
 carbontax <- read.dta(file.choose())
attach(carbontax)
	
###################################################
 dataprep.out <-
              dataprep(foo = carbontax,
                       predictors = c("GDP_per_capita" , "gas_cons_capita" , "vehicles_capita" ,
                                      "urban_pop") ,
                       predictors.op = "mean" ,
                       time.predictors.prior = 1980:1989 ,
                       special.predictors = list(
                         list("CO2_transport_capita" , 1989 , "mean"),
                         list("CO2_transport_capita" , 1980 , "mean"),
                         list("CO2_transport_capita" , 1970 , "mean")
                                                ),
                       dependent = "CO2_transport_capita",
                       unit.variable = "Countryno",
                       unit.names.variable = "country",
                       time.variable = "year",
                       treatment.identifier = 13,
                       controls.identifier = c(1:12, 14:15),
                       time.optimize.ssr = 1960:1989,
                       time.plot = 1960:2005
                       )
	

###################################################
 synth.out <- synth(data.prep.obj = dataprep.out,
                    method = "All")
	
###################################################
 synth.tables <- synth.tab(dataprep.res = dataprep.out,
                           synth.res = synth.out
                           )

###################################################
### Table 1: CO2 Emissions From Transport Predictor Means Before Tax Reform	
###################################################
 synth.tables$tab.pred[1:7, ]
 
###################################################
### Table 2: Country Weights in Synthetic Sweden
###################################################
 synth.tables$tab.w[1:14, ]

###################################################
### Figure 4: Path Plot of per capita CO2 Emissions from Transport
###################################################
  path.plot(synth.res = synth.out,
           dataprep.res = dataprep.out,
           Ylab = "Metric tons per capita (CO2 from transport)",
           Xlab = "Year",
           Ylim = c(0,3),
           Legend = c("Sweden","synthetic Sweden"),
           Legend.position = "bottomright"
           )
# Add line 
abline(v=1990,lty="dotted",lwd=2)
arrows(1987,1.0,1989,1.0,col="black",length=.1)	
Cex.set <- 1
text(1981,1.0,"VAT + Carbon tax",cex=Cex.set)

###################################################
### Figure 5: Gap in per capita CO2 Emissions from Transport between Sweden and Synthetic Sweden
###################################################
 gaps.plot(synth.res = synth.out,
           dataprep.res = dataprep.out,
           Ylab = "Gap in metric tons per capita (CO2 from transport)",
           Xlab = "Year",
           Ylim = c(-0.5,0.5),
           Main = NA
           )

#Add line
abline(v=1990,lty="dotted",lwd=2)
arrows(1987,0.3,1989,0.3,col="black",length=.1)	
Cex.set <- 1
text(1981,0.3,"VAT + Carbon tax",cex=Cex.set)


###################################################
### Figure 6:  Placebo in-time tests 1980/1970
###################################################
#Placebo test in time for 1980 
dataprep.out <-
              dataprep(foo = carbontax,
                       predictors = c("GDP_per_capita" , "gas_cons_capita" , "vehicles_capita" ,
                                      "urban_pop") ,
                       predictors.op = "mean" ,
                       time.predictors.prior = 1970:1979 ,
                       special.predictors = list(
                         list("CO2_transport_capita" , 1979 , "mean"),
                         list("CO2_transport_capita" , 1970 , "mean"),
                         list("CO2_transport_capita" , 1965 , "mean")
                                                ),
                       dependent = "CO2_transport_capita",
                       unit.variable = "Countryno",
                       unit.names.variable = "country",
                       time.variable = "year",
                       treatment.identifier = 13,
                       controls.identifier = c(1:12,14:15),
                       time.optimize.ssr = 1960:1979,
                       time.plot = 1960:1990
                       )

synth.out <- synth(
                   data.prep.obj = dataprep.out,
                   method = "BFGS"
                   )

 path.plot(synth.res = synth.out,
           dataprep.res = dataprep.out,
           Ylab = "Metric tons per capita (CO2 from transport)",
           Xlab = "Year",
           Ylim = c(0,3),
           Legend = c("Sweden","synthetic Sweden"),
           Legend.position = "bottomright"
           )

# Add line 
abline(v=1980,lty="dotted",lwd=2)
arrows(1977,1.0,1979,1.0,col="black",length=.1)	
Cex.set <- 1
text(1974,1.0,"Placebo tax",cex=Cex.set)


#Placebo test in time for 1970 (excluding Poland due to missing GDP data 1960-69)
 dataprep.out <-
              dataprep(foo = carbontax,
                       predictors = c("GDP_per_capita" , "gas_cons_capita" , "vehicles_capita" ,
                                      "urban_pop") ,
                       predictors.op = "mean" ,
                       time.predictors.prior = 1960:1969 ,
                       special.predictors = list(
                         list("CO2_transport_capita" , 1960:1970 , "mean")
                                   ),
                       dependent = "CO2_transport_capita",
                       unit.variable = "Countryno",
                       unit.names.variable = "country",
                       time.variable = "year",
                       treatment.identifier = 13,
                       controls.identifier = c(1:9, 11:12, 14:15),
                       time.optimize.ssr = 1960:1969,
                       time.plot = 1960:1990
                       )

synth.out <- synth(
                   data.prep.obj = dataprep.out,
                   method = "All"
                   )

 path.plot(synth.res = synth.out,
           dataprep.res = dataprep.out,
           Ylab = "Metric tons per capita (CO2 from transport)",
           Xlab = "Year",
           Ylim = c(0,3),
           Legend = c("Sweden","synthetic Sweden"),
           Legend.position = "bottomright"
           )

# Add line 
abline(v=1970,lty="dotted",lwd=2)
arrows(1968,2.0,1969.5,2.0,col="black",length=.1)	
Cex.set <- 1
text(1965,2.0,"Placebo tax",cex=Cex.set)


###################################################
### Figure 7:  Placebo in-space tests
###################################################
store <- matrix(NA,length(1960:2005),15)
colnames(store) <- unique(carbontax$country)

# run placebo test
for(iter in 1:15)
 {
 dataprep.out <-
              dataprep(foo = carbontax,
                       predictors = c("GDP_per_capita" , "gas_cons_capita" , "vehicles_capita" ,
                                      "urban_pop") ,
                       predictors.op = "mean" ,
                       time.predictors.prior = 1980:1989 ,
                       special.predictors = list(
                         list("CO2_transport_capita" , 1989 , "mean"),
				list("CO2_transport_capita" , 1980 , "mean") ,
				list("CO2_transport_capita" , 1970 , "mean")
					  ),
                       dependent = "CO2_transport_capita",
                       unit.variable = "Countryno",
                       unit.names.variable = "country",
                       time.variable = "year",
                       treatment.identifier = iter,
                       controls.identifier = c(1:15)[-iter],
                       time.optimize.ssr = 1960:1989,
                       time.plot = 1960:2005
                       )



# run synth
synth.out <- synth(
                   data.prep.obj = dataprep.out,
                   method = "BFGS"
                   )

# store gaps
store[,iter] <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)
}

# now do figure
data <- store
rownames(data) <- 1960:2005

# Set bounds in gaps data
gap.start     <- 1
gap.end       <- nrow(data)
years         <- 1960:2005
gap.end.pre  <- which(rownames(data)=="1989")

#  MSPE Pre-Treatment
mse        <- apply(data[ gap.start:gap.end.pre,]^2,2,mean)
sweden.mse <- as.numeric(mse[13])
# Exclude states with 20 times higher MSPE than Sweden (to include all countries, set value to 1000)
data <- data[,mse<20*sweden.mse]
Cex.set <- 1

# Plot
plot(years,data[gap.start:gap.end,which(colnames(data)=="Sweden")],
     ylim=c(-1,1),xlab="Year",
     xlim=c(1960,2005),ylab="Gap in metric tons per capita (CO2 from transport)",
     type="l",lwd=2,col="black",
     xaxs="i",yaxs="i")

# Add lines for control states
for (i in 1:ncol(data)) { lines(years,data[gap.start:gap.end,i],col="gray") }

## Add Sweden Line
lines(years,data[gap.start:gap.end,which(colnames(data)=="Sweden")],lwd=2,col="black")

# Add grid
abline(v=1990,lty="dotted",lwd=2)
abline(h=0,lty="dashed",lwd=2)
legend("bottomleft",legend=c("Sweden","control countries"),
lty=c(1,1),col=c("black","gray"),lwd=c(2,1),cex=.8)
arrows(1987,-0.5,1989,-0.5,col="black",length=.1)
text(1981,-0.5,"VAT + Carbon Tax",cex=Cex.set)
abline(v=1960)
abline(v=2005)
abline(h=-1)
abline(h=1)

###################################################
### Figure 8: Ratio Test: Ratios of Post-Treatment MSPE to Pre-Treatment MSPE
###################################################
# Figure 8 was made using Microsoft Excel. Below is an example on how the data for this figure was created
# Example: Postperiod MSPE / Preperiod MSPE for Sweden (treatment.identifier = 13)

 dataprep.out <-
              dataprep(foo = carbontax,
                       predictors = c("GDP_per_capita" , "gas_cons_capita" , "vehicles_capita" ,
                                      "urban_pop") ,
                       predictors.op = "mean" ,
                       time.predictors.prior = 1980:1989 ,
                       special.predictors = list(
                         list("CO2_transport_capita" , 1989 , "mean"),
                         list("CO2_transport_capita" , 1980 , "mean"),
                         list("CO2_transport_capita" , 1970 , "mean")
                                                ),
                       dependent = "CO2_transport_capita",
                       unit.variable = "Countryno",
                       unit.names.variable = "country",
                       time.variable = "year",
                       treatment.identifier = 13,
                       controls.identifier = c(1:12, 14:15),
                       time.optimize.ssr = 1960:1989,
                       time.plot = 1960:2005
                       )
                       
 synth.out <- synth(data.prep.obj = dataprep.out,
                    method = "All")                      

 gaps <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)
 mspepost <- mean((gaps[31:46, 1])^2)
 mspepre <- mean((gaps[1:30, 1])^2)
 mspepost/mspepre
 
###################################################
### Figure 9: Leave-One-Out: Distribution of the Synthetic Control for Sweden
###################################################
# Iteratively eliminate one of the six control countries that got a W weight larger than 0.001
# Example: eliminating New Zealand

 dataprep.out <-
              dataprep(foo = carbontax,
                       predictors = c("GDP_per_capita" , "gas_cons_capita" , "vehicles_capita" ,
                                      "urban_pop") ,
                       predictors.op = "mean" ,
                       time.predictors.prior = 1980:1989 ,
                       special.predictors = list(
                         list("CO2_transport_capita" , 1989 , "mean"),
                         list("CO2_transport_capita" , 1980 , "mean"),
                         list("CO2_transport_capita" , 1970 , "mean")
                                                ),
                       dependent = "CO2_transport_capita",
                       unit.variable = "Countryno",
                       unit.names.variable = "country",
                       time.variable = "year",
                       treatment.identifier = 13,
                       controls.identifier = c(1:8, 10:12, 14:15),
                       time.optimize.ssr = 1960:1989,
                       time.plot = 1960:2005
                       )
                       
synth.out <- synth(data.prep.obj = dataprep.out,
                    method = "All")
                    
  path.plot(synth.res = synth.out,
           dataprep.res = dataprep.out,
           Ylab = "Metric tons per capita (CO2 from transport)",
           Xlab = "Year",
           Ylim = c(0,3),
           Legend = c("Sweden","synthetic Sweden"),
           Legend.position = "bottomright"
           )
# Add line 
abline(v=1990,lty="dotted",lwd=2)
arrows(1987,1.0,1989,1.0,col="black",length=.1)	
Cex.set <- 1
text(1981,1.0,"VAT + Carbon tax",cex=Cex.set)

# To create Figure 9, load file: leave_one_out_data.dta
leaveoneout <- read.dta(file.choose())
attach(leaveoneout)

plot(Year[1:46], sweden, type="l", lwd=2, col="black", ylim=c(0,3.0), xlab="Year", ylab="Metric tons per capita (CO2 from transport)", xaxs="i",yaxs="i" )
abline(v=1990,lty="dotted",lwd=2)
legend("bottomleft",legend=c("Sweden","synthetic Sweden", "synthetic Sweden (leave-one-out)   "),
lty=c(1,2,1),col=c("black","black","gray"),lwd=c(2,2,2),cex=0.8)
lines(Year, excl_belgium, lty="solid" ,lwd=2, col="gray")
lines(Year, excl_denmark, lty="solid",lwd=2, col="gray")
lines(Year, excl_greece, lty="solid" ,lwd=2, col="gray")
lines(Year, excl_newzealand, lty="solid" ,lwd=2, col="gray")
lines(Year, excl_switzerland, lty="solid" ,lwd=2, col="gray")
lines(Year, excl_unitedstates, lty="solid" ,lwd=2, col="gray")
lines(Year, synth_sweden, lty="dashed" , lwd=2, col="black")
lines(Year, sweden, lty="solid" , lwd=2, col="black")
arrows(1987,1.0,1989,1.0,col="black",length=.1)	
Cex.set <- 1
text(1981,1.0,"VAT + Carbon tax",cex=Cex.set)   

###################################################
### Figure 10: Path and Gap plot of per capita CO2 Emissions from Transport: Main Results vs. Full Sample 
###################################################
# Load file: carbontax_fullsample_data.dta

rm(list = ls())
 carbontax_fullsample <- read.dta(file.choose())
attach(carbontax_fullsample)

 dataprep.out <-
              dataprep(foo = carbontax_fullsample,
                       predictors = c("GDP_per_capita" , "gas_cons_capita" , "vehicles_capita" ,
                                      "urban_pop") ,
                       predictors.op = "mean" ,
                       time.predictors.prior = 1980:1989 ,
                       special.predictors = list(
                         list("CO2_transport_capita" , 1989 , "mean"),
                         list("CO2_transport_capita" , 1980 , "mean"),
                         list("CO2_transport_capita" , 1970 , "mean")
                                                ),
                       dependent = "CO2_transport_capita",
                       unit.variable = "Countryno",
                       unit.names.variable = "country",
                       time.variable = "year",
                       treatment.identifier = 21,
                       controls.identifier = c(1:20, 22:25),
                       time.optimize.ssr = 1960:1989,
                       time.plot = 1960:2005
                       )
                       
                        
synth.out <- synth(data.prep.obj = dataprep.out,
                    method = "All")
                    
  path.plot(synth.res = synth.out,
           dataprep.res = dataprep.out,
           Ylab = "Metric tons per capita (CO2 from transport)",
           Xlab = "Year",
           Ylim = c(0,3),
           Legend = c("Sweden","synthetic Sweden"),
           Legend.position = "bottomright"
           )
# Add line 
abline(v=1990,lty="dotted",lwd=2)
arrows(1987,1.0,1989,1.0,col="black",length=.1)	
Cex.set <- 1
text(1981,1.0,"VAT + Carbon tax",cex=Cex.set)     

# To create Figure 10, load file: fullsample_figures.dta
fullsample_figures <- read.dta(file.choose())
attach(fullsample_figures)   

# Gap plot
plot(Year, CO2_reductions_full_sample, type="l", lwd=2, col="gray", ylim=c(-0.50,0.50), xlab="Year" , ylab="Gap in metric tons per capita (CO2 from transport)", xaxs="i",yaxs="i")
lines(Year, CO2_reductions_full_sample, lty="solid", lwd=2, col="gray")
abline(v=1990,lty="dotted",lwd=2)
abline(h = 0, col = "black", lty = "dashed", lwd = 2)
legend("bottomleft",legend=c("Main result (14 control countries)" ,"Full sample (24 control countries)"),
lty=c(1,1),col=c("black","gray"),lwd=c(2,2),cex=0.8)
lines(Year, CO2_reductions_original_sample, lty="solid", lwd=2, col="black")
arrows(1987,0.3,1989,0.3,col="black",length=.1)	
Cex.set <- 1
text(1981,0.3,"VAT + Carbon tax",cex=Cex.set) 

# Path plot
plot(Year[1:46], sweden, type="l", lwd=2, col="black", ylim=c(0,3.0), xlab="Year", ylab="Metric tons per capita (CO2 from transport)", xaxs="i",yaxs="i" )
abline(v=1990,lty="dotted",lwd=2)
legend("bottomleft",legend=c("Sweden","synthetic Sweden", "synthetic Sweden (full sample)   "),
lty=c(1,2,1),col=c("black","black","gray"),lwd=c(2,2,2),cex=0.8)
lines(Year, synth_sweden_full_sample, lty="solid" ,lwd=2, col="gray")
lines(Year, synth_sweden_original_sample, lty="dashed" , lwd=2, col="black")
lines(Year, sweden, lty="solid" , lwd=2, col="black")
arrows(1987,1.0,1989,1.0,col="black",length=.1)	
Cex.set <- 1
text(1981,1.0,"VAT + Carbon tax",cex=Cex.set)

###################################################