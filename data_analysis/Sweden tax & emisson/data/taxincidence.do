******************************************************
* Tax incidence analysis
******************************************************
* Load file: tax_incidence_data.dta

tsset year 

* First differencing:
newey D.(retail_price oilprice_SEK energycarbon_tax), lag(16)
test D1.energycarbon_tax=1

* Number of lags were chosen using the Newey-West (1994) method.
 
* Splitting up the total tax into its energy and carbon tax part
newey D.( retail_price oilprice_SEK energytax carbontax ), lag(16)
test D1.energytax=1
test D1.carbontax=1

