******************************************************
* Table 3: Estimation Results from Gasoline Consumption Regressions
******************************************************
* Load file: disentangling_regression_data.dta

tsset year

* Table 3
eststo clear
eststo: newey log_gas_cons real_carbontaxexclusive_with_vat real_carbontax_with_vat d_carbontax t, lag(16)
eststo: newey log_gas_cons real_carbontaxexclusive_with_vat real_carbontax_with_vat d_carbontax t real_gdp_cap_1000 , lag(16)
eststo: newey log_gas_cons real_carbontaxexclusive_with_vat real_carbontax_with_vat d_carbontax t real_gdp_cap_1000 urban_pop , lag(16)
eststo: newey log_gas_cons real_carbontaxexclusive_with_vat real_carbontax_with_vat d_carbontax t real_gdp_cap_1000 urban_pop unemploymentrate , lag(16)
eststo: ivregress 2sls log_gas_cons (real_carbontaxexclusive_with_vat=real_energytax_with_vat) real_carbontax_with_vat d_carbontax t real_gdp_cap_1000 urban_pop unemploymentrate , vce(hac bartlett opt)
eststo: ivregress 2sls log_gas_cons (real_carbontaxexclusive_with_vat=real_oil_price_sek) real_carbontax_with_vat d_carbontax t real_gdp_cap_1000 urban_pop unemploymentrate , vce(hac bartlett opt) 
esttab, r2 label mtitles("OLS" "OLS" "OLS" "OLS" "IV(EnTax)" "IV(OilPrice)")se(3) star(* 0.10 ** 0.05 *** 0.01) 

* p-value b1=b2
test real_carbontaxexclusive_with_vat = real_carbontax_with_vat

* Instrument F-statistic and testing for weak instruments
gen rctewvat= real_carbontaxexclusive_with_vat
ivreg2 log_gas_cons (rctewvat = real_energytax_with_vat ) real_carbontax_with_vat d_carbontax t real_gdp_cap_1000 urban_pop unemploymentrate  , bw(auto) robust first
ivreg2 log_gas_cons (rctewvat = real_oil_price_sek ) real_carbontax_with_vat d_carbontax t real_gdp_cap_1000 urban_pop unemploymentrate  , bw(auto) robust first 

* Estimated elasticities, using results from column (4)
newey log_gas_cons real_carbontaxexclusive_with_vat real_carbontax_with_vat d_carbontax t real_gdp_cap_1000 urban_pop unemploymentrate , lag(16)
margins, dyex(real_carbontax_with_vat real_carbontaxexclusive_with_vat) at(real_carbontax_with_vat=8.478676 real_carbontaxexclusive_with_vat=8.478676)

******************************************************
* Creating data set disentangling_data.dta used for Figure 13 and Figure 14
******************************************************

* First, predict gasoline consumption using the full model
newey log_gas_cons real_carbontaxexclusive_with_vat real_carbontax_with_vat d_carbontax t real_gdp_cap_1000 urban_pop unemploymentrate , lag(16) 
predict yhat

* Second, predict gasoline consumption without the carbon tax 
preserve
newey log_gas_cons real_carbontaxexclusive_with_vat real_carbontax_with_vat d_carbontax t real_gdp_cap_1000 urban_pop unemploymentrate , lag(16)
replace d_carbontax=0
replace real_carbontax_with_vat=0
predict yhat_nocarb
restore

* Third, predict emissions without the carbon tax and without the VAT
preserve
newey log_gas_cons real_carbontaxexclusive_with_vat real_carbontax_with_vat d_carbontax t real_gdp_cap_1000 urban_pop unemploymentrate , lag(16)
replace real_carbontaxexclusive_with_vat= real_carbontaxexclusive
replace d_carbontax=0
replace real_carbontax_with_vat=0
predict yhat_nocarb_novat
restore

* To convert predicted gasoline consumption to CO2 emission estimates I added empirical data on diesel consumption and multiplied with a combined (weighted) emissions factor.   

