
global inputs "C:\Users\z99ka\OneDrive\Desktop\STATA\Input"
global outputs "C:\Users\z99ka\OneDrive\Desktop\STATA\Output"

import excel "C:\Users\z99ka\OneDrive\Desktop\STATA\Data\hh_debt_gdp_us.xlsx" , sheet ("Quarterly") firstrow
br
d 
codebook date debt_gdp_hh
save "$outputs\hh_debt_gdp_us.dta", replace

*** Research Questions 
* 1- How has the household debt to GDP ratio changed over the years?

gen year = year(date) // Extract the year from the date
collapse (mean) debt_gdp_hh, by(year) //calculate the average debt to gdp by year
br

* Plotting the trend over the years
twoway line debt_gdp_hh year, ///
    title("Household Debt to GDP Ratio Over the Years") ///
    ytitle("Debt to GDP (%)") xtitle("Year") ///
    legend(off) graphregion(color(white))

* calculating the summary statistics
summarize debt_gdp_hh

** 2- Are there specific periods of significant increase or decrease in the ratio?

* calculating the overall change
// Calculate yearly change in debt to GDP ratio
gen yearly_change = debt_gdp_hh[_n] - debt_gdp_hh[_n-1] if year[_n] != year[_n-1]
summarize yearly_change
br

// Identify periods with maximum and minimum change
list year debt_gdp_hh yearly_change if yearly_change == r(max) // Maximum change
list year debt_gdp_hh yearly_change if yearly_change == r(min) // Minimum change

// Plot yearly change
twoway (bar yearly_change year), ///
    title("Yearly Change in Household Debt to GDP Ratio") ///
    ytitle("Change in Debt to GDP (%)") xtitle("Year")

save "$outputs\hh_debt_gdp_us_trend.dta", replace

** Seasonality and Cycles 
** a. Is there any seasonal pattern in the quarterly data?
// Extract quarter from the date variable
use "$outputs\hh_debt_gdp_us.dta",clear 
gen quarter = quarter(date)

// Create a seasonal plot
graph box debt_gdp_hh, over(quarter, label(angle(45))) ///
    title("Seasonality in Household Debt to GDP Ratio") ytitle("Debt to GDP (%)")

*b. Are there recurring cycles of growth or decline in the ratio?
// Plot quarterly data to observe cycles
twoway line debt_gdp_hh date, title("Quarterly Household Debt to GDP Ratio") ///
    ytitle("Debt to GDP (%)") xtitle("Date") legend(off)

save "$outputs\hh_debt_gdp_us_seasonality.dta", replace

*** Reserach Question 3 - Impact of Economic Events 
* a. How did the Household Debt to GDP ratio respond during significant economic events?
// Create indicator variables for key events

use "$outputs\hh_debt_gdp_us.dta",clear

gen year = year(date)
gen crisis_2008 = (year >= 2007 & year <= 2009) // Financial crisis
gen covid_2020 = (year == 2020)                 // COVID-19 pandemic
br
// Compare averages during these periods
collapse (mean) debt_gdp_hh, by(crisis_2008)
list crisis_2008 debt_gdp_hh
br
collapse (mean) debt_gdp_hh, by(covid_2020)
list covid_2020 debt_gdp_hh
br

*** RQ - Rate of Change
* a. What is the average rate of quarterly change in the Household Debt to GDP ratio?
use "$outputs\hh_debt_gdp_us.dta", clear
// Calculate quarterly change
gen quarterly_change = debt_gdp_hh - debt_gdp_hh[_n-1]

// Calculate average change
summarize quarterly_change if _n > 1

*b. Which quarter or year experienced the highest or lowest change? 
// Identify quarters with the highest and lowest change
list date debt_gdp_hh quarterly_change if quarterly_change == r(max)
list date debt_gdp_hh quarterly_change if quarterly_change == r(min)



****************************** Cointegration Analysis of Exports and Imports in Japan (1961-2005) ****************************


import excel "C:\Users\z99ka\OneDrive\Desktop\STATA\Data\im_ex_japan.xls" , sheet ("Trade Balance") firstrow clear
save "C:\Users\z99ka\OneDrive\Desktop\STATA\\Input\im_ex_japan.dta", replace

use "$inputs\im_ex_japan.dta"

tsset Time
tsline x m 

gen lx = log(x)
gen lm = log(m)

sum lx 
br

//gen date = daily(Time, "DMY")   format date %td 
//sort date 

//gen t = _n
tsset t
drop if missing(lx)
tsset Time

// TO CHECK for stationarity we do ADF test
dfuller lx

//0.1557
// Null hypothesis : the series has a unit root and is therefore non stationary
// At 5% significance level  -> bigger than 0.05 -> our model has a unit root and the variable is non-stationary

dfuller lm 
// 0.1415
// both exports and imports  are non-stationary 

// we have to verify whether both of these variables are going to be integrated in the same model
// write our variable in differences
dfuller d.lx
// 0.0000 -> variable is stationary 

dfuller d.lm
//0.0000 -> its stationary 

// Conclusions: both variables are stationary in first differences

// long-run relationship 
regress lx lm //log exports explained by log imports 
// the coefficient of imports is statistically significant - imports does help explain exports
// If imports increase by 1% , exports will increase by 1.02%
// In the long-run the exports increase more than imports - positive trade balance

// Risiduals - cointegration Test
predict error, residuals 
sum error
// mean  -2.51e-11 which is close to 0

tsline error, yline( -2.51e-11 )

// check whether the errors do  not have unit root and is statioary 

dfuller error, noconstant
// we do not rely on the critical values for a series that is coming out of a regression - it is a non-observable series
// -> the t-value is -3.431 , we need cointegration table 
// critical values for regression - residual based conintegration tests
// 10% -> 3.02   5%-> 3.37    1%-> 4.00

// The above method is time-consuming 

// using egranger command in stata -> Engle and Granger tests for cointegration 
// Null hypothesis is: "Series are not conintegrated". Thus, rejecting the Null hypothesis will result in 
// series being cointegrated
// Granger test is a residual-based test for cointegration , it is simply a unit root tests applied to the 
//series residuals.

ssc install egranger
egranger lx lm 
// -3.431 the same value as obtained above, but the critical values are different 
// Instead of using the cointegration test for critical values, we use the egranger command
// -3.431 is bigger than the 5% level (3.37) , the variables are cointegrated -> integration 

// The above was the long-run model 
// Now lets do the short run

// 1. For this short-run model, the variables must be in differences - stationary form 
// 2. We need to incorporate the error correction term into our model , which are the residuals ofthe long-run regression
// but lagged one period

regress d.lx d.lm l.error
//all coefficients are statistically significant
// lm --> 0.63 import increase by 1% - export increases by 0.63% -> short turn exprts increase in lower pattern
// there is a discrepency in the long-run and short-run
// error term coefficient -> -0.116 -> this term is statistically significant
// - 0.1165, suggesting that almost 12% of the discrepency between the long run and 
// short run is corrected within a quarter. It is gonna take two years to fix the discrepency


// The codes below does not work
// Model diagnostics
// Residuals Normality test 
predict residuals, residuals 
tsline residuals
// does look stationary
// normality test 
swilk residuals 

//portmanteau test 
wntestq residuals

// residuals are white noise

***********************************************WAGE DATA*********************************************************

***Experimenting "Unbiasedness" of OLS Estimates by Assuming the ME=5%

use "$inputs\WAGE1.dta"
br
desc 

gen cpi_1976=26.10
gen cpi_2021=124.27
gen wage_2021=wage*cpi_2021/cpi_1976
label variable wage_2021 "average hourly earnings in terms of 2021 USD"

save "$outputs\WAGE1_unbiasedness_5percent.dta", replace

count
set obs 526
gen ID=_n

*OLS (True) Coefficients in the Imaginary Population Regression Function: Assume N=526 as the size 
*of the population census

reg wage_2021 educ

*eststo is used to store estimation results for later tabulation
eststo model_pop

* intercept: -4.308
* Beta 1 : 2.5775

*OLS Estimates in the sample 1
gen u1=runiform()
sort u1

br ID u1 

reg wage_2021 educ in 1/223
eststo model1

*OLS Estimates in the sample 2
gen u2=runiform()
sort u2

br ID u1

reg wage_2021 educ in 1/223
eststo model2

*OLS Estimates in the sample 3
gen u3=runiform()
sort u3
reg wage_2021 educ in 1/223
eststo model3

*OLS Estimates in the sample 4
gen u4=runiform()
sort u4
reg wage_2021 educ in 1/223
eststo model4

*OLS Estimates in the sample 5
gen u5=runiform()
sort u5
reg wage_2021 educ in 1/223
eststo model5

*esttab -> bring all the results in a table 
* without t and star 
esttab, not nostar

esttab using 5%unbiasedness.csv, not nostar

save "$outputs\WAGE1_unbiasedness_1percent.dta", replace


***Experimenting "Unbiasedness" of OLS Estimates by Assuming the ME=1%

**WAGE1.dta dataset
clear all
use "$inputs\WAGE1.dta"

gen cpi_1976=26.10
gen cpi_2021=124.27
gen wage_2021=wage*cpi_2021/cpi_1976
label variable wage_2021 "average hourly earnings in terms of 2021 USD"

save "$outputs\WAGE1_unbiasedness_1percent.dta", replace

set obs 526
gen ID=_n

*OLS Coefficients in the Imaginary Poppulation Regression Function: Assume N=526 as the size of the population census

reg wage_2021 educ
eststo model_pop

*OLS Estimates in the sample 1
gen u1=runiform()
sort u1
reg wage_2021 educ in 1/499
eststo model1

*OLS Estimates in the sample 2
gen u2=runiform()
sort u2
reg wage_2021 educ in 1/499
eststo model2

*OLS Estimates in the sample 3
gen u3=runiform()
sort u3
reg wage_2021 educ in 1/499
eststo model3

*OLS Estimates in the sample 4
gen u4=runiform()
sort u4
reg wage_2021 educ in 1/499
eststo model4

*OLS Estimates in the sample 5
gen u5=runiform()
sort u5
reg wage_2021 educ in 1/499
eststo model5

esttab, not nostar

esttab using 1%unbiasedness.csv, not nostar

save "$outputs\WAGE1_unbiasedness_1percent.dta", replace

***************************************************************************************
***Algebraic Properties of the SLR Estimators***

***Wage1.dta file
clear all
use "$inputs\WAGE1.dta"
desc
gen cpi_1976=26.10
gen cpi_2021=124.27
gen wage_2021=wage*cpi_2021/cpi_1976
//wage, which is in 1976 US dollars, is identified in terms of 2021 US dollars rather than 
label variable wage_2021 "average hourly earnings in terms of 2021 USD"

save "$outputs\WAGE1_SLR_algebraic properties.dta", replace

*To show basic descriptive statistics (mean, std.dev. etc.) of the model variables
summ wage wage_2021 educ

*If sb wants to have a look at the distributions of these variables, one can draw the histograms
hist wage_2021, percent ytitle(Percentage of wage_2021 values (%)) xtitle(Wage in terms of 2021 US dollars (wage_2021))

*To show the various values of a variable with their frequencies
tab wage
tab wage_2021

*To draw the graph of wage_2021 wrt educ:
twoway (scatter wage_2021 educ) (lfit wage_2021 educ)

*To estimate the OLS model of wage on educ
reg wage_2021 educ

*To save the fitted values of wage and the residuals
predict wage2021hat, xb
predict uhat, residuals

***To check the algebraic properties of the model, type the following:

*Is sum of uhat's 0?? (Property 1)
summ uhat
tabstat uhat, stat(sum)

*Is sum of multiplication of educ and uhat 0?? (Property 1) In other words, is there any correlation between educ and uhat?? 
///Here cor1 is just a variable that denotes for the multiplication of educ and uhat.
gen cor1=educ*uhat
summ cor1
tabstat cor1, stat(sum)
* cor -> correlation

*Mean values of Wage and Educ are on the regression line?? (Property 3)
summ educ
scalar educmean=r(mean)
* scalar is defined as placeholders in Stata that can store a single number or a single piece of string
summ wage_2021
scalar wage2021mean=r(mean)

reg wage_2021 educ

set obs 527
//generating a new additional observation numbered 527
replace educ=educmean in 527
//I want to show whether I find the mean value of wage_2021 when I write the mean value of educ in the SLR model  
predict wage_2021_hat0
list educ wage_2021_hat0 in 527
 //this value is the exact value of wagebar, or wagehatbar. So, the mean value of wage, or educ, can be calculated by using the regression model

***Calculating the Goodness-of-Fit, R^2.
*Calculating SST
summ wage_2021
gen difwage2021=(wage_2021-r(mean))^2
summ difwage2021
scalar SST=r(sum)
display SST

*Calculating SSE
summ wage2021hat
gen difwage2021hat=(wage2021hat-r(mean))^2
summ difwage2021hat
scalar SSE=r(sum)
display SSE

*Calculating SSR
gen uhatsq=uhat^2
summ uhatsq
scalar SSR=r(sum)
display SSR

*Calculating R^2: 1st Way
scalar Rsq=SSE/SST*100
display Rsq

*Calculating R^2: 2nd Way
scalar Rsq2=(1-SSR/SST)*100
display Rsq2


***Alternative Models including the level-level model
*Level-level model
reg wage_2021 educ
//In the level-level model, both of the variables are in level, meaning that their units are $/hour and years, respectively.
//The model claims that (a) the education level has increasing effect on hourly wage, represented by a positive beta1hat (=2.58), which is dwage_2021/deduc, and
//                      (b) the contribution of the education to the wage stays the same with the level of education, d(dwage/deduc)/deduc=0

*Log-level model
gen lnwage2021=log(wage_2021)
reg lnwage2021 educ
//In this model, wage is in log so its unit of measurement is percent now and education's unit of measurement is years as before.
//The model claims that (a) the education level has increasing effect on hourly wage, represented by a positive beta1hat (=0.0827), which is dlnwage2021/deduc,
//which can be interpreted as "If education level increases (decreases) by 1 year, estimated hourly wage increases by 8.27% (=100*0.0827)"
//                      (b) the contribution of the education to the wage increases with the level of education, d(dwage2021/deduc)/deduc=beta1hat*exp(beta1hat*educ)>0
//To understand the case (b), let's do the following experiment:
clear all
set obs 19
//As the min educ=0 and max.educ=18, then
gen educ=_n-1
gen educ_lag=_n-2

//Model 1: level-level
scalar beta0hat_model1=-4.308272
scalar beta1hat_model1=2.577575
gen wage_model1=beta1hat_model1*educ
gen wage_model1_lag=beta1hat_model1*educ_lag

//Model 2: log-level
scalar beta1hat_model2=2.144294
scalar beta1hat_model2=0.0827444
gen wage_model2=exp(beta1hat_model2*educ)
gen wage_model2_lag=beta1hat_model2*educ_lag

//Define the changes in wage wrt educ in each model as
gen time=_n
tsset time

gen wage_model1_diff=wage_model1-wage_model1_lag

gen wage_model2_diff=wage_model2-wage_model2_lag

br educ wage_model1_diff wage_model2_diff
//When education level increases from 16 to 17, the contribution of education in the second model surpasses of the contribution in the first model (2.76$ vs. 2.58$)


*Level-log model
gen lneduc=log(educ)
reg wage_2021 lneduc

*Log-log model
reg lnwage2021 lneduc


***Calculating the SER (standard error of regression)
reg wage_2021 educ
scalar SER=sqrt(SSR/(N-2))
display SER
//SER is just the square root of the estimate of Var(u)

*Calculating Variance-Covariance Matrix of OLS Estimates
quietly reg wage_2021 educ
estat vce   
///Here var(beta1hat)=0.06427747, var(beta0hat)=10.636332 and cov(beta0hat, beta1hat)=-0.80750095


clear all
