 
log using "F:\udel\sem3\time series\project1\p1_jaggi_bhoomika.smcl"
 
/* p1_jaggi_bhoomika*/
/*	Project 1: ARIMA Modeling and Forecasting of U.S. Monetary Base	*/ 



			/***  Import Data ***/


/* Import data in .csv format */
insheet using "F:\udel\sem3\time series\project1\US_IFS_2006.csv", names comma clear
	
describe			/* check numeric vs. string variables */
sort time
list time base	/* check for missing values: none at the beginning or in the middle of the sample */

/* Specify the time variable & declare the data to be time series */   
gen time_2=quarterly(time,"yq")	/* quarterly() converts a string to a numeric time representation, 1960q1=0 */
tsset time_2, quarterly			/* declare the data to be time series, gaps allowed */

describe
list time time_2			/* double check on the numeric time variables */

save "F:\udel\sem3\time series\project1\US_IFS_2006.dta", replace
use "F:\udel\sem3\time series\project1\US_IFS_2006.dta", clear


			/*** Summary Statistics & Time Series Plots ***/


/* summary statistics */
codebook base

/* time series plot */
tsline base


			/*** 1. Unit Root Test ***/


gen yyy=log(base)
label var yyy "log(base)"
tsline yyy
tsline d.yyy
tsline d2.yyy
gen trend=_n		/* this step is necessary b/c _n cannot be used directly as a regressor */

/* 1.1. Standard ADF Tests */
/* Schwert (1989), pmax=int[12*{(T+1)/100}^0.25]=14 */
scalar T=191		/* # of nonmissing observations */
scalar pmax=int(12*((T+1)/100)^0.25)
scalar miss=8		/* # of missing observations at the beginning of the sample */

/* 1.1.1 level */
scalar diff=0
scalar p_reg=pmax+1+diff+miss
scalar list pmax diff p_reg

reg yyy trend l.yyy l(1/14)d.yyy
estat ic
matrix y_order=r(S)
reg yyy trend l.yyy l(1/13)d.yyy if _n>p_reg
estat ic
matrix y_order=(y_order \ r(S))
reg yyy trend l.yyy l(1/12)d.yyy if _n>p_reg
estat ic
matrix y_order=(y_order \ r(S))
reg yyy trend l.yyy l(1/11)d.yyy if _n>p_reg
estat ic
matrix y_order=(y_order \ r(S))
reg yyy trend l.yyy l(1/10)d.yyy if _n>p_reg
estat ic
matrix y_order=(y_order \ r(S))
reg yyy trend l.yyy l(1/9)d.yyy if _n>p_reg
estat ic
matrix y_order=(y_order \ r(S))
reg yyy trend l.yyy l(1/8)d.yyy if _n>p_reg
estat ic
matrix y_order=(y_order \ r(S))
reg yyy trend l.yyy l(1/7)d.yyy if _n>p_reg
estat ic
matrix y_order=(y_order \ r(S))
reg yyy trend l.yyy l(1/6)d.yyy if _n>p_reg
estat ic
matrix y_order=(y_order \ r(S))
reg yyy trend l.yyy l(1/5)d.yyy if _n>p_reg
estat ic
matrix y_order=(y_order \ r(S))
reg yyy trend l.yyy l(1/4)d.yyy if _n>p_reg
estat ic
matrix y_order=(y_order \ r(S))
reg yyy trend l.yyy l(1/3)d.yyy if _n>p_reg
estat ic
matrix y_order=(y_order \ r(S))
reg yyy trend l.yyy l(1/2)d.yyy if _n>p_reg
estat ic
matrix y_order=(y_order \ r(S))
reg yyy trend l.yyy ld.yyy if _n>p_reg
estat ic
matrix y_order=(y_order \ r(S))
reg yyy trend l.yyy if _n>p_reg
estat ic
matrix y_order=(y_order \ r(S))

matlist y_order		/* AIC: P=13, SIC: P=12 */
dfuller yyy, trend lag(13)
dfuller yyy, trend lag(12)

/* 1.1.2. first-difference */
scalar diff=1
scalar p_reg=pmax+1+diff+miss
scalar list pmax diff p_reg

reg d.yyy ld.yyy l(1/14)d2.yyy
estat ic
matrix dy_order=r(S)
reg d.yyy ld.yyy l(1/13)d2.yyy if _n>p_reg
estat ic
matrix dy_order=(dy_order \ r(S))
reg d.yyy ld.yyy l(1/12)d2.yyy if _n>p_reg
estat ic
matrix dy_order=(dy_order \ r(S))
reg d.yyy ld.yyy l(1/11)d2.yyy if _n>p_reg
estat ic
matrix dy_order=(dy_order \ r(S))
reg d.yyy ld.yyy l(1/10)d2.yyy if _n>p_reg
estat ic
matrix dy_order=(dy_order \ r(S))
reg d.yyy ld.yyy l(1/9)d2.yyy if _n>p_reg
estat ic
matrix dy_order=(dy_order \ r(S))
reg d.yyy ld.yyy l(1/8)d2.yyy if _n>p_reg
estat ic
matrix dy_order=(dy_order \ r(S))
reg d.yyy ld.yyy l(1/7)d2.yyy if _n>p_reg
estat ic
matrix dy_order=(dy_order \ r(S))
reg d.yyy ld.yyy l(1/6)d2.yyy if _n>p_reg
estat ic
matrix dy_order=(dy_order \ r(S))
reg d.yyy ld.yyy l(1/5)d2.yyy if _n>p_reg
estat ic
matrix dy_order=(dy_order \ r(S))
reg d.yyy ld.yyy l(1/4)d2.yyy if _n>p_reg
estat ic
matrix dy_order=(dy_order \ r(S))
reg d.yyy ld.yyy l(1/3)d2.yyy if _n>p_reg
estat ic
matrix dy_order=(dy_order \ r(S))
reg d.yyy ld.yyy l(1/2)d2.yyy if _n>p_reg
estat ic
matrix dy_order=(dy_order \ r(S))
reg d.yyy ld.yyy ld2.yyy if _n>p_reg
estat ic
matrix dy_order=(dy_order \ r(S))
reg d.yyy ld.yyy if _n>p_reg
estat ic
matrix dy_order=(dy_order \ r(S))

matlist dy_order		/* AIC: P=14, SIC: P=11 */
dfuller d.yyy, lag(14)
dfuller d.yyy, lag(11)

/* AIC: yyy ~ I(1) with drift */			


  			
/* 1.2 ADF Test with Seasonal Dummies */
 gen quarter=quarter(dofq(time_2))
 gen sd_1=(quarter==1)
 gen sd_2=(quarter==2)
 gen sd_3=(quarter==3)
 gen sd_4=(quarter==4)
 list time_2 quarter sd_* in 1/20,sep(4)
 
 /* Schwert (1989), pmax=int[12*{(T+1)/100}^0.25]=14 */
scalar T=191		/* # of nonmissing observations */
scalar pmax=int(12*((T+1)/100)^0.25)
scalar miss=8		/* # of missing observations at the beginning of the sample */

/* 1.2.1 level */
scalar diff=0
scalar p_reg=pmax+1+diff+miss
scalar list pmax diff p_reg

reg yyy trend l.yyy sd_1 sd_2 sd_3 l(1/14)d.yyy
estat ic
matrix y_order=r(S)
reg yyy trend l.yyy sd_1 sd_2 sd_3 l(1/13)d.yyy if _n>p_reg
estat ic
matrix y_order=(y_order \ r(S))
reg yyy trend l.yyy sd_1 sd_2 sd_3 l(1/12)d.yyy if _n>p_reg
estat ic
matrix y_order=(y_order \ r(S))
reg yyy trend l.yyy sd_1 sd_2 sd_3 l(1/11)d.yyy if _n>p_reg
estat ic
matrix y_order=(y_order \ r(S))
reg yyy trend l.yyy sd_1 sd_2 sd_3 l(1/10)d.yyy if _n>p_reg
estat ic
matrix y_order=(y_order \ r(S))
reg yyy trend l.yyy sd_1 sd_2 sd_3 l(1/9)d.yyy if _n>p_reg
estat ic
matrix y_order=(y_order \ r(S))
reg yyy trend l.yyy sd_1 sd_2 sd_3 l(1/8)d.yyy if _n>p_reg
estat ic
matrix y_order=(y_order \ r(S))
reg yyy trend l.yyy sd_1 sd_2 sd_3 l(1/7)d.yyy if _n>p_reg
estat ic
matrix y_order=(y_order \ r(S))
reg yyy trend l.yyy sd_1 sd_2 sd_3 l(1/6)d.yyy if _n>p_reg
estat ic
matrix y_order=(y_order \ r(S))
reg yyy trend l.yyy sd_1 sd_2 sd_3 l(1/5)d.yyy if _n>p_reg
estat ic
matrix y_order=(y_order \ r(S))
reg yyy trend l.yyy sd_1 sd_2 sd_3 l(1/4)d.yyy if _n>p_reg
estat ic
matrix y_order=(y_order \ r(S))
reg yyy trend l.yyy sd_1 sd_2 sd_3 l(1/3)d.yyy if _n>p_reg
estat ic
matrix y_order=(y_order \ r(S))
reg yyy trend l.yyy sd_1 sd_2 sd_3 l(1/2)d.yyy if _n>p_reg
estat ic
matrix y_order=(y_order \ r(S))
reg yyy trend l.yyy sd_1 sd_2 sd_3 ld.yyy if _n>p_reg
estat ic
matrix y_order=(y_order \ r(S))
reg yyy trend l.yyy sd_1 sd_2 sd_3 if _n>p_reg
estat ic
matrix y_order=(y_order \ r(S))

matlist y_order		/* AIC: p=8 */

reg yyy trend l.yyy sd_1 sd_2 sd_3 l(1/8)d.yyy		


/* 1.2.2. first-difference */
scalar diff=1
scalar p_reg=pmax+1+diff+miss
scalar list pmax diff p_reg

reg d.yyy ld.yyy sd_1 sd_2 sd_3 l(1/14)d2.yyy
estat ic
matrix dy_order=r(S)
reg d.yyy ld.yyy sd_1 sd_2 sd_3 l(1/13)d2.yyy if _n>p_reg
estat ic
matrix dy_order=(dy_order \ r(S))
reg d.yyy ld.yyy sd_1 sd_2 sd_3 l(1/12)d2.yyy if _n>p_reg
estat ic
matrix dy_order=(dy_order \ r(S))
reg d.yyy ld.yyy sd_1 sd_2 sd_3 l(1/11)d2.yyy if _n>p_reg
estat ic
matrix dy_order=(dy_order \ r(S))
reg d.yyy ld.yyy sd_1 sd_2 sd_3 l(1/10)d2.yyy if _n>p_reg
estat ic
matrix dy_order=(dy_order \ r(S))
reg d.yyy ld.yyy sd_1 sd_2 sd_3 l(1/9)d2.yyy if _n>p_reg
estat ic
matrix dy_order=(dy_order \ r(S))
reg d.yyy ld.yyy sd_1 sd_2 sd_3 l(1/8)d2.yyy if _n>p_reg
estat ic
matrix dy_order=(dy_order \ r(S))
reg d.yyy ld.yyy sd_1 sd_2 sd_3 l(1/7)d2.yyy if _n>p_reg
estat ic
matrix dy_order=(dy_order \ r(S))
reg d.yyy ld.yyy sd_1 sd_2 sd_3 l(1/6)d2.yyy if _n>p_reg
estat ic
matrix dy_order=(dy_order \ r(S))
reg d.yyy ld.yyy sd_1 sd_2 sd_3 l(1/5)d2.yyy if _n>p_reg
estat ic
matrix dy_order=(dy_order \ r(S))
reg d.yyy ld.yyy sd_1 sd_2 sd_3 l(1/4)d2.yyy if _n>p_reg
estat ic
matrix dy_order=(dy_order \ r(S))
reg d.yyy ld.yyy sd_1 sd_2 sd_3 l(1/3)d2.yyy if _n>p_reg
estat ic
matrix dy_order=(dy_order \ r(S))
reg d.yyy ld.yyy sd_1 sd_2 sd_3 l(1/2)d2.yyy if _n>p_reg
estat ic
matrix dy_order=(dy_order \ r(S))
reg d.yyy ld.yyy ld2.yyy sd_1 sd_2 sd_3 if _n>p_reg
estat ic
matrix dy_order=(dy_order \ r(S))
reg d.yyy ld.yyy sd_1 sd_2 sd_3 if _n>p_reg
estat ic
matrix dy_order=(dy_order \ r(S))

matlist dy_order		/* AIC: P=7 */

reg d.yyy ld.yyy sd_1 sd_2 sd_3 l(1/7)d2.yyy	
			
/* AIC: yyy ~ I(1) with drift */

             /*** 2. Seasonal Unit Root Test ***/
			 
hegy4 yyy, det(strend) lags(1/14)

			/*** 3. ARIMA Modeling ***/ 

/* 3.1 ARIMA Modeling: d.yyy */ 

/* Identification */
tsline d.yyy
ac d.yyy, lags(24)	level (95)	/* standard error: Bartlett formula for MA(q), see classnotes 2.3.2 */
pac d.yyy, lags(24) level(95)	/* standard error: 1/sqrt(T) */

/*Filter out seasonality for identification*/  
reg d.yyy sd_2 sd_3 sd_4
cap drop res
predict res, residuals
tsline res
corrgram res, lags(24)
ac res, lags(24) level (95)
pac res, lags(24) level (95)

/* Estimation & Diagnostics */
/* By default, Stata estimate ARIMA using exact MLE */

/* Standard ARMA Models */

arima yyy, arima(2,1,2)
cap drop res
predict res, residuals
tsline res
corrgram res, lags(24) 
wntestq res, lags(24)	
armaroots	
	
arima yyy, arima(2,1,4)
cap drop res
predict res, residuals
tsline res
corrgram res, lags(24) 
wntestq res, lags(24)	
armaroots	 

arima yyy, arima(4,1,2)
cap drop res
predict res, residuals
tsline res
corrgram res, lags(24) 
wntestq res, lags(24)	
armaroots	

arima yyy, arima(4,1,4)
cap drop res
predict res, residuals
tsline res
corrgram res, lags(24) 
wntestq res, lags(24)	
armaroots	

/* additive seasonal ARIMA models */ 
arima d.yyy, ar(2 4) ma(2)	
cap drop res
predict res, residuals
tsline res
corrgram res, lags(24)
wntestq res, lags(24)	
armaroots

arima d.yyy, ar(2) ma(2 4)
cap drop res
predict res, residuals
tsline res
corrgram res, lags(24)
wntestq res, lags(24)	
armaroots

/*pure SARMA model*/

arima d.yyy, sarima(2,0,2,4) 
cap drop res
predict res, residuals
tsline res
corrgram res, lags(24)
wntestq res, lags(24)	
armaroots

/*multilpicative seasonal ARMA model*/

arima yyy, arima(2,1,4) mar(1,4)
cap drop res
predict res, residuals
tsline res
corrgram res, lags(24)
wntestq res, lags(24)	
armaroots

arima yyy, arima(2,1,4) mma(1,4)
cap drop res
predict res, residuals
tsline res
corrgram res, lags(24)
wntestq res, lags(24)	
armaroots

arima yyy, arima(4,1,2) mar(1,4)
cap drop res
predict res, residuals
tsline res
corrgram res, lags(24)
wntestq res, lags(24)	
armaroots

arima yyy, arima(4,1,2) mma(1,4)
cap drop res
predict res, residuals
tsline res
corrgram res, lags(24)
wntestq res, lags(24)	
armaroots

/* 3.2. Structural ARMA models */

reg d.yyy sd_2 sd_3 sd_4
cap drop res
predict res, residuals
ac res, lags(24)level (95)
pac res, lags(24)level(95)

arima d.yyy sd_2 sd_3 sd_4, arima(2,0,2)
cap drop res
predict res, residuals
tsline res
corrgram res, lags(24)
wntestq res, lags(24)	
armaroots

arima d.yyy sd_2 sd_3 sd_4, arima(4,0,2)
cap drop res
predict res, residuals
tsline res
corrgram res, lags(24)
wntestq res, lags(24)	
armaroots

arima d.yyy sd_2 sd_3 sd_4, arima(2,0,4)
cap drop res
predict res, residuals
tsline res
corrgram res, lags(24)
wntestq res, lags(24)	
armaroots

arima d.yyy sd_2 sd_3 sd_4, ar(2/4) ma(2)
cap drop res
predict res, residuals
tsline res
corrgram res, lags(24)
wntestq res, lags(24)	
armaroots

arima d.yyy sd_2 sd_3 sd_4,ar(2) ma(2/4)
cap drop res
predict res, residuals
tsline res
corrgram res, lags(24)
wntestq res, lags(24)	
armaroots

/* model selection: use common sample */

quietly arima d.yyy sd_2 sd_3 sd_4, arima(4,0,2)	
estat ic
quietly arima d.yyy sd_2 sd_3 sd_4, arima(2,0,4) /* Best model according to AIC */
estat ic
quietly arima d.yyy sd_2 sd_3 sd_4, ar(2/4) ma(2)		
estat ic
quietly arima d.yyy sd_2 sd_3 sd_4,ar(2) ma(2/4) 
estat ic


			/*** 4. Forecast yyy ***/

/* A. In-sample fitted values */
/* use full sample for estimation */
arima d.yyy sd_2 sd_3 sd_4, arima(2,0,4)

cap drop y_fit dy_fit	
predict y_fit, y 		
predict dy_fit, xb
label var y_fit "In-sample fitted values of levels"
label var dy_fit "In-sample fitted values of differences"
list time y_fit dy_fit
tsline yyy y_fit if tin(1999q1, )

cap drop y_error mse rmspe mppe mappe
gen y_error=y_fit-yyy
replace y_error=. if time_2<tq(1999q1)		/* compute the forecast evaluation statistics for post-1999Q1 sample */
egen mse=mean(y_error^2)
gen rmspe=sqrt(mse)      		
egen mppe=mean(y_error/yyy)
egen mappe=mean(abs(y_error/yyy))
list rmspe mppe mappe in 1/1


/* B. Dynamic/Extrapolation/Recursive) forecasts) */
/* estimation sample is fixed as (1957q1, 1998q4), forecast horizon ranges from 1 to 30 */
/* dynamic forecasts start with 1999q1 */

arima d.yyy sd_2 sd_3 sd_4 if tin(,1998q4), arima(2,0,4)
cap drop y_dyn dy_dyn	
predict y_dyn, y dynamic(tq(1999q1))		/* forecasts start in 1999Q1 */	
	
predict dy_dyn, xb dynamic(tq(1999q1))	
label var y_dyn "dynamic forecasts of levels"
label var dy_dyn "dynamic forecasts of differences"
list time y_dyn dy_dyn if time_2>=tq(1999q1)
tsline yyy y_dyn if tin(1999q1,)

cap drop y_error mse rmspe mppe mappe
gen y_error=y_dyn-yyy
replace y_error=. if time_2<tq(1999q1)
egen mse=mean(y_error^2)
gen rmspe=sqrt(mse)      		
egen mppe=mean(y_error/yyy)
egen mappe=mean(abs(y_error/yyy))
list rmspe mppe mappe in 1/1

/* C. 1-step-ahead forecasts */
/* estimation sample starts with (1957q1, 1998q4), then expands with one additonal observation at a time, forecast horizon is fixed as h=1 */
/* reestimate the model with each additional observation */
cap drop y_hat
gen y_hat = .
cap drop y_hat_table
gen y_hat_table = .
label var y_hat_table "yyy, ARIMA, h=1"

set more off
local h = 1                  /* set the h for h-step-ahead-forcast */ 
local i=168                  /* set last obs of estimation sample */
local l=`i'+`h'
local k=198                  /* set last obs of forecast sample */

while `i' <=`k'-`h' {


	quietly arima d.yyy sd_2 sd_3 sd_4, arima(2,0,4)


cap drop y_hat	
predict y_hat, y 
		
local j=`i'+`h' 
*	list time y_hat in `j'/`j'
replace y_hat_table = y_hat in `j'/`j'

local i=`i'+1
}

list time yyy y_hat_table in `l'/`k'
tsline yyy y_hat_table in `l'/`k'

cap drop y_error mse rmspe mppe mappe
gen y_error=y_hat_table-yyy
egen mse= mean(y_error^2)
gen rmspe= sqrt(mse)      		
egen mppe=mean(y_error/yyy)
egen mappe=mean(abs(y_error/yyy))
list rmspe mppe mappe in 1/1

cap drop y_h1
gen y_h1=y_hat_table
label var y_h1 "1-step-ahead forecasts of levels"

/* D. 4-step-ahead forecasts */
/* estimation sample starts with (1957q1, 1998q4), then expands with one additonal observation at a time, forecast horizon is fixed as h=4 */
/* reestimate the model with each additional observation */
cap drop y_hat
gen y_hat = .
cap drop y_hat_table
gen y_hat_table = .
label var y_hat_table "yyy, ARIMA, h=4"

set more off
local h = 4                  /* set the h for h-step-ahead-forcast */ 
local i=168                  /* set last obs of estimation sample */
local t0=tq(1998q4)			/* set ending date of estimation sample */
local l=`i'+`h'
local k=198                  /* set last obs of forecast sample */

while `i' <=`k'-`h' {


	quietly arima d.yyy sd_2 sd_3 sd_4, arima(2,0,4)


cap drop y_hat	
predict y_hat, y dynamic(`t0'+1)
		
local j=`i'+`h' 
*	list time y_hat in `j'/`j'
replace y_hat_table = y_hat in `j'/`j'

local i=`i'+1
local t0=`t0'+1
}
list time yyy y_hat_table in `l'/`k'
tsline yyy y_hat_table in `l'/`k'

cap drop y_error mse rmspe mppe mappe
gen y_error=y_hat_table-yyy
egen mse= mean(y_error^2)
gen rmspe= sqrt(mse)      		
egen mppe=mean(y_error/yyy)
egen mappe=mean(abs(y_error/yyy))
list rmspe mppe mappe in 1/1

cap drop y_h4
gen y_h4=y_hat_table
label var y_h4 "4-step-ahead forecasts of levels"


/* Forecast evaluation statistics: RMSPE, MPPE, MAPPE, check Notes_ARIMA.pdf for details */
tsline yyy y_fit y_dyn y_h1 y_h4 if tin(1999q1,)
 
log close
