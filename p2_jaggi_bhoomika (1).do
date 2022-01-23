  
  
log using "F:\udel\sem3\time series\project2\p2_jaggi_bhoomika.smcl"



clear				/* drop all data and value labels from memory */
clear matrix
set memory 100m		/* reset the amount of memory allocated to Stata */ 



			/*** 1. Import Data ***/


/* A. Import data in .csv format */
insheet using "F:\udel\sem3\time series\project2\US.csv", names comma clear
*	insheet using ~/econ825/lab_var/G7GDP.csv, names comma clear		/* UNIX */

/* missing values in .csv should be coded as ".", not "n.a." or " " */
/* record all variable names in lower case b/c Stata is case sensitive */
	
describe		/* check numeric vs. string variables */

/* B. Declare the data to be time series & specify the time variable: two alternatives */   			
gen time=quarterly(date,"yq")	/* quarterly() convert a string to a numeric time representation, 1960q1=0 */
tsset time, quarterly			/* gaps allowed in time series */

describe		/* double check if the numeric time variable is correct */
list date time gdp85 gdpdef m1 govt, sep(4) 		/* check for missing values */	


			/*** 2. Summary Statistics & Time Series Plots ***/


codebook gdp85 gdpdef m1 govt

gen y=log(gdp85)
gen p=log(gdpdef)
gen m=log(m1)
gen x=log(govt)
label var y "log(GDP)"
label var p "log(GDP Deflator)"
label var m "log(Money Supply)"
label var x "log(Government Purchases)"
tsline y
tsline p
tsline m
tsline x

tsline d.y
tsline d2.p
tsline d.m
tsline d.x




			/*** 3. Standard VAR ***/

/* A. Order Selection */

varsoc d.y d2.p d.m d.x, maxlag(8)		/* AIC: P=4, SBC: P=1 */


/* B. Estimation & Diagnostics */
var d.y d2.p d.m d.x, lag(1/4)
varlmar, mlag(4)		/* LM test for residual serial correlation */
varstable		/*  eigenvalues of the companion matrix, i.e., inverted roots of |phi(z)|=0 */
varnorm			/* tests are applied to the orthogonalized residuals */

cap drop r_y r_p r_m r_x
predict r_y, equation(#1) residuals
predict r_p, equation(#2) residuals
predict r_m, equation(#3) residuals
predict r_x, equation(#4) residuals
tsline r_y
tsline r_p
tsline r_m
tsline r_x
corrgram r_y, lags(32)	/* ok */
corrgram r_p, lags(32)	/* ok */
corrgram r_m, lags(32)  /* ok */
corrgram r_x, lags(32)	/* ok */



			/*** 4. Augmented VAR (VARX) ***/
			

/* Dummy for 1982Q1 */
cap drop d_82q1
gen d_82q1=0
replace d_82q1=1 if time>=q(1982Q1)
list time d_82q1

/* Adding seasonal dummies */
cap drop quarter 
cap drop sd_1
cap drop sd_2
cap drop sd_3
cap drop sd_4
gen quarter =quarter(dofq(time))
gen sd_1=(quarter==1)
gen sd_2=(quarter==2)
gen sd_3=(quarter==3)
gen sd_4=(quarter==4)
list time quarter sd_*,sep(4)


/* Order Selection */

varsoc d.y d2.p d.m d.x, exog(d_82q1 sd_1 sd_2 sd_3) maxlag(8)	/* AIC: P=3, SBC: P=0 */


/* Estimation & Diagnostics */
var d.y d2.p d.m d.x, exog(d_82q1 sd_1 sd_2 sd_3) lag(1/3)	

test d_82q1	sd_1 sd_2 sd_3	/* test of the joint significance of the exogenous variables */

varlmar, mlag(4)		
varstable
varnorm

cap drop r_y r_p r_m r_x
predict r_y, equation(#1) residuals
predict r_p, equation(#2) residuals
predict r_m, equation(#3) residuals
predict r_x, equation(#4) residuals

tsline r_y
tsline r_p
tsline r_m
tsline r_x

corrgram r_y, lags(32)
wntestq r_y, lags(32)		/* Q-tests for white noise */

corrgram r_p, lags(32)	
wntestq r_p, lags(32)		/* Q-tests for white noise */

corrgram r_m, lags(32)
wntestq r_m, lags(32)		/* Q-tests for white noise */

corrgram r_x, lags(32)
wntestq r_x, lags(32)		/* Q-tests for white noise */





			/*** 5. Testing the Effectiveness of Monetary and Fiscal Policies ***/

			
/* Granger Causality Test */

var d.y d2.p d.m d.x, exog(d_82q1 sd_1 sd_2 sd_3) lag(1/3)	/* augmented VAR */

test ([D_m]:ld.y ld2.p l2d.y l2d2.p l3d.y l3d2.p) ([D_x]:ld.y ld2.p l2d.y l2d2.p l3d.y l3d2.p)
display invchi2(12,1-.05)

test ([D_y]:ld.m ld.x l2d.m l2d.x l3d.m l3d.x) ([D2_p]:ld.m ld.x l2d.m l2d.x l3d.m l3d.x) 
display invchi2(12,1-.05)


/* Test for Contemporaneous Effect via the Error Covariance Matrix */

var d.y d2.p d.m d.x, exog(d_82q1 sd_1 sd_2 sd_3) lag(1/3)	
scalar ll_sys=e(ll)							/* system log likelihood */

reg d.y ld.y ld2.p ld.m ld.x l2d.y l2d2.p l2d.m l2d.x l3d.y l3d2.p l3d.m l3d.x d_82q1 sd_1 sd_2 sd_3	/* single equation OLS estimation */
scalar ll_dy=e(ll)									                                            /* single equation log-likelihood */

reg d2.p ld.y ld2.p ld.m ld.x l2d.y l2d2.p l2d.m l2d.x l3d.y l3d2.p l3d.m l3d.x d_82q1 sd_1 sd_2 sd_3
scalar ll_d2p=e(ll)	

reg d.m ld.y ld2.p ld.m ld.x l2d.y l2d2.p l2d.m l2d.x l3d.y l3d2.p l3d.m l3d.x d_82q1 sd_1 sd_2 sd_3	
scalar ll_dm=e(ll)

reg d.x ld.y ld2.p ld.m ld.x l2d.y l2d2.p l2d.m l2d.x l3d.y l3d2.p l3d.m l3d.x d_82q1 sd_1 sd_2 sd_3
scalar ll_dx=e(ll)

scalar lrt=2*(ll_sys-ll_dy-ll_d2p-ll_dm-ll_dx)
scalar list ll_sys ll_dy ll_d2p ll_dm ll_dx lrt

display invchi2(6,1-.05)	/* 5% critical value */



			/*** 6. IRF & FEVD ***/

cap drop dy d2p dm dx	/* -irf- command does not work with time series operators */
gen dy=d.y 
gen d2p=d2.p 
gen dm=d.m
gen dx=d.x

/* A. Orthogonalized IRF & FEVD */
irf set oir_ovd, replace	/* activate, and if necessary create, an irf file */

var dy d2p dm dx, exog(d_82q1 sd_1 sd_2 sd_3) lag(1/3)
irf create order1, step(8) replace 
irf table oirf, irf(order1) 
irf graph oirf, irf(order1)
irf table oirf, irf(order1) impulse(dm) response(dy d2p) level(90)/* Orthogonalized IR of d.y and d2.p to a onetime unit shock to equation d.m */
irf graph oirf, irf(order1) impulse(dm) response(dy d2p) level(90)
irf table oirf, irf(order1) impulse(dx) response(dy d2p) level(90)/* Orthogonalized IR of d.y and d2.p to a onetime unit shock to equation d.x */
irf graph oirf, irf(order1) impulse(dx) response(dy d2p) level(90)


irf table fevd, irf(order1) 
irf graph fevd, irf(order1) 
irf table fevd, irf(order1) impulse(dm) response(dy d2p) level(99)/* Orthogonalized VD of d.y and d2.p to the innovations in equation d.m */	
irf graph fevd, irf(order1) impulse(dm) response(dy d2p) level(99)
irf table fevd, irf(order1) impulse(dx) response(dy d2p) level(99)/* Orthogonalized VD of d.y and d2.p to the innovations in equation d.x */	
irf graph fevd, irf(order1) impulse(dx) response(dy d2p) level(99)


/* OVD d.y sum to 1 */
irf table fevd, irf(order1) impulse(dy d2p dm dx) response(dy) level(99)

/* OVD d2.p sum to 1 */
irf table fevd, irf(order1) impulse(dy d2p dm dx) response(d2p) level(99)



/* B. Generalized IRF and FEVD */
var dy d2p dm dx, exog(d_82q1 sd_1 sd_2 sd_3) lag(1/3)

mat omega=e(Sigma)					/* error covariance matrix */
mat list omega
scalar sd_y=sqrt(omega[1,1])		/* standard deviation of GDP innovations */
scalar sd_p=sqrt(omega[2,2])		/* standard deviation of GDP deflator innovations */
scalar sd_m=sqrt(omega[3,3])		/* standard deviation of moeny supply innovations */
scalar sd_x=sqrt(omega[4,4])        /* standard deviation of government purchases innovations */
scalar list sd_y sd_p sd_m sd_x

irf set gir_gvd, replace	

var dm dy d2p dx, exog(d_82q1 sd_1 sd_2 sd_3) lag(1/3) /* Money Supply Shock */
irf create order1, step(8) replace 	
irf table oirf, irf(order1) impulse(dm) response(dy d2p)  /* Generalized IR of d.y & d2.p to a one time unit shock to equation d.m */
irf graph oirf, irf(order1) impulse(dm) response(dy d2p)
irf table fevd, irf(order1) impulse(dm) response(dy d2p) /* Generalized VD of d.y & d2.p to the innovations in equation d.m */
irf graph fevd, irf(order1) impulse(dm) response(dy d2p)  

var dx dy d2p dm, exog(d_82q1 sd_1 sd_2 sd_3) lag(1/3) /* Government Purchases shock */
irf create order2, step(8) replace
irf table oirf, irf(order2) impulse(dx) response(dy d2p)/* Generalized IR of d.y & d2.p to a one time unit shock to equation d.x */
irf graph oirf, irf(order2) impulse(dx) response(dy d2p)
irf table fevd, irf(order2) impulse(dx) response(dy d2p) /* Generalized VD of d.y & d2.p to the innovations in equation d.x */
irf graph fevd, irf(order2) impulse(dx) response(dy d2p)

var dy d2p dm dx, exog(d_82q1 sd_1 sd_2 sd_3) lag(1/3)	/* GDP shock */
irf create order3, step(8) replace 	
var d2p dy dm dx, exog(d_82q1 sd_1 sd_2 sd_3) lag(1/3)	/* GDP Deflator shock */
irf create order4, step(8) replace 


preserve /* rescale the GIRF : a 1% shock to d.m and d.x */
use gir_gvd.irf,clear
replace oirf=oirf/sd_m if impulse=="dm" & irfname=="order1"
replace stdoirf=stdoirf/sd_m if impulse=="dm" & irfname=="order1"
replace oirf=oirf/sd_x if impulse=="dx" & irfname=="order2"
replace stdoirf=stdoirf/sd_x if impulse=="dx" & irfname=="order2"
save gir_gvd.irf,replace
restore
irf table oirf, irf(order1) impulse(dm) response(dy d2p)/* Generalized IR of d.y & d2.p to a one time 1% shock to equation d.m */
irf graph oirf, irf(order1) impulse(dm) response(dy d2p)
irf table oirf, irf(order2) impulse(dx) response(dy d2p)/* Generalized IR of d.y & d2.p to a one time 1% shock to equation d.x */
irf graph oirf, irf(order2) impulse(dx) response(dy d2p)

/* GVD d.y sum to greater than 1 */
irf table fevd, irf(order1) impulse(dm) response(dy) 
irf table fevd, irf(order2) impulse(dx) response(dy) 
irf table fevd, irf(order3) impulse(dy) response(dy) 
irf table fevd, irf(order4) impulse(d2p) response(dy) 

/* GVD d2.p sum to greater than 1 */
irf table fevd, irf(order1) impulse(dm) response(d2p) 
irf table fevd, irf(order2) impulse(dx) response(d2p) 
irf table fevd, irf(order3) impulse(dy) response(d2p) 
irf table fevd, irf(order4) impulse(d2p) response(d2p)



/*** 7. Forecasts: Augmented VAR ***/

/* A. Dynammic forecasts */
/* estimation sample is fixed as (1960Q1-1986Q4), forecast horizon ranges from 1 to 20 (1987Q1-1991Q4)*/

var d.y d2.p d.m d.x if tin(,1986q4), exog(d_82q1 sd_1 sd_2 sd_3) lag(1/3) 
fcast compute dyn_, step(20) replace				/* forecasts for 1987Q1-1991Q4 */
label var dyn_D_y "d.y, VAR(1), dynamic forecasts"
label var dyn_D2_p "d2.p, VAR(1), dynamic forecasts"

list date dyn_D_y dyn_D2_p		/* dyn_* actually starts in 1986Q4 (h=0) */

cap drop yyy y_fit y_error mse rmspe mppe mappe
gen yyy=d.y
gen y_fit=dyn_D_y
gen y_error=y_fit-yyy
replace y_error=. if time<tq(1987q1)
egen mse=mean(y_error^2)
gen rmspe=sqrt(mse)      		
egen mppe=mean(y_error/yyy)
egen mappe=mean(abs(y_error/yyy))
list rmspe mppe mappe in 1/1

cap drop yyy y_fit y_error mse rmspe mppe mappe
gen yyy=d2.p
gen y_fit=dyn_D2_p
gen y_error=y_fit-yyy
replace y_error=. if time<tq(1987q1)
egen mse=mean(y_error^2)
gen rmspe=sqrt(mse)      		
egen mppe=mean(y_error/yyy)
egen mappe=mean(abs(y_error/yyy))
list rmspe mppe mappe in 1/1


/* B. 1-step-ahead forecasts */

/* estimation sample starts (1960Q1-1986Q4), then expands with one additonal observation at a time, forecast horizon is fixed as h=1 */
cap drop y1_hat_table y2_hat_table
gen y1_hat_table = .
gen y2_hat_table = .

set more off
local h=1					/* set the h for h-step-ahead-forcast */ 
local i=108					/* set last obs of estimation sample*/
local l=`i'+`h'
local k=128					/* set last obs of forecast sample */
while `i' <=`k'-`h' {

quietly var d.y d2.p d.m d.x in 1/`i', exog(d_82q1 sd_1 sd_2 sd_3) lag(1/3)
fcast compute hat_, step(`h') replace nose		
		
local j=`i'+`h' 
*	list date hat_D_y hat_D2_p in `j'/`j'
replace y1_hat_table = hat_D_y in `j'/`j'
replace y2_hat_table = hat_D2_p in `j'/`j'
local i=`i'+1
}
tsline d.y y1_hat_table in `l'/`k'
tsline d2.p y2_hat_table in `l'/`k'

list date y1_hat_table y2_hat_table

cap drop yyy y_fit y_error mse rmspe mppe mappe
gen yyy=d.y
gen y_fit=y1_hat_table
gen y_error=y_fit-yyy
egen mse=mean(y_error^2)
gen rmspe=sqrt(mse)      		
egen mppe=mean(y_error/yyy)
egen mappe=mean(abs(y_error/yyy))
list rmspe mppe mappe in 1/1

cap drop yyy y_fit y_error mse rmspe mppe mappe
gen yyy=d2.p
gen y_fit=y2_hat_table
gen y_error=y_fit-yyy
egen mse=mean(y_error^2)
gen rmspe=sqrt(mse)      		
egen mppe=mean(y_error/yyy)
egen mappe=mean(abs(y_error/yyy))
list rmspe mppe mappe in 1/1

cap drop h1_D_y h1_D2_p 
gen h1_D_y=y1_hat_table
gen h1_D2_p=y2_hat_table
label var h1_D_y "d.y, VAR(1), h=1"
label var h1_D2_p "d2.p, VAR(1), h=1"


/* C. 4-step-ahead forecasts */
/* estimation sample starts (1960Q1-1986Q4), then expands with one additonal observation at a time, forecast horizon is fixed as h=4 */
cap drop y1_hat_table y2_hat_table 
gen y1_hat_table = .
gen y2_hat_table = .


set more off
local h=4					/* set the h for h-step-ahead-forcast */ 
local i=108					/* set last obs of estimation sample*/
local l=`i'+`h'
local k=128					/* set last obs of forecast sample */
while `i' <=`k'-`h' {

quietly var d.y d2.p d.m d.x in 1/`i', exog(d_82q1 sd_1 sd_2 sd_3) lags(1/3) 
fcast compute hat_, step(`h') replace nose		
		
local j=`i'+`h' 
*	list date hat_D_lyusa hat_D_lyjap hat_D_lyger in `j'/`j'
replace y1_hat_table = hat_D_y in `j'/`j'
replace y2_hat_table = hat_D2_p in `j'/`j'
local i=`i'+1
}

tsline d.y y1_hat_table in `l'/`k'
tsline d2.p y2_hat_table in `l'/`k'

list date y1_hat_table y2_hat_table

cap drop yyy y_fit y_error mse rmspe mppe mappe
gen yyy=d.y
gen y_fit=y1_hat_table
gen y_error=y_fit-yyy
egen mse=mean(y_error^2)
gen rmspe=sqrt(mse)      		
egen mppe=mean(y_error/yyy)
egen mappe=mean(abs(y_error/yyy))
list rmspe mppe mappe in 1/1

cap drop yyy y_fit y_error mse rmspe mppe mappe
gen yyy=d2.p
gen y_fit=y2_hat_table
gen y_error=y_fit-yyy
egen mse=mean(y_error^2)
gen rmspe=sqrt(mse)      		
egen mppe=mean(y_error/yyy)
egen mappe=mean(abs(y_error/yyy))
list rmspe mppe mappe in 1/1

cap drop h4_D_y h4_D2_p
gen h4_D_y=y1_hat_table
gen h4_D2_p=y2_hat_table
label var h4_D_y "d.y, VAR(1), h=4"
label var h4_D2_p "d2.p, VAR(1), h=4"


		/*** 8. Forecasts: Restricted VAR ***/

constraint define 1 [D_m]ld.y=0
constraint define 2 [D_m]l2d.y=0
constraint define 3 [D_m]l3d.y=0
constraint define 4 [D_m]ld2.p=0
constraint define 5 [D_m]l2d2.p=0
constraint define 6 [D_m]l3d2.p=0
constraint define 7 [D_x]ld.y=0
constraint define 8 [D_x]l2d.y=0
constraint define 9 [D_x]l3d.y=0
constraint define 10 [D_x]ld2.p=0
constraint define 11 [D_x]l2d2.p=0
constraint define 12 [D_x]l3d2.p=0


/* A. Dynammic forecasts */

/* estimation sample is fixed as (1960Q1-1986Q4), forecast horizon ranges from 1 to 20 (1987Q1-1991Q4)*/

var d.y d2.p d.m d.x if tin(,1986q4), exog(d_82q1 sd_1 sd_2 sd_3) lag(1/3) constraints(1/12)
fcast compute dyn_, step(20) replace				/* forecasts for 1987Q1-1991Q4 */

label var dyn_D_y "d.y, VAR,dynamic forecasts"
label var dyn_D2_p "d2.p, VAR, dynamic forecasts"

tsline d.y dyn_D_y if tin(1987q1,)
tsline d2.p dyn_D2_p if tin(1987q1,)

list date dyn_D_y dyn_D2_p		/* dyn_* actually starts in 1986Q4 (h=0) */

cap drop yyy y_fit y_error mse rmspe mppe mappe
gen yyy=d.y
gen y_fit=dyn_D_y
gen y_error=y_fit-yyy
replace y_error=. if time<tq(1987q1)
egen mse=mean(y_error^2)
gen rmspe=sqrt(mse)      		
egen mppe=mean(y_error/yyy)
egen mappe=mean(abs(y_error/yyy))
list rmspe mppe mappe in 1/1

cap drop yyy y_fit y_error mse rmspe mppe mappe
gen yyy=d2.p
gen y_fit=dyn_D2_p
gen y_error=y_fit-yyy
replace y_error=. if time<tq(1987q1)
egen mse=mean(y_error^2)
gen rmspe=sqrt(mse)      		
egen mppe=mean(y_error/yyy)
egen mappe=mean(abs(y_error/yyy))
list rmspe mppe mappe in 1/1


/* B. 1-step-ahead forecasts */

/* estimation sample starts (1960Q1-1986Q4), then expands with one additonal observation at a time, forecast horizon is fixed as h=1 */

var d.y d2.p d.m d.x if tin(,1986q4), exog(d_82q1 sd_1 sd_2 sd_3) lag(1/3) constraints(1/12)
fcast compute h1_, step(1) replace		/* forecasts start in 1987Q1 */
list date h1_D_y h1_D2_p if date=="1987Q1"

cap drop y1_hat_table y2_hat_table
gen y1_hat_table = .
gen y2_hat_table = .

set more off
local h=1					/* set the h for h-step-ahead-forcast */ 
local i=108					/* set last obs of estimation sample*/
local l=`i'+`h'
local k=128					/* set last obs of forecast sample */
while `i' <=`k'-`h' {

quietly var d.y d2.p d.m d.x in 1/`i', exog(d_82q1 sd_1 sd_2 sd_3) lag(1/3) constraints(1/12)
fcast compute hat_, step(`h') replace nose		
		
local j=`i'+`h' 
*	list date hat_D_y hat_D2_p in `j'/`j'
replace y1_hat_table = hat_D_y in `j'/`j'
replace y2_hat_table = hat_D2_p in `j'/`j'
local i=`i'+1
}

cap drop h1_D_y h1_D2_p 
gen h1_D_y=y1_hat_table
gen h1_D2_p=y2_hat_table

label var h1_D_y "d.y, VAR, h=1"
label var h1_D2_p "d2.p, VAR, h=1"

tsline d.y h1_D_y in `l'/`k'
tsline d2.p h1_D2_p in `l'/`k'

list date y1_hat_table y2_hat_table 

cap drop yyy y_fit y_error mse rmspe mppe mappe
gen yyy=d.y
gen y_fit=y1_hat_table
gen y_error=y_fit-yyy
egen mse=mean(y_error^2)
gen rmspe=sqrt(mse)      		
egen mppe=mean(y_error/yyy)
egen mappe=mean(abs(y_error/yyy))
list rmspe mppe mappe in 1/1

cap drop yyy y_fit y_error mse rmspe mppe mappe
gen yyy=d2.p
gen y_fit=y2_hat_table
gen y_error=y_fit-yyy
egen mse=mean(y_error^2)
gen rmspe=sqrt(mse)      		
egen mppe=mean(y_error/yyy)
egen mappe=mean(abs(y_error/yyy))
list rmspe mppe mappe in 1/1


/* C. 4-step-ahead forecasts */

/* estimation sample starts (1960Q1-1986Q4), then expands with one additonal observation at a time, forecast horizon is fixed as h=4 */

var d.y d2.p d.m d.x if tin(,1986q4), exog(d_82q1 sd_1 sd_2 sd_3) lag(1/3) constraints(1/12)
fcast compute h4_, step(4) replace		/* forecasts start in 1987Q4 */
list date h4_D_y h4_D2_p if date=="1987Q4"

cap drop y1_hat_table y2_hat_table 
gen y1_hat_table = .
gen y2_hat_table = .


set more off
local h=4					/* set the h for h-step-ahead-forcast */ 
local i=108					/* set last obs of estimation sample*/
local l=`i'+`h'
local k=128					/* set last obs of forecast sample */
while `i' <=`k'-`h' {

quietly var d.y d2.p d.m d.x in 1/`i', exog(d_82q1 sd_1 sd_2 sd_3) lags(1/3) constraints(1/12)
fcast compute hat_, step(`h') replace nose		
		
local j=`i'+`h' 
*	list date hat_D_lyusa hat_D_lyjap hat_D_lyger in `j'/`j'
replace y1_hat_table = hat_D_y in `j'/`j'
replace y2_hat_table = hat_D2_p in `j'/`j'
local i=`i'+1
}


cap drop h4_D_y h4_D2_p
gen h4_D_y=y1_hat_table
gen h4_D2_p=y2_hat_table

label var h4_D_y "d.y,VAR, h=4"
label var h4_D2_p "d2.p,VAR, h=4"

tsline d.y h4_D_y in `l'/`k'
tsline d2.p h4_D2_p in `l'/`k'

list date y1_hat_table y2_hat_table 

cap drop yyy y_fit y_error mse rmspe mppe mappe
gen yyy=d.y
gen y_fit=y1_hat_table
gen y_error=y_fit-yyy
egen mse=mean(y_error^2)
gen rmspe=sqrt(mse)      		
egen mppe=mean(y_error/yyy)
egen mappe=mean(abs(y_error/yyy))
list rmspe mppe mappe in 1/1

cap drop yyy y_fit y_error mse rmspe mppe mappe
gen yyy=d2.p
gen y_fit=y2_hat_table
gen y_error=y_fit-yyy
egen mse=mean(y_error^2)
gen rmspe=sqrt(mse)      		
egen mppe=mean(y_error/yyy)
egen mappe=mean(abs(y_error/yyy))
list rmspe mppe mappe in 1/1

log close


