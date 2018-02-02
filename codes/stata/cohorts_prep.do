capture clear all
capture clear matrix
capture macro drop _all
capture log close
set more off
set scheme s1color
set seed 42			/* for random reweighting */


global data 		"/Users/giacomomason/Documents/Data"
global bcsraw   	"$data/BCS/raw"
global ncdsraw   	"$data/NCDS/raw"
global mcsraw   	"$data/MCS/raw"
global temp   		"$data/temp"

global proj			"/Users/giacomomason/Documents/Projects/CohortStudies"
global dofiles  	"$proj/codes/stata"
global tables  		"$proj/tables"
global graphs  		"$proj/graphs"
global suppdata  	"$proj/suppdata"
global rdata  		"$proj/rdata"


********************************************************************************
*****                REAL GDP DATA              ********************************
********************************************************************************
import delimited "$suppdata/ukrealgdp.csv", varn(1)
rename ïlocation location
keep if location == "GBR"
keep time value


// compounded growth 1980 forwards
tsset time
gen def80 = 1 if time ==1980
replace def80 = L.def80*(1+(value/100)) if time>1980

// to base 2015
su def80 if time == 2015
gen def15 = def80/r(mean)

su def15 if time==1980
scalar rgdp1980 = r(mean)

su def15 if time==2010
scalar rgdp2010 = r(mean)

********************************************************************************
*****                NOMINAL GDP DATA              *****************************
********************************************************************************
clear
import delimited "$suppdata/ukgdp.csv", varn(1)
rename ïlocation location
keep if location == "GBR"
keep time value

// compounded growth 1980 forwards
tsset time
gen def80 = 1 if time ==1980
replace def80 = L.def80*(1+(value/100)) if time>1980

// to base 2015
su def80 if time == 2015
gen def15 = def80/r(mean)

su def15 if time==1980
scalar ngdp1980 = r(mean)

su def15 if time==2010
scalar ngdp2010 = r(mean)

********************************************************************************
*****                CPI DATA              *************************************
********************************************************************************
clear

/* ONS dataset */
import delimited "$suppdata/ukcpi.csv", varn(1)
gen year = real(substr(date,length(date)-3,.))
collapse (mean) cpi, by(year)

su cpi if year==1980
scalar cpi1980 = r(mean)/100

su cpi if year==2010
scalar cpi2010 = r(mean)/100


/* IFS BHC deflators 
1980			3.0765334
2010/11			1.1060773
2015/16			1.0000000
*/

scalar ifs1980 = 1/3.0765334
scalar ifs2010 = 1/1.1060773


* SELECT
di "CPI 1980: " cpi1980 
di "BHC 1980: " ifs1980
di "Nom. GDP 1980: " ngdp1980
di "Real GDP 1980: " rgdp1980
di "CPI 2010: " cpi2010 
di "BHC 2010: " ifs2010
di "GDP 2010: " ngdp2010
di "Nom. GDP 2010: " ngdp2010
di "Real GDP 2010: " rgdp2010


/* select which */
scalar adjBCS = ifs1980*rgdp1980
scalar inflBCS = ifs1980
scalar adjMCS = ifs2010*rgdp2010
scalar inflMCS = ifs2010
di "FINAL 1980: " adjBCS
di "FINAL 2010: " adjMCS


****************************************** BCS *************************************
do "$dofiles/cohorts_prep_bcs.do"


****************************************** MCS *************************************
do "$dofiles/cohorts_prep_mcs.do"

ex
/* compare ages at testing (5y) */
use "$rdata/mcs5yeng_rwt.dta", clear
keep agetest5 faminc*
gen cohort = 2
tempfile mcsage
save `mcsage'

use "$rdata/bcs5yeng.dta", clear
keep agetest5 faminc*
gen cohort = 1
append using `mcsage'
tw 		(kdensity faminc_real if cohort==1 , width(1)  color(navy)) || ///
		(kdensity faminc_real if cohort==2 , width(1)  lcolor(red)), ///
		legend(label(1 "BCS") label(2 "MCS")) title("Weekly Family Income")

tw 		(hist agetest5 if cohort==1 , width(1)  color(navy)) || ///
		(hist agetest5 if cohort==2 , width(1)  fcolor(none) lcolor(red)), ///
		legend(label(1 "BCS") label(2 "MCS")) title("Age at interview")
