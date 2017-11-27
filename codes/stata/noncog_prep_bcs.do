********************************************************************************
***** PREP BCS 70 DATA		                ************************************
********************************************************************************

*Demographics ******************************************************************

/* birth data (sex, country, region) */
use "$bcsraw/1970_birth/bcs7072a.dta", clear
rename a0255 sex
keep bcsid sex
tempfile bcsdem1
save `bcsdem1'

use "$bcsraw/1970_birth/bcs1derived.dta", clear

rename BCSID bcsid
rename BD1CNTRY bcs_country 
lab var bcs_country 		"Country of Interview"
rename BD1REGN bcs_region
lab var bcs_region 			"Standard Region of Residence"

keep bcsid bcs_country bcs_region
tempfile bcsdem2
save `bcsdem2'

/* 5y data (age at interview, parental education, and ethnicity) */
use "$bcsraw/1975/f699b.dta", clear
rename e245 bcs_ethn

rename e195		ysch_moth
rename e196		ysch_fath
recode ysch_moth ysch_fath (-3 -2 -1 =.)

recode e271 (-3=.)
tostring e271, gen(datestr)

gen year = 1900 + real((substr(datestr,1,2)))
gen month = real((substr(datestr,3,2)))
gen dateint5 = ym(year,month)
format dateint %tm

gen dateb = tm(1970-04)
format dateb %tm

gen ageint5 = dateint - dateb
lab var ageint5		"Age at 5y interview (months)"

keep bcsid bcs_ethn ageint5 ysch_*
tempfile bcsdem3
save `bcsdem3'


*SKILLS	 ******************************************************************

* 5 Year Survey - Rutter (parental module)
use "$bcsraw/1975/f699a.dta", clear

keep bcsid d025-d043 d006-d009
tempfile bcsrutter5y
save `bcsrutter5y'

* 5 Year Survey - Cognitive Ability, age at testing
use "$bcsraw/1975/f699c.dta", clear
rename f120 epvt_z
rename f121 hfd_z
rename f122 copy_z
recode *_z (-5 -4 =.)

recode f112 (-3=.)
gen agetest5 = floor(f112/30.42)
lab var agetest5			"Age at COG testing (months)"

keep bcsid agetest5 *_z
tempfile bcscog5y
save `bcscog5y'

* merge all
use `bcsdem1', clear
merge 1:1 bcsid using `bcsdem2', nogen
merge 1:1 bcsid using `bcsdem3', gen(bcs_merge_ethn)
merge 1:1 bcsid using `bcsrutter5y', gen(bcs_merge_rut)
merge 1:1 bcsid using `bcscog5y', gen(bcs_merge_cog)


*INCOME	 ******************************************************************
merge 1:1 bcsid using "$data/SEPdata/create37_BCS1980.dta" // income
rename q_bu_net_total_p incq
gen faminc 			= bu_net_total_p
replace faminc 		= faminc/adjBCS/1000

drop if sex<1		/* missing observations */


********************************************************************************
********************************************************************************


* RUTTER RECODING
* apply label
lab def rutbcs -3 "Not Stated" 1 "Does Not Apply" 2 "Applies Somewhat" 3 "Certainly Applies"
forvalues i=1(1)19 {
	local j = `i'+24
	rename d0`j' bcs_rut`i'
	lab val bcs_rut`i' rutbcs
}
* (additional items)
rename d006 bcs_rutA
rename d007 bcs_rutB
rename d008 bcs_rutC
rename d009 bcs_rutD

* recode to binary and positive
forvalues i=1(1)19 {
	local lab: variable label bcs_rut`i'
	recode bcs_rut`i' (min/-1=.) (1=1) (2 3=0)		, gen(bcs_rutb`i') /* b = binary (1 is better) */
	recode bcs_rut`i' (min/-1=.) (1=2) (2=1) (3=0)	, gen(bcs_rutc`i') /* c = three categories (higher is better) */
	lab var bcs_rutb`i' "(Bin) `lab'"
	lab var bcs_rutc`i' "(3cat) `lab'"
}
local additems "A B C D"
foreach i of local additems {
	local lab: variable label bcs_rut`i'
	recode bcs_rut`i' (min/-1=.) (1 2=1) (3 4=0), gen(bcs_rutb`i') /* b = binary (1 is better) */
	lab var bcs_rutb`i' "(Bin) `lab'"
}

egen rutcomp = rowtotal(bcs_rutb1 bcs_rutb2 bcs_rutb15 bcs_rutb6 bcs_rutb9 bcs_rutb16)
egen rutcompall = rowtotal(bcs_rutb*)


********************************************************************************

* SAMPLE SELECTION

egen ncmiss=rowmiss(bcs_rut*)
drop if ncmiss >22

/* standardise cognitive tests internally */
foreach x in epvt_z copy_z hfd_z {
	egen `x'z = std(`x')
	drop `x'
	rename `x'z `x'
	}
	
********************************************************************************
********************************************************************************
/* SAVE OUTPUT */

keep bcsid bcs_country sex bcs_region age*5 incq faminc ysch_moth ysch_fath epvt_z copy_z hfd_z bcs_rut*
saveold "$rdata/bcsrut5y.dta", replace version(12)

// england only
keep if bcs_country==1
saveold "$rdata/bcsrut5yeng.dta", replace version(12)







