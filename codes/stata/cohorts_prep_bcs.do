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


*SKILLS at 5	 ***************************************************************

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
lab var agetest5			"Age at 5y COG testing (months)"
keep bcsid agetest5 *_z
tempfile bcscog5y
save `bcscog5y'

*SKILLS at 10	 ***************************************************************

* 10 Year Survey
use "$bcsraw/1980/3723.dta", clear

recode m10 m11 m12 (min/-1=.)
replace m12 = 1900 + m12
gen datemint = ym(m12,m11)
format datemint %tm

recode i3503 i3503m i3503y (min/-1=.)
replace i3503y = 1900 + i3503y
gen datebas = ym(i3503y,i3503m)
format datebas %tm

gen dateb = tm(1970-04)
format dateb %tm

gen agemint10 = datemint - dateb
lab var agemint10		"Age at 10y Maternal interview (months)"

gen agetest10 = datebas - dateb
lab var agetest10		"Age at 10y BAS testing (months)"

keep bcsid agemint10 agetest10 ///
		m15 m16 m17 	/// maternal non-rutter items
		m43-m61 		/// maternal rutter items (scale)
		j127-j177		/// teacher items (scale)
		i3575-i3616		/// BAS verbal similarities

tempfile bcsall10y
save `bcsall10y'


*INCOME	 ******************************************************************
use "$data/SEPdata/create37_BCS1980.dta", clear // income
rename q_bu_net_total_p incq
gen faminc 			= bu_net_total_p
replace faminc 		= faminc/adjBCS/1000
tempfile bcsinc10y
save `bcsinc10y'



********************************************************************************
// MERGE
********************************************************************************

use `bcsdem1', clear
merge 1:1 bcsid using `bcsdem2', nogen
merge 1:1 bcsid using `bcsdem3', gen(bcs_merge_ethn)
merge 1:1 bcsid using `bcsrutter5y', gen(bcs_merge_rut5y)
merge 1:1 bcsid using `bcscog5y', gen(bcs_merge_cog5y)
merge 1:1 bcsid using `bcsinc10y', gen(bcs_merge_inc10y)
merge 1:1 bcsid using `bcsall10y', gen(bcs_merge_all10y)

drop if sex<1		/* missing observations */


********************************************************************************

********************************************************************************
// 10Y SURVEY
********************************************************************************

* SIMILARITIES RECODING
// group name is equivalent to the MCS version
unab similitems: i3575-i3616
di "`similitems'"

local i = 1
foreach v of local similitems {
	local lab: variable label `v'
	if substr("`lab'", -4,4)=="NAME" {
		gen bcs10_ws`i'=`v'
		lab var bcs10_ws`i' "BAS WS: Item `i'"
		recode bcs10_ws`i' (min/-1=.) (2=0) (9=0)	// assume missing response is wrong
		local i=`i'+1
		drop `v'								// drop unused variable
	}
	else drop `v'
}

* RUTTER RECODING

forvalues i=1(1)19 {
	local j = 42+`i'
	_strip_labels m`j'
	rename m`j'	bcs10_rut`i'
	recode bcs10_rut`i'		(min/-1=.)
}
_strip_labels m15
_strip_labels m16
_strip_labels m17
rename m15 bcs10_rutA
rename m16 bcs10_rutB
rename m17 bcs10_rutD
recode bcs10_rutA bcs10_rutB bcs10_rutD (min/-1=.) (1 2=1) (3 4=0) /* b = binary (1 is better) */

lab var bcs10_rut1 "(scl) Very restless. Often running about or jumping up and down. Hardly ever still"
lab var bcs10_rut2 "(scl) Is squirmy or fidgety"
lab var bcs10_rut3 "(scl) Often destroys own or others’ belongings"
lab var bcs10_rut4 "(scl) Frequently fights with other children"
lab var bcs10_rut5 "(scl) Not much liked by other children"
lab var bcs10_rut6 "(scl) Often worried, worries about many things"
lab var bcs10_rut7 "(scl) Tends to do things on his/her own – rather solitary"
lab var bcs10_rut8 "(scl) Irritable. Is quick to fly off the handle"
lab var bcs10_rut9 "(scl) Often appears miserable, unhappy, tearful or distressed"
lab var bcs10_rut10 "(scl) Sometimes takes things belonging to others"
lab var bcs10_rut11 "(scl) Has twitches, mannerisms or tics of the face or body"
lab var bcs10_rut12 "(scl) Frequently sucks thumb or finger"
lab var bcs10_rut13 "(scl) Frequently bites nails or fingers"
lab var bcs10_rut14 "(scl) Is often disobedient"
lab var bcs10_rut15 "(scl) Cannot settle to anything for more than a few moments"
lab var bcs10_rut16 "(scl) Tends to be fearful or afraid of new things or new situations"
lab var bcs10_rut17 "(scl) Is over fussy or over particular"
lab var bcs10_rut18 "(scl) Often tells lies"
lab var bcs10_rut19 "(scl) Bullies other children"

lab var bcs10_rutA "(bin) Complains of headaches"
lab var bcs10_rutB "(bin) Complains of stomach-ache or has vomited"
lab var bcs10_rutD "(bin) Has temper tantrums (that is, complete loss of temper with shouting, angry movements, etc.)"



********************************************************************************
// 5Y SURVEY
********************************************************************************

* RUTTER RECODING
* apply label
lab def rutbcs -3 "Not Stated" 1 "Does Not Apply" 2 "Applies Somewhat" 3 "Certainly Applies"
forvalues i=1(1)19 {
	local j = `i'+24
	rename d0`j' bcs5_rut`i'
	lab val bcs5_rut`i' rutbcs
}
* (additional items)
rename d006 bcs5_rutA
rename d007 bcs5_rutB
rename d008 bcs5_rutC
rename d009 bcs5_rutD

* recode to binary and positive
forvalues i=1(1)19 {
	local lab: variable label bcs5_rut`i'
	recode bcs5_rut`i' (min/-1=.) (1=1) (2 3=0)		, gen(bcs5_rutb`i') /* b = binary (1 is better) */
	recode bcs5_rut`i' (min/-1=.) (1=2) (2=1) (3=0)	, gen(bcs5_rutc`i') /* c = three categories (higher is better) */
	lab var bcs5_rutb`i' "(Bin) `lab'"
	lab var bcs5_rutc`i' "(3cat) `lab'"
}
local additems "A B C D"
foreach i of local additems {
	local lab: variable label bcs5_rut`i'
	recode bcs5_rut`i' (min/-1=.) (1 2=1) (3 4=0), gen(bcs5_rutb`i') /* b = binary (1 is better) */
	lab var bcs5_rutb`i' "(Bin) `lab'"
}

egen rutcomp = rowtotal(bcs5_rutb1 bcs5_rutb2 bcs5_rutb15 bcs5_rutb6 bcs5_rutb9 bcs5_rutb16)
egen rutcompall = rowtotal(bcs5_rutb*)


/* standardise 5y cognitive tests internally */
foreach x in epvt_z copy_z hfd_z {
	egen `x'z = std(`x')
	drop `x'
	rename `x'z `x'
	}
	
****************************
/* SAVE 10Y FILE for R */

preserve
* SAMPLE SELECTION
egen ncmiss=rowmiss(bcs10_rut*)
drop if ncmiss >22

keep bcsid bcs_country sex bcs_region age*10 incq faminc ysch_moth ysch_fath bcs10_rut* bcs10_ws*
saveold "$rdata/bcsrut10y.dta", replace version(12)

// england only
keep if bcs_country==1
saveold "$rdata/bcsrut10yeng.dta", replace version(12)
restore


****************************
/* SAVE 5Y FILE for R */

preserve
* SAMPLE SELECTION
egen ncmiss=rowmiss(bcs5_rut*)
drop if ncmiss >22

keep bcsid bcs_country sex bcs_region age*5 incq faminc ysch_moth ysch_fath epvt_z copy_z hfd_z bcs5_rut*
saveold "$rdata/bcsrut5y.dta", replace version(12)

// england only
keep if bcs_country==1
saveold "$rdata/bcsrut5yeng.dta", replace version(12)
restore






