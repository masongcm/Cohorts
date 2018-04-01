
********************************************************************************
***** PREP BCS 70 DATA		                ************************************
********************************************************************************

*Demographics ******************************************************************

/* birth data (sex, country, region) */
use "$bcsraw/1970_birth/bcs7072a.dta", clear
rename a0255 sex
recode a0278 (-3/-2=.), gen(bwt)
recode a0043b (-3=.) (1/3=0) (4/6=1), gen(smkpr)
rename a0005a mothageb
rename a0166 parity
recode a0195b (-3 -2 = .), gen(gestaw)

lab var sex				"CM sex"
lab var bwt				"CM Birthweight (g)"
lab var parity			"Parity"
lab var mothageb		"Mother age at CM birth"
lab var smkpr			"Smoked during pregnancy"
lab var gestaw			"Gestational age (weeks)"

keep bcsid sex bwt smkpr mothageb parity gestaw
tempfile bcsdem1
save `bcsdem1'

use "$bcsraw/1970_birth/bcs1derived.dta", clear
rename BCSID bcsid
rename BD1CNTRY country 
lab var country 		"Country at Birth"
rename BD1REGN region
lab var region 			"Standard Region of Residence"

keep bcsid country region
tempfile bcsdem2
save `bcsdem2'

/* 5y data (age at interview, parental education, and ethnicity) */
use "$bcsraw/1975/f699b.dta", clear

recode e245 (-3 -2 = .) (1 2 = 0) (3/7=1), gen(ethn)

rename e189a	educ_moth5
rename e189b	educ_fath5
recode educ_moth5 educ_fath5 (-3 -2 -1 8 =.)

rename e195		ysch_moth5
rename e196		ysch_fath5
recode ysch_moth5 ysch_fath5 (-3 -2 -1 =.)

recode e271 (-3=.)
tostring e271, gen(datestr)

gen year = 1900 + real((substr(datestr,1,2)))
gen month = real((substr(datestr,3,2)))
gen dateint5 = ym(year,month)
format dateint5 %tm
gen dateb = tm(1970-04)
format dateb %tm
gen ageint5 = dateint5 - dateb

gen numch5 = e006 + e007 if e007!=-1 & e006!=-1

lab var ageint5		"Age at 5y interview (months)"
lab var ethn		"Nonwhite ethnicity"
lab var ysch_fath5	"Father years of schooling (5y)"
lab var ysch_moth5	"Mother years of schooling (5y)"
lab var numch5		"Number other children in HH at 5"

keep bcsid ethn ageint5 *_moth5 *_fath5 numch5
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

*QUALIFICATIONS AT 30	 *******************************************************

* 10 Year Survey
use "$bcsraw/2000/bcs6derived.dta", clear
rename _all, lower
keep bcsid hinvq00
recode hinvq00 (-9=.)
tempfile bcsnvq30y
save `bcsnvq30y'


*INCOME	 ******************************************************************
use "$data/SEPdata/create37_BCS1980.dta", clear // income
rename q_bu_net_total_p incq
gen faminc_real 	= bu_net_total_p
replace faminc_real = faminc_real/adjBCS/1000
gen faminc_infl 	= bu_net_total_p
replace faminc_infl = faminc_infl/inflBCS/1000
tempfile bcsinc10y
save `bcsinc10y'

*SOCIAL CLASS	 ******************************************************************
use "$data/CohortsHarmonisedSES/raw/BCS70 Harmonised ChildhoodSES.dta", clear // income
decode BCSID, gen(bcsid)
rename BCS3FCL scl10
recode scl10 (13=.) // no questionnaire
keep bcsid scl10
tempfile bcsscl10y
save `bcsscl10y'

********************************************************************************
// MERGE
********************************************************************************

use `bcsdem1', clear
merge 1:1 bcsid using `bcsdem2', nogen
merge 1:1 bcsid using `bcsdem3', gen(bcs_merge_ethn)
merge 1:1 bcsid using `bcsrutter5y', gen(bcs_merge_rut5y)
merge 1:1 bcsid using `bcscog5y', gen(bcs_merge_cog5y)
merge 1:1 bcsid using `bcsinc10y', gen(bcs_merge_inc10y)
merge 1:1 bcsid using `bcsscl10y', gen(bcs_merge_scl10y)
merge 1:1 bcsid using `bcsall10y', gen(bcs_merge_all10y)
merge 1:1 bcsid using `bcsnvq30y', gen(bcs_merge_nvq30y)


// SAMPLE SELECTION
keep if country==1			// England only
drop if sex<1				/* missing observations */
drop if region > 9			// drop interviews not in England

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


local covarstokeep country region sex bwt smkpr gestaw mothageb scl10 region incq faminc_real faminc_infl ysch_moth5 ysch_fath5 numch5

preserve
* SAMPLE SELECTION
egen ncmiss=rowmiss(bcs10_rut*)
drop if ncmiss >22

keep bcsid age*10 bcs10_rut* bcs10_ws* hinvq00 `covarstokeep'
saveold "$rdata/bcs10yeng.dta", replace version(12)
restore


****************************
/* SAVE 5Y FILE for R */

preserve
* SAMPLE SELECTION
egen ncmiss=rowmiss(bcs5_rut*)
drop if ncmiss >22

keep bcsid age*5 epvt_z copy_z hfd_z bcs5_rut* hinvq00 `covarstokeep'
saveold "$rdata/bcs5yeng.dta", replace version(12)
restore






