********************************************************************************
*****                MCS                   *************************************
********************************************************************************

*longitudinal file (country, stratum, weights)
use "$mcsraw/S1/mcs_longitudinal_family_file.dta", clear
keep mcsid sentry country pttype2 weight1 weight2
tempfile mcslong
save `mcslong'


*Demographics ******************************************************************

*birth parent interview (sex and parental education)
use "$mcsraw/S1/mcs1_parent_interview.dta", clear
rename ahcsexa0 sex
recode amlfte00 aplfte00 (-9 -8 -1 =.)
keep mcsid sex amlfte00 aplfte00
tempfile mcsdem1
save `mcsdem1'

* derived (country, region, ethnicity)
use "$mcsraw/S1/mcs1_derived_variables.dta", clear
rename ADCTRY00 mcs_country
rename ADREGN00 mcs_region
rename ADC06EA0 mcs_ethn
rename MCSID mcsid
keep mcsid mcs_country mcs_region mcs_ethn
tempfile mcsdem2
save `mcsdem2'

* 5y parent interview (parental education, age at interview)
use "$mcsraw/S3/mcs3_parent_interview.dta", clear
recode cmlfte00 cplfte00 (-9 -8 -1 =.)
merge 1:1 mcsid using `mcsdem1', nogen keepusing(amlfte00 aplfte00)

gen lfte_moth = amlfte00 // update age left FTE at birth and 5y
replace lfte_moth = cmlfte00 if cmlfte00!=.
gen lfte_fath = aplfte00
replace lfte_fath = cplfte00 if cplfte00!=.
recode lfte_moth lfte_fath (0=.) // still in FTE to missing

gen ysch_moth = lfte_moth-15	// years continued school after 15
gen ysch_fath = lfte_fath-15
recode ysch_moth ysch_fath (-15/-1 = 0) // put to 0 everyone who had less than 15 years education

gen ageint5 = floor(chcagea0/30.42)
lab var ageint5		"Age at 5y interview (months)"

keep mcsid ageint5 ysch_*
tempfile mcsdem3
save `mcsdem3'

* 5y derived (parental NVQ)
use "$mcsraw/S3/mcs3_derived_variables.dta", clear
rename _all, lower
rename cmdnvq00 hnvq_main
rename cpdnvq00 hnvq_part
keep mcsid hnvq_main hnvq_part
tempfile mcsnvq
save `mcsnvq'

*SKILLS	 ******************************************************************

/* SDQ at 5 */
use "$mcsraw/S3/mcs3_parent_interview.dta", clear
keep mcsid cmcrel00 cmpsex00 cmsd*0
tempfile mcssdq5y
save `mcssdq5y'

/* TESTS at 5 */
use "$mcsraw/S3/mcs3_child_assessment_data.dta", clear
keep if chcnum00==1					// keep only first child
rename cdnvtscr		nvoc_bast
rename cdpstscr		psim_bast
rename cdpctscr		patc_bast
recode *_bast (-1=.)
keep mcsid *_bast
tempfile mcscog5y
save `mcscog5y'

* merge all
use `mcslong', clear
merge 1:1 mcsid using `mcsdem1', nogen
merge 1:1 mcsid using `mcsdem2', nogen
merge 1:1 mcsid using `mcssdq5y', gen(mcs_merge_sdq)
merge 1:1 mcsid using `mcsdem3', gen(mcs_merge_educ)
merge 1:1 mcsid using `mcscog5y', gen(mcs_merge_cog)

*INCOME	 ******************************************************************
merge 1:1 mcsid using "$data/SEPdata/create37_MCS2012.dta"
rename q_bu_net_total_p incq
gen faminc 			= bu_net_total_p
replace faminc 		= faminc/adjMCS/1000

********************************************************************************
********************************************************************************

* SDQ RECODING
* rename
rename cmsdpfa0 mcs_sdq1
rename cmsdroa0 mcs_sdq2
rename cmsdhsa0 mcs_sdq3 
rename cmsdsra0 mcs_sdq4 
rename cmsdtta0 mcs_sdq5 
rename cmsdspa0 mcs_sdq6 
rename cmsdora0 mcs_sdq7 
rename cmsdmwa0 mcs_sdq8 
rename cmsdhua0 mcs_sdq9 
rename cmsdfsa0 mcs_sdq10 
rename cmsdgfa0 mcs_sdq11 
rename cmsdfba0 mcs_sdq12 
rename cmsduda0 mcs_sdq13 
rename cmsdlca0 mcs_sdq14 
rename cmsddca0 mcs_sdq15 
rename cmsdnca0 mcs_sdq16 
rename cmsdkya0 mcs_sdq17 
rename cmsdoaa0 mcs_sdq18 
rename cmsdpba0 mcs_sdq19 
rename cmsdvha0 mcs_sdq20 
rename cmsdsta0 mcs_sdq21 
rename cmsdcsa0 mcs_sdq22 
rename cmsdgba0 mcs_sdq23 
rename cmsdfea0 mcs_sdq24 
rename cmsdtea0 mcs_sdq25 

lab def ncmcs 		-9 "Refusal" ///
					-8 "Don't Know" /// 
					-1 "Not Applicable" ///
					1 "Not True" ///
					2 "Somewhat True" ///
					3 "Certainly True" ///
					4 "Can't Say"
					
foreach v of varlist mcs_sdq* {
	lab val `v' ncmcs
	}


* recode to binary and positive
local pos "1 4 7 9 11 14 17 20 21 25"
foreach i of local pos  {
	recode mcs_sdq`i' (min/-1=.) (1 2 = 0) (3=1) (4=.), gen(mcs_sdqb`i') /* b = binary (1 is better) */
}

local neg "2 3 5 6 8 10 12 13 15 16 18 19 22 23 24"
foreach i of local neg {
	recode mcs_sdq`i' (min/-1=.) (2 3 = 0) (1=1) (4=.)		, gen(mcs_sdqb`i') /* b = binary (1 is better) */
	recode mcs_sdq`i' (min/-1=.) (1=2) (2=1) (3=0) (4=.)	, gen(mcs_sdqc`i') /* c = three categories (higher is better) */
}
forvalues i=1(1)25 {
	local lab: variable label mcs_sdq`i'
			lab var mcs_sdqb`i' "(Bin) `lab'"
	cap 	lab var mcs_sdqc`i' "(3cat) `lab'"
}

********************************************************************************

* SAMPLE SELECTION
// keep only interviews with mother
keep if cmpsex00 == 2
egen ncmiss=rowmiss(mcs_sdq*)
drop if ncmiss >21

foreach x in nvoc_bast psim_bast patc_bast {
	egen `x'z = std(`x')
	}

********************************************************************************
********************************************************************************
/* SAVE OUTPUT */

keep mcsid sentry sex country pttype2 ageint5 incq faminc ysch_* mcs_sdq* nvoc_bastz psim_bastz patc_bastz
saveold "$rdata/mcssdq5y.dta", replace version(12)

// keep england only
keep if country == 1
saveold "$rdata/mcssdq5yeng.dta", replace version(12)

/* reweight sample

selection probability by stratum (ENG)
					Raw			Normalised		% to select
Advantaged: 		0.0208			1				100
Disadvantaged: 		0.0383			1.84			54.35
Ethnic: 			0.112			5.38			18.59
*/


/* advantaged ------------------------- */
preserve
keep if pttype2==1
tempfile adv
save `adv'
restore

/* disadvantaged ---------------------- */
preserve
keep if pttype2==2
/* how many to select */
count 
local toselect = floor(r(N)*0.5435)
di `toselect'

gen rand = uniform()
sort rand
keep in 1/`toselect'

tempfile disadv
save `disadv'
restore

/* ethnic ---------------------- */
preserve
keep if pttype2==3
/* how many to select */
count 
local toselect = floor(r(N)*0.1859)
di `toselect'

gen rand = uniform()
sort rand
keep in 1/`toselect'

tempfile ethn
save `ethn'
restore


use `adv', clear
append using `disadv'
append using `ethn'
saveold "$rdata/mcssdq5yeng_reweighted.dta", replace version(12)

















