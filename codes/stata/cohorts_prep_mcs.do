********************************************************************************
*****                MCS                   *************************************
********************************************************************************

*longitudinal file (country, stratum, weights)
use "$mcsraw/S1/mcs_longitudinal_family_file.dta", clear
keep mcsid sentry country pttype2 weight1 weight2
tempfile mcslong
save `mcslong'

********************************************************************************
*birth parent interview (sex and parental education)
use "$mcsraw/S1/mcs1_parent_interview.dta", clear
rename ahcsexa0 sex
recode amlfte00 aplfte00 (-9 -8 -1 =.)

recode amcipr00 (-8/-1=.) (1/max=1), gen(smkprepreg)
replace smkprepreg = 0 if amsmev00==2 | amsmty00==2
gen smkpr = smkprepreg
replace smkpr = 0 if amcich00==0 & inlist(amwhch00,1,2)
// gave up, less than 1/day, changed in 1st/2nd month

recode amwkpr00 (-9/-1=.) (2=0), gen(mempl) // paid job when pregnant
gen ymendlastjob = ym(amjeyr00, amjemt00) if inrange(amjeyr00,0,2002) & inrange(amjemt00,1,12)
gen ymconc = ym(ahcdbya0,ahcdbma0)-9 
replace mempl = 1 if (ymconc - ymendlastjob)<12 // employed in year before pregnancy

// still births
local lets "a b c d e f g h i"
gen nprevst = 0
foreach l of local lets {
	replace nprevst = nprevst+1 if amlive`l'0 == 2
}

recode amdewm0a (-9/-1=.) (5 6 = 1) (1/4 51/95 = 0), gen(caesbirth)

egen jaundice = anymatch(amwrbmaa amwrbmab amwrbmac amwrbmad amwrbmae amwrbmaf amwrbmag), values(4)
replace jaundice=. if amwrbmaa <0

egen preecl = anymatch(amilwm0a amilwm0b amilwm0c amilwm0d amilwm0e amilwm0f amilwm0g), values(5)
replace preecl=. if amilpr00 <0

/*
// relationship of main respondent
rename ahprelaa ahprela1
rename ahprelab ahprela2
rename ahprelac ahprela3
rename ahprelad ahprela4
rename ahprelae ahprela5
rename ahprelaf ahprela6
gen relmain = .
levelsof ampnum00, local(mainrcodes)
di "`mainrcodes'"
foreach x of local mainrcodes {
	di `x'
	replace relmain = ahprela`x' if ampnum00==`x'
}
lab val relmain LABF
lab var relmain		"CM relationship to main respondent"

*/

// marital status
recode amfcin00 (-9/-1=.) (2 3 = 0) (1 4 5 6 = 1), gen(singlem)

// maternal malaise

recode 	amtire00 amdepr00 amworr00 amrage00 ///
		amscar00 amupse00 amkeyd00 amnerv00 amhera00 ///
		(-9/-1=.) (2=0)

local mlq "amtire00 amdepr00 amworr00 amrage00 amscar00 amupse00 amkeyd00 amnerv00 amhera00"
gen malaise=0
foreach v of local mlq {
	replace malaise = malaise + `v'
}

lab var sex			"CM sex"
lab var smkpr		"Smoked during pregnancy (main resp)"
lab var singlem		"Single mother (main resp)"
lab var malaise		"Mother malaise (0y)"
lab var mempl		"Mother employment before pregnancy"
lab var nprevst		"Num previous stillbirths"
lab var caesbirth	"Caesarean birth"
lab var jaundice	"Jaundiced baby"
lab var preecl		"Raised blood pressure, eclampsia /preeclampsia, or toxaemia"

keep mcsid amlfte00 aplfte00 amotcn00 sex smkpr singlem malaise mempl nprevst caesbirth preecl
tempfile mcsdem1
save `mcsdem1'

******************************************************************
* derived (country, region, ethnicity)
use "$mcsraw/S1/mcs1_derived_variables.dta", clear
rename MCSID mcsid
gen region = ADREGN00
recode region (1 2 = 1) (3=2) (4=3) (5=4) (9=5) (6 7 8=6)

lab def reglab 	1 "North" ///
				2 "Yorksh. + Humbers." ///
				3 "East Midlands" ///
				4 "West Midlands" ///
				5 "South West" ///
				6 "East + SE" ///
				10 "Wales" ///
				11 "Scotland" ///
				12 "Northern Ireland"
lab val region reglab
lab var region 		"Region of Birth (Harmonised)"

recode ADC06EA0 (-9/-1=.) (1=0) (2/6=1), gen(ethn)
recode ADBWGTA0 (-8 -1 = .), gen(bwt)
replace bwt = bwt
gen lowbwt = bwt<2.5 if bwt!=.
recode AMDAGB00 (-2=.), gen(mothageb)
gen teenm = mothageb<20 if mothageb!=.
gen gestaw = floor(ADGESTA0/7) if ADGESTA0>0
gen preterm = gestaw<37 if gestaw!=.

// total num of biological siblings (in HH, not in HH)
merge 1:1 mcsid using `mcsdem1', keepusing(amotcn00)
gen parity = ADOTHS00 
replace parity= parity + amotcn00 if amotcn00 >0 
gen firstb = parity==0 // no natural siblings

recode AMHGTM00 (-1=.), gen(mheight)
recode AMWGBK00 (-8/-1=.), gen(mweight) // before CM born
recode AMDBMIB0 (-8/-1=.), gen(mbmi) // before CM born

lab var ethn 			"Nonwhite ethnicity"
lab var bwt				"Birthweight (kg)"
lab var lowbwt			"Low birthweight (<2500g)"
lab var gestaw			"Gestational age (weeks)"
lab var preterm			"Preterm (<37w)"
lab var mothageb		"Age at CM birth (main resp)"
lab var teenm			"Teen mother"
lab var parity			"Parity"
lab var firstb			"First born"
lab var mheight			"Mother height (m)"
lab var mweight			"Mother weight (kg)"
lab var mbmi			"Mother BMI"

keep mcsid region ethn bwt lowbwt gestaw preterm mothageb teenm parity firstb mheight mweight mbmi
tempfile mcsdem2
save `mcsdem2'

******************************************************************
* 5y parent interview (parental education, age at interview)
use "$mcsraw/S3/mcs3_parent_interview.dta", clear
recode cmlfte00 cplfte00 (-9 -8 -1 =.)
merge 1:1 mcsid using `mcsdem1', nogen keepusing(amlfte00 aplfte00)

gen lfte_moth = amlfte00 // update age left FTE at birth and 5y
replace lfte_moth = cmlfte00 if cmlfte00!=.
gen lfte_fath = aplfte00
replace lfte_fath = cplfte00 if cplfte00!=.
recode lfte_moth lfte_fath (0=.) // still in FTE to missing

gen mysch5 = lfte_moth	// age left FTE
gen fysch5 = lfte_fath
recode mysch5 fysch5 (1/15 = 15) // put to 15 everyone under 15


// parents year of birth
recode cmpdby00 (-2=.), gen(myob) 
recode cppdby00 (-2=.), gen(fyob)

// school leaving age
gen msla = 14 if myob<1933
replace msla = 15 if inrange(myob,1933,1957)
replace msla = 16 if myob>1957 & myob!=.
gen fsla = 14 if fyob<1933
replace fsla = 15 if inrange(fyob,1933,1957)
replace fsla = 16 if fyob>1957 & fyob!=.

// past compuslory schooling
gen mpsla5 = (lfte_moth>msla) if lfte_moth!=.
gen fpsla5 = (lfte_fath>fsla) if lfte_fath!=.

gen ageint5 = floor(chcagea0/30.42)
lab var ageint5		"Age at 5y interview (months)"
lab var fysch5		"Age Father left FTE (5y)"
lab var mysch5		"Age Mother left FTE (5y)"
lab var mpsla5		"Mother education past SLA (5y)"
lab var fpsla5		"Father education past SLA (5y)"

keep mcsid ageint5 ?ysch5 ?psla5
tempfile mcsdem3
save `mcsdem3'

******************************************************************
* 5y derived 
use "$mcsraw/S3/mcs3_derived_variables.dta", clear
rename _all, lower
recode cmdres00 (-1=.) (2/max=0), gen(naturalmother5)
rename cmdnvq00 hnvq_main
recode hnvq_main (-1=.) (1/3=0) (4 5 = 1) (95=.) (96 = 0), gen(mhied5)
rename cpdnvq00 hnvq_part
gen numch5 = cdtots00-1

recode cmdact00 (-8/-1=.) (1 2 = 1) (3 4 5 6 7 8 9 =0), gen(mempl5)

// bring in hours
merge 1:1 mcsid using "$mcsraw/S3/mcs3_parent_interview.dta", keepusing(cmwkhr00) keep(1 3) nogen
replace mempl5 = 2 if cmwkhr00>20 // PT is < 20h
lab def pft 0 "Unempl./Home" 1 "Part time" 2 "Full time"
lab val mempl5 pft

recode cmwgtk00 (-8/-1 = .), gen(mweight5)
recode cmhgtm00 (-8/-1 = .), gen(mheight5)
recode cmdbmi00 (-8/-1 = .), gen(mbmi5)

lab var numch5			"Number other children in HH (5y)"
lab var mhied5			"Mother HE degree, NVQ4-5 (5y)"
lab var mheight5		"Mother height (5y) (m)"
lab var mweight5		"Mother weight (5y) (kg)"
lab var mbmi5			"Mother BMI (5y)"
lab var mempl5			"Mother unempl/PT/FT (5y)"


keep mcsid hnvq_main hnvq_part numch5 naturalmother5 mhied5 mweight5 mheight5 mbmi5 mempl5
tempfile mcs5d
save `mcs5d'



*SKILLS AT 5	 ***************************************************************

/* SDQ at 5 */
use "$mcsraw/S3/mcs3_parent_interview.dta", clear

gen datesdq5 = ym(chidty00, chidtm00)
gen dateb = ym(chcdbya0, chcdbma0)
format datesdq5 dateb %tm
gen agesdq5 = datesdq5 - dateb

keep mcsid agesdq5 cmsdpfa0-cmsdtea0
tempfile mcssdq5y
save `mcssdq5y'

/* TESTS at 5 */
use "$mcsraw/S3/mcs3_child_assessment_data.dta", clear
keep if chcnum00==1					// keep only first child
merge 1:1 mcsid using "$mcsraw/S3/mcs3_parent_interview.dta", keepusing(chcdbma0 chcdbya0) keep(3)

gen datetest5 = ym(ccindy01, ccindm01)
gen dateb = ym(chcdbya0, chcdbma0)
format datetest5 dateb %tm
gen agetest5 = datetest5 - dateb

rename cdnvtscr		nvoc_bast
rename cdpstscr		psim_bast
rename cdpctscr		patc_bast
recode *_bast (-1=.)
keep mcsid *_bast agetest5
tempfile mcscog5y
save `mcscog5y'

*SKILLS at 11	 ***************************************************************

* BAS
use "$mcsraw/S5/mcs5_cm_asssessment.dta", clear
rename _all, lower
keep if eccnum00==1					// keep only first child

gen datetest11 = ym(ecinty00, ecintm00)
gen dateb = ym(eccdby00, eccdbm00)
format datetest11 dateb %tm
gen agetest11 = datetest11 - dateb

keep mcsid agetest11 ///
		ecq16i00-ecq37q00 /// verbal similarities test

tempfile mcscog11y
save `mcscog11y'

* SDQ
use "$mcsraw/S5/mcs5_cm_capi.dta", clear
rename _all, lower
keep if eccnum00==1					// keep only first child
keep if eppnum00==1					// keep only MAIN (exclude partner)

gen datesdq11 = ym(ecinty00, ecintm00)
gen dateb = ym(eccdby00, eccdbm00)
format datesdq11 dateb %tm
gen agesdq11 = datesdq11 - dateb

keep mcsid agesdq11 epsdpf00-epsdte00
tempfile mcssdq11y
save `mcssdq11y'

		
*INCOME	 ******************************************************************
use "$data/SEPdata/create37_MCS2012.dta", clear
rename q_bu_net_total_p incq10
gen faminc10_real 	= bu_net_total_p
replace faminc10_real = faminc10_real/adjMCS/1000
gen faminc10_infl 	= bu_net_total_p
replace faminc10_infl = faminc10_infl/inflMCS/1000
tempfile mcsinc11y
save `mcsinc11y'	

*SOCIAL CLASS	 ******************************************************************
use "$data/CohortsHarmonisedSES/raw/MCS Harmonised ChildhoodSES.dta", clear // income
decode MCSID, gen(mcsid)
rename MCS5PCL scl10
replace scl10 = scl10-1
lab def scllab 	1 "I Professional" ///
				2 "II Managerial-technical" ///
				3 "IIINM Skilled non-manual" ///
				4 "IIIM Skilled manual" ///
				5 "IV Partly skilled" ///
				6 "V Unskilled" ///
				7 "Unclassifiable/other"
lab val scl10 scllab

keep mcsid scl10
tempfile mcsscl11y
save `mcsscl11y'

********************************************************************************
// MERGE
********************************************************************************


* merge all
use `mcslong', clear
merge 1:1 mcsid using `mcsdem1', nogen
merge 1:1 mcsid using `mcsdem2', nogen
merge 1:1 mcsid using `mcsdem3', gen(mcs_merge_dem)
merge 1:1 mcsid using `mcssdq5y', gen(mcs_merge_sdq5)
merge 1:1 mcsid using `mcscog5y', gen(mcs_merge_cog5)
merge 1:1 mcsid using `mcs5d', gen(mcs_merge_der5)
merge 1:1 mcsid using `mcssdq11y', gen(mcs_merge_sdq10)
merge 1:1 mcsid using `mcscog11y', gen(mcs_merge_cog10)
merge 1:1 mcsid using `mcsinc11y', gen(mcs_merge_inc11)
merge 1:1 mcsid using `mcsscl11y', gen(mcs_merge_scl11)


********************************************************************************
// CLEAN 11Y SURVEY
********************************************************************************

qui {

* VERBAL SIMILARITIES RECODING
// generate correct answers for each item
// NOTE: drop items 1-7 (only asked of 2 children)
forvalues i=8(1)37 {
	if (`i'<10) local i2 = "0`i'" 
	else local i2 = "`i'" 
	
	gen mcs11_ws`i' = ecq`i2'i00				
	replace mcs11_ws`i' = 1 if ecq`i2'q00 == 1	// replace prompted if correct
	recode mcs11_ws`i' (min/-1=.) (2 3 = 0)		// recode
	if (inlist(`i',16,17)) replace mcs11_ws`i' = 1 if ecq`i2't00 == 1 // training items
	
	local lab: variable label ecq`i2'i00		// copy label
	lab var mcs11_ws`i' "`lab'"	
}

// compute correct answers by subgroup of items
// items 16-28----------------------------
egen correct_16_28 = rowtotal(mcs11_ws16-mcs11_ws28), mi

// child has less than 3 correct responses --> ask items 8-15
// --> if has 3+ correct, put 8-15 to correct
forvalues i=8(1)15 {
	replace mcs11_ws`i' = 1 if correct_16_28>=3 & correct_16_28!=.
}
// child has less than 3 incorrect responses --> ask items 29-33
// --> if has 3+ incorrect (<11 correct), put 29-33 to incorrect
forvalues i=29(1)33 {
	replace mcs11_ws`i' = 0 if correct_16_28<11 & correct_16_28!=.
}

// consecutive fails stopping rule -------------------
// compute consecutive failures for core items
gen consecfail16 = mcs11_ws16
recode consecfail16 (0=1) (1=0)
forvalues i=17(1)28 {
	gen consecfail`i' = 0 if mcs11_ws`i'!=.
	local im1 = `i'-1

	forvalues j=16(1)`i' {
		local jm1 = `j'-1
		replace consecfail`i' = consecfail`im1'+ 1 if mcs11_ws`j'==0	
	}
	replace consecfail`i'=0 if mcs11_ws`i'==1
}
// if failed for 5+ times consecutively, all subsequent items are fails
forvalues i=17(1)28 {
	local im1 = `i'-1
	forvalues j=`i'(1)28 {
		replace mcs11_ws`j'=0 if consecfail`im1'>=5 & consecfail`im1'!=.
	}
}

// items 29-33 ----------------------------
egen correct_29_33 = rowtotal(mcs11_ws29-mcs11_ws33), mi

// child has less than 3 incorrect responses --> ask items 34-37
// --> if has 3+ incorrect (<11 correct), put 29-33 to incorrect
forvalues i=34(1)37 {
	replace mcs11_ws`i' = 0 if correct_29_33<3 & correct_29_33!=.
}

// clear up
drop ecq????? consecfail?? correct_*

} //qui

* SDQ RECODING
* rename
rename epsdpf00 mcs11_sdq1
rename epsdro00 mcs11_sdq2
rename epsdhs00 mcs11_sdq3 
rename epsdsr00 mcs11_sdq4 
rename epsdtt00 mcs11_sdq5 
rename epsdsp00 mcs11_sdq6 
rename epsdor00 mcs11_sdq7 
rename epsdmw00 mcs11_sdq8 
rename epsdhu00 mcs11_sdq9 
rename epsdfs00 mcs11_sdq10 
rename epsdgf00 mcs11_sdq11 
rename epsdfb00 mcs11_sdq12 
rename epsdud00 mcs11_sdq13 
rename epsdlc00 mcs11_sdq14 
rename epsddc00 mcs11_sdq15 
rename epsdnc00 mcs11_sdq16 
rename epsdky00 mcs11_sdq17 
rename epsdoa00 mcs11_sdq18 
rename epsdpb00 mcs11_sdq19 
rename epsdvh00 mcs11_sdq20 
rename epsdst00 mcs11_sdq21 
rename epsdcs00 mcs11_sdq22 
rename epsdgb00 mcs11_sdq23 
rename epsdfe00 mcs11_sdq24 
rename epsdte00 mcs11_sdq25 


********************************************************************************
// CLEAN 5Y SURVEY
********************************************************************************

* Standardise cognitive scores at 5
foreach x in nvoc_bast psim_bast patc_bast {
	egen `x'z = std(`x')
	}


* SDQ RECODING
* rename
rename cmsdpfa0 mcs5_sdq1
rename cmsdroa0 mcs5_sdq2
rename cmsdhsa0 mcs5_sdq3 
rename cmsdsra0 mcs5_sdq4 
rename cmsdtta0 mcs5_sdq5 
rename cmsdspa0 mcs5_sdq6 
rename cmsdora0 mcs5_sdq7 
rename cmsdmwa0 mcs5_sdq8 
rename cmsdhua0 mcs5_sdq9 
rename cmsdfsa0 mcs5_sdq10 
rename cmsdgfa0 mcs5_sdq11 
rename cmsdfba0 mcs5_sdq12 
rename cmsduda0 mcs5_sdq13 
rename cmsdlca0 mcs5_sdq14 
rename cmsddca0 mcs5_sdq15 
rename cmsdnca0 mcs5_sdq16 
rename cmsdkya0 mcs5_sdq17 
rename cmsdoaa0 mcs5_sdq18 
rename cmsdpba0 mcs5_sdq19 
rename cmsdvha0 mcs5_sdq20 
rename cmsdsta0 mcs5_sdq21 
rename cmsdcsa0 mcs5_sdq22 
rename cmsdgba0 mcs5_sdq23 
rename cmsdfea0 mcs5_sdq24 
rename cmsdtea0 mcs5_sdq25 

lab def ncmcs 		-9 "Refusal" ///
					-8 "Don't Know" /// 
					-1 "Not Applicable" ///
					1 "Not True" ///
					2 "Somewhat True" ///
					3 "Certainly True" ///
					4 "Can't Say"
					
foreach v of varlist mcs*_sdq* {
	lab val `v' ncmcs
	}

	
********************************************************************************
// RECODE ALL SDQ
********************************************************************************

qui{
* recode to binary and positive
local pos "1 4 7 9 11 14 17 20 21 25"
foreach i of local pos  {
	recode mcs5_sdq`i' (min/-1=.) (1 2 = 0) (3=1) (4=.), gen(mcs5_sdqb`i') /* b = binary (1 is better) */
	recode mcs11_sdq`i' (min/-1=.) (1 2 = 0) (3=1) (4=.), gen(mcs11_sdqb`i') /* b = binary (1 is better) */
}

local neg "2 3 5 6 8 10 12 13 15 16 18 19 22 23 24"
foreach i of local neg {
	recode mcs5_sdq`i' (min/-1=.) (2 3 = 0) (1=1) (4=.)		, gen(mcs5_sdqb`i') /* b = binary (1 is better) */
	recode mcs5_sdq`i' (min/-1=.) (1=2) (2=1) (3=0) (4=.)	, gen(mcs5_sdqc`i') /* c = three categories (higher is better) */
	recode mcs11_sdq`i' (min/-1=.) (2 3 = 0) (1=1) (4=.)	, gen(mcs11_sdqb`i') /* b = binary (1 is better) */
	recode mcs11_sdq`i' (min/-1=.) (1=2) (2=1) (3=0) (4=.)	, gen(mcs11_sdqc`i') /* c = three categories (higher is better) */
}
forvalues i=1(1)25 {
	local lab: variable label mcs11_sdq`i'
			lab var mcs5_sdqb`i' 	"(Bin) `lab'"
			lab var mcs11_sdqb`i' 	"(Bin) `lab'"
	cap 	lab var mcs5_sdqc`i' 	"(3cat) `lab'"
	cap 	lab var mcs11_sdqc`i' 	"(3cat) `lab'"
}
} //qui


********************************************************************************
// REWEIGHT SAMPLE (ENGLAND)
********************************************************************************

* SAMPLE SELECTION
keep if naturalmother5 == 1	// keep only interviews with natural mother
drop if region > 6			// drop interviews not in England
keep if country == 1
keep if sentry == 1 // only those who entered in sweep 1

sort mcsid

/*
selection probability by stratum (ENG)
					Raw			Normalised		% to select
Advantaged: 		0.0208			1				100
Disadvantaged: 		0.0383			1.84			54.35
Ethnic: 			0.112			5.38			18.59
*/

/* advantaged ------------------------- */
preserve
keep if pttype2==1
gen rwtd=1
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
gen rwtd = 0
replace rwtd = 1 in 1/`toselect'

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
gen rwtd = 0
replace rwtd = 1 in 1/`toselect'

tempfile ethn
save `ethn'
restore

use `adv', clear
append using `disadv'
append using `ethn'


****************************
/* SAVE FILES for R */

local covarstokeep country region ///
					sex smkpr singlem malaise mempl nprevst caesbirth ///
					ethn bwt lowbwt gestaw preterm mothageb teenm parity firstb mheight mweight mbmi ///
					mysch5 fysch5 mhied5 numch5 mweight5 mheight5 mbmi5 mempl5 ?psla5 ///
					scl10 incq10 faminc10_real faminc10_infl

/* SAVE 5y FILE */
preserve
egen ncmiss=rowmiss(mcs5_sdq*)
drop if ncmiss >21
keep mcsid sentry pttype2 mcs5_sdq* nvoc_bastz psim_bastz patc_bastz age*5 rwtd `covarstokeep'
saveold "$rdata/mcs5yeng.dta", replace version(12)
keep if rwtd==1
saveold "$rdata/mcs5yeng_rwt.dta", replace version(12)
restore


/* SAVE 11y FILE */
preserve
egen ncmiss=rowmiss(mcs11_sdq*)
drop if ncmiss >21
keep mcsid sentry pttype2 mcs11_sdq* mcs11_ws* age*11 rwtd `covarstokeep'
saveold "$rdata/mcs11yeng.dta", replace version(12)
keep if rwtd==1
saveold "$rdata/mcs11yeng_rwt.dta", replace version(12)
restore




***************************************************************


*BMI (harmonised)	 **********************************************
use "$data/CohortsHarmonisedBMI/raw/mcs_closer_wp1.dta", clear // income
keep if visitage==11
rename bmi bmi11
keep mcsid bmi11
decode mcsid, gen(mcsid2)
drop mcsid
rename mcsid2 mcsid
tempfile mcsbmi11
save `mcsbmi11'

// BMI at 14
use "$mcsraw/S6/mcs6_cm_measurement.dta", clear
rename _all, lower
keep if fcnum00==1 // CM number 1
gen bmi14 = fcwtcm00/((fchtcm00/100)^2) if fcwtcm00>0 & fchtcm00>0
keep mcsid bmi14
tempfile mcsbmi14
save `mcsbmi14'


// behaviours at 14-15
use "$mcsraw/S6/mcs6_cm_interview.dta", clear
rename _all, lower
keep if fcnum00==1 // CM number 1

rename fccage00 age14
recode fcalcd00 (-9/-1=.) (2=0), gen(alctry14)
recode fcsmok00 (-9/-1=.) (1=0) (2/6=1), gen(smktry14)
gen hadsex14 = 0 if inlist(fchhnd00,1,2)
replace hadsex14 =1 if fcsexx00==1
gen drugtry14 = 0 if fccanb00>0
replace drugtry14 = 1 if fccanb00==1 | fcotdr00==1
recode fcstln00 (-9/-1=.) (2=0), gen(stole14)
recode fcharm00 (-9/-1=.) (2=0), gen(selfharm14)

lab var smktry14		"Tried smoking (14)"
lab var alctry14		"Tried alcohol (14)"
lab var hadsex14		"Had sex (14)"
lab var drugtry14		"Tried drugs (14)"
lab var stole14			"Ever stole anything (14)"
lab var selfharm14		"Self harmed in past year (14)"

keep mcsid age14 smktry14 alctry14 hadsex14 drugtry14 stole14 selfharm14 


merge 1:1 mcsid using `mcsbmi11', nogen keep(1 3)
merge 1:1 mcsid using `mcsbmi14', nogen keep(1 3)


saveold "$rdata/mcs14outc.dta", replace version(12)
