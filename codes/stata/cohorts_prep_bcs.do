
********************************************************************************
***** PREP BCS 70 DATA		                ************************************
********************************************************************************

********************************************************************************
/* birth data (sex, country, region) */
use "$bcsraw/1970_birth/bcs7072a.dta", clear
rename a0255 sex
recode a0278 (-3/-2=.), gen(bwt)
replace bwt = bwt/1000
gen lowbwt = bwt<2.5 if bwt!=.
recode a0043b (-3=.) (1/3=0) (4/6=1), gen(smkpr)

recode a0005a (-2=.), gen(mothageb)
gen teenm = mothageb<20 if mothageb!=.
gen myob = 1970 - mothageb

rename a0166 parity
gen firstb = parity==0 if parity!=.
rename a0169 nprevst

recode a0195b (-3 -2 = .), gen(gestaw)
gen preterm = gestaw<37 if gestaw!=.
recode a0012 (-2=.) (1 3 4 5 = 1) (2 = 0), gen(singlem)

recode a0013 (-2 -1=.) (1/17=1) (18/22=0), gen(fempl)
replace fempl = 1 if fempl==. & a0015==1
replace fempl = 0 if a0015==2

recode a0017 (-2=.) (1/17=1) (18/22=0), gen(mempl)
replace mempl = 1 if mempl==. & a0019==1
replace mempl = 0 if a0019==2

recode a0262 (-3/-1 = .) (7=1) (1/6 8 = 0), gen(caesbirth)

gen mheight = a0197/100 if a0197>0

lab var sex				"Sex"
lab var bwt				"Birthweight (kg)"
lab var lowbwt			"Low birthweight (<2500g)"
lab var parity			"Parity"
lab var nprevst			"Num previous stillbirths"
lab var firstb			"First born"
lab var mothageb		"Mother age at CM birth"
lab var teenm			"Teen mother"
lab var smkpr			"Smoked during pregnancy"
lab var gestaw			"Gestational age (weeks)"
lab var preterm			"Preterm (<37w)"
lab var singlem			"Single mother"
lab var mempl			"Mother employment before pregnancy"
lab var fempl			"Father employment"
lab var caesbirth		"Caesarean birth"
lab var mheight			"Mother height (m)"
lab var myob			"Mother year of birth"


/*
rename a0163 nprevpreg
rename a0167 nprevms
recode a0317 (-3/-1 = .) (1 2 = 1) (3 = 0), gen(jaundice)
recode a0288 (-3 -2 = .) (2 = 0), gen(preecl)
replace preecl = 1 if a0293==12
replace preecl = 1 if a0228==1
recode a0261a (-3 -2 = .) (2=0), gen(meconm)
lab var jaundice		"Jaundiced baby"
lab var preecl			"Eclampsia during labor, signs of pre-eclampsia, eclamptic fits"
lab var meconm			"Meconium"
lab var nprevpreg		"Num previous pregnancies"
lab var nprevms			"Num previous miscarriages"

*/

keep bcsid sex bwt lowbwt smkpr mothageb teenm parity firstb nprevst ///
				gestaw preterm singlem mempl caesbirth mheight myob
tempfile bcsdem1
save `bcsdem1'

********************************************************************************
// Derived
use "$bcsraw/1970_birth/bcs1derived.dta", clear
rename BCSID bcsid
rename BD1CNTRY country 
lab var country 		"Country at Birth"
gen region = BD1REGN
recode region (1 8 =1) (2=2) (3=3) (7=4) (6=5) (4 5 =6) (9=10) (10=11) (11=12)
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

keep bcsid country region
tempfile bcsdem2
save `bcsdem2'

********************************************************************************
/* 5y data (age at interview, parental education, and ethnicity) */
use "$bcsraw/1975/f699b.dta", clear

// ethnicity
recode e245 (-3 -2 = .) (1 2 = 0) (3/7=1), gen(ethn)

// parents year of birth
merge 1:1 bcsid using `bcsdem1', nogen keepusing(myob)
gen fyob = 1975 - e009 if e009>0

// school leaving age
gen msla = 14 if myob<1933
replace msla = 15 if inrange(myob,1933,1957)
replace msla = 16 if myob>1957 & myob!=.
gen fsla = 14 if fyob<1933
replace fsla = 15 if inrange(fyob,1933,1957)
replace fsla = 16 if fyob>1957 & fyob!=.

// parental education
rename e195		mysch5
rename e196		fysch5
recode mysch5 fysch5 (-3 -2 -1 =.)

// higher education
recode e189a (-3/-1 = .) (6 7=1) (1/5 8 =0), gen(mhied5)
replace mhied5=0 if mhied5==. & e193==0 // no additional years of FT education after school

// age left FTE
replace mysch5 = mysch5+15 // age left FTE
replace fysch5 = fysch5+15

// past compulsory school (more than 0 years past 16
gen mpsla5 = (mysch5>msla) if mysch5!=.
gen fpsla5 = (fysch5>fsla) if fysch5!=.

// maternal employment/FTE
recode e217 (-3/-1 70 80=.) (10 50 = 0) (21 22 30 41 42 = 1) (20 40 60 61 = 2), gen(mempl5)
lab def pft 0 "Unempl./Home" 1 "Part time" 2 "Full time"
lab val mempl5 pft


// age child at interview
recode e271 (-3=.)
tostring e271, gen(datestr)
gen year = 1900 + real((substr(datestr,1,2)))
gen month = real((substr(datestr,3,2)))
gen dateint5 = ym(year,month)
format dateint5 %tm
gen dateb = tm(1970-04)
format dateb %tm
gen ageint5 = dateint5 - dateb

// number of children
gen numch5 = e006 + e007 if e007!=-1 & e006!=-1

lab var ageint5			"Age at 5y interview (months)"
lab var ethn			"Nonwhite ethnicity"
lab var fysch5			"Age Father left FTE (5y)"
lab var mysch5			"Age Mother left FTE (5y)"
lab var mhied5			"Mother HE degree (5y)"
lab var numch5			"Number other children in HH (5y)"
lab var mempl5			"Mother unempl/PT/FT (5y)"
lab var mpsla5			"Mother education past SLA (5y)"
lab var fpsla5			"Father education past SLA (5y)"


keep bcsid ethn ageint5 ?ysch5 numch5 mhied5 mempl5 ?psla5
tempfile bcsdem3
save `bcsdem3'



*SKILLS at 5	 ***************************************************************

* 5 Year Survey - Rutter (parental module), maternal malaise
use "$bcsraw/1975/f699a.dta", clear
keep bcsid d025-d043 d006-d009 d045-d064

// maternal malaise score
recode d045 d046 d048 d052 d055 d057 d059 d063 d064 (-3=.) (2=0)
local mlq "d045 d046 d048 d052 d055 d057 d059 d063 d064"
gen malaise5=0
foreach v of local mlq {
	replace malaise5 = malaise5 + `v'
}
lab var malaise5	"Mother malaise (5y)"

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
rename q_bu_net_total_p incq10
gen faminc10_real 	= bu_net_total_p
replace faminc10_real = faminc10_real/adjBCS/1000
gen faminc10_infl 	= bu_net_total_p
replace faminc10_infl = faminc10_infl/inflBCS/1000
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


// SAMPLE SELECTION
keep if country==1			// England only
drop if sex<1				/* missing observations */
drop if region > 6			// drop interviews not in England

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
	qui {
		gen bcs10_ws`i'=`v'
		lab var bcs10_ws`i' "BAS WS: Item `i'"
		recode bcs10_ws`i' (min/-1=.) (2=0) (9=0)	// assume missing response is wrong
		local i=`i'+1
		drop `v'								// drop unused variable
	}
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
qui {
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
} //qui
	
****************************
/* SAVE 10Y FILE for R */


local covarstokeep country region ///
					sex smkpr singlem mempl nprevst caesbirth ///
					ethn bwt lowbwt gestaw preterm mothageb teenm parity firstb mheight ///
					?ysch5 numch5 mhied5 ?empl5 ?psla5 ///
					scl10 incq10 faminc10_real faminc10_infl

preserve
* SAMPLE SELECTION
egen ncmiss=rowmiss(bcs10_rut*)
drop if ncmiss >22

keep bcsid age*10 bcs10_rut* bcs10_ws* `covarstokeep'
saveold "$rdata/bcs10yeng.dta", replace version(12)
restore


****************************
/* SAVE 5Y FILE for R */

preserve
* SAMPLE SELECTION
egen ncmiss=rowmiss(bcs5_rut*)
drop if ncmiss >22

keep bcsid age*5 epvt_z copy_z hfd_z bcs5_rut* `covarstokeep'
saveold "$rdata/bcs5yeng.dta", replace version(12)
restore


*QUALIFICATIONS AT 30	 *******************************************************

* 30 Year Survey
use "$bcsraw/2000/bcs6derived.dta", clear
rename _all, lower
keep bcsid hinvq00
rename hinvq00 nvq30
recode nvq30 (-9=.)
tempfile bcsoutc30y
save `bcsoutc30y'

*BEHAVIOURS AT 16	 ***********************************************************

use "$bcsraw/1986/bcs4derived.dta", clear
rename _all, lower
gen age16 = floor(bd4age)
replace age16 = 98 if age16==-1 // missing age, use factor
keep bcsid age16
tempfile bcs16age
save `bcs16age'

use "$bcsraw/1986/bcs7016x.dta", clear
recode f44 (-2 -1 =.) (1/4 =1), gen(smktry16a)
recode gh1 (-2 -1 =.) (1=0) (2/4 =1), gen(smktry16b)
gen smktry16 =  smktry16a
replace smktry16 = smktry16b if smktry16==.
recode f56 (-2 -1 =.) (1/max=1), gen(alcoh16)
recode hd1 (-2 -1 =.) (7=0) (1/6=1), gen(alctry16)
recode gf2 (-2 -1 =.) (1/3=1) (4=0), gen(porn16)
egen hadsex16 = anymatch(hb9_2 hb9_3 hb9_4 hb9_5 hb9_6 hb9_8 hb9_9), values(1)
replace hadsex16 = . if hb9_2==-1
egen drugtry16 = anymatch(q31_3 q31_5 q31_7 q31_9 q31_11 q31_13 q31_15), values(2 3 4 5) 
replace drugtry16 = . if q31_3==-1
recode hd14 (-2 -1=.) (1 6=0) (2/5=1), gen(drunk16)
recode jc13 (-2 -1=.) (2=0), gen(read16)
recode q22_1 (-2 -1=.) (6=0) (1/5=1), gen(propdam16)
recode q22_7 (-2 -1=.) (6=0) (1/5=1), gen(shplift16)

lab var smktry16		"Tried smoking (16)"
lab var alctry16		"Tried alcohol (16)"
lab var alcoh16			"Alcohol in past week (16)"
lab var drunk16			"Ever been drunk (16)"
lab var porn16			"Porn in past month (16)"
lab var hadsex16		"Had sex (16)"
lab var drugtry16		"Tried drugs (16)"
lab var read16			"Read book for pleasure in past week (16)"
lab var propdam16		"Damaged other's property in past year (16)"
lab var shplift16		"Shoplifted >5£ in past year (16)"

egen tokeep = rownonmiss(smktry16 alctry16 alcoh16 drunk16 porn16 hadsex16 drugtry16 read16 propdam16 shplift16)
drop if tokeep==0
keep bcsid smktry16 alctry16 alcoh16 drunk16 porn16 hadsex16 drugtry16 read16 propdam16 shplift16

merge 1:1 bcsid using `bcs16age', nogen keep(3)
merge 1:1 bcsid using `bcsoutc30y', nogen keep(1 3)

saveold "$rdata/bcs16outc.dta", replace version(12)




