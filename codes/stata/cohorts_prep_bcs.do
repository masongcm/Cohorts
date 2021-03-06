
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

gen mothagefb = .
replace mothagefb = mothageb if firstb==1
replace mothagefb = (1900 + a0052) - myob if a0052>0
replace mothagefb = . if mothagefb<10

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

recode a0014 (-2=.) (8=7), gen(fscl)
replace fscl = 8 if fscl==.
recode a0018 (-2=.) (8=7), gen(mscl)
// label for social class
lab def scllab 	1 "I Professional" ///
				2 "II Managerial-technical" ///
				3 "IIINM Skilled non-manual" ///
				4 "IIIM Skilled manual" ///
				5 "IV Partly skilled" ///
				6 "V Unskilled" ///
				7 "Unempl/uncl/army/other" ///
				8 "No partner"
lab val fscl scllab
lab val mscl scllab

lab var sex				"Sex"
lab var bwt				"Birthweight (kg)"
lab var lowbwt			"Low birthweight (<2500g)"
lab var parity			"Parity"
lab var nprevst			"Num previous stillbirths"
lab var firstb			"First born"
lab var mothageb		"Mother age at CM birth"
lab var mothagefb		"Mother age at first birth"
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
lab var fscl			"Father SC"
lab var mscl			"Mother SC (most recent job)"


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

keep bcsid sex bwt lowbwt smkpr mothageb mothagefb teenm parity firstb nprevst ///
				gestaw preterm singlem mempl caesbirth mheight myob fscl mscl
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
merge 1:1 bcsid using `bcsdem1', nogen keepusing(myob) keep(1 3)
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

// social class
recode e197 (-2=.) (-1=8) (-3=7), gen(fscl5)
recode e206 (-2=.) (-1=.) (-3=7), gen(mscl5)
replace mscl5=7 if mempl5==0
lab val fscl5 scllab
lab val mscl5 scllab


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
lab var mscl5			"Mother SC (5)"
lab var fscl5			"Father SC (5)"


keep bcsid ethn ageint5 ?ysch5 numch5 mhied5 mempl5 ?psla5 ?scl5
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

*SOCIAL CLASS (childhood)	 **********************************************
use "$data/CohortsHarmonised/raw/bcs70_harmonised_childhoodses.dta", clear // income
rename BCSID bcsid
rename BCS3FCL scl10
recode scl10 (3.1 = 3) (3.2=4) (4=5) (5=6) (6=7) (11=.)
lab def scllab 	1 "I Professional" ///
				2 "II Managerial-technical" ///
				3 "IIINM Skilled non-manual" ///
				4 "IIIM Skilled manual" ///
				5 "IV Partly skilled" ///
				6 "V Unskilled" ///
				7 "Unclassifiable/other"
lab val scl10 scllab
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
					mscl fscl ///
					sex smkpr singlem mempl nprevst caesbirth ///
					ethn bwt lowbwt gestaw preterm mothageb mothagefb teenm parity firstb mheight ///
					?ysch5 numch5 mhied5 ?empl5 ?psla5 ?scl5 ///
					scl10 incq10 faminc10_real faminc10_infl

preserve
* SAMPLE SELECTION
egen ncmiss=rowmiss(bcs10_rut*)
//drop if ncmiss >22

keep bcsid age*10 bcs10_rut* bcs10_ws* `covarstokeep'
saveold "$rdata/bcs10yeng.dta", replace version(12)
restore


****************************
/* SAVE 5Y FILE for R */

preserve
* SAMPLE SELECTION
egen ncmiss=rowmiss(bcs5_rut*)
//drop if ncmiss >22

keep bcsid age*5 epvt_z copy_z hfd_z bcs5_rut* `covarstokeep'
saveold "$rdata/bcs5yeng.dta", replace version(12)
restore

********************************************************************************
***** ADULT OUTCOMES		                ************************************
********************************************************************************

*BMI (harmonised)	 **********************************************
use "$data/CohortsHarmonised/raw/bcs70_closer_wp1.dta", clear // income
keep if inlist(visitage, 10, 16, 42)
keep bcsid bmi visitage xage
reshape wide bmi xage, i(bcsid) j(visitage)
rename xage?? agebmi??
tempfile bcsbmi
save `bcsbmi'


*QUALIFICATIONS AT 30	 *******************************************************

use "$bcsraw/2000/bcs6derived.dta", clear
rename _all, lower
keep bcsid hinvq00
rename hinvq00 nvq30
recode nvq30 (-9=.)
tempfile bcsoutc30y
save `bcsoutc30y'

*QUALIFICATIONS AT 34	 *******************************************************

use "$bcsraw/2004/bcs7derived.dta", clear
rename _all, lower
rename bd7hnvq nvq34
keep bcsid nvq34
tempfile bcsqual34y
save `bcsqual34y'

*EARNINGS AT 34	 *******************************************************

use "$bcsraw/2004/bcs_2004_followup.dta", clear

// smoking
recode bd7smoke (-7 = .) (0 1 2 = 0) (3/6 = 1), gen(smoke34)

// income
recode b7cgropd (1 = 1) (2 = 0.5) (3 = 0.25) (4 = 0.23333) (5 = 0.019178) (6 9 = .), gen(towgpay)
gen gpay7 = b7cgropy*towgpay if b7cgropy>0
qui su gpay7, det
replace gpay7 = . if gpay7>r(p99) // trim
gen lgpay7 = log(gpay7)

gen gearn7 = b7seearn/52 if b7seearn>=0 // earnings
qui su gearn7, det
replace gearn7 = . if gearn7>r(p99) // trim
gen lgearn7 = log(gearn7)

recode b7cnetpd (1 = 1) (2 = 0.5) (3 = 0.25) (4 = 0.23333) (5 = 0.019178) (6 9 = .), gen(townpay)
gen npay7 = b7cnetpy*townpay if b7cnetpy>0
gen gprof7 = b7seprit/52 if b7seprit>=0 // profit
egen ginc7 = rowtotal(gpay7 gprof7 gearn7), mi
replace ginc7 = 0 if !inlist(bd7ecact,1,2,3,4,.) & ginc7==. // not working
gen lginc7 = log(ginc7)


// employment
recode bd7ecact (1 2 = 1) (3 4 = 2) (5/12 = 0) (-9/-1 = .), gen(empst34)
lab def empl	0 "Not employed" 1 "Employed" 2 "Self employed"
lab val empst34 empl

rename gpay7 gpay34
rename lgpay7 lgpay34
rename gearn7 gearn34
rename lgearn7 lgearn34

lab var smoke34		"Daily smoker (34)"
lab var gpay34		"Gross pay (34)"
lab var lgpay34		"(log) Gross pay (34)"
lab var gearn34		"Income from Self-employment (34)"
lab var lgearn34	"(log) Income from Self-employment (34)"
lab var empst34		"Employment (34)"


rename lginc7 lginc34
keep bcsid smoke34 empst34 lgpay34 lgearn34
tempfile bcsoutc34y
save `bcsoutc34y'


*SOCIAL CLASS (adult, 42)	 **********************************************
use "$data/CohortsHarmonised/raw/bcs70_harmonised_adultses.dta", clear // income
rename BCSID bcsid
rename BCS9CL scl42
recode scl42 (3.1 = 3) (3.2=4) (4=5) (5=6) (6=7) (11=.)
lab def scllab 	1 "I Professional" ///
				2 "II Managerial-technical" ///
				3 "IIINM Skilled non-manual" ///
				4 "IIIM Skilled manual" ///
				5 "IV Partly skilled" ///
				6 "V Unskilled" ///
				7 "Unclassifiable/other"
lab val scl42 scllab
lab var scl42	"Social Class at 42"
keep bcsid scl42
tempfile bcsscl42y
save `bcsscl42y'

* economic activity at 42
use "$bcsraw/2012/bcs70_2012_derived.dta", clear
rename _all, lower
keep bcsid bd9ecact
tempfile ecact42
save `ecact42'

*SMOKING at 42 
use "$bcsraw/2012/bcs70_2012_flatfile.dta", clear
rename _all, lower
merge 1:1 bcsid using `ecact42', nogen

// smoking
recode b9smokig (-9/-1 = .) (1 2 3 = 0) (4 = 1), gen(smoke42)

// income
recode b9grop (1 = 1) (2 = 0.5) (3 = 0.333) (4=0.25) (5 = 0.23333) (6 = 0.116666) (52 = 0.019178) (-9 -8 -1 9 10 90 95 96 = .), gen(towgpay)
gen gpay9 = b9groa*towgpay if b9groa>0
qui su gpay9, det
replace gpay9 = . if gpay9>r(p99) // trim
gen lgpay9 = log(gpay9)

gen nearn9 = b9sepa /52 if b9sepa >=0 // take home income from SE
qui su nearn9, det
replace nearn9 = . if nearn9>r(p99) // trim
gen lnearn9 = log(nearn9)

recode b9netp (1 = 1) (2 = 0.5) (3 = 0.333) (4=0.25) (5 = 0.23333) (6 = 0.116666) (52 = 0.019178) (-9 -8 -1 9 10 90 95 96 = .), gen(townpay)
gen npay9 = b9neta*townpay if b9neta>0
egen ninc9 = rowtotal(npay9 nearn9), mi
replace ninc9 = 0 if !inlist(bd9ecact,1,2,3,4,.) & ninc9==. // not working
gen lninc9 = log(ninc9)

// employment
recode bd9ecact (1 2 = 1) (3 4 = 2) (5/12 = 0) (-9/-1 = .), gen(empst42)
lab def empl	0 "Not employed" 1 "Employed" 2 "Self employed"
lab val empst42 empl

rename gpay9 gpay42
rename lgpay9 lgpay42
rename nearn9 nearn42
rename lnearn9 lnearn42
lab var smoke42		"Daily smoker (42)"
lab var gpay42		"Gross pay (42)"
lab var lgpay42		"(log) Gross pay (42)"
lab var nearn42		"Income from Self-employment (42)"
lab var lnearn42	"(log) Income from Self-employment (42)"
lab var empst42		"Employment (42)"

keep bcsid smoke42 empst42 lgpay42 lnearn42
tempfile bcsoutc42y
save `bcsoutc42y'


*EARNINGS AT 38	 *******************************************************

use "$bcsraw/2008/bcs_2008_followup.dta", clear

gen hgrpay38 = b8cgrowk/b8chour1 if b8chour1>0 & b8cgrowk>=0
gen lhgrpay38 = log(hgrpay38)

// smoking
recode bd8smoke (-8/-1 = .) (0 1 2 = 0) (3/6 = 1), gen(smoke38)

lab var lhgrpay38		"Log gross hourly pay (38y)"
lab var smoke38			"Daily smoker (38y)"
keep bcsid lhgrpay38 smoke38
tempfile bcsoutc38y
save `bcsoutc38y'


*BEHAVIOURS AT 16	 ***********************************************************


use "$bcsraw/1986/bcs7016x.dta", clear

/* age (use date of document O) */
gen ymb = ym(1970,4)
gen ymo = ym(1900+odoc_yr,odoc_mt) if odoc_yr>0 & odoc_mt>0
gen age16 = round((ymo-ymb)/12,.1)

recode f44 (-2 -1 =.) (1/4 =1), gen(smktry16a)
recode gh1 (-2 -1 =.) (1=0) (2/4 =1), gen(smktry16b)
gen smktry16 =  smktry16a
replace smktry16 = smktry16b if smktry16==.
recode f56 (-2 -1 =.) (1/max=1), gen(alcoh16)
recode f57_tot (-4/-1 = .) (0/9 = 0) (10/max=1), gen(hialc16)
replace hialc16 = 0 if alcoh16==0
replace alcoh16 = 1 if f57_tot>0
recode hd1 (-2 -1 =.) (7=0) (1/6=1), gen(alctry16)
recode gf2 (-2 -1 =.) (1/3=1) (4=0), gen(porn16)
egen hadsex16 = anymatch(hb9_2 hb9_3 hb9_4 hb9_5 hb9_6 hb9_8 hb9_9), values(1)
replace hadsex16 = . if hb9_2==-1
egen drugtry16 = anymatch(q31_3 q31_5 q31_7 q31_9 q31_11 q31_13 q31_15), values(2 3 4 5) 
replace drugtry16 = . if q31_3==-1
recode q31_7 (-2 -1 =.) (1=0) (2/5=1), gen(canntry16)
recode hd14 (-2 -1=.) (1 6=0) (2/5=1), gen(drunk16)
recode jc13 (-2 -1=.) (2=0), gen(read16)
recode q22_1 (-2 -1=.) (6=0) (1/5=1), gen(propdam16)
recode q22_7 (-2 -1=.) (6=0) (1/5=1), gen(shplift16)

lab var age16			"Age at parent interview (16)"
lab var smktry16		"Tried smoking (16)"
lab var alctry16		"Tried alcohol (16)"
lab var alcoh16			"Alcohol in past week (16)"
lab var hialc16			">10 Units alcohol in past week (16)"
lab var drunk16			"Ever been drunk (16)"
lab var porn16			"Porn in past month (16)"
lab var hadsex16		"Had sex (16)"
lab var drugtry16		"Tried drugs (16)"
lab var canntry16		"Tried cannabis (16)"
lab var read16			"Read book for pleasure in past week (16)"
lab var propdam16		"Damaged other's property in past year (16)"
lab var shplift16		"Shoplifted >5£ in past year (16)"

egen tokeep = rownonmiss(smktry16 alctry16 alcoh16 drunk16 porn16 hadsex16 drugtry16 read16 propdam16 shplift16)
drop if tokeep==0
keep bcsid smktry16 alctry16 alcoh16 drunk16 porn16 hadsex16 drugtry16 read16 propdam16 shplift16 canntry16 age16 

merge 1:1 bcsid using `bcsoutc30y', nogen keep(1 3)
merge 1:1 bcsid using `bcsoutc34y', nogen keep(1 3)
merge 1:1 bcsid using `bcsqual34y', nogen keep(1 3)
merge 1:1 bcsid using `bcsoutc38y', nogen keep(1 3)
merge 1:1 bcsid using `bcsoutc42y', nogen keep(1 3)
merge 1:1 bcsid using `bcsscl42y', nogen keep(1 3)
merge 1:1 bcsid using `bcsbmi', nogen keep(1 3)

saveold "$rdata/bcsoutc.dta", replace version(12)




