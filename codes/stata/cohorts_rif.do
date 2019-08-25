/* 
RIF DECOMPOSITION 

Performs a decomposition of the differences between BCS and MCS cohorts based on
sets of observable characteristics

*/


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

#delimit ;
set more off;

use "$rdata/regdata.dta", clear;

tab fscl5wb, gen (fscl5wbd);
tab preterm, gen (pretermd);

/* (maybe) recode dummies */
local dummies mpsla5 mempl5b singlem ethn firstb smkpr;
foreach x of local dummies {;
	fre `x';
};

/* list of variables to include */
local decvars_med mpsla5;
local decvars_mem mempl5b;
local decvars_ses fscl5wbd2 fscl5wbd3;
local decvars_mch mothageb mheight singlem ethn numch5;
local decvars_prg firstb nprevst smkpr pretermd2 pretermd3 lbwt;
local decvars `decvars_med' `decvars_mem' `decvars_ses' `decvars_mch' `decvars_prg';
local numdv : list sizeof local(decvars);
di "`numdv'";

/* RIF decompositions */
local skill EXT INT;
foreach s of local skill {;
	forvalues g = 1(1)2 {;
	preserve;
	qui oaxaca_rif `s' `decvars' if sex == `g', by(cohort) wgt(1) rif(iqr(.75 .25)) rwlogit(`decvars');
	mat R = r(table);
	mat R = R';
	
	clear;
	xsvmat R, norestore names(col) roweqnames(eqname) rownames(varname);
	export delimited "$tables/rif_`s'_`g'.csv", replace;

	restore;
	};
};
