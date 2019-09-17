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
set seed 4129736			/* for random reweighting */
set matsize 10000

global data 		"/Users/giacomomason/Documents/Data"
global bcsraw   	"$data/BCS/raw"
global ncdsraw   	"$data/NCDS/raw"
global mcsraw   	"$data/MCS/raw"
global temp   		"$data/temp"

global proj			"/Users/giacomomason/Documents/Projects/CohortStudies"
global dofiles  	"$proj/codes/stata"
global tables  		"$proj/tables"
global stata_estimates  		"$proj/stata_estimates"
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

/* create interactions */
local decvars_int `decvars';
local nvar : word count `decvars';
forval i = 1/`nvar' {;
  forval j = 1/`=`i'-1' {;
    local x : word `i' of `decvars';
    local y : word `j' of `decvars';
    generate `x'_X_`y' = `x' * `y';
	local decvars_int `decvars_int' `x'_X_`y';
  };
};


/* rescale EXT/INT to be all positive */
replace EXT = EXT+100;
replace INT = INT+100;
su EXT if sex == 1 & cohort == 1, det;
su EXT if sex == 1 & cohort == 2, det;


/* RIF decompositions */

log using "$stata_estimates/riflog.txt", text replace;
local glab1 "M";
local glab2 "F";

local iq1 = "25 75";
local iq2 = "50 90";
local iq3 = "10 50";

local skill EXT INT;
foreach s of local skill {;
	forvalues q = 1(1)3 {;
	local qstr = subinstr("`iq`q''"," ","",.);
		forvalues g = 1(1)2 {;
		preserve;
		qui log on;
		
		/* normal SEs */
		di "";
		di "";
		di "***********************************************";
		di "RIF Decomposition estimates for `s' - `glab`g'' - NORMAL STANDARD ERRORS";
		di "";
		oaxaca_rif `s' `decvars' if sex == `g', by(cohort) wgt(1) rif(iqr(`iq`q'')) rwlogit(`decvars_int') relax noisily;
		mat R = r(table);
		qui log off;
		mat R = R';
		xsvmat R, norestore names(col) roweqnames(eqname) rownames(varname);
		export delimited "$stata_estimates/rif_`s'_`glab`g''_`qstr'.csv", replace;
		restore;
		
		
		/* bootstrap SEs */
		di "";
		di "";
		di "***********************************************";
		di "RIF Decomposition estimates for `s' - `glab`g'' - BOOTSTRAP STANDARD ERRORS";
		di "";
		preserve;
		bootstrap, reps(1000) nodrop: oaxaca_rif `s' `decvars' if sex == `g', by(cohort) wgt(1) rif(iqr(`iq`q'')) rwlogit(`decvars_int') relax noisily;
		mat R = r(table);
		qui log off;
		mat R = R';
		xsvmat R, norestore names(col) roweqnames(eqname) rownames(varname);
		export delimited "$stata_estimates/rif_`s'_`glab`g''_`qstr'_boot.csv", replace;
		restore;
		
		};
	};
};
