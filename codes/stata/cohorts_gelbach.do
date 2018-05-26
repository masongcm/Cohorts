
#delimit ;
set more off;

use "$rdata/finaldata.dta", clear;

tab scl10b, gen(scl10bd);
tab region, gen(regiond);
tab mempl5, gen(mempl5d);
tab fscl5wb, gen(fscl5wbd);
tab preterm, gen(pretermd);

local med_mem "mempl5d2 mempl5d3";
local med_fsc "fscl5wbd2 fscl5wbd3";
local med_mch "mothageb mheight singlem ethn numch5";
local med_prg "firstb nprevst smkpr pretermd2 pretermd3 lbwt";

local med_all = "`med_mem' `med_mch' `med_prg'";
di "`med_all'";

local outcomes "EXT INT";
local dvar mpsla5;

local glab1 "Maternal employment (5)";
local glab2 "Father occupation (5)";
local glab3 "Maternal background (birth)";
local glab4 "Pregnancy";


/**************************************************************************/
/* GELBACH DECOMPOSITION */
/**************************************************************************/

forvalues c = 1(1)2 {; 				/* cohort */
foreach o of local outcomes {;		/* outcome */
forvalues g = 1(1)2 {; 				/* gender */
	
	qui b1x2 `o' if cohort == `c' & sex==`g', 	
						x1all(`dvar' regiond2 regiond3 regiond4 regiond5 regiond6) /* DVAR FIRST */
						x2all(`med_all')
						robust
						x1only(`dvar')
						x2delta(mem = `med_mem' : fsc = `med_fsc' : mch = `med_mch' : prg = `med_prg')
						;	
	mat b1base		= e(b1base);
	mat V1base		= e(V1base);
	mat b1full		= e(b1full);
	mat V1full		= e(V1full);
	mat decompmat 	= e(b);				/* decomposition */
	mat decompsemat = e(V);
	local groups    = e(groupnames);

	/* post results to dataset */
	tempname medtab;
	tempfile mediation_`o'_`c'_`g';
	postfile `medtab' 
		str100 outcome			/* outcome */
		str100 varname 			/* mediator */
		str100 varlab			/* mediator label */
		str100 coef_`c'_`g'		/* coefficients */
		str100 percent_`c'_`g'	/* % explained */
		using "`mediation_`o'_`c'_`g''", replace;
			
	/* DIFFERENCE IN BASE MODEL ---------------------------------------------------- */
	local basediff 		= b1base[1,1];
	di `basediff';
	local basediffse 	= sqrt(V1base[1,1]);
	local tstat1		= abs(`basediff'/`basediffse');		/* t-stat */

	/* add stars */
	if (inrange(`tstat1',0,1.645)) 			local basediffstr 	= ("$" + strtrim("`: di %10.4f `basediff''") + "$");
	else if (inrange(`tstat1',1.645,1.96)) 	local basediffstr 	= ("$" + strtrim("`: di %10.4f `basediff''") + "^{*}$");
	else if (inrange(`tstat1',1.96,2.58)) 	local basediffstr 	= ("$" + strtrim("`: di %10.4f `basediff''") + "^{**}$");
	else if (`tstat1'>2.58) 				local basediffstr 	= ("$" + strtrim("`: di %10.4f `basediff''") + "^{***}$");
	/* add SE */
	local basediffstr = "`basediffstr'" 	+ ("\newline ($" 	+ strtrim("`: di %10.4f `basediffse''") + "$)");
	
	/* unconditional effect */
	post `medtab' 	("`o'") ("baseeff") ("Unconditional Difference") ("`basediffstr'") ("");
	
	/* ITT IN FULL MODEL --------------------------------------------------------- */
	local fulldiff 	= b1full[1,1];
	local fulldiffse = sqrt(V1full[1,1]);
	local tstat1	= abs(`fulldiff'/`fulldiffse');		/* t-stat */

	/* add stars */
	if (inrange(`tstat1',0,1.645)) 			local fulldiffstr 	= ("$" + strtrim("`: di %10.4f `fulldiff''") + "$");
	else if (inrange(`tstat1',1.645,1.96)) 	local fulldiffstr 	= ("$" + strtrim("`: di %10.4f `fulldiff''") + "^{*}$");
	else if (inrange(`tstat1',1.96,2.58)) 	local fulldiffstr 	= ("$" + strtrim("`: di %10.4f `fulldiff''") + "^{**}$");
	else if (`tstat1'>2.58) 				local fulldiffstr 	= ("$" + strtrim("`: di %10.4f `fulldiff''") + "^{***}$");
	/* add SE */
	local fulldiffstr = "`fulldiffstr'" 	+ ("\newline ($" 	+ strtrim("`: di %10.4f `fulldiffse''") + "$)");
	
	/* conditional effect */
	post `medtab' 	("`o'") ("fulleff") ("Conditional Difference") ("`fulldiffstr'") ("");
	
	/* MEDIATOR EFFECTS ----------------------------------------------------- */	
	
	
	local i = 1;
	foreach m of local groups {;		/* mediator loop (as long as mediators have same name as groups) */
	
		/* DECOMPOSITION */
		local decomp 		= decompmat[1,`i'];						/* effect */
		local decompse		= sqrt(decompsemat[`i',`i']);				/* standard error */	
		local tstat			= abs(`decomp'/`decompse');		/* t-stat */
		/* add stars */
		if (inrange(`tstat',0,1.645)) 			local decomp 	= ("$" + strtrim("`: di %10.4f `decomp''") + "$");
		else if (inrange(`tstat',1.645,1.96)) 	local decomp 	= ("$" + strtrim("`: di %10.4f `decomp''") + "^{*}$");
		else if (inrange(`tstat',1.96,2.58)) 	local decomp 	= ("$" + strtrim("`: di %10.4f `decomp''") + "^{**}$");
		else if (`tstat'>2.58) 					local decomp 	= ("$" + strtrim("`: di %10.4f `decomp''") + "^{***}$");
		/* add SE */
		local decomp = "`decomp'" 	+ ("\newline ($" 	+ strtrim("`: di %10.4f `decompse''") + "$)");
		/* fraction mediated */
		local decompf 	= decompmat[1,`i']/`basediff'*100;	/* percent of treat explained */
		local decompf		= ("$" + strtrim("`: di %10.1f `decompf''") + "$");
		
		/* mediated effect */
		post `medtab' 	("`o'") ("med`i'") ("Explained by: \newline `glab`i''") ("`decomp'") ("`decompf'");
		
		local i=`i'+1;
	};
	
	/* TOTAL MEDIATED EFFECT ------------------------------------------------ */
	/* the last value of `i' works since it's the last column of the matrix */
	local totmed 	= decompmat[1,`i'];
	local totmedse 	= sqrt(decompsemat[`i',`i']);
	local tstat2	= abs(`totmed'/`totmedse');		/* t-stat */
		
	/* add stars */
	if (inrange(`tstat2',0,1.645)) 			local totmed 	= ("$" + strtrim("`: di %10.4f `totmed''") + "$");
	else if (inrange(`tstat2',1.645,1.96)) 	local totmed 	= ("$" + strtrim("`: di %10.4f `totmed''") + "^{*}$");
	else if (inrange(`tstat2',1.96,2.58)) 	local totmed 	= ("$" + strtrim("`: di %10.4f `totmed''") + "^{**}$");
	else if (`tstat2'>2.58) 				local totmed 	= ("$" + strtrim("`: di %10.4f `totmed''") + "^{***}$");
	/* add SE */
	local totmed = "`totmed'" 	+ ("\newline ($" 	+ strtrim("`: di %10.4f `totmedse''") + "$)");
		
	/* percentage mediated */
	local totmedf 		= decompmat[1,`i']/`basediff'*100;
	local totmedf		= ("$" + strtrim("`: di %10.1f `totmedf''") + "$");
	
	/* mediated effect */
	post `medtab' 	("`o'") ("totmed") ("Total explained") ("`totmed'") ("`totmedf'");

	postclose `medtab';
	
	di "Done: `o' `c' `g'";
};
};
};


use `mediation_EXT_1_1', clear;
merge 1:1 outcome varname varlab using `mediation_EXT_2_1', nogen;
merge 1:1 outcome varname varlab using `mediation_EXT_1_2', nogen; /* females BCS */
merge 1:1 outcome varname varlab using `mediation_EXT_2_2', nogen; /* females MCS */
tempfile EXTtab;
save `EXTtab';

use `mediation_INT_1_1', clear;
merge 1:1 outcome varname varlab using `mediation_INT_2_1', nogen;
merge 1:1 outcome varname varlab using `mediation_INT_1_2', nogen; /* females BCS */
merge 1:1 outcome varname varlab using `mediation_INT_2_2', nogen; /* females MCS */
tempfile INTtab;
save `INTtab';

use `EXTtab', clear;
append using `INTtab';

saveold "$rdata/gelbres.dta", replace version(12);

gen group=.;
replace group = 1 if outcome == "EXT";		lab def glab 1 "\textbf{Externalising}", modify;
replace group = 2 if outcome == "INT";		lab def glab 2 "\textbf{Internalising}", modify;
lab val group glab;

gen ord=_n;
bysort group: ingap, g(gind);
replace ord=0 if gind==1;
replace gind = group if gind==1;
sort group ord;
levelsof group, local(levs);
di "`levs'";
foreach x of local levs {;
	local grouplab : label group `x';
	local grouplab	= "`:label (group) `x''";
	replace varlab = "\textit{" + "`grouplab'" + "}" if gind==`x';
};
replace varlab = "\hspace{6pt} " + varlab if gind==0;
drop varname group* gind ord outcome;

#delimit ;
/* make latex table */
listtab * using "$tables/med_gelb.tex",
    rstyle(tabular) replace
    head("\begin{tabular}{L{.3\textwidth-2\tabcolsep} C{0.125\textwidth-2\tabcolsep} C{0.125\textwidth-2\tabcolsep} C{0.125\textwidth-2\tabcolsep} C{0.125\textwidth-2\tabcolsep}  C{0.125\textwidth-2\tabcolsep} C{0.125\textwidth-2\tabcolsep} C{0.125\textwidth-2\tabcolsep} C{0.125\textwidth-2\tabcolsep}}" 
    "\toprule" 
	"& \multicolumn{4}{c}{\textbf{Males}} & \multicolumn{4}{c}{\textbf{Females}} \\ \cmidrule(lr){2-5} \cmidrule(lr){6-9}"
	"& \multicolumn{2}{c}{\textbf{BCS}} & \multicolumn{2}{c}{\textbf{MCS}} & \multicolumn{2}{c}{\textbf{BCS}} & \multicolumn{2}{c}{\textbf{MCS}} \\ \cmidrule(lr){2-3} \cmidrule(lr){4-5} \cmidrule(lr){6-7} \cmidrule(lr){8-9}"
	"& Coef. & \% explained & Coef. & \% explained & Coef. & \% explained & Coef. & \% explained \\"
	"& (1) & (2) & (3) & (4) & (6) & (7) & (8) & (9)  \\"
    "\midrule") 
    foot("\bottomrule" "\end{tabular}")
	end("\\ \\[-1.6em]")
;

/* make latex table (EXT only) */
use `EXTtab', clear;
drop varname outcome;
listtab * using "$tables/med_gelb_ext.tex",
    rstyle(tabular) replace
    head("\begin{tabular}{L{.3\textwidth-2\tabcolsep} C{0.15\textwidth-2\tabcolsep} C{0.1\textwidth-2\tabcolsep} C{0.15\textwidth-2\tabcolsep} C{0.1\textwidth-2\tabcolsep}C{0.15\textwidth-2\tabcolsep} C{0.1\textwidth-2\tabcolsep}C{0.15\textwidth-2\tabcolsep} C{0.1\textwidth-2\tabcolsep}}" 
    "\toprule" 
	"& \multicolumn{4}{c}{\textbf{Males}} & \multicolumn{4}{c}{\textbf{Females}} \\ \cmidrule(lr){2-5} \cmidrule(lr){6-9}"
	"& \multicolumn{2}{c}{\textbf{BCS}} & \multicolumn{2}{c}{\textbf{MCS}} & \multicolumn{2}{c}{\textbf{BCS}} & \multicolumn{2}{c}{\textbf{MCS}} \\ \cmidrule(lr){2-3} \cmidrule(lr){4-5} \cmidrule(lr){6-7} \cmidrule(lr){8-9}"
	"& Coef. & \% explained & Coef. & \% explained & Coef. & \% explained & Coef. & \% explained \\"
	"& (1) & (2) & (3) & (4) & (6) & (7) & (8) & (9)  \\"
    "\midrule") 
    foot("\bottomrule" "\end{tabular}")
	end("\\ \\[-1em]")
;

/* make latex table (INT only) */
use `INTtab', clear;
drop varname outcome;
listtab * using "$tables/med_gelb_int.tex",
    rstyle(tabular) replace
    head("\begin{tabular}{L{.3\textwidth-2\tabcolsep} C{0.15\textwidth-2\tabcolsep} C{0.1\textwidth-2\tabcolsep} C{0.15\textwidth-2\tabcolsep} C{0.1\textwidth-2\tabcolsep}C{0.15\textwidth-2\tabcolsep} C{0.1\textwidth-2\tabcolsep}C{0.15\textwidth-2\tabcolsep} C{0.1\textwidth-2\tabcolsep}}" 
    "\toprule" 
	"& \multicolumn{4}{c}{\textbf{Males}} & \multicolumn{4}{c}{\textbf{Females}} \\ \cmidrule(lr){2-5} \cmidrule(lr){6-9}"
	"& \multicolumn{2}{c}{\textbf{BCS}} & \multicolumn{2}{c}{\textbf{MCS}} & \multicolumn{2}{c}{\textbf{BCS}} & \multicolumn{2}{c}{\textbf{MCS}} \\ \cmidrule(lr){2-3} \cmidrule(lr){4-5} \cmidrule(lr){6-7} \cmidrule(lr){8-9}"
	"& Coef. & \% explained & Coef. & \% explained & Coef. & \% explained & Coef. & \% explained \\"
	"& (1) & (2) & (3) & (4) & (6) & (7) & (8) & (9)  \\"
    "\midrule") 
    foot("\bottomrule" "\end{tabular}")
	end("\\ \\[-1em]")
;

