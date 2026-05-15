*** Initialize and set paths
clear all
set more off
*ssc install distinct
*ssc install ftools, replace
*ssc install reghdfe, replace
*ssc install estout, replace
ftools, compile
*reghdfe, compile
global main_path "C:\Users\Public\Documents\Nadia_Haytham"
global data "$main_path\Data"
global output "$main_path\Outputs\Stata_01"

*-----------------------------------------------------------------------*
*  0 : Importing and merging data
*-----------------------------------------------------------------------*

*** Part 1 - Exporting 2021 EEC
use "$data\eec_2021_salaried.dta", clear
append using "$data\eec_2022_salaried.dta"
append using "$data\eec_2023_salaried.dta"
append using "$data\eec_2024_salaried.dta"

drop if missing(DEPCOM) // 0 ids with no depcom, we drop them

* Variable Labels
label variable sick_leave "Maladie pendant la semaine de ref (précédent l'échange)"
label variable sick_leave_spell "Duree en jour au moment de la déclaration"
label variable disabled_procedure "En cours de reconnaissance handicap"

* TO DO :
* 1: import sentinelles data
* 2: keep year 2021-2024
* 3: create Region variable 

* Create REG variable
gen DEP = substr(DEPCOM,1,2)
gen REG = .

** Mapping DEP - REG
replace REG = 84 if inlist(DEP, "01", "03","07","15","26","38") | inlist(DEP, "42","43","63","69","73","74")
replace REG = 32 if inlist(DEP,"02","59","60","62","80")
replace REG = 93 if inlist(DEP, "04","05","06","13","83","84")
replace REG = 44 if inlist(DEP, "08", "10","51","52","54") | inlist(DEP,"55","57","67","68","88")
replace REG = 76 if inlist(DEP, "09","11","12","30","31","32","34")| inlist(DEP,"46","48","65","66","81","82")
replace REG = 28 if inlist(DEP, "14","27","50","61","76")
replace REG = 24 if inlist(DEP, "18","28","36","37","41","45")
replace REG = 27 if inlist(DEP, "21","25","39","58","70","71","89","90")
replace REG = 53 if inlist(DEP, "22","29","35","56")
replace REG = 75 if inlist(DEP, "16","17","19","23","24","33") | inlist(DEP,"40","47","64","79","86","87")
replace REG = 52 if inlist(DEP, "44", "49", "53", "72", "85")
replace REG = 11 if inlist(DEP, "75", "77","78","91","92","93","94","95")
replace REG = 94 if inlist(DEP, "2A","2B")

* We focus only on Metropolitan France without Corsica
drop if REG == 94 | DEP == "97"
** Mapping time : week (sentinelle) - week_ref (eec)
gen yr = year(week_ref)
gen wk = week(week_ref)
gen year_week = string(yr) +string(wk,"%02.0f")
destring year_week, replace
sort year_week
rename year_week week

* Save temporary file
tempfile eec_21_24
save `eec_21_24'

*** Part 2 - exporting and merging Sentinelle data
use "$data\sentinelles\inc_2020_2026.dta", clear
keep if week > 202053 & week < 202501 // keep only 2021 observations
rename geo_insee REG
drop if REG == 94 // drop Corsica

* check if merging possible + merging
distinct week REG, joint // good
merge 1:m week REG using `eec_21_24'
assert _merge == 3
drop _merge

*-----------------------------------------------------------------------*
*  1 : Sanity Checks
*-----------------------------------------------------------------------*

*** Part 1 - distinct observations

distinct id_panel
count if missing(week) // 0
count if missing(migrant) // 0
count if missing(sick_leave) // 0

* Observation per individual :
preserve
	bysort id_panel : gen n_obs = _N
	by id_panel : keep if _n == 1
	tab n_obs migrant, col
	sum n_obs, detail
restore

* Raw leave rate per migration status
preserve
keep if yr ==2021
tab migrant sick_leave, row
/*
           | Maladie pendant la
           |   semaine de ref
           |   (précédent
           |    l'échange)
   migrant |         0          1 |     Total
-----------+----------------------+----------
         0 |   349,539     15,252 |   364,791
           |     95.82       4.18 |    100.00
-----------+----------------------+----------
         1 |    17,455        635 |    18,090
           |     96.49       3.51 |    100.00
-----------+----------------------+----------
     Total |   366,994     15,887 |   382,881
           |     95.85       4.15 |    100.00
*/
preserve
	keep if sick_leave == 1
	mean sick_leave_spell, over(migrant)
restore
/*

Mean estimation                             Number of obs = 15,887

--------------------------------------------------------------
                     |       Mean   Std. err.   [95% conf. interval]
---------------------+--------------------------------------------
c.sick_leave_spell@migrant |
                   0 |   4.602052   .0070662    4.588202    4.615903
                   1 |   4.466929   .0411226    4.386324    4.547534
--------------------------------------------------------------
*/
restore 
*-----------------------------------------------------------------------*
*  2 : Flu Schock
*-----------------------------------------------------------------------*

*** Part 1 : Identify peak weeks
preserve
	collapse (mean) nat_flu = inc100, by(REG week)
	sum nat_flu, detail
	gen peak = nat_flu > r(p95)
	list week nat_flu, sepby(peak) noobs
	save "$output\natflu_weekly_2021_2024.dta", replace
restore

*-----------------------------------------------------------------------*
*  3 : Descriptive evidence
*-----------------------------------------------------------------------*

merge m:1 week REG using "$output\natflu_weekly_2021_2024.dta", nogen keep(match master)
table(migrant) (peak), stat(mean sick_leave) stat(freq) nformat(%6.4f) // balanced peak shocks !

* Simple DDD
preserve
	collapse (mean) sick_leave, by(migrant peak)
	list, noobs sep(0)
	reshape wide sick_leave, i(migrant) j(peak)
	gen diff_peak = sick_leave1 - sick_leave0
	list, noobs sep(0)
restore

/*
 +----------------------------------------------+
 | migrant   sick_le~0   sick_le~1    diff_p~k |
 |----------------------------------------------|
 |       0   .04190569   .04597457    .0040689 |
 |       1   .03540162   .03858521    .0031836 |
 +----------------------------------------------+
*/

* Simple DDD x age = 30-50
preserve
	keep if age_group3 == 2
	collapse (mean) sick_leave, by(migrant peak)
	list, noobs sep(0)
	reshape wide sick_leave, i(migrant) j(peak)
	gen diff_peak = sick_leave1 - sick_leave0
	list, noobs sep(0)
restore

/*
 +----------------------------------------------+
 | migrant   sick_le~0   sick_le~1    diff_p~k |
 |----------------------------------------------|
 |       0   .03529324   .04317697    .0078837 |
 |       1   .02954851    .0317757    .0022272 |
 +----------------------------------------------+
*/

*-----------------------------------------------------------------------*
*  4 : Regressions Setup
*-----------------------------------------------------------------------*

*** Setup

* Standardized flu
sum inc100
gen flu_std = inc100/100 // per 100 cases / 100k
gen epidemic = (inc100 > 150)
gen mig_epidemic = migrant*epidemic

* Interactions
gen mig_flu = migrant*flu_std
gen loweduc = (higher_education == 0)
gen epi_low = loweduc * epidemic
gen mig_low = migrant*loweduc
gen mig_epi_low = migrant * epidemic * loweduc
gen mig_short = migrant*short_term
gen epi_short = epidemic*short_term
gen mig_epi_short = migrant*epidemic*short_term

* FE groups
egen reg_grp = group(REG migrant) // FE : region x group
egen grp_yrwk = group(migrant week) // FE : group x week
egen reg_yrwk = group(REG week) // FE : region x year-week

* Age squared (if using continous age)
gen age2 = age^2

*-----------------------------------------------------------------------*
*  5 : T1 - Sequential controls
*-----------------------------------------------------------------------*

*1/ Continous flu shock

*** Spec 0 : raw gap, no controls
reghdfe sick_leave migrant flu_std mig_flu, absorb(reg_grp grp_yrwk) cluster(reg_yrwk)
eststo raw

*** Spec 1 : + demographics
reghdfe sick_leave migrant flu_std mig_flu female age age2 single_parent kids nb_kids, ///
	absorb(reg_grp grp_yrwk) cluster(reg_yrwk)
eststo demo

*** Spec 2 : + Human capital
reghdfe sick_leave migrant flu_std mig_flu ///
	female age age2 single_parent kids nb_kids higher_education no_diploma disabled, ///
	absorb(reg_grp grp_yrwk) cluster(reg_yrwk)
eststo hcap

*** Spec 3 : + Job characteristics
reghdfe sick_leave migrant flu_std mig_flu ///
	female age age2 single_parent kids nb_kids higher_education no_diploma disabled ///
	parttime short_term unskilled, ///
	absorb(reg_grp grp_yrwk) cluster(reg_yrwk)
eststo full

esttab raw demo hcap full using "$output\table1_continousfluvar.tex", ///
	keep(mig_flu flu_std migrant) ///
	order(mig_flu flu_std migrant) ///
	b(4) se(4) star(* 0.10 ** 0.05 *** 0.01) ///
	mtitles("Raw" "Demographics" "Human Capital" "Job Controls") ///
	label ///
	stats(N r2_a, fmt(0 3) labels("Observations" "Adj. R\$^2\$")) ///
	addnotes("All specifications include region x group and group x year-week FE." ///
		"Standard errors clustered at region x year-week level.") ///
	replace

*2/ Binary flu shock

*** Spec 0 : raw gap, no controls
reghdfe sick_leave migrant epidemic mig_epidemic, absorb(reg_grp grp_yrwk) cluster(reg_yrwk)
eststo raw_b

*** Spec 1 : + demographics
reghdfe sick_leave migrant epidemic mig_epidemic female age age2 single_parent kids nb_kids, ///
	absorb(reg_grp grp_yrwk) cluster(reg_yrwk)
eststo demo_b

*** Spec 2 : + Human capital
reghdfe sick_leave migrant epidemic mig_epidemic ///
	female age age2 single_parent kids nb_kids higher_education no_diploma disabled, ///
	absorb(reg_grp grp_yrwk) cluster(reg_yrwk)
eststo hcap_b

*** Spec 3 : + Job characteristics
reghdfe sick_leave migrant epidemic mig_epidemic ///
	female age age2 single_parent kids nb_kids higher_education no_diploma disabled ///
	parttime short_term unskilled, ///
	absorb(reg_grp grp_yrwk) cluster(reg_yrwk)
eststo full_b

esttab raw_b demo_b hcap_b full_b using "$output\table1_binaryfluvar.tex", ///
	keep(mig_flu flu_std migrant) ///
	order(mig_flu flu_std migrant) ///
	b(4) se(4) star(* 0.10 ** 0.05 *** 0.01) ///
	mtitles("Raw" "Demographics" "Human Capital" "Job Controls") ///
	label ///
	stats(N r2_a, fmt(0 3) labels("Observations" "Adj. R\$^2\$")) ///
	addnotes("All specifications include region x group and group x year-week FE." ///
		"Standard errors clustered at region x year-week level.") ///
	replace

*3/ Binary flu shock with sick leave spell

*** Spec 0 : raw gap, no controls
reghdfe sick_leave_spell migrant epidemic mig_epidemic, absorb(reg_grp grp_yrwk) cluster(reg_yrwk)
eststo raw_b_sp

*** Spec 1 : + demographics
reghdfe sick_leave_spell migrant epidemic mig_epidemic female age age2 single_parent kids nb_kids, ///
	absorb(reg_grp grp_yrwk) cluster(reg_yrwk)
eststo demo_b_sp

*** Spec 2 : + Human capital
reghdfe sick_leave_spell migrant epidemic mig_epidemic ///
	female age age2 single_parent kids nb_kids higher_education no_diploma disabled, ///
	absorb(reg_grp grp_yrwk) cluster(reg_yrwk)
eststo hcap_b_sp

*** Spec 3 : + Job characteristics
reghdfe sick_leave_spell migrant epidemic mig_epidemic ///
	female age age2 single_parent kids nb_kids higher_education no_diploma disabled ///
	parttime short_term unskilled, ///
	absorb(reg_grp grp_yrwk) cluster(reg_yrwk)
eststo full_b_sp

esttab raw_b_sp demo_b_sp hcap_b_sp full_b_sp using "$output\table1_binaryfluvar_spell.tex", ///
	keep(mig_flu flu_std migrant) ///
	order(mig_flu flu_std migrant) ///
	b(4) se(4) star(* 0.10 ** 0.05 *** 0.01) ///
	mtitles("Raw" "Demographics" "Human Capital" "Job Controls") ///
	label ///
	stats(N r2_a, fmt(0 3) labels("Observations" "Adj. R\$^2\$")) ///
	addnotes("All specifications include region x group and group x year-week FE." ///
		"Standard errors clustered at region x year-week level.") ///
	replace

*4/ Binary flu shock with sick leave spell only on short sick leaves

*** Spec 3 : + Job characteristics
preserve
drop if sick_leave_spell > 2
reghdfe sick_leave_spell migrant epidemic mig_epidemic ///
	female age age2 single_parent kids nb_kids higher_education no_diploma disabled ///
	parttime short_term unskilled, ///
	absorb(reg_grp grp_yrwk) cluster(reg_yrwk)
eststo full_b_sp_short
restore

esttab full_b_sp_short using "$output\table1_binaryfluvar_spell_short.tex", ///
	keep(mig_flu flu_std migrant) ///
	order(mig_flu flu_std migrant) ///
	b(4) se(4) star(* 0.10 ** 0.05 *** 0.01) ///
	mtitles("Raw" "Demographics" "Human Capital" "Job Controls") ///
	label ///
	stats(N r2_a, fmt(0 3) labels("Observations" "Adj. R\$^2\$")) ///
	addnotes("All specifications include region x group and group x year-week FE." ///
		"Standard errors clustered at region x year-week level.") ///
	replace

*-----------------------------------------------------------------------*
*  4 : Regressions by age group
*-----------------------------------------------------------------------*

levelsof age_group3, local(ages)
foreach a of local ages {
	reghdfe sick_leave migrant epidemic mig_epidemic ///
		female age age2 single_parent kids nb_kids higher_education no_diploma disabled ///
		parttime short_term unskilled ///
		if age_group3 == `a', ///
		absorb(reg_grp grp_yrwk) cluster(reg_yrwk)
	eststo age_`a'
}

esttab age_*, keep(mig_epidemic) ///
	be(4) se(4) star(* 0.10 ** 0.05 *** 0.01) ///
	label stats(N, fmt(0)) ///
	title ("sick leave difference by age group")

*-----------------------------------------------------------------------*
*  5 : Triple intercation : Migrant x Flu x
*  short_term and Migrant x Flu x LowEduc
*-----------------------------------------------------------------------*

* Full Sample, full controls
reghdfe sick_leave_spell migrant short_term epidemic epi_short mig_short mig_epidemic mig_epi_short ///
	female age age2 single_parent kids nb_kids higher_education no_diploma disabled ///
	unskilled, ///
	absorb(reg_grp grp_yrwk) cluster(reg_yrwk)
eststo triple_full_short_term

* Full Sample, full controls
reghdfe sick_leave_spell migrant loweduc epidemic epi_low mig_low mig_epidemic mig_epi_low ///
	female age age2 single_parent kids nb_kids disabled ///
	parttime short_term unskilled, ///
	absorb(reg_grp grp_yrwk) cluster(reg_yrwk)
eststo triple_full_low_educ

*-----------------------------------------------------------------------*
*  6 : Triple intercation : Migrant x Flu x LowEduc + sick_leave_Spell
*-----------------------------------------------------------------------*

* Define two outcomes depending the sick leave spell
gen short_leave = (sick_leave_spell > 0 & sick_leave_spell <=3)
gen long_leave = (sick_leave_spell >= 4 & sick_leave_spell <=7)

reghdfe short_leave migrant loweduc epidemic epi_low mig_low mig_epidemic mig_epi_low ///
	female age age2 single_parent kids nb_kids disabled ///
	parttime short_term unskilled, ///
	absorb(reg_grp grp_yrwk) cluster(reg_yrwk)
eststo triple_full

reghdfe long_leave migrant loweduc epidemic epi_low mig_low mig_epidemic mig_epi_low ///
	female age age2 single_parent kids nb_kids disabled ///
	parttime short_term unskilled, ///
	absorb(reg_grp grp_yrwk) cluster(reg_yrwk)
eststo triple_full
