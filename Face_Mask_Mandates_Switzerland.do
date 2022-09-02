//**
* Paper: The impact of face-mask mandates on all-cause mortality in Switzerland: a quasi-experimental study
* Authors: Giacomo De Giorgi, Pascal Geldsetzer, Felix Michalik, M. Maddalena Speziali
* 
* Sources of the data used in this analysis:
* - Data on total deaths and population: Federal Statistical Office website (FSO—Cause of Death Statistics, 2020). 
*   https://www.bfs.admin.ch/bfs/en/home/statistics/health/state-health/mortality-causes-death.html [accessed: April 5, 2021].
* - Data on Statistical Office of the Canton of Zurich. Cause of deaths statisticsSARS-CoV-2 open government data reported by 
*    the Swiss Cantons and the Principality of Liechtenstein. 2021. https://github.com/openZH/covid_19 [accessed: October 4, 2021]
//**

// Prepare data
use "Face_Masks_Mandates_Switzerland.dta",clear

*set panel
xtset, clear 
xtset canton_id date_week

*get log of All-cause mortality per 100,000 for the total, female and male popultation 
g logdeathsrateT = log(deathsrateT)
g logdeathsrateF = log(deathsrateF)
g logdeathsrateM = log(deathsrateM)

*get log IHS tranformed Covid Cases and Deaths per 100,000
g logIHScovidcasesrate = log(covidcasesrate+sqrt((covidcasesrate^2) +1))
g logIHScoviddeathsrate = log(coviddeathsrate+sqrt((coviddeathsrate^2) +1))

*drop observations for all of Switzerland and keep Canton-level observations
drop if canton == "CH"


// Figure 1
preserve
*keep observations for duration of study period
keep if calendar_week <= 40 & year==2020

*obtain mean population size for weighting
bys canton_id: egen mean_pop_T = mean(populationT)
bys canton_id: egen mean_pop_M = mean(populationM)
bys canton_id: egen mean_pop_F = mean(populationF)

** Regression
*Total
eventdd logdeathsrateT i.calendar_week [aw=mean_pop_T], fe cluster(canton_id) timevar(timeToTreat) ci(rarea) level(95) graph_op(title("Total") ytitle("log(all-cause mortality)") xtitle("Weeks since treatment") graphregion(fcolor(white)) xline(-0.5, lwidth(vvthick) lcolor(gs2))) 

*Female
eventdd logdeathsrateF i.calendar_week [aw=mean_pop_F], fe cluster(canton_id) timevar(timeToTreat) ci(rarea) level(95) graph_op(title("Female") ytitle("log(all-cause mortality)") xtitle("Weeks since treatment") graphregion(fcolor(white)) xline(-0.5, lwidth(vvthick) lcolor(gs2)))

*Male
eventdd logdeathsrateM i.calendar_week [aw=mean_pop_M], fe cluster(canton_id) timevar(timeToTreat) ci(rarea) level(95) graph_op(title("Male") ytitle("log(all-cause mortality)") xtitle("Weeks since treatment") graphregion(fcolor(white)) xline(-0.5, lwidth(vvthick) lcolor(gs2)))

restore



// Figure 2
preserve
*keep observations for duration of study period
keep if calendar_week <=40 & year >= 2012

** Regression
*Total
reg logdeathsrateT treat_##post_dyn i.calendar_week i.year i.canton_id [aw=populationT], cluster(canton_id)

*Female
reg logdeathsrateF treat_##post_dyn i.calendar_week i.year i.canton_id [aw=populationF], cluster(canton_id)

*Male
qui reg logdeathsrateM treat_##post_dyn i.calendar_week i.year i.canton_id [aw=populationM], cluster(canton_id)

restore 



// Figure 3
preserve
*keep observations for duration of study period
keep if calendar_week <=40

** Regression
*Covid Cases
reg logIHScovidcasesrate treat_##post_dyn [aw=populationT], cluster(canton_id) 

*Covid Deaths
reg logIHScoviddeathsrate treat_##post_dyn i.calendar_week i.year i.canton_id [aw=populationT], cluster(canton_id)

restore



// Table 1 
preserve
*keep observations for duration of study period
keep if year>=2012 & calendar_week <=40

** Regression
local first_loop yes
local second_loop yes
local third_loop yes
foreach s in M F T {
 regress logdeathsrate`s' treat_ post_policy Diff [aw=population`s'], cluster(canton_id)
 quietly summ logdeathsrate`s'   
  if "`first_loop'"=="yes"{
    outreg2 using "table1", replace ctitle(`s') addtext (Year FE, NO, Canton FE, NO, Week FE,NO) addstat(Mean, r(mean)) label keep(treat_ post_policy Diff ) word dec(3) stats(coef se )
    local first_loop no
  }
    else {
    outreg2 using "table1", append ctitle(`s') addtext (Year FE, NO, Canton FE, NO, Week FE,NO) addstat(Mean, r(mean)) label keep(treat_ post_policy Diff ) word dec(3) stats(coef se )
}
 qui regress logdeathsrate`s' treat_ post_policy Diff i.canton_id [aw=population`s'], cluster(canton_id)
  quietly summ logdeathsrate`s'   
  if "`second_loop'"=="yes"{
    outreg2 using "table1", append ctitle(`s') addtext (Year FE, NO, Canton FE, YES, Week FE, NO) addstat(Mean, r(mean)) label keep(treat_ post_policy Diff ) word dec(3) stats(coef se )
    local second_loop no
  }
    else {
    outreg2 using "table1", append ctitle(`s') addtext (Year FE, NO, Canton FE, YES, Week FE,NO) addstat(Mean, r(mean)) label keep(treat_ post_policy Diff ) word dec(3) stats(coef se )
}
  qui  regress logdeathsrate`s' treat_ post_policy Diff i.canton_id i.calendar_week i.year [aw=population`s'], cluster(canton_id)
  quietly summ logdeathsrate`s'   
  if "`third_loop'"=="yes"{
    outreg2 using "table1", append ctitle(`s') addtext (Year FE, YES, Canton FE, YES, Week FE, YES) addstat(Mean, r(mean)) label keep(treat_ post_policy Diff ) word dec(3) stats(coef se )
    local third_loop no
  }
  else {
    outreg2 using "table1", append ctitle(`s') addtext (Year FE, YES, Canton FE, YES, Week FE,YES) addstat(Mean, r(mean)) label keep(treat_ post_policy Diff ) word dec(3) stats(coef se )
  }
}

restore