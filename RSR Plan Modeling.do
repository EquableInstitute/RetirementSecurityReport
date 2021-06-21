***RSR Plan Modeling***
***Author: Jon Moody***
***Date: March 5, 2021***
***Last Modified: May 24, 2021***

*This script is intended to generate the benefit values (NPVs), annuitized values, and the replacement rates*
*Note that we will calculate the benefit adequacy thresholds in a separate script*
*Similarly, the actual RSR scoring will be a separate script*

clear
set more off
cd "C:\Users\jonwm\Dropbox (Equable)\Equable Core\Research_Equable Projects\Project_Retirement Security Report\Benefit Models\Scripting\Stata"

*Final Average Salary Defined Benefit Plans*

*We begin by importing the data from csvs, convert them into Stata format (for easier merging), and then we can go from there*
*Importing and converting to Stata format*
import delimited using Salary_Scales_Final.csv, varnames(1)
save Salary_Scales.dta, replace
clear
import delimited using PUB-2010-Mortality.csv, varnames(1)
save Mortality.dta, replace
clear
import delimited using Starting_Salaries_Final.csv, varnames(1)
save Starting_Salaries.dta, replace
clear
import delimited using Features_Final.csv, varnames(1)
save Features.dta, replace

*It's easiest to start with the salary scales, as there are more rows there than others*
*The data have all been configured that we can join them based on the modified equableclassid*
clear
use Features.dta
*Then we reduce these data to only what we will need for the salary accruals*
keep equableclassid fas_years 
*We save this as a separate file to merge into the salary scales*
save salaryfeatures.dta, replace

*Next we go to the salary scales and merge in the starting salaries and salary features*
clear
use Salary_Scales.dta
joinby equableclassid using salaryfeatures.dta, unmatched(both)
drop _merge
joinby equableclassid using Starting_Salaries.dta, unmatched(both)
drop _merge

*For the sake of the code being easier, let's relabel a few variables*
rename yearsofservice YOS
rename startingsalary_25 startsal25
rename startingsalary_40 startsal40

*We need to panel set the data 
xtset equableclassid YOS

*From here the salary accruals are easy to calculate within each class*
gen salary25 = .
replace salary25 = startsal25 if YOS==0
replace salary25 = L1.salary25*(1+L1.annualrate) if salary25==.
gen salary40 = .
replace salary40 = startsal40 if YOS==0
replace salary40 = L1.salary40*(1+L1.annualrate) if salary40==.

*We would normally calculate the final average salaries here, but we need the maximum pensionable salary variable that's in the features data*

*At this point, it makes sense to save this as we're ready to address the mortality data and may need to revert to this point*
save Salaries_Complete.dta, replace

*We want to load the mortality data and make sure they're sorted by age*
clear
use Mortality.dta
sort mortality_category age

*First up we want to calculate survival probability and life expectancy*
*Now we need to build out the mortality related elements*
encode mortality_category, gen(mortnum)
xtset mortnum age

*Next step is to translate mortality rate into survival rate*
gen survival = .
replace survival=1 if age==20
replace survival= L1.survival*(1-L1.mortality) if survival==.

*So that worked fine*
*Next is life expectancy*
forvalues i = 20(1)120 {
by mortnum: egen reducedsurvival`i' = total(survival) if age>=`i' 
} 
gen totalsurvival = .
forvalues i = 20(1)120 {
replace totalsurvival = reducedsurvival`i' if age==`i'
}
drop reducedsurvival*
gen lifeexpect = totalsurvival/survival

*These data now need some elements from the features data*
save Mortality_Temp.dta, replace

clear
use Features.dta
keep equableclassid class_name mortality_category discrate fas_compoundcola fas_colarate

*Now we bring in the mortality elements thus far*
joinby mortality_category using Mortality_Temp.dta, unmatched(both)
drop _merge
sort equableclassid age

*From here we want to tackle the discounted survival probability*
gen discsurvive = survival/(1+discrate)^(age-20)

*Next the matrix of survivals up to age 120*
gen survivecola = .
replace survivecola = discsurvive*((1+fas_colarate)^(age-20)) if fas_compoundcola==1
replace survivecola = discsurvive*(1+(fas_colarate*(age-20))) if fas_compoundcola==0
format survivecola %32.0g

*Then we calculate the annuity factor*
*We need to also generate a variable that's a sum of survival for all ages prior to the given age*
gen survivecolarevsum = .
forvalues i = 20(1)120 {
gen survivecoladisc`i' = .
replace survivecoladisc`i' = survivecola if age>=`i'
by equableclassid: egen totalsurvivecolasum`i' = total(survivecoladisc`i')
replace survivecolarevsum = totalsurvivecolasum`i' if age==`i'
}
drop survivecoladisc20 - totalsurvivecolasum120

*Then we need the reverse cumulative sum of the discounted survival*
gen discsurviverevsum = .
forvalues i = 20(1)120 {
gen discsurvivevalue`i' = .
replace discsurvivevalue`i' = discsurvive if age>=`i'
by equableclassid: egen totaldiscsurvivesum`i' = total(discsurvivevalue`i')
replace discsurviverevsum = totaldiscsurvivesum`i' if age==`i'
}
drop discsurvivevalue20 - totaldiscsurvivesum120

*Now we can reduce this into the adjusted annuity variable*
gen adjannuity = .
replace adjannuity = survivecolarevsum/survivecola if fas_compoundcola==1
replace adjannuity = (survivecolarevsum-discsurviverevsum*fas_colarate*(age-20))/discsurvive if fas_compoundcola==0

*This is the annuity factor we need*
*All we need from these data are the annuity factors by age*
keep age equableclassid survival discsurvive survivecola adjannuity
save Annuity_Complete.dta, replace

*These are ready to now be assembled with the features data*
clear
use Salaries_Complete.dta

*We need to build an age variable to allow the files to merge*
sort equableclassid YOS
by equableclassid: egen age = seq(), f(25) t(70)
*We also need YOS for an age 40 entrant age group - we will use this later*
by equableclassid: egen YOS40 = seq(), f(-15) t(30)
replace YOS40 = . if YOS40<0

*Next we can pull in the adjusted annuity factor*
joinby equableclassid age using Annuity_Complete.dta, unmatched(both)

*Note this will produce a ton of values with missing data, as our annuity factors technically run from ages 20-120, while the other data are only from 25-70*
*This isn't an issue, as we will clean it up once the features are pulled in*
drop _merge

*Lastly, we pull in the features data*
joinby equableclassid using Features.dta, unmatched(both)
drop _merge

*Before we go any further, let's clean up the data and get everything up to speed*
drop if age<25
drop if age>70
drop if equableclassid==.
xtset equableclassid age
rename salary40 sal40temp
gen salary40 = .
replace salary40 = L15.sal40temp
drop sal40temp

*Saving these data just so that we don't have to run everything up to here if we want/need to backtrack to this point*
save Merged.dta, replace

*Now that we're merged, we can build final average salaries*
*So let's cap the salaries using the FAS_Max_Pensionable_Salary*
*It will probably be better to build new variables to address this*
rename salary25 rawsalary25
rename salary40 rawsalary40
gen salary25 = rawsalary25
replace salary25 = fas_max_pensionable_salary if salary25>fas_max_pensionable_salary & salary25!=.
gen salary40 = rawsalary40
replace salary40 = fas_max_pensionable_salary if salary40>fas_max_pensionable_salary & salary40!=.

*The next step is to calculate the final average salaries using the salary accruals*
*This involves calculating moving averages, so we will generate them for 25-year-old and 40-year-old entrants separately*
*Note that the FAS years are not all whole numbers in the raw data*
*To address this, we simplify by moving them up to whole integer values*
replace fas_years=3 if fas_years==2.5
replace fas_years=4 if fas_years==3.5
foreach i in 1 2 3 4 5 6 8 {
tssmooth ma fas`i' = salary25, window(`i',0,0)
replace fas`i'=. if YOS<fas_years
}
*Now that we have these different averages, we want to then collapse down to just one FAS value for 25 year old entrants*
gen fas25 = .
foreach i in 1 2 3 4 5 6 8 {
replace fas25 = fas`i' if fas_years==`i'
}
drop fas1 - fas8

*Then we repeat the process for 40-year-old entrants*
foreach i in 1 2 3 4 5 6 8 {
tssmooth ma fas`i' = salary40, window(`i',0,0)
replace fas`i'=. if YOS40<fas_years
}
gen fas40 = .
foreach i in 1 2 3 4 5 6 8 {
replace fas40 = fas`i' if fas_years==`i'
}
drop fas1 - fas8

*At this point, now we can step into the modeling for the FAS DB plans*
*Contributions*
*First age 25 entrants*
*Employee*
gen eecont25 = fas_eecont2 * salary25 if YOS<fas_eecont2_yos
replace eecont25 = fas_eecont1 * salary25 if YOS<fas_eecont1_yos
gen adjeecont25 = .
replace adjeecont25 = 0 if YOS==0
replace adjeecont25 = (L1.adjeecont25*(1+fas_creditingrate))+L1.eecont25 if YOS>0
*Employer*
gen empcont25 = fas_emcont * salary25
gen adjempcont25 = .
replace adjempcont25 = 0 if YOS==0
replace adjempcont25 = (L1.adjempcont25*(1+fas_creditingrate))+L1.empcont25 if YOS>0
*Present values of contributions*
gen pveecont25 = adjeecont25/((1+inflation)^(age-25))
gen pvempcont25 = adjempcont25/((1+inflation)^(age-25))
*Then age 40 entrants*
*Employee*
gen eecont40 = fas_eecont2 * salary40 if YOS40<fas_eecont2_yos
replace eecont40 = fas_eecont1 * salary40 if YOS40<fas_eecont1_yos
gen adjeecont40 = .
replace adjeecont40 = 0 if YOS40==0
replace adjeecont40 = (L1.adjeecont40*(1+fas_creditingrate))+L1.eecont40 if YOS40>0
*Employer*
gen empcont40 = fas_emcont * salary40
gen adjempcont40 = .
replace adjempcont40 = 0 if YOS40==0
replace adjempcont40 = (L1.adjempcont40*(1+fas_creditingrate))+L1.empcont40 if YOS40>0
*Present values of contributions*
gen pveecont40 = adjeecont40/((1+inflation)^(age-40))
gen pvempcont40 = adjempcont40/((1+inflation)^(age-40))

*Now that we've built out the contributions, we can turn to the actual pension wealth*
*Replacement Rate and Pension Wealth*
*Simple Replacement Rate*
*Note that we have to handle multiple multipliers - both retroactive and additive*
*Age 25 Entrant*
gen replrate25 = .
replace replrate25 = YOS*fas_multiplier5 if YOS<fas_multiplier5_yos & fas_additive_multipliers==0
replace replrate25 = YOS*fas_multiplier4 if YOS<fas_multiplier4_yos & fas_additive_multipliers==0
replace replrate25 = YOS*fas_multiplier3 if YOS<fas_multiplier3_yos & fas_additive_multipliers==0
replace replrate25 = YOS*fas_multiplier2 if YOS<fas_multiplier2_yos & fas_additive_multipliers==0
replace replrate25 = YOS*fas_multiplier1 if YOS<fas_multiplier1_yos & fas_additive_multipliers==0
replace replrate25 = YOS*fas_multiplier1 if YOS<fas_multiplier1_yos & fas_additive_multipliers==1
replace replrate25 = (fas_multiplier1*(fas_multiplier1_yos-1)) + (fas_multiplier2*(YOS -(fas_multiplier1_yos-1))) if YOS>=fas_multiplier1_yos & YOS<fas_multiplier2_yos & fas_additive_multipliers==1
replace replrate25 = (fas_multiplier1*(fas_multiplier1_yos-1)) + (fas_multiplier2*(fas_multiplier2_yos-fas_multiplier1_yos)) + (fas_multiplier3*(YOS-(fas_multiplier2_yos-1))) if YOS>=fas_multiplier2_yos & YOS<fas_multiplier3_yos & fas_additive_multipliers==1
replace replrate25 = (fas_multiplier1*(fas_multiplier1_yos-1)) + (fas_multiplier2*(fas_multiplier2_yos-fas_multiplier1_yos)) + (fas_multiplier3*(fas_multiplier3_yos-fas_multiplier2_yos)) + (fas_multiplier4*(YOS-(fas_multiplier3_yos-1))) if YOS>=fas_multiplier3_yos & YOS<fas_multiplier4_yos & fas_additive_multipliers==1
replace replrate25 = (fas_multiplier1*(fas_multiplier1_yos-1)) + (fas_multiplier2*(fas_multiplier2_yos-fas_multiplier1_yos)) + (fas_multiplier3*(fas_multiplier3_yos-fas_multiplier2_yos)) + (fas_multiplier4*(fas_multiplier4_yos-fas_multiplier3_yos)) + (fas_multiplier5*(YOS-(fas_multiplier4_yos-1))) if YOS>=fas_multiplier4_yos & YOS<fas_multiplier5_yos & fas_additive_multipliers==1
replace replrate25 = 0 if YOS<fas_vest
*Age 40 Entrant*
gen replrate40 = .
replace replrate40 = YOS40*fas_multiplier5 if YOS40<fas_multiplier5_yos 
replace replrate40 = YOS40*fas_multiplier4 if YOS40<fas_multiplier4_yos
replace replrate40 = YOS40*fas_multiplier3 if YOS40<fas_multiplier3_yos
replace replrate40 = YOS40*fas_multiplier2 if YOS40<fas_multiplier2_yos
replace replrate40 = YOS40*fas_multiplier1 if YOS40<fas_multiplier1_yos
replace replrate40 = (fas_multiplier1*(fas_multiplier1_yos-1)) + (fas_multiplier2*(YOS40 -(fas_multiplier1_yos-1))) if YOS40>=fas_multiplier1_yos & YOS40<fas_multiplier2_yos & fas_additive_multipliers==1
replace replrate40 = (fas_multiplier1*(fas_multiplier1_yos-1)) + (fas_multiplier2*(fas_multiplier2_yos-fas_multiplier1_yos)) + (fas_multiplier3*(YOS40-(fas_multiplier2_yos-1))) if YOS40>=fas_multiplier2_yos & YOS40<fas_multiplier3_yos & fas_additive_multipliers==1
replace replrate40 = (fas_multiplier1*(fas_multiplier1_yos-1)) + (fas_multiplier2*(fas_multiplier2_yos-fas_multiplier1_yos)) + (fas_multiplier3*(fas_multiplier3_yos-fas_multiplier2_yos)) + (fas_multiplier4*(YOS40-(fas_multiplier3_yos-1))) if YOS40>=fas_multiplier3_yos & YOS40<fas_multiplier4_yos & fas_additive_multipliers==1
replace replrate40 = (fas_multiplier1*(fas_multiplier1_yos-1)) + (fas_multiplier2*(fas_multiplier2_yos-fas_multiplier1_yos)) + (fas_multiplier3*(fas_multiplier3_yos-fas_multiplier2_yos)) + (fas_multiplier4*(fas_multiplier4_yos-fas_multiplier3_yos)) + (fas_multiplier5*(YOS40-(fas_multiplier4_yos-1))) if YOS40>=fas_multiplier4_yos & YOS40<fas_multiplier5_yos & fas_additive_multipliers==1
replace replrate40 = 0 if YOS40<fas_vest

*Note that the multipliers are unique for NY SLRS ERS General and NY STRS*
replace replrate25 = . if equableclassid==33010106
replace replrate25 = . if equableclassid==33020106
replace replrate40 = . if equableclassid==33010106
replace replrate40 = . if equableclassid==33020106

*First NY SLRS ERS General*
replace replrate25 = YOS*fas_multiplier1 if YOS<fas_multiplier1_yos-1 & equableclassid==33010106
replace replrate25 = (fas_multiplier2*YOS) if YOS==(fas_multiplier1_yos-1) & equableclassid==33010106
replace replrate25 = YOS*fas_multiplier2 + (fas_multiplier3*(YOS -(fas_multiplier1_yos-1))) if YOS>(fas_multiplier1_yos-1) & equableclassid==33010106
replace replrate40 = YOS40*fas_multiplier1 if YOS40<fas_multiplier1_yos-1 & equableclassid==33010106
replace replrate40 = (fas_multiplier2*YOS40) if YOS40==(fas_multiplier1_yos-1) & equableclassid==33010106
replace replrate40 = YOS40*fas_multiplier2 + (fas_multiplier3*(YOS40 -(fas_multiplier1_yos-1))) if YOS40>(fas_multiplier1_yos-1) & equableclassid==33010106
replace replrate25 = 0 if YOS<fas_vest & equableclassid==33010106
replace replrate40 = 0 if YOS40<fas_vest & equableclassid==33010106

*And then for NY STRS*
replace replrate25 = YOS*fas_multiplier1 if YOS<fas_multiplier1_yos-1 & equableclassid==33020106
replace replrate25 = (fas_multiplier2*YOS) if YOS==(fas_multiplier1_yos-1) & equableclassid==33020106
replace replrate25 = YOS*fas_multiplier2 + (fas_multiplier3*(YOS -(fas_multiplier1_yos-1))) if YOS>(fas_multiplier1_yos-1) & equableclassid==33020106
replace replrate40 = YOS40*fas_multiplier1 if YOS40<fas_multiplier1_yos-1 & equableclassid==33020106
replace replrate40 = (fas_multiplier2*YOS40) if YOS40==(fas_multiplier1_yos-1) & equableclassid==33020106
replace replrate40 = YOS40*fas_multiplier2 + (fas_multiplier3*(YOS40 -(fas_multiplier1_yos-1))) if YOS40>(fas_multiplier1_yos-1) & equableclassid==33020106
replace replrate25 = 0 if YOS<fas_vest & equableclassid==33020106
replace replrate40 = 0 if YOS40<fas_vest & equableclassid==33020106

********************************************************
*Pension Wealth (Year 1)
*Age 25 Entrant*
gen pwealth25 = .
replace pwealth25 = replrate25*fas25
replace pwealth25 = 0 if YOS<fas_vest
*Age 40 Entrant*
gen pwealth40 = .
replace pwealth40 = replrate40*fas40
replace pwealth40 = 0 if YOS40<fas_vest

*The Final Adjusted Annuity Rate*
*Age 25 Entrant*
gen survivetoretire25 = .
replace survivetoretire25 = survival if age==fas_retire25
by equableclassid: egen temp = max(survivetoretire25)
replace survivetoretire25=temp
drop temp
gen yearstoretire25 = fas_retire25-age
gen annuityatretiretemp = .
replace annuityatretiretemp = adjannuity if age==fas_retire25
by equableclassid: egen annuityatretire25 = max(annuityatretiretemp)
drop annuityatretiretemp
gen discsurvivalatretiretemp = .
replace discsurvivalatretiretemp = discsurvive if age==fas_retire25
by equableclassid: egen discsurvivalatretire25 = max(discsurvivalatretiretemp)
drop discsurvivalatretiretemp
gen finalannuity25 = .
replace finalannuity25 = annuityatretire * discsurvivalatretire / discsurvive if age<= fas_retire25
replace finalannuity25 = adjannuity if age>fas_retire25
replace finalannuity25 = 0 if YOS<fas_vest
*Age 40 Entrant*
gen survivetoretire40 = .
replace survivetoretire40 = survival if age==fas_retire40
by equableclassid: egen temp = max(survivetoretire40)
replace survivetoretire40=temp
drop temp
gen yearstoretire40 = fas_retire40-age
gen annuityatretiretemp = .
replace annuityatretiretemp = adjannuity if age==fas_retire40
by equableclassid: egen annuityatretire40 = max(annuityatretiretemp)
drop annuityatretiretemp
gen discsurvivalatretiretemp = .
replace discsurvivalatretiretemp = discsurvive if age==fas_retire40
by equableclassid: egen discsurvivalatretire40 = max(discsurvivalatretiretemp)
drop discsurvivalatretiretemp
gen finalannuity40 = .
replace finalannuity40 = annuityatretire40 * discsurvivalatretire40 / discsurvive if age<= fas_retire40
replace finalannuity40 = adjannuity if age>fas_retire40
replace finalannuity40 = 0 if YOS40<fas_vest
replace finalannuity40 = . if YOS40==.

*Pension NPV - not adjusted for inflation*
*Age 25 Entrant*
gen adjpwealth25 = .
replace adjpwealth25 = pwealth25*finalannuity25 if YOS>fas_vest
replace adjpwealth25 = 0 if adjpwealth25==.
gen fullpwealth25 = .
replace fullpwealth25 = 0 if YOS==0
replace fullpwealth25 = adjeecont25 if adjeecont25>adjpwealth25
replace fullpwealth25 = adjpwealth25 if adjeecont25<adjpwealth25
*Age 40 Entrant*
gen adjpwealth40 = .
replace adjpwealth40 = pwealth40*finalannuity40 if YOS40>fas_vest
replace adjpwealth40 = 0 if adjpwealth40==.
gen fullpwealth40 = .
replace fullpwealth40 = 0 if YOS40==0
replace fullpwealth40 = adjeecont40 if adjeecont40>adjpwealth40
replace fullpwealth40 = adjpwealth40 if adjeecont40<adjpwealth40

*Finally pension NPV adjusted for inflation*
*Age 25 Entrant*
gen pwealthfinal25 = .
replace pwealthfinal25 = fullpwealth25/((1+inflation)^(age-25))
*Age 40 Entrant*
gen pwealthfinal40 = .
replace pwealthfinal40 = fullpwealth40/((1+inflation)^(age-40))

*Let's save the data to be accessible this far without having to re-run the whole script*
save PensionCompleteInProgress.dta, replace

*Now that we have the inflation adjusted pension wealth, we can move to the next plan type*
*Thankfully, DC, GR, and the remaining portion of the hybrid plans are much easier to model*

*Defined Contribution Plans*

*First, let's calculate the contributions to the plan*
*Employee contributions*
*Age 25 Entrant*
gen dceecont25 = .
replace dceecont25 = dcgr_eecont*salary25 if YOS<dcgr_eecont1_yos
*Age 40 Entrant*
gen dceecont40 = .
replace dceecont40 = dcgr_eecont*salary40 if YOS40<dcgr_eecont1_yos
*Employer contributions*
*Age 25 Entrant*
gen dcempcont25 = .
replace dcempcont25 = dcgr_ercont4*salary25 if YOS<dcgr_ercont4_yos
replace dcempcont25 = dcgr_ercont3*salary25 if YOS<dcgr_ercont3_yos
replace dcempcont25 = dcgr_ercont2*salary25 if YOS<dcgr_ercont2_yos
replace dcempcont25 = dcgr_ercont1*salary25 if YOS<dcgr_ercont1_yos
*Age 40 Entrant*
gen dcempcont40 = .
replace dcempcont40 = dcgr_ercont4*salary40 if YOS40<dcgr_ercont4_yos
replace dcempcont40 = dcgr_ercont3*salary40 if YOS40<dcgr_ercont3_yos
replace dcempcont40 = dcgr_ercont2*salary40 if YOS40<dcgr_ercont2_yos
replace dcempcont40 = dcgr_ercont1*salary40 if YOS40<dcgr_ercont1_yos

*Now that we have these contributions, we can model the benefits*
*Note that for our purposes, we are modeling the returns as being the broader plan ARR minus 50 basis points*
*First a simple model - we will adjust for graded vesting and inflation in a minute*
*Age 25 Entrant*
gen dcwealth25 = .
replace dcwealth25 = dceecont25+dcempcont25 if age==25 & gr_creditingrate==.
replace dcwealth25 = (L1.dcwealth25*(1+(arr-0.005))+(dceecont25+dcempcont25)) if age>25 & gr_creditingrate==.
*Age 40 Entrant*
gen dcwealth40 = .
replace dcwealth40 = dceecont40+dcempcont40 if age==40 & gr_creditingrate==.
replace dcwealth40 = (L1.dcwealth40*(1+(arr-0.005))+(dceecont40+dcempcont40)) if age>40 & gr_creditingrate==.

*Next we tackle the graded vesting*
rename dcgr_fullvest dcgr_fullvesting
*We want to generate total employer contributions, so we know how much to subtract based on the graded vesting*
*Note that this leaves out the interest that would accrue on the employer contributions - it's not perfect, but it's good enough for our purposes*
gen employertotalcont25 = .
replace employertotalcont25 = dcempcont25 if age==25
replace employertotalcont25 = L1.employertotalcont25+dcempcont25 if age>25 & gr_creditingrate==.
gen employertotalcont40 = .
replace employertotalcont40 = dcempcont40 if age==40
replace employertotalcont40 = L1.employertotalcont40+dcempcont40 if age>40 & gr_creditingrate==.

*Age 25 Entrant*
gen adjdcwealth25 = .
replace adjdcwealth25 = dcwealth25-((1-dcgr_grade10)*employertotalcont25) if YOS<dcgr_grade10_yos
replace adjdcwealth25 = dcwealth25-((1-dcgr_grade9)*employertotalcont25) if YOS<dcgr_grade9_yos
replace adjdcwealth25 = dcwealth25-((1-dcgr_grade8)*employertotalcont25) if YOS<dcgr_grade8_yos
replace adjdcwealth25 = dcwealth25-((1-dcgr_grade7)*employertotalcont25) if YOS<dcgr_grade7_yos
replace adjdcwealth25 = dcwealth25-((1-dcgr_grade6)*employertotalcont25) if YOS<dcgr_grade6_yos
replace adjdcwealth25 = dcwealth25-((1-dcgr_grade5)*employertotalcont25) if YOS<dcgr_grade5_yos
replace adjdcwealth25 = dcwealth25-((1-dcgr_grade4)*employertotalcont25) if YOS<dcgr_grade4_yos
replace adjdcwealth25 = dcwealth25-((1-dcgr_grade3)*employertotalcont25) if YOS<dcgr_grade3_yos
replace adjdcwealth25 = dcwealth25-((1-dcgr_grade2)*employertotalcont25) if YOS<dcgr_grade2_yos
replace adjdcwealth25 = dcwealth25-((1-dcgr_grade1)*employertotalcont25) if YOS<dcgr_grade1_yos
replace adjdcwealth25 = dcwealth25 if YOS>=dcgr_fullvesting

*Now for anyone who does NOT have graded vesting and hasn't reached full vesting, they will get only their own contributions plus interest*
*These won't be perfect, but they will work for these corner cases*
replace adjdcwealth25 = dcwealth25 - dcempcont25 if YOS<dcgr_fullvesting & dcgr_gradevest==0

*Age 40 Entrant*
gen adjdcwealth40 = .
replace adjdcwealth40 = dcwealth40-((1-dcgr_grade10)*employertotalcont40) if YOS40<dcgr_grade10_yos
replace adjdcwealth40 = dcwealth40-((1-dcgr_grade9)*employertotalcont40) if YOS40<dcgr_grade9_yos
replace adjdcwealth40 = dcwealth40-((1-dcgr_grade8)*employertotalcont40) if YOS40<dcgr_grade8_yos
replace adjdcwealth40 = dcwealth40-((1-dcgr_grade7)*employertotalcont40) if YOS40<dcgr_grade7_yos
replace adjdcwealth40 = dcwealth40-((1-dcgr_grade6)*employertotalcont40) if YOS40<dcgr_grade6_yos
replace adjdcwealth40 = dcwealth40-((1-dcgr_grade5)*employertotalcont40) if YOS40<dcgr_grade5_yos
replace adjdcwealth40 = dcwealth40-((1-dcgr_grade4)*employertotalcont40) if YOS40<dcgr_grade4_yos
replace adjdcwealth40 = dcwealth40-((1-dcgr_grade3)*employertotalcont40) if YOS40<dcgr_grade3_yos
replace adjdcwealth40 = dcwealth40-((1-dcgr_grade2)*employertotalcont40) if YOS40<dcgr_grade2_yos
replace adjdcwealth40 = dcwealth40-((1-dcgr_grade1)*employertotalcont40) if YOS40<dcgr_grade1_yos
replace adjdcwealth40 = dcwealth40 if YOS40>=dcgr_fullvesting
replace adjdcwealth40 = dcwealth40 - dcempcont40 if YOS40<dcgr_fullvesting & dcgr_gradevest==0

*Finally, we inflation adjust the dc wealth*
*Age 25 Entrant*
gen dcwealthfinal25 = .
replace dcwealthfinal25 = adjdcwealth25/((1+inflation)^(age-25))
*Age 40 Entrant*
gen dcwealthfinal40 = .
replace dcwealthfinal40 = adjdcwealth40/((1+inflation)^(age-40))

*We can save this far to again have an easy return-to point*
save DCCompleteInProgress.dta, replace

*Guaranteed Return Plans*
*These are essentially the same as DC models, but they will use the guaranteed return rate rather than the ARR minus 50 basis points*

*First, let's calculate the contributions to the plan*
*Employee contributions*
*Age 25 Entrant*
gen greecont25 = .
replace greecont25 = dcgr_eecont*salary25 if YOS<dcgr_eecont1_yos
*Age 40 Entrant*
gen greecont40 = .
replace greecont40 = dcgr_eecont*salary40 if YOS40<dcgr_eecont1_yos

*Employer contributions*
*Age 25 Entrant*
gen grempcont25 = .
replace grempcont25 = dcgr_ercont4*salary25 if YOS<dcgr_ercont4_yos
replace grempcont25 = dcgr_ercont3*salary25 if YOS<dcgr_ercont3_yos
replace grempcont25 = dcgr_ercont2*salary25 if YOS<dcgr_ercont2_yos
replace grempcont25 = dcgr_ercont1*salary25 if YOS<dcgr_ercont1_yos
*Age 40 Entrant*
gen grempcont40 = .
replace grempcont40 = dcgr_ercont4*salary40 if YOS40<dcgr_ercont4_yos
replace grempcont40 = dcgr_ercont3*salary40 if YOS40<dcgr_ercont3_yos
replace grempcont40 = dcgr_ercont2*salary40 if YOS40<dcgr_ercont2_yos
replace grempcont40 = dcgr_ercont1*salary40 if YOS40<dcgr_ercont1_yos

*Now that we have these contributions, we can model the benefits*
*Note that for our purposes, we are modeling the returns as being the broader plan ARR minus 50 basis points*
*First a simple model - we will adjust for graded vesting and inflation in a minute*
*Age 25 Entrant*
gen grwealth25 = .
replace grwealth25 = greecont25+grempcont25 if age==25 & gr_creditingrate!=.
replace grwealth25 = (L1.grwealth25*(1+(gr_creditingrate))+(greecont25+grempcont25)) if age>25 & gr_creditingrate!=.
*Age 40 Entrant*
gen grwealth40 = .
replace grwealth40 = greecont40+grempcont40 if age==40 & gr_creditingrate!=.
replace grwealth40 = (L1.grwealth40*(1+(gr_creditingrate))+(greecont40+grempcont40)) if age>40 & gr_creditingrate!=.

*Next we tackle the graded vesting*
*Similar to how we did the DC plans, we need to adjust for employer contributions*
gen gremployertotalcont25 = .
replace gremployertotalcont25 = grempcont25 if age==25
replace gremployertotalcont25 = L1.gremployertotalcont25+grempcont25 if age>25 & gr_creditingrate!=.
gen gremployertotalcont40 = .
replace gremployertotalcont40 = grempcont40 if age==40
replace gremployertotalcont40 = L1.gremployertotalcont40+grempcont40 if age>40 & gr_creditingrate!=.

*Age 25 Entrant*
gen adjgrwealth25 = .
replace adjgrwealth25 = grwealth25-((1-dcgr_grade10)*gremployertotalcont25) if YOS<dcgr_grade10_yos
replace adjgrwealth25 = grwealth25-((1-dcgr_grade9)*gremployertotalcont25) if YOS<dcgr_grade9_yos
replace adjgrwealth25 = grwealth25-((1-dcgr_grade8)*gremployertotalcont25) if YOS<dcgr_grade8_yos
replace adjgrwealth25 = grwealth25-((1-dcgr_grade7)*gremployertotalcont25) if YOS<dcgr_grade7_yos
replace adjgrwealth25 = grwealth25-((1-dcgr_grade6)*gremployertotalcont25) if YOS<dcgr_grade6_yos
replace adjgrwealth25 = grwealth25-((1-dcgr_grade5)*gremployertotalcont25) if YOS<dcgr_grade5_yos
replace adjgrwealth25 = grwealth25-((1-dcgr_grade4)*gremployertotalcont25) if YOS<dcgr_grade4_yos
replace adjgrwealth25 = grwealth25-((1-dcgr_grade3)*gremployertotalcont25) if YOS<dcgr_grade3_yos
replace adjgrwealth25 = grwealth25-((1-dcgr_grade2)*gremployertotalcont25) if YOS<dcgr_grade2_yos
replace adjgrwealth25 = grwealth25-((1-dcgr_grade1)*gremployertotalcont25) if YOS<dcgr_grade1_yos
replace adjgrwealth25 = grwealth25 if YOS>=dcgr_fullvesting

*Again addressing corner cases where there isn't graded vesting, but they haven't reached full vesting*
replace adjgrwealth25 = grwealth25 - grempcont25 if YOS<dcgr_fullvesting & dcgr_gradevest==0

*Age 40 Entrant*
gen adjgrwealth40 = .
replace adjgrwealth40 = grwealth40-((1-dcgr_grade10)*gremployertotalcont40) if YOS40<dcgr_grade10_yos
replace adjgrwealth40 = grwealth40-((1-dcgr_grade9)*gremployertotalcont40) if YOS40<dcgr_grade9_yos
replace adjgrwealth40 = grwealth40-((1-dcgr_grade8)*gremployertotalcont40) if YOS40<dcgr_grade8_yos
replace adjgrwealth40 = grwealth40-((1-dcgr_grade7)*gremployertotalcont40) if YOS40<dcgr_grade7_yos
replace adjgrwealth40 = grwealth40-((1-dcgr_grade6)*gremployertotalcont40) if YOS40<dcgr_grade6_yos
replace adjgrwealth40 = grwealth40-((1-dcgr_grade5)*gremployertotalcont40) if YOS40<dcgr_grade5_yos
replace adjgrwealth40 = grwealth40-((1-dcgr_grade4)*gremployertotalcont40) if YOS40<dcgr_grade4_yos
replace adjgrwealth40 = grwealth40-((1-dcgr_grade3)*gremployertotalcont40) if YOS40<dcgr_grade3_yos
replace adjgrwealth40 = grwealth40-((1-dcgr_grade2)*gremployertotalcont40) if YOS40<dcgr_grade2_yos
replace adjgrwealth40 = grwealth40-((1-dcgr_grade1)*gremployertotalcont40) if YOS40<dcgr_grade1_yos
replace adjgrwealth40 = grwealth40 if YOS40>=dcgr_fullvesting
replace adjgrwealth40 = grwealth40 - grempcont40 if YOS40<dcgr_fullvesting & dcgr_gradevest==0

*Finally, we inflation adjust the GR wealth*
*Age 25 Entrant*
gen grwealthfinal25 = .
replace grwealthfinal25 = adjgrwealth25/((1+inflation)^(age-25))
*Age 40 Entrant*
gen grwealthfinal40 = .
replace grwealthfinal40 = adjgrwealth40/((1+inflation)^(age-40))

*We can save this far to again have an easy return-to point*
save GRCompleteInProgress.dta, replace

*Hybrid Plans*
*We actually can do this very simply, as we have already generated the benefits for each component part, so we just need to add them together*
*We will add the non-inflation adjusted benefit totals and then will adjust for inflation on the tail end*
*Note that it should come out as the same values, but we're doing it this way just to be sure*
*Also note that we can add all three plan types here without worry of over-counting, as the value for the missing plan type will just be zero*

*Benefit total*
*Age 25 Entrant*
replace fullpwealth25 = 0 if fullpwealth25==.
replace adjdcwealth25 = 0 if adjdcwealth25==.
replace adjgrwealth25 = 0 if adjgrwealth25==.
gen hybwealth25 = .
replace hybwealth25 = fullpwealth25+adjdcwealth25+adjgrwealth25 if plantype=="Hybrid"
replace hybwealth25 = 0 if plantype=="FAS"
replace hybwealth25 = 0 if plantype=="DC"
replace hybwealth25 = 0 if plantype=="GR"
*Age 40 Entrant*
replace fullpwealth40 = 0 if fullpwealth40==.
replace adjdcwealth40 = 0 if adjdcwealth40==.
replace adjgrwealth40 = 0 if adjgrwealth40==.
gen hybwealth40 = .
replace hybwealth40 = fullpwealth40+adjdcwealth40+adjgrwealth40 if plantype=="Hybrid"
replace hybwealth40 = 0 if plantype=="FAS"
replace hybwealth40 = 0 if plantype=="DC"
replace hybwealth40 = 0 if plantype=="GR"

*Then we inflation adjust this combined term*
*Age 25 Entrant*
gen hybwealthfinal25 = .
replace hybwealthfinal25 = hybwealth25/((1+inflation)^(age-25))
*Age 40 Entrant*
gen hybwealthfinal40 = .
replace hybwealthfinal40 = hybwealth40/((1+inflation)^(age-40))

*We can save here as a last return to point before we simplify the data down to just the values we need and finish things up*
save HybCompleteInProgress.dta, replace

*Now our final task is to simplify the data down to just the final, inflation adjusted values of all our model types - then we can annuitize them and calculate replacement rates*
*Age 25 Entrant*
gen totalbenefitwealth25 = .
replace totalbenefitwealth25 = pwealthfinal25 if plantype=="FAS"
replace totalbenefitwealth25 = dcwealthfinal25 if plantype=="DC"
replace totalbenefitwealth25 = grwealthfinal25 if plantype=="GR"
replace totalbenefitwealth25 = hybwealthfinal25 if plantype=="Hybrid"
*Age 40 Entrant*
gen totalbenefitwealth40 = .
replace totalbenefitwealth40 = pwealthfinal40 if plantype=="FAS"
replace totalbenefitwealth40 = dcwealthfinal40 if plantype=="DC"
replace totalbenefitwealth40 = grwealthfinal40 if plantype=="GR"
replace totalbenefitwealth40 = hybwealthfinal40 if plantype=="Hybrid"

*To annuitize our values, we will need to calculate how many payments we have from retirement until age 86, the age we're assuming payments stop*
*Age 25 Entrant*
gen payments25 = .
replace payments25 = 86-fas_retire25 if plantype=="FAS"
replace payments25 = 86-fas_retire25 if plantype=="Hybrid"
replace payments25 = 86-67 if plantype=="DC"
replace payments25 = 86-fas_retire25 if plantype=="GR"
*Age 40 Entrant*
gen payments40 = .
replace payments40 = 86-fas_retire40 if plantype=="FAS"
replace payments40 = 86-fas_retire40 if plantype=="Hybrid"
replace payments40 = 86-67 if plantype=="DC"
replace payments40 = 86-fas_retire40 if plantype=="GR"

*We will also need inflation adjusted salary to calculate the replacement rate targets*
gen infsalary25 = salary25/((1+inflation)^(age-25))
gen infsalary40 = salary40/((1+inflation)^(age-40))
*From these, we can calculate FAS targets*
*Age 25 Entrant*
foreach i in 1 2 3 4 5 6 8 {
tssmooth ma inffas`i' = infsalary25, window(`i',0,0)
replace inffas`i'=. if YOS<fas_years
}
*Now that we have these different averages, we want to then collapse down to just one FAS value for 25 year old entrants*
gen inffas25 = .
foreach i in 1 2 3 4 5 6 8 {
replace inffas25 = inffas`i' if fas_years==`i'
}
*And we can drop the other fas averages*
drop inffas1 - inffas8

*Age 40 Entrant*
foreach i in 1 2 3 4 5 6 8 {
tssmooth ma inffas`i' = infsalary40, window(`i',0,0)
replace inffas`i'=. if YOS40<fas_years
}
*Now that we have these different averages, we want to then collapse down to just one FAS value for 25 year old entrants*
gen inffas40 = .
foreach i in 1 2 3 4 5 6 8 {
replace inffas40 = inffas`i' if fas_years==`i'
}
replace inffas40 = . if YOS40==.
*And we can drop the other fas averages*
drop inffas1 - inffas8

*Now we can generate inflation adjusted target salaries for replacement rates*
*Age 25 Entrant*
gen tempfastarget25 = .
replace tempfastarget25 = inffas25 if age==67 & plantype=="FAS"
replace tempfastarget25 = inffas25 if age==67 & plantype=="Hybrid"
by equableclassid: egen fastarget25 = max(tempfastarget25)
drop tempfastarget25
gen tempdcgrtarget25 = .
replace tempdcgrtarget25 = infsalary25 if age==67 & plantype=="DC"
replace tempdcgrtarget25 = infsalary25 if age==67 & plantype=="GR"
by equableclassid: egen dcgrtarget25 = max(tempdcgrtarget25)
drop tempdcgrtarget25
gen finalsalarytarget25 = .
replace finalsalarytarget25 = fastarget25 if plantype=="FAS"
replace finalsalarytarget25 = dcgrtarget25 if plantype=="DC"
replace finalsalarytarget25 = dcgrtarget25 if plantype=="GR"
replace finalsalarytarget25 = fastarget25 if plantype=="Hybrid"
*Age 40 Entrant*
gen tempfastarget40 = .
replace tempfastarget40 = inffas40 if age==67 & plantype=="FAS"
replace tempfastarget40 = inffas40 if age==67 & plantype=="Hybrid"
by equableclassid: egen fastarget40 = max(tempfastarget40)
drop tempfastarget40
gen tempdcgrtarget40 = .
replace tempdcgrtarget40 = infsalary40 if age==67 & plantype=="DC"
replace tempdcgrtarget40 = infsalary40 if age==67 & plantype=="GR"
by equableclassid: egen dcgrtarget40 = max(tempdcgrtarget40)
drop tempdcgrtarget40
gen finalsalarytarget40 = .
replace finalsalarytarget40 = fastarget40 if plantype=="FAS"
replace finalsalarytarget40 = dcgrtarget40 if plantype=="DC"
replace finalsalarytarget40 = dcgrtarget40 if plantype=="GR"
replace finalsalarytarget40 = fastarget40 if plantype=="Hybrid"

*Now we can annuitize the total benefitwealth values*
*This is generally a pretty simple process*
*First we need to create the "rate" variable - we assume a 3.6% Annuitization growth, and then set the cola adjustment equal to the cola rate for the pension*
gen annrate = .
replace annrate = (1.036)/(1+(inflation))-1 

*We already have the number of payments from earlier*
*Finally, we can estimate the annuity*
*The formula is P = (Pv*R) / [1 - (1 + R)^(-n)], where Pv is the pension present value, R is the rate, and n is the number of payments*
*Age 25 Entrant*
gen annuitybenefit25 = (totalbenefitwealth25*annrate)/(1-((1+annrate)^(-1*payments25)))
replace annuitybenefit25 = 0 if annuitybenefit25==.
replace annuitybenefit25 = L1.annuitybenefit25 if L1.annuitybenefit25>annuitybenefit25 & age>fas_retire25
*Age 40 Entrant*
gen annuitybenefit40 = (totalbenefitwealth40*annrate)/(1-((1+annrate)^(-1*payments40)))
replace annuitybenefit40 = 0 if annuitybenefit40==. 
replace annuitybenefit40 = . if YOS40==.
replace annuitybenefit40 = L1.annuitybenefit40 if L1.annuitybenefit40>annuitybenefit40 & age>fas_retire40

*Saving the data here for the threshold calculations*
save RSR_Benefit_Models_Complete.dta, replace

*And now we can simplify the data even further to our final output*
keep equableclassid plan_fullname class_name ssa_enroll YOS YOS40 age plantype totalbenefitwealth25 totalbenefitwealth40 annuitybenefit25 annuitybenefit40 fas_max_pensionable_salary fas_replacement_cap rawsalary25 rawsalary40 fas_retire25 fas_retire40

*That should be everything*
save Benefit_Models_Output.dta, replace
