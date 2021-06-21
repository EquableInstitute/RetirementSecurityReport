***RSR Complete***
***Author: Jon Moody***
***Date: April 26, 2021***
***Last Modified: May 26, 2021***

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

***RSR Threshold Modeling***
***Author: Jon Moody***
***Date: April 19, 2021***
***Last Modified: May 17, 2021***

clear
set more off
cd "C:\Users\jonwm\Dropbox (Equable)\Equable Core\Research_Equable Projects\Project_Retirement Security Report\Benefit Models\Scripting\Stata"

*Let's load the full data, as that has the right structure for what we're trying to do*
*It also conveniently has several of the variables we want to incorporate*
use RSR_Benefit_Models_Complete.dta

*For this we can simplify the data significantly*
keep equableclassid class_name plantype YOS YOS40 salary25 fas25 salary40 fas40 age ssa_enroll discrate inflation fas_colarate totalbenefitwealth25 totalbenefitwealth40 annuitybenefit25 annuitybenefit40 fas_years payments25 payments40 fas_retire25 fas_retire40

*I want to generate a set of unified retirement age variables*
replace fas_retire25 = 67 if plantype=="DC"
rename fas_retire25 retire_age25
replace fas_retire40 = 67 if plantype=="DC"
rename fas_retire40 retire_age40

*Let's rename the annuity benefit values otherwise they will get dropped by the script for the thresholds*
rename annuitybenefit25 annuitized25
rename annuitybenefit40 annuitized40

*Let's fix the salary40 variable, as it has values prior to age 40*
replace salary40=. if age<40

*Unlike the prior approach that built out a DC model, for this one, we don't need to make any real assumptions*
*Instead, we just build a version that will get the annuitized benefit to reach 70% (or less depending on SSA enrollment) of the FAS*
*So first, we need to calculate the inflation adjusted FAS values*

*Let's start by panel setting the data*
xtset equableclassid age

*We inflation adjust the salary so that the FAS is actually meaningful*
gen inflationsalary25 = salary25/((1+inflation)^(age-25))
gen inflationsalary40 = salary40/((1+inflation)^(age-40))

*Then an inflation adjusted FAS*
*Age 25 Entrant*
foreach i in 1 2 3 4 5 6 8 {
tssmooth ma inflationfas`i' = inflationsalary25, window(`i',0,0)
replace inflationfas`i'=. if YOS<fas_years
}
*Now that we have these different averages, we want to then collapse down to just one FAS value for 25 year old entrants*
gen inflationfas25 = .
foreach i in 1 2 3 4 5 6 8 {
replace inflationfas25 = inflationfas`i' if fas_years==`i'
}
*And we can drop the other fas averages*
drop inflationfas1 - inflationfas8

*Age 40 Entrant*
foreach i in 1 2 3 4 5 6 8 {
tssmooth ma inflationfas`i' = inflationsalary40, window(`i',0,0)
replace inflationfas`i'=. if YOS40<fas_years
}
*Now that we have these different averages, we want to then collapse down to just one FAS value for 25 year old entrants*
gen inflationfas40 = .
foreach i in 1 2 3 4 5 6 8 {
replace inflationfas40 = inflationfas`i' if fas_years==`i'
}
replace inflationfas40 = . if YOS40==.
*And we can drop the other fas averages*
drop inflationfas1 - inflationfas8

*Note that we don't have to build out the annuity factor elements becuse we will be building the threshold based on the annuitized value to begin with*
*So we move straight on to developing salary targets that account for the inflation adjustment*
*Age 25 Entrant*
gen finalfas25 = .
replace finalfas25 = inflationfas25 if age==67 & plantype=="FAS"
replace finalfas25 = inflationfas25 if age==67 & plantype=="Hybrid"
replace finalfas25 = inflationsalary25 if age==67 & plantype=="DC"
replace finalfas25 = inflationsalary25 if age==67 & plantype=="GR"
by equableclassid: egen fastarget25 = max(finalfas25)
drop finalfas25
*Age 40 Entrant*
gen finalfas40 = .
replace finalfas40 = inflationfas40 if age==67 & plantype=="FAS"
replace finalfas40 = inflationfas40 if age==67 & plantype=="Hybrid"
replace finalfas40 = inflationsalary40 if age==67 & plantype=="DC"
replace finalfas40 = inflationsalary40 if age==67 & plantype=="GR"
by equableclassid: egen fastarget40 = max(finalfas40)
drop finalfas40

*From here, it's just a matter of building out the targets we're shooting for*
*Let's save the data here just in case we want to come back to this point*
save Pre-Thresholds.dta, replace

*Let's start off here by generating a starting benefit value for year 0*
gen startbenefit25 = 0
gen startbenefit40 = 0

*Next we need to build out the actual shares of FAS we are looking for to equate to the 60, 70, and 80% replacement rates*
*These are based on the targets we set just a minute ago*
*Note that age 25 and age 40 entrants will be handled differently*
gen targetfas25 = .
replace targetfas25 = fastarget25*.7 if ssa_enroll=="No"
replace targetfas25 = fastarget25*.7 if ssa_enroll=="Mixed"
replace targetfas25 = fastarget25*(.7-.33) if ssa_enroll=="Yes"

*Then we calculate the difference between the start and finish values and divide it into equal portions for each age*
gen shift25 = .
replace shift25 = (targetfas25-startbenefit25)/(67-25)

*From here, we can calculate the annuitized total that we need*
gen threshold25 = .
replace threshold25 = startbenefit25 if age==25
replace threshold25 = L1.threshold25+shift25 if age>25

*Now that we have these thresholds, we can convert them into replacement rates*
gen thresholdreplace25 = threshold25/fastarget25

*For 40-year-old-entrants, things are a bit more complicated, as we want to assume that they have some savings set aside already*
*To figure this out, we can use the benefis from the 25-year-old entrant threshold and just assume they have that much*
*But then we adjust the target down to meet that*
gen startsavingstemp = .
replace startsavingstemp = thresholdreplace25 if age==40
by equableclassid: egen entrysavings40 = max(startsavingstemp)
drop startsavingstemp
gen targetfas40 = .
replace targetfas40 = fastarget40*.7 if ssa_enroll=="No"
replace targetfas40 = fastarget40*.7 if ssa_enroll=="Mixed"
replace targetfas40 = fastarget40*(.7-.33) if ssa_enroll=="Yes"
replace targetfas40 = targetfas40 - (entrysavings40*fastarget40)
gen shift40 = .
replace shift40 = (targetfas40-startbenefit40)/(67-40)
gen threshold40 = .
replace threshold40 = startbenefit40 if age==40
replace threshold40 = L1.threshold40+shift40 if age>40
gen thresholdreplace40 = threshold40/fastarget40

*Lastly, we need replacement rates for the actual benefits for comparative purposes*
gen benefitreplace25 = .
replace benefitreplace25 = annuitized25/fastarget25
gen benefitreplace40 = .
replace benefitreplace40 = annuitized40/fastarget40

*Now we want to repeat this process to get 60% and 80% thresholds as well*
gen targetfas25_60 = .
replace targetfas25_60 = fastarget25*.6 if ssa_enroll=="No"
replace targetfas25_60 = fastarget25*.6 if ssa_enroll=="Mixed"
replace targetfas25_60 = fastarget25*(.6-.33) if ssa_enroll=="Yes"
gen shift25_60 = .
replace shift25_60 = (targetfas25_60-startbenefit25)/(67-25)
gen threshold25_60 = .
replace threshold25_60 = startbenefit25 if age==25
replace threshold25_60 = L1.threshold25_60+shift25_60 if age>25
gen thresholdreplace25_60 = threshold25_60/fastarget25
gen startsavingstemp_60 = .
replace startsavingstemp_60 = thresholdreplace25_60 if age==40
by equableclassid: egen entrysavings40_60 = max(startsavingstemp_60)
drop startsavingstemp_60
gen targetfas40_60 = .
replace targetfas40_60 = fastarget40*.6 if ssa_enroll=="No"
replace targetfas40_60 = fastarget40*.6 if ssa_enroll=="Mixed"
replace targetfas40_60 = fastarget40*(.6-.33) if ssa_enroll=="Yes"
replace targetfas40_60 = targetfas40_60 - (entrysavings40_60*fastarget40)
gen shift40_60 = .
replace shift40_60 = (targetfas40_60-startbenefit40)/(67-40)
gen threshold40_60 = .
replace threshold40_60 = startbenefit40 if age==40
replace threshold40_60 = L1.threshold40_60+shift40_60 if age>40
gen thresholdreplace40_60 = threshold40_60/fastarget40
gen targetfas25_80 = .
replace targetfas25_80 = fastarget25*.8 if ssa_enroll=="No"
replace targetfas25_80 = fastarget25*.8 if ssa_enroll=="Mixed"
replace targetfas25_80 = fastarget25*(.8-.33) if ssa_enroll=="Yes"
gen shift25_80 = .
replace shift25_80 = (targetfas25_80-startbenefit25)/(67-25)
gen threshold25_80 = .
replace threshold25_80 = startbenefit25 if age==25
replace threshold25_80 = L1.threshold25_80+shift25_80 if age>25
gen thresholdreplace25_80 = threshold25_80/fastarget25
gen startsavingstemp_80 = .
replace startsavingstemp_80 = thresholdreplace25_80 if age==40
by equableclassid: egen entrysavings40_80 = max(startsavingstemp_80)
drop startsavingstemp_80
gen targetfas40_80 = .
replace targetfas40_80 = fastarget40*.8 if ssa_enroll=="No"
replace targetfas40_80 = fastarget40*.8 if ssa_enroll=="Mixed"
replace targetfas40_80 = fastarget40*(.8-.33) if ssa_enroll=="Yes"
replace targetfas40_80 = targetfas40_80 - (entrysavings40_80*fastarget40)
gen shift40_80 = .
replace shift40_80 = (targetfas40_80-startbenefit40)/(67-40)
gen threshold40_80 = .
replace threshold40_80 = startbenefit40 if age==40
replace threshold40_80 = L1.threshold40_80+shift40_80 if age>40
gen thresholdreplace40_80 = threshold40_80/fastarget40

*That should cover everything we need*
*Note that if we wanted to calculate a pure NPV for the thresholds, we would just need to use the annuitization formula backwards to solve*

*Let's build in the graphics versions of the annuitized and replacement rates here*
*First 25-year-old entrants*
gen ssa_share = .
replace ssa_share = 0.33 * fastarget25
gen graphicannuitybenefit25 = .
replace graphicannuitybenefit25 = annuitized25 if ssa_enroll=="No" | ssa_enroll=="Mixed"
replace graphicannuitybenefit25 = ((ssa_share/43)*(age-24)) if ssa_enroll=="Yes" & age==25
replace graphicannuitybenefit25 = ((ssa_share/43)*(age-24))+annuitized25 if ssa_enroll=="Yes" & age>25
gen graphicreplacementbenefit25 = .
replace graphicreplacementbenefit25 = graphicannuitybenefit25/fastarget25
gen graphicannuitythreshold25 = .
replace graphicannuitythreshold25 = threshold25 if ssa_enroll=="No" | ssa_enroll=="Mixed"
replace graphicannuitythreshold25 = ((ssa_share/43)*(age-24))+threshold25 if ssa_enroll=="Yes"
gen graphicreplacementthreshold25 = .
replace graphicreplacementthreshold25 = graphicannuitythreshold25/fastarget25

*40-year-old entrants are a bit more complicated, as we have to add in the starting savings, but not incredibly so*
gen ssa_and_start_share = .
replace ssa_and_start_share = (0.33+entrysavings40)*fastarget40
gen graphicannuitybenefit40 = .
replace graphicannuitybenefit40 = annuitized40 if ssa_enroll=="No" | ssa_enroll=="Mixed"
replace graphicannuitybenefit40 = ((ssa_and_start_share/27)*(age-40))+annuitized40 if ssa_enroll=="Yes"
gen graphicreplacementbenefit40 = .
replace graphicreplacementbenefit40 = graphicannuitybenefit40/fastarget40
gen graphicannuitythreshold40 = .
replace graphicannuitythreshold40 = threshold40 if ssa_enroll=="No" | ssa_enroll=="Mixed"
replace graphicannuitythreshold40 = ((ssa_and_start_share/27)*(age-40))+threshold40 if ssa_enroll=="Yes"
gen graphicreplacementthreshold40 = .
replace graphicreplacementthreshold40 = graphicannuitythreshold40/fastarget40

*And the graphics versions for 60% and 80% thresholds
gen graphicannuitythreshold25_60 = .
replace graphicannuitythreshold25_60 = threshold25_60 if ssa_enroll=="No" | ssa_enroll=="Mixed"
replace graphicannuitythreshold25_60 = ((ssa_share/43)*(age-24))+threshold25_60 if ssa_enroll=="Yes"
gen graphicreplacementthreshold25_60 = .
replace graphicreplacementthreshold25_60 = graphicannuitythreshold25_60/fastarget25
gen ssa_and_start_share_60 = .
replace ssa_and_start_share_60 = (0.33+entrysavings40_60)*fastarget40
gen graphicannuitythreshold40_60 = .
replace graphicannuitythreshold40_60 = threshold40_60 if ssa_enroll=="No" | ssa_enroll=="Mixed"
replace graphicannuitythreshold40_60 = ((ssa_and_start_share_60/27)*(age-40))+threshold40_60 if ssa_enroll=="Yes"
gen graphicreplacementthreshold40_60 = .
replace graphicreplacementthreshold40_60 = graphicannuitythreshold40_60/fastarget40
gen graphicannuitythreshold25_80 = .
replace graphicannuitythreshold25_80 = threshold25_80 if ssa_enroll=="No" | ssa_enroll=="Mixed"
replace graphicannuitythreshold25_80 = ((ssa_share/43)*(age-24))+threshold25_80 if ssa_enroll=="Yes"
gen graphicreplacementthreshold25_80 = .
replace graphicreplacementthreshold25_80 = graphicannuitythreshold25_80/fastarget25
gen ssa_and_start_share_80 = .
replace ssa_and_start_share_80 = (0.33+entrysavings40_80)*fastarget40
gen graphicannuitythreshold40_80 = .
replace graphicannuitythreshold40_80 = threshold40_80 if ssa_enroll=="No" | ssa_enroll=="Mixed"
replace graphicannuitythreshold40_80 = ((ssa_and_start_share_80/27)*(age-40))+threshold40_80 if ssa_enroll=="Yes"
gen graphicreplacementthreshold40_80 = .
replace graphicreplacementthreshold40_80 = graphicannuitythreshold40_80/fastarget40

*That should cover everything*
save Benefit_Modeling_Complete.dta, replace

*And we can even output a graphics only data version*
keep equableclassid class_name age YOS YOS40 totalbenefitwealth25 totalbenefitwealth40 graphicannuitybenefit25 - graphicreplacementthreshold40_80
drop ssa_and_start_share*
*We want the NPVs and replacement rates, not annuitized benefits*
rename totalbenefitwealth25 NPV25
rename totalbenefitwealth40 NPV40
rename graphicannuitybenefit25 benefit25
rename graphicreplacementbenefit25 replacement25
rename graphicannuitythreshold25 annuitythreshold25
rename graphicreplacementthreshold25 replacementthreshold25
rename graphicannuitybenefit40 benefit40
rename graphicreplacementbenefit40 replacement40
rename graphicannuitythreshold40 annuitythreshold40
rename graphicreplacementthreshold40 replacementthreshold40
rename graphicannuitythreshold25_60 annuitythreshold25_60
rename graphicreplacementthreshold25_60 replacementthreshold25_60
rename graphicannuitythreshold40_60 annuitythreshold40_60
rename graphicreplacementthreshold40_60 replacementthreshold40_60
rename graphicannuitythreshold25_80 annuitythreshold25_80
rename graphicreplacementthreshold25_80 replacementthreshold25_80
rename graphicannuitythreshold40_80 annuitythreshold40_80
rename graphicreplacementthreshold40_80 replacementthreshold40_80
drop benefit25 benefit40
drop annuity*
replace replacementthreshold25 = . if age>67
replace replacementthreshold25_60 = . if age>67
replace replacementthreshold25_80 = . if age>67
replace replacementthreshold40 = . if age>67
replace replacementthreshold40_60 = . if age>67
replace replacementthreshold40_80 = . if age>67

*We can also add the Figure_Tag variables here*
gen Figure_Tag25 = ""
replace Figure_Tag25 = "Short-Term & Full Career Workers" if YOS<=10
replace Figure_Tag25 = "Medium-Term & Full Career Workers" if YOS>10 & YOS<=20
replace Figure_Tag25 = "Full Career Workers" if YOS>20
gen Figure_Tag40 = ""
replace Figure_Tag40 = "Short-Term & Full Career Workers" if YOS40<=10
replace Figure_Tag40 = "Medium-Term & Full Career Workers" if YOS40>10 & YOS40<=20
replace Figure_Tag40 = "Full Career Workers" if YOS40>20
replace Figure_Tag40 = "" if age<40

save RSR_Graphics_Data.dta, replace

***RSR SCORING!***
***Author: Jon Moody***
***Date: March 26, 2021***
***Last Modified: June 14, 2021***

clear
set more off
cd "C:\Users\jonwm\Dropbox (Equable)\Equable Core\Research_Equable Projects\Project_Retirement Security Report\Benefit Models\Scripting\Stata"

use Benefit_Modeling_Complete.dta

*Now with our benefits calculated and our thresholds modeled, we can calculate some scores*
*For the benefit models, we just need to divide the benefit replacement rate by the threshold benefit rate*
gen testscore1 = .
replace testscore1 = (annuitized25/fastarget25)/thresholdreplace25
gen testscore2 = .
replace testscore2 = ((annuitized25/fastarget25)+.33)/(thresholdreplace25+.33) if ssa_enroll=="Yes"

gen adequacyshare25 = .
replace adequacyshare25 = (annuitized25/threshold25)*15
gen adequacyshare40 = .
replace adequacyshare40 = (annuitized40/threshold40)*15

*From this, we convert them into scores for each of the different profiles*
*The adequacy scores for short-term and medium-term workers are the 10-year average share at 5 years, 10 years, 15 years, and 20 years*
*To calculate these, we can use a moving average variable*
xtset equableclassid age

*Here's for a 25-year-old entrant*
tssmooth ma ad25temp = adequacyshare25, window(9,1,0)
gen shortad25temp = .
replace shortad25temp = ad25temp if YOS==10
gen midad25temp = .
replace midad25temp = ad25temp if YOS==20
by equableclassid, sort: egen shortadequacy25score = max(shortad25temp)
by equableclassid, sort: egen mediumadequacy25score = max(midad25temp)
drop shortad25temp* midad25temp
replace shortadequacy25score = 15 if shortadequacy25score>15
replace mediumadequacy25score = 15 if mediumadequacy25score>15

*Note that we cannot rename the variables to match up what we provided the developers in Stata, as it won't let us have variables that lead off with numbers*
*Now for a 40-year-old entrant*
tssmooth ma ad40temp = adequacyshare40, window(9,1,0)
gen shortad40temp = .
replace shortad40temp = ad40temp if YOS40==10
gen midad40temp = .
replace midad40temp = ad40temp if YOS40==20
by equableclassid, sort: egen shortadequacy40score = max(shortad40temp)
by equableclassid, sort: egen mediumadequacy40score = max(midad40temp)
drop shortad40temp* midad40temp
replace shortadequacy40score = 15 if shortadequacy40score>15
replace mediumadequacy40score = 15 if mediumadequacy40score>15

*We can collapse this here if we want and just keep age 67 for the ease of tackling the scores, as all entrants are measured at age 67*
keep if age==67

*Now we want the full career workers*
gen replace25 = annuitized25/fastarget25
gen fulladequacy25score = 0
replace fulladequacy25score = 25*(replace25/.7) if ssa_enroll=="No" | ssa_enroll=="Mixed"
replace fulladequacy25score = 25 if replace25>=.7 & ssa_enroll=="No" 
replace fulladequacy25score = 25 if replace25>=.7 & ssa_enroll=="Mixed"
replace fulladequacy25score = 30 if replace25>.8 & ssa_enroll=="No" 
replace fulladequacy25score = 30 if replace25>.8 & ssa_enroll=="Mixed"
replace fulladequacy25score = 25*(replace25/(.7-.33)) if ssa_enroll=="Yes"
replace fulladequacy25score = 25 if replace25>=(.7-.33) & ssa_enroll=="Yes"
replace fulladequacy25score = 30 if replace25>(.8-.33) & ssa_enroll=="Yes"

gen replace40 = annuitized40/fastarget40
gen fulladequacy40score = 0
*Note that we have to adjust the score targets for both SSA and the starting savings*
*This is easier to just generate a new target value minus the starting savings*
gen scoretarget40 = 0.7-entrysavings40
gen scoretarget40plus = 0.8-entrysavings40
replace fulladequacy40score = 25*(replace40/scoretarget40) if ssa_enroll=="No" | ssa_enroll=="Mixed"
replace fulladequacy40score = 25 if replace40>=scoretarget40 & ssa_enroll=="No" 
replace fulladequacy40score = 25 if replace40>=scoretarget40 & ssa_enroll=="Mixed"
replace fulladequacy40score = 30 if replace40>scoretarget40plus & ssa_enroll=="No" 
replace fulladequacy40score = 30 if replace40>scoretarget40plus & ssa_enroll=="Mixed"
replace fulladequacy40score = 25*(replace40/(scoretarget40-.33)) if ssa_enroll=="Yes"
replace fulladequacy40score = 25 if replace40>=(scoretarget40-.33) & ssa_enroll=="Yes"
replace fulladequacy40score = 30 if replace40>(scoretarget40plus-.33) & ssa_enroll=="Yes"

*We can generate the total score values too*
gen short_ben_avail = 15
gen mid_ben_avail = .
replace mid_ben_avail = 20 if plantype=="FAS" | plantype=="GR" | plantype=="Hybrid"
replace mid_ben_avail = 15 if plantype=="DC"
gen repl_rate_available = 25

*Similarly, we can calculate the percentages too*
gen short_ben_pct_25 = shortadequacy25score/short_ben_avail
gen medium_ben_pct_25 = mediumadequacy25score/mid_ben_avail
gen repl_rate_pct_25 = fulladequacy25score/repl_rate_available
gen short_ben_pct_40 = shortadequacy40score/short_ben_avail
gen medium_ben_pct_40 = mediumadequacy40score/mid_ben_avail
gen repl_rate_pct_40 = fulladequacy40score/repl_rate_available

*Now let's just collapse this to just the scores, as we will use separate files for the various merges*
keep equableclassid shortadequacy25score shortadequacy40score mediumadequacy25score mediumadequacy40score fulladequacy25score fulladequacy40score short_ben_avail mid_ben_avail repl_rate_available short_ben_pct_25 short_ben_pct_40 medium_ben_pct_25 medium_ben_pct_40 repl_rate_pct_25 repl_rate_pct_40

*Now we can save this as the benefits scores to be merged with the other scores shortly*
save RSR_Benefit_Scores.dta, replace

*Now let's get the sustainability scores*
clear
import delimited using Sustainability_Score_Data.csv, varnames(1)

*We also have 1 too many years*
drop if year==2009

*Again, we don't need DC plans for this*
drop if plantype=="DC"

*We only need the one investment expense variable*
drop investmentexpenses brokercommissions

*Cleaning up these names just for the sake of making life easier*
rename yearreturns investreturn
rename paidemployercontribution contpaid
rename fundedratio_actuarial fr
rename investmentexpense_scfnp investexp

*Let's start with the amo period variable*
gen fund_path_score = .
replace fund_path_score = 0 if amortperiodr==999
replace fund_path_score = 1 if amortperiodr<999
replace fund_path_score = 2 if amortperiodr<=35
replace fund_path_score = 3 if amortperiodr<=25
replace fund_path_score = 4 if amortperiodr<=15
replace fund_path_score = 5 if amortperiodr<=5
replace fund_path_score = . if year<2019
gen fund_path_available = .
replace fund_path_available = 5
replace amortperiodr = . if year<2019

*Now we can build our scores for the contributions and investment earnings*
gen pay_score = .
replace pay_score = 1 if contpaid==adec
replace pay_score = 2 if contpaid>adec
replace pay_score = -1 if contpaid<adec
gen pay_available = .
replace pay_available = 10
gen invest_score = .
replace invest_score = 2 if investreturn>=(arr+.005)
replace invest_score = 1 if investreturn>=(arr-.005) & investreturn<(arr+.005)
replace invest_score = -1 if investreturn<(arr-.005)
gen invest_available = .
replace invest_available = 10

*Then we build some of the other variables for reporting*
gen bill_full_pay = .
replace bill_full_pay = 1 if pay_score>0 & pay_score!=.
gen bill_overpay = .
replace bill_overpay = 1 if pay_score==2
gen bill_underpay = .
replace bill_underpay = 1 if pay_score==-1
gen bill_exactpay = .
replace bill_exactpay = 1 if pay_score==1

gen good_invest = .
replace good_invest = 1 if invest_score>0 & invest_score!=.
gen bad_invest = .
replace bad_invest = 1 if invest_score==-1

*We also want a few values for reporting*
xtset equableclassid year
gen earnings = l1.mva * investreturn
gen earnings_2019 = .
replace earnings_2019 = earnings if year==2019
gen ROA = .
replace ROA=investreturn if year==2019
gen MVA_2018 = .
replace MVA_2018 = mva if year==2018
gen MVA_2019 = .
replace MVA_2019 = mva if year==2019
gen feespaid = .
replace feespaid = investexp/earnings
replace feespaid = . if year<2019

*We want to be sure the funded ratio doesn't get mixed up in the collapse*
gen fr_constant = .
replace fr_constant = fr if year==2019

*One last adjustment*
replace arr = . if year<2019

*Now a collapse can get this all down to just 1 row like we want*
collapse (sum) pay_score invest_score bill_full_pay bill_overpay bill_underpay bill_exactpay good_invest bad_invest (max) fund_path_score fund_path_available pay_available invest_available earnings_2019 MVA_2018 MVA_2019 feespaid ROA arr amortperiodr fr_constant, by(equableclassid)

*Note that we can clean things up a bit here, but we will need to adjust these back to missing or "N/A" for DC plans once we merge things back together*
rename fr_constant fr
replace pay_score = 10 if pay_score>10 & pay_score!=.
replace invest_score = 10 if invest_score>10 & invest_score!=.
replace pay_score = 0 if pay_score<0
replace invest_score = 0 if invest_score<0
format earnings_2019 - MVA_2019 %16.0g
gen invest_pct = .
replace invest_pct = invest_score/invest_available
gen pay_pct = .
replace pay_pct = pay_score/pay_available
gen fund_path_pct = .
replace fund_path_pct = fund_path_score/fund_path_available

*I believe this takes care of the sustainability scores*
save RSR_Sustainability_Scores.dta, replace

*Last, we want to port in the benefits data to get our last remaining items. Then we can merge things and reorganize to make sure we have all the data*
*Oddly, comma and other delimited files don't work here given the use of commas in the string variables*
clear
import excel using RSR_Benefit_General_Final.xlsx, firstrow
*We need to destring the EquableClassID and rename it all lower-case for any/all merges*
rename EquableClassID equableclassid
destring equableclassid, replace
save RSR_Benefit_General.dta, replace

*Now let's build out the remaining variables we need*
*Vesting*
*Note that we went and dropped vesting from the benefit general file. This isn't an issue though, as we can just grab it from the features data and calculate scores based off there*
clear
use Features.dta
keep equableclassid fas_vest dcgr_fullvest dcgr_gradevest plantype
drop if equableclassid==.
*Of note, we want to treat GR plans like FAS plans when it comes to scoring vesting*
replace fas_vest = dcgr_fullvest if plantype=="GR"
gen fas_vesting_score = .
replace fas_vesting_score = 0 if fas_vest>=10 
replace fas_vesting_score = 1 if fas_vest>=6 & fas_vest<10
replace fas_vesting_score = 3 if fas_vest>=4 & fas_vest<6
replace fas_vesting_score = 5 if fas_vest<4
gen dcgr_vesting_score = .
replace dcgr_vesting_score = 0 if dcgr_fullvest>=10
replace dcgr_vesting_score = . if dcgr_fullvest==.
replace dcgr_vesting_score = 1 if dcgr_fullvest>=6 & dcgr_fullvest<10
replace dcgr_vesting_score = 2 if dcgr_fullvest>=6 & dcgr_fullvest<10 & dcgr_gradevest==1
replace dcgr_vesting_score = 3 if dcgr_fullvest>=4 & dcgr_fullvest<6
replace dcgr_vesting_score = 4 if dcgr_fullvest>=4 & dcgr_fullvest<6 & dcgr_gradevest==1
replace dcgr_vesting_score = 5 if dcgr_fullvest<4
gen vesting_score = .
replace vesting_score = fas_vesting_score if plantype=="FAS"
replace vesting_score = dcgr_vesting_score if plantype=="DC" | plantype=="GR"
replace vesting_score = dcgr_vesting_score if dcgr_vesting_score<fas_vesting_score & plantype=="Hybrid"
replace vesting_score = fas_vesting_score if dcgr_vesting_score>fas_vesting_score & plantype=="Hybrid"
replace vesting_score = fas_vesting_score if dcgr_vesting_score==fas_vesting_score & plantype=="Hybrid"
gen vesting_available = 5
gen vesting_pct = vesting_score/vesting_available
gen vesting_years = .
replace vesting_years = fas_vest if plantype=="FAS" | plantype=="GR"
replace vesting_years = dcgr_fullvest if plantype=="DC"
replace vesting_years = dcgr_fullvest if dcgr_fullvest>fas_vest & plantype=="Hybrid"
replace vesting_years = fas_vest if dcgr_fullvest<fas_vest & plantype=="Hybrid"
replace vesting_years = fas_vest if dcgr_fullvest==fas_vest & plantype=="Hybrid"
keep equableclassid vesting_score vesting_available vesting_pct vesting_years
save RSR_Vesting_Scores.dta, replace

*Refunding Policy*
*For FAS and GR plans the we can do some of the work via code and other portions by hand - as we somehow overlooked the actual policy variable in the data collection*
*Essentially, we work off the fas_creditingrate. Any plan that has a non-zero crediting rate will be assumed to be giving a refund plus interest, those with a crediting rate of zero will be assumed a refund with no interest*
*Beyond that, we will verify the scores, upgrade any plans that offer a portion of the employer contributions, and downgrade any plans that don't offer refunds*
*Any modifications will be made in the script below for the sake of transparency*
clear
use Features.dta
keep equableclassid plan_fullname class_name plantype fas_creditingrate
drop if plantype=="DC"
gen refund_score = .
replace refund_score = 0 if fas_creditingrate==0
replace refund_score = 2 if fas_creditingrate>0
*Note that GR plans, by definition, are applying interest to the member's contributions*
*But the employer contributions depend on vesting*
replace refund_score = 5 if plantype=="GR"
*Here is where we will make any changes to scores*
replace refund_score = 4 if equableclassid==6010500 | equableclassid==6010100 | equableclassid==6010300 | equableclassid==6010400 | equableclassid==36020190 | equableclassid==36020290 | equableclassid==36020390 | equableclassid==36020100 | equableclassid==36020200 | equableclassid==36040190 | equableclassid==38010190 | equableclassid==38010290 | equableclassid==38010390 | equableclassid==42010190 | equableclassid==42010290 | equableclassid==42010390 | equableclassid==48010102
replace refund_score = 5 if equableclassid== 11010190 | equableclassid==11010290
gen refund_score_available = 5
gen refund_score_pct = refund_score/refund_score_available
gen refund_policy_text = ""
replace refund_policy_text = "All member contributions refunded without interest" if refund_score==0
replace refund_policy_text = "All member contributions refunded with interest" if refund_score==2
replace refund_policy_text = "A share of employer contributions and all member contributions refunded with interest" if refund_score==4
replace refund_policy_text = "100% of all employer and member contributions are refunded with interest or returns on investments" if refund_score==5
keep equableclassid refund_score refund_score_available refund_score_pct refund_policy_text
save RSR_Refund_Scores.dta, replace

*Then for DC plans we do it based on the graded vesting point*
clear
use Features.dta
keep if plantype=="DC" | plantype=="Hybrid"
keep equableclassid class_name fas_vest dcgr_fullvest dcgr_gradevest
gen dc_refund_score = .
replace dc_refund_score = 0 if dcgr_fullvest>=10 & dcgr_gradevest==1
replace dc_refund_score = 1 if dcgr_fullvest>=6 & dcgr_fullvest<10 & dcgr_gradevest==1
replace dc_refund_score = 2 if dcgr_fullvest>=4 & dcgr_fullvest<6 & dcgr_gradevest==1
replace dc_refund_score = 3 if dcgr_fullvest>1 & dcgr_fullvest<4 & dcgr_gradevest==1
replace dc_refund_score = 4 if dcgr_fullvest>0 & dcgr_fullvest<=1
replace dc_refund_score = 5 if dcgr_fullvest==0
replace dc_refund_score = 0 if dcgr_fullvest>=10 & dcgr_gradevest==0
replace dc_refund_score = 1 if dcgr_fullvest>=6 & dcgr_fullvest<10 & dcgr_gradevest==0
replace dc_refund_score = 2 if dcgr_fullvest>=4 & dcgr_fullvest<6 & dcgr_gradevest==0
replace dc_refund_score = 3 if dcgr_fullvest>1 & dcgr_fullvest<4 & dcgr_gradevest==0
replace dc_refund_score = 4 if dcgr_fullvest>0 & dcgr_fullvest<=0
replace dc_refund_score = 5 if dcgr_fullvest==0
gen dc_refund_available = 5
gen dc_refund_pct = dc_refund_score/dc_refund_available
gen dc_refund_policy_text = ""
replace dc_refund_policy_text = "Graded vesting of employer contributions requires 10 or more years" if dc_refund_score==0 & dcgr_gradevest==1
replace dc_refund_policy_text = "No graded vesting" if dc_refund_score==0 & dcgr_gradevest==0
replace dc_refund_policy_text = "Graded vesting of employer contributions is 4 or 5 years" if dc_refund_score==2
replace dc_refund_policy_text = "Graded vesting of employer contributions is 3 years or less" if dc_refund_score==3
replace dc_refund_policy_text = "Employer contributions are not immediate, but in one year or less" if dc_refund_score==4
replace dc_refund_policy_text = "Immediate vesting of employer contributions" if dc_refund_score==5
keep equableclassid dc_refund_score dc_refund_available dc_refund_pct dc_refund_policy_text
save RSR_DC_Refund_Scores.dta, replace

*Now the crediting rate scores*
clear
use Features.dta
keep if plantype=="FAS" | plantype=="Hybrid"
keep equableclassid class_name fas_creditingrate inflation
rename fas_creditingrate credit_rate
rename inflation assumed_inflation
gen credit_rate_score = .
replace credit_rate_score = 0 if credit_rate==0
replace credit_rate_score = 2 if credit_rate>0 & credit_rate<assumed_inflation
replace credit_rate_score = 3 if credit_rate>=assumed_inflation & credit_rate<(assumed_inflation+.01)
replace credit_rate_score = 4 if credit_rate>=(assumed_inflation+.01) & credit_rate<(assumed_inflation+.02)
replace credit_rate_score = 5 if credit_rate>=(assumed_inflation+.02)
gen credit_rate_available = 5
gen credit_rate_pct = credit_rate_score/credit_rate_available
keep equableclassid credit_rate_score credit_rate_available credit_rate_pct credit_rate assumed_inflation
save RSR_Credit_Rate_Scores.dta, replace

*And minimum GR Rate scores*
clear
*Note that the features data only had the GR crediting rate adjusted for the upside*
*The minimum rate is reported in the general data*
use RSR_Benefit_General.dta
drop if equableclassid<1000
rename Plan_Type plantype
keep if plantype=="GR" | plantype=="Hybrid"
rename Class_Name class_name
rename GRCreditingInterestRate gr_credit
keep equableclassid plantype class_name gr_credit
*We have to fix NE PERS State to be able to destring things*
replace gr_credit = .05 if equableclassid==28010260
*Now we need to merge in the features data to get the inflation assumptions*
drop class_name plantype
joinby equableclassid using Features.dta, unmatched(both) 
drop _merge
keep if plantype=="GR" | plantype=="Hybrid"
keep equableclassid plantype class_name gr_credit arr inflation
drop if gr_credit==. & plantype=="Hybrid"

*Lastly, to produce the scores*
rename gr_credit mingr_rate
gen mingr_score = .
replace mingr_score = 0 if mingr_rate==0
replace mingr_score = 2 if mingr_rate<inflation & mingr_rate>0
replace mingr_score = 3 if mingr_rate>inflation & mingr_rate<(arr-.015)
replace mingr_score = 4 if mingr_rate>=(arr-.015) & mingr_rate<(arr+.005)
replace mingr_score = 5 if mingr_rate>=(arr+.005)
gen mingr_score_available = 5
gen mingr_pct = mingr_score/mingr_score_available
keep equableclassid mingr_score mingr_score_available mingr_pct mingr_rate arr inflation
save RSR_Min_GR_Scores.dta, replace

*Then COLA scores*
clear
use RSR_Benefit_General.dta
drop if equableclassid<1000
keep equableclassid Plan_Type Class_Name DBCOLAAmountProvisions - DBCOLADistributionProvision
rename Plan_Type plantype
keep if plantype=="FAS" | plantype=="Hybrid" | plantype=="GR"
rename DBCOLAPolicyProvisions cola_policy_1
rename DBCOLADistributionProvision cola_policy_2
replace cola_policy_2 = "Ad hoc" if cola_policy_2=="Ad Hoc"
rename COLA_Amount cola_value
rename DBCOLAAccumulationProv* compound_cola
gen cola_score = .
replace cola_score = 0 if cola_policy_1=="No COLA" | cola_policy_1=="No provision" 
replace cola_score = 1 if cola_policy_1=="Ad hoc"
replace cola_score = 2 if cola_policy_1=="Linked to funded status"
replace cola_score = 3 if cola_policy_1=="Fixed" & cola_value<.02
replace cola_score = 4 if cola_policy_1=="Linked to inflation" | cola_policy_1=="Linked to inflation and funded status"
replace cola_score = 5 if cola_policy_1=="Fixed" & cola_value>=.02
gen cola_available = 5
gen cola_available_pct = cola_score/cola_available
*We still want to build out the COLA text to show for COLA_Policy and COLA_Value*
rename cola_value cola_value_estimate
rename DBCOLAAmountProvisions cola_value
gen comma = ", "
egen cola_policy = concat(cola_policy_2 comma cola_policy_1)
replace cola_policy = "Ad hoc COLA" if cola_policy=="Ad Hoc, Ad hoc"
replace cola_policy = "No COLA" if cola_policy=="No provision, No COLA"
replace cola_policy = "No COLA" if cola_policy=="None, No COLA"
replace cola_policy = "Ad hoc COLA" if cola_policy_1=="Ad hoc" | cola_policy_2=="Ad Hoc"
*This covers what we need*
*Now to wittle it down to just the values we need for the dataset*
keep equableclassid cola_value cola_score cola_available cola_available_pct cola_policy
save RSR_COLA_Scores.dta, replace

*Next are risk-sharing scores*
clear
use RSR_Benefit_General.dta
drop if equableclassid<1000
keep equableclassid Plan_Type Class_Name RiskSharingToolRelatedtoBen RiskSharingToolDetails
rename RiskSharingToolDetails risk_status
rename Plan_Type plantype
replace risk_status = "DC plans do not expose the retirement system to risk of underfunding" if plantype=="DC"
replace risk_status = "Plan does not have risk-sharing tools" if plantype=="FAS" & risk_status==""
replace risk_status = "Auto-adjusted COLA that is tied to plan funded status" if risk_status=="Auto-Adjusted COLA Pegged to Funded Status"
replace risk_status = "Hybrid plans mitigate risk by utilizing multiple retirement plan designs" if plantype=="Hybrid"
replace risk_status = "GR plans offer upside sharing opportunities for strong investment performance, with other investment gains offsetting underfunding risk" if risk_status=="Return Upside Share"
gen risk_score = .
replace risk_score = 2 if RiskSharingToolRelatedtoBen==1
replace risk_score = 0 if RiskSharingToolRelatedtoBen==0
gen risk_available = 2
replace risk_score = . if plantype=="DC"
replace risk_available = . if plantype=="DC"
gen risk_pct = risk_score/risk_available
keep equableclassid risk_score risk_available risk_pct risk_status
save RSR_Risk_Scores.dta, replace

*The only things left are just text items for the non-scored section - member contribution rates, ssa status (formatted), supplemental status, annuity offerings, and default investments*
clear
use RSR_Benefit_General.dta
keep equableclassid Plan_Type Class_Name State RetirementSystem Plan_Name Occupation Tier_HireDates SocialSecurityCoverage ChoiceStructureMandatory0 ChoicesAvailablebyPlanID DCDefaultintoTargetDateFund FASBenefitSupplementBinary Annuity_Access
rename RetirementSystemName System_Name
rename ChoiceStructureMandatory0 Plan_Choice
rename ChoicesAvailablebyPlanID Alt_PlanLink
gen Optionality_Language = "Members of this plan do not have the option of other primary income retirement plans, though they may have supplemental benefits available to them.." if Plan_Choice==0
replace Optionality_Language = "Members of this plan can choose an alternative  plan that would be their primary source of retirement income." if Plan_Choice==1 | Plan_Choice==2
gen Options_Status = "have" if Plan_Choice==1 | Plan_Choice==2
replace Options_Status = "do not have" if Plan_Choice==0
rename FASBenefitSupplementBinary Supplement
gen Sup_Status = "does" if Supplement==1
replace Sup_Status = "does not have" if Supplement==0
rename SocialSecurityCoverage SSA
gen SSA_Status = "required" if SSA=="Yes"
replace SSA_Status = "mixed" if SSA=="Mixed" 
replace SSA_Status = "not available" if SSA=="No"
gen SSA_Analysis = "members pay into their state retirement plan and contribute payroll taxes to the federal government. Upon retiring, individuals will receive income from both the state plan and federal plan. Depending on the size of state retirement benefits, the federal government might reduce Social Security income." if SSA_Status=="required"
replace SSA_Analysis = "members are not enrolled in Social Security and do not have to make the typical 6.2% of payroll contribution into the federal retirement system. Individuals may still qualify for Social Security benefits if they work enough years in the private sector before or after their public service job." if SSA_Status=="not available"
replace SSA_Analysis = "that the Social Security status of covered employees in this retirement plan is will vary from jurisdiction-to-jurisdiction. Individuals should confirm with their employer what their status is if they do not know. For the sake of our analysis, we will evaluate the security of this retirement plan as though members are NOT enrolled." if SSA_Status=="mixed"
rename DCDefaultintoTargetDateFund DCDefault
gen Default_Detail = "a target date fund." if DCDefault==1
replace Default_Detail = "a simple portfolio mix." if DCDefault==2
replace Default_Detail = "an unclear status." if DCDefault==0
gen Annuity_Status = "do" if Annuity_Access==1
replace Annuity_Status = "do not" if Annuity_Access==0
keep equableclassid Plan_Name Plan_Type Class_Name State System_Name Tier_HireDates Occupation Optionality_Language Alt_PlanLink Options_Status Sup_Status SSA_Status SSA_Analysis Default_Detail Annuity_Status
save RSR_Plan_Details.dta, replace

*We also want to add two variables that are attentive to the Legacy_Plan and Municipal_Plan variables*
clear
use RSR_Benefit_General.dta
drop if equableclassid>1000
keep equableclassid State RetirementSystemName Plan_Name Class_Name Plan_Type Legacy_Plan Municipal_Plan
rename RetirementSystemName System_Name
gen Legacy_Text=""
replace Legacy_Text = "N/A" if Legacy_Plan==0
replace Legacy_Text = "Your plan is no longer offered to new hires in your state. This doesn't affect the value of your benefits, but we haven't added it to the RSR coverage yet. If you would like information regarding the benefits this plan offers or to be alerted when we've added to the RSR, contact us." if Legacy_Plan==1
gen Municipal_Text=""
replace Municipal_Text = "N/A" if Municipal_Plan==1
replace Municipal_Text = "We are steadily building out the number of plans covered by the RSR. We haven't quite gotten to your plan yet, though we intend to do so in the near future. If you would like information regarding the benefits this plan offers or to be alerted when we've added to the RSR, contact us." if Municipal_Plan==1
keep equableclassid State System_Name Plan_Name Class_Name Plan_Type Legacy_Plan Legacy_Text Municipal_Plan Municipal_Text
save RSR_Legacy_Municipal_Plans.dta, replace

*We also need a few smaller bits from the Features data*
clear
use Features.dta
keep equableclassid class_name plantype fas_eecont1 dcgr_eecont ssa_enroll
gen Member_Cont_Rate = .
replace Member_Cont_Rate = fas_eecont1 if plantype=="FAS"
replace Member_Cont_Rate = dcgr_eecont if plantype=="DC" | plantype=="GR"
replace Member_Cont_Rate = fas_eecont1 + dcgr_eecont if plantype=="Hybrid"
*We also want to make some averages and ranges for the contribution rate graphic across different SSA enrollment*
by ssa_enroll, sort: sum Member_Cont_Rate
gen Average_Cont_Rate = .
replace Average_Cont_Rate = .073037 if ssa_enroll=="Mixed"
replace Average_Cont_Rate = .0952095 if ssa_enroll=="No"
replace Average_Cont_Rate = .0631059 if ssa_enroll=="Yes"
gen Cont_Rate_Upper_Bound = .
replace Cont_Rate_Upper_Bound = .178 if ssa_enroll=="Mixed"
replace Cont_Rate_Upper_Bound = .2025 if ssa_enroll=="No"
replace Cont_Rate_Upper_Bound = .125 if ssa_enroll=="Yes"
gen Cont_Rate_Lower_Bound = .
replace Cont_Rate_Lower_Bound = 0 if ssa_enroll=="Mixed"
replace Cont_Rate_Lower_Bound = 0 if ssa_enroll=="No"
replace Cont_Rate_Lower_Bound = 0 if ssa_enroll=="Yes"
keep equableclassid Member_Cont_Rate Average_Cont_Rate Cont_Rate_Upper_Bound Cont_Rate_Lower_Bound
save RSR_Member_Cont_Rate.dta, replace

*I believe all is ready for merging and cleaning at this point*
clear
use RSR_Benefit_Scores.dta
joinby equableclassid using RSR_Sustainability_Scores.dta, unmatched(both)
drop _merge
joinby equableclassid using RSR_Vesting_Scores.dta, unmatched(both)
drop _merge
joinby equableclassid using RSR_Refund_Scores.dta, unmatched(both)
drop _merge
joinby equableclassid using RSR_DC_Refund_Scores.dta, unmatched(both)
drop _merge
joinby equableclassid using RSR_Credit_Rate_Scores.dta, unmatched(both)
drop _merge
joinby equableclassid using RSR_Min_GR_Scores.dta, unmatched(both)
drop _merge
joinby equableclassid using RSR_COLA_Scores.dta, unmatched(both)
drop _merge
joinby equableclassid using RSR_Risk_Scores.dta, unmatched(both)
drop _merge
joinby equableclassid using RSR_Plan_Details.dta, unmatched(both)
drop _merge
joinby equableclassid using RSR_Member_Cont_Rate.dta, unmatched(both)
drop _merge

*I think that covers everything except for generating the total scores*
*While it would be possible to add the composite scores together in Excel, it doesn't handle missing values well, especially after we add the "N/A" designations for the web developers*
*So we can go ahead and put all the additive scores together here*
*We will work through them backwards as the eventual final piece is a Total score for each profile*

*First up, sustainability*
*Again, while this would differ by plantype in Excel, here, since there are just missing values and because Stata treats those as 0 for the sake of adding, we can do this very simply*
gen sustain_total = .
replace sustain_total = risk_score+pay_score+invest_score+fund_path_score
replace sustain_total = . if Plan_Type=="DC"
replace sustain_total = . if equableclassid<1000
gen sustain_available = 27
replace sustain_available = . if equableclassid<1000
gen sustain_pct = .
replace sustain_pct = sustain_total/sustain_available

*Next up let's get the short-term totals*
gen short_ben_score25 = .
replace short_ben_score25 = shortadequacy25score 
gen short_ben_score40 = .
replace short_ben_score40 = shortadequacy40score
gen short_ben_available = 15
replace short_ben_available = . if equableclassid<1000
gen short_ben_pct25 = short_ben_score25/short_ben_available
gen short_ben_pct40 = short_ben_score40/short_ben_available
gen short_flex_score = .
replace short_flex_score = refund_score + credit_rate_score if Plan_Type=="FAS"
replace short_flex_score = dc_refund_score if Plan_Type=="DC"
replace short_flex_score = refund_score + mingr_score if Plan_Type=="GR"
replace short_flex_score = refund_score + credit_rate_score if Plan_Type=="Hybrid"
replace short_flex_score = . if equableclassid<1000
gen short_flex_available = .
replace short_flex_available = 10 if Plan_Type=="FAS"
replace short_flex_available = 5 if Plan_Type=="DC"
replace short_flex_available = 10 if Plan_Type=="GR"
replace short_flex_available = 10 if Plan_Type=="Hybrid"
replace short_flex_available = . if equableclassid<1000
gen short_flex_pct = short_flex_score/short_flex_available
gen short_total_score25 = .
replace short_total_score25 = vesting_score + short_ben_score25 + short_flex_score
replace short_total_score25 = . if equableclassid<1000
gen short_total_score40 = .
replace short_total_score40 = vesting_score + short_ben_score40 + short_flex_score
replace short_total_score40 = . if equableclassid<1000
gen short_total_available = .
replace short_total_available = 30 if Plan_Type=="FAS"
replace short_total_available = 25 if Plan_Type=="DC"
replace short_total_available = 30 if Plan_Type=="GR"
replace short_total_available = 30 if Plan_Type=="Hybrid"
replace short_total_available = . if equableclassid<1000
replace short_total_score25 = short_total_available if short_total_score25>short_total_available
replace short_total_score40 = short_total_available if short_total_score40>short_total_available
gen short_total_pct25 = .
replace short_total_pct25 = short_total_score25/short_total_available
replace short_total_pct25 = . if equableclassid<1000
gen short_total_pct40 = .
replace short_total_pct40 = short_total_score40/short_total_available
replace short_total_pct40 = . if equableclassid<1000

*Then medium-term totals*
gen medium_ben_score25 = .
replace medium_ben_score25 = mediumadequacy25score + cola_score if Plan_Type=="FAS" | Plan_Type=="Hybrid" | Plan_Type=="GR"
replace medium_ben_score25 = mediumadequacy25score if Plan_Type=="DC" 
replace medium_ben_score25 = . if equableclassid<1000
gen medium_ben_score40 = .
replace medium_ben_score40 = mediumadequacy40score + cola_score if Plan_Type=="FAS" | Plan_Type=="Hybrid" | Plan_Type=="GR"
replace medium_ben_score40 = mediumadequacy40score if Plan_Type=="DC" 
replace medium_ben_score40 = . if equableclassid<1000
gen medium_ben_available = .
replace medium_ben_available = 20 if Plan_Type=="FAS" | Plan_Type=="Hybrid" | Plan_Type=="GR"
replace medium_ben_available = 15 if Plan_Type=="DC"
replace medium_ben_available = . if equableclassid<1000
gen medium_ben_pct25 = medium_ben_score25/medium_ben_available
gen medium_ben_pct40 = medium_ben_score40/medium_ben_available
gen medium_flex_score = .
replace medium_flex_score = short_flex_score
gen medium_flex_available = .
replace medium_flex_available = short_flex_available
gen medium_flex_pct = medium_flex_score/medium_flex_available
gen medium_total_score25 = .
replace medium_total_score25 = medium_ben_score25 + medium_flex_score
replace medium_total_score25 = . if equableclassid<1000
gen medium_total_score40 = .
replace medium_total_score40 = medium_ben_score40 + medium_flex_score
replace medium_total_score40 = . if equableclassid<1000
gen medium_total_available = .
replace medium_total_available = 30 if Plan_Type=="FAS" | Plan_Type=="Hybrid"
replace medium_total_available = 20 if Plan_Type=="DC"
replace medium_total_available = 30 if Plan_Type=="GR"
replace medium_total_available = . if equableclassid<1000
replace medium_total_score25 = medium_total_available if medium_total_score25>medium_total_available
replace medium_total_score40 = medium_total_available if medium_total_score40>medium_total_available
gen medium_total_pct25 = .
replace medium_total_pct25 = medium_total_score25/medium_total_available
replace medium_total_pct25 = . if equableclassid<1000
gen medium_total_pct40 = .
replace medium_total_pct40 = medium_total_score40/medium_total_available
replace medium_total_pct40 = . if equableclassid<1000

*And the full career totals*
gen full_ben_score25 = .
replace full_ben_score25 = fulladequacy25score + cola_score if Plan_Type=="FAS" | Plan_Type=="GR" | Plan_Type=="Hybrid"
replace full_ben_score25 = fulladequacy25score if Plan_Type=="DC"
replace full_ben_score25 = . if equableclassid<1000
gen full_ben_score40 = .
replace full_ben_score40 = fulladequacy40score + cola_score if Plan_Type=="FAS" | Plan_Type=="GR" | Plan_Type=="Hybrid"
replace full_ben_score40 = fulladequacy40score if Plan_Type=="DC"
replace full_ben_score40 = . if equableclassid<1000
gen full_total_score25 = .
replace full_total_score25 = full_ben_score25
gen full_total_score40 = .
replace full_total_score40 = full_ben_score40
gen full_total_available = .
replace full_total_available = 30 if Plan_Type=="FAS" | Plan_Type=="GR" | Plan_Type=="Hybrid"
replace full_total_available = 25 if Plan_Type=="DC"
replace full_total_available = . if equableclassid<1000
gen full_ben_pct25 = full_ben_score25/full_total_available
gen full_ben_pct40 = full_ben_score40/full_total_available
replace full_total_score25 = full_total_available if full_total_score25>full_total_available
replace full_total_score40 = full_total_available if full_total_score40>full_total_available
gen full_total_pct25 = .
replace full_total_pct25 = full_total_score25/full_total_available
replace full_total_pct25 = 1 if full_total_pct25>1
replace full_total_pct25 = . if equableclassid<1000
gen full_total_pct40 = .
replace full_total_pct40 = full_total_score40/full_total_available
replace full_total_pct40 = 1 if full_total_pct40>1
replace full_total_pct40 = . if equableclassid<1000

*Lastly, we can fill in the assessments too*
gen short_assess25 = ""
replace short_assess25 = "serves Short-Term Workers well." if short_total_pct25>=0.75
replace short_assess25 = "serves Short-Term Workers moderately well." if short_total_pct25>=0.5 & short_total_pct25<0.75
replace short_assess25 = "does not serve Short-Term Workers well." if short_total_pct25<0.5
replace short_assess25 = "" if equableclassid<1000
gen short_assess40 = ""
replace short_assess40 = "serves Short-Term Workers well." if short_total_pct40>=0.75
replace short_assess40 = "serves Short-Term Workers moderately well." if short_total_pct40>=0.5 & short_total_pct40<0.75
replace short_assess40 = "does not serve Short-Term Workers well." if short_total_pct40<0.5
replace short_assess40 = "" if equableclassid<1000
gen medium_assess25 = ""
replace medium_assess25 = "serves Medium-Term Workers well." if medium_total_pct25>=0.75
replace medium_assess25 = "serves Medium-Term Workers moderately well." if medium_total_pct25>=0.5 & medium_total_pct25<0.75
replace medium_assess25 = "does not serve Medium-Term Workers well." if medium_total_pct25<0.5
replace medium_assess25 = "" if equableclassid<1000
gen medium_assess40 = ""
replace medium_assess40 = "serves Medium-Term Workers well." if medium_total_pct40>=0.75
replace medium_assess40 = "serves Medium-Term Workers moderately well." if medium_total_pct40>=0.5 & medium_total_pct40<0.75
replace medium_assess40 = "does not serve Medium-Term Workers well." if medium_total_pct40<0.5
replace medium_assess40 = "" if equableclassid<1000
gen full_assess25 = ""
replace full_assess25 = "serves Full Career Workers well." if full_total_pct25>=0.75
replace full_assess25 = "serves Full Career Workers moderately well." if full_total_pct25>=0.5 & full_total_pct25<0.75
replace full_assess25 = "does not serve Full Career Workers well." if full_total_pct25<0.5
replace full_assess25 = "" if equableclassid<1000
gen full_assess40 = ""
replace full_assess40 = "serves Full Career Workers well." if full_total_pct40>=0.75
replace full_assess40 = "serves Full Career Workers moderately well." if full_total_pct40>=0.5 & full_total_pct40<0.75
replace full_assess40 = "does not serve Full Career Workers well." if full_total_pct40<0.5
replace full_assess40 = "" if equableclassid<1000
gen sustain_assess = ""
replace sustain_assess = "possibly distressed and improvement should be a high priority." if sustain_pct<0.5
replace sustain_assess = "are stable today but are likely fragile, with clear areas for improvement." if sustain_pct>=0.5
replace sustain_assess = "are positive and reasonably strong." if sustain_pct>=0.75
replace sustain_assess = "N/A" if Plan_Type=="DC"
replace sustain_assess = "" if equableclassid<1000
gen sustain_assess_short = ""
replace sustain_assess_short = "Possibly distressed" if sustain_pct<0.5
replace sustain_assess_short = "Likely fragile" if sustain_pct>=0.5
replace sustain_assess_short = "Reasonably strong" if sustain_pct>=0.75
replace sustain_assess_short = "N/A" if Plan_Type=="DC"
replace sustain_assess_short = "" if equableclassid<1000


*We also can add the Notes variable now*
gen Notes = ""
replace Notes = "This is a retirement plan offered to judges. Judges do not follow the same career path as other public employees, often not being appointed or elected to their position until much later in life. However, we include this plan to illustrate the quality of the benefits being offered, even if it is highly unlikely for the plan to have 25-year-old entrants or members that would require it to provide a secure retirement income." if Occupation=="judges"
replace Notes = "This is a retirement plan offered to elected officials. In most cases elected officials do not follow the same career path as other public employees, often only serving for a short period of time. However, we include this plan to illustrate the quality of the benefits being offered, even if it is highly unlikely for the plan to have members enrolled for an entire career." if Occupation=="local elected officials" | Occupation=="state elected officials"
replace Notes = "This plan is offered to teachers, faculty, and seasonal instructors who do not qualify as full-time employees, according to CalSTRS guidelines. The plan is not necessarily intended to provide adequate retirement income, but we have included it in the RSR since it is a major plan offered to some California educators." if Class_Name=="CalSTRS GR Option"

*We also need to collapse our two inflation variables*
replace assumed_inflation = inflation if assumed_inflation==. & inflation!=.
drop inflation

*I believe this gets us to where we want for the scores*
*Now to move things around and rename stuff in Excel*
save RSR_Complete_Totals.dta, replace

*Now we just have to join in the Legacy and Municipal Plans*
joinby equableclassid State System_Name Plan_Name Class_Name Plan_Type using RSR_Legacy_Municipal_Plans.dta, unmatched(both)
drop _merge

*Note that the equableclassid variable values for the legacy and municipal plans are simply placeholder values*
replace equableclassid = . if Legacy_Plan==1
replace equableclassid = . if Municipal_Plan==1

*Last adjustments and fixes I discovered were still missing or that needed adjustment in converting to Excel*
replace Plan_Type = "traditional pension plan" if Plan_Type=="FAS"
replace Plan_Type = "defined contribution plan" if Plan_Type=="DC"
replace Plan_Type = "guaranteed return plan" if Plan_Type=="GR"
replace Plan_Type = "hybrid plan" if Plan_Type=="Hybrid"

drop short_ben_avail - repl_rate_pct_40

*That should do it*
*Everything is here*
*Huzzah!*
save RSR_Complete.dta, replace

*That should cover everything*
