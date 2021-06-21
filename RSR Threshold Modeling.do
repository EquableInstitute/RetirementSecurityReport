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
