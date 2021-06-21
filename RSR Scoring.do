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
