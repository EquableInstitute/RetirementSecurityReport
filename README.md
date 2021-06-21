# RetirementSecurityReport
These are the source data and code files used to compile the first edition of Equable's Retirement Security Report. All scripts will need updating to reference the correct file pathways and locations on your machine to run properly.

Please note that code is provided using both R and Stata. Notation is provided throughout each script, but any added questions should be directed to jon@equable.org. 

The breakdown of files is as follows:

R Scripts:
 - Updated All Benefits Model_24May21.R - This script contains all the necessary code to go from the source data files (referenced below) through benefit model outputs. Note, however, that the translation of data to the specific scores presented in the RSR is not currently included in this script. It will be added at a later date.
 
 Stata Scripts:
 - RSR Complete.do - This script contains the necessary code to go from the source data files through fully transformed RSR scores. It will produce data outputs at numerous stages of the process to allow for further examination.

If a more piece-meal approach is preferred, the Stata scripts are also provided in three separate pieces.
 - RSR Plan Modeling - This script contains the necessary code to go from the source data files through benefit model outputs.
 - RSR Threshold Modeling - This script contains the necessary code to go from benefit model outputs through the adequacy threshold modeling stages.
 - RSR Scoring - This script contains the necessary code to translate the source data files, benefit model outputs, and adequacy threshold data into the scores presented in the RSR.

Data:
Note that all data for the project are provided in .csv format to maximize accessibility for all users.
- Features_Final.csv - This file contains the primary benefit design information related to estimating net present value and amortized benefit totals for the 335 plans currently included in the RSR. These include things like vesting points, employee and employer contribution rates, benefit multipliers, guaranteed return rates, inflation assumptions, and assumed rates of return. All data are drawn from publications produced by each retirement plan or their consulting actuaries.
- Salary_Scales_Final.csv - This file contains the salary increase schedules used by each plan in their actuarial valuation reports. These reflect the rates at which plan members' salaries are assumed to increase over the course of their careers.
- Starting_Salaries_Final.csv - This file contains the average starting salaries for 25-year-old and 40-year-old entrants for each retirement system.
- Pub-2010-Mortality.csv - This file contains the PUB 2010 mortality assumptions as produced by the Society of Actuaries. It has been adapted for the RSR to provide mortality rates for female entrants in three different occupational categories - general, teachers, and public safety.
- RSR_Benefit_General_Final.csv - This file contains additional policy provisions or benefit information required to complete the policy scoring elements of the RSR. **NOTE that the script references an Excel version of this file, so an Excel version is included here as well.***

Please direct any questions related to these scripts or data files to jon@equable.org
