library(tidyverse)
options(scipen = 999)
options(digits = 15)

#Make sure to save the different datasets in your working directory if you are intending to read them in in the manner below.  
#Read in Features Data
feat <- read.csv("Features_Final.csv")
#Rename EquableClassID variable so that it is actually EquableClassID
names(feat)[names(feat) == "ï..EquableClassID"] <- "EquableClassID"
#Here is just a quick patch for updating Louisiana Muni Police's Additive_retroactive Multiplier variable, which is currently NA but should be 0.
feat$FAS_Additive_Multipliers[feat$EquableClassID == 19070100] <- 0

#Read In SalaryScales
pay <- read.csv("Salary_Scales_Final.csv")
#Rename EquableClassID variable so that it is actually EquableClassID
names(pay)[names(pay) == "ï..EquableClassID"] <- "EquableClassID"

#Read In Starting Salaries
startsal <- read.csv("Starting_Salaries_Final.csv")
#Rename EquableClassID variable so that it is actually EquableClassID
names(startsal)[names(startsal) == "ï..EquableClassID"] <- "EquableClassID"

#Read In Pub 2010 Mortality Table
mort <- read.csv("Pub-2010-Mortality.csv")

#Lets First Merge Starting Salary with Features
M1 <- merge(feat,startsal, by=c("EquableClassID"))
M1<- M1 %>%
  select(-Plan_FullName.y, -Class_Name.y)
names(M1)[names(M1) == "Plan_FullName.x"] <- "Plan_FullName"
names(M1)[names(M1) == "Class_Name.x"] <- "Class_Name"
#And drop the duplicate Variables we don't need
#M1 <- M1 %>%
  #select(-Plan_FullName.y,-Class_Name.y)
pay2 <- pay %>%
  group_by(EquableClassID, Years.of.Service)

#Now let's merge in the salary scales as well
M2a <- merge(M1, pay2, by.x=c("EquableClassID"), by.y=c("EquableClassID"))
M2a<- M2a %>%
  select(-Plan_FullName.y)
names(M2a)[names(M2a) == "Plan_FullName.x"] <- "Plan_FullName"
#M2b <- merge(M1, pay2, by=c("EquableClassID", "Plan_FullName"))

#And let's rename Years.of.Service as YOS for simplicity
names(M2a)[names(M2a) == "Years.of.Service"] <- "YOS"
M2a <- M2a %>%
  group_by(EquableClassID)%>%
  arrange(EquableClassID, YOS)
#Here is a block of code that does all of the salary accruals really easily instead of the huge mess below
M2 <- M2a %>%
  group_by(EquableClassID)%>%
  mutate(Raise = cumprod(1+Annual.Rate),
         Sal25 = StartingSalary_25 * ifelse(is.na(lag(Raise)),1,lag(Raise)),
         Sal40 = StartingSalary_40 * ifelse(is.na(lag(Raise)),1,lag(Raise)))

#This is for Salary Caps
M2 <- M2 %>%
  group_by(EquableClassID)%>%
  arrange(EquableClassID, YOS)%>%
  mutate(Sal25 = case_when(
           Plan.Type == "FAS" & Sal25 >= FAS_Max_Pensionable_Salary ~ FAS_Max_Pensionable_Salary,
           Plan.Type == "FAS" & Sal25 < FAS_Max_Pensionable_Salary ~ Sal25,
           Plan.Type == "Hybrid" & Sal25 >= FAS_Max_Pensionable_Salary ~ FAS_Max_Pensionable_Salary,
           Plan.Type == "Hybrid" & Sal25 < FAS_Max_Pensionable_Salary ~ Sal25,
           Plan.Type == "DC" | Plan.Type == "GR" ~ Sal25
         ),
         Sal40 = case_when(
           Plan.Type == "FAS" & Sal40 >= FAS_Max_Pensionable_Salary ~ FAS_Max_Pensionable_Salary,
           Plan.Type == "FAS" & Sal40 < FAS_Max_Pensionable_Salary ~ Sal40,
           Plan.Type == "Hybrid" & Sal40 >= FAS_Max_Pensionable_Salary ~ FAS_Max_Pensionable_Salary,
           Plan.Type == "Hybrid" & Sal40 < FAS_Max_Pensionable_Salary ~ Sal40,
           Plan.Type == "DC" | Plan.Type == "GR"  ~ Sal40))
#Creating FAS

M2$FAS_Years[M2$FAS_Years == 2.5] <- 3
M2$FAS_Years[M2$FAS_Years == 3.5] <- 4
library(zoo)
M2 <- M2 %>% 
  group_by(EquableClassID) %>% 
  mutate(FAS25 = ifelse(is.na(FAS_Years), NA,
                        rollmean(Sal25, k = FAS_Years, fill = 0, align = "right"))) %>% 
  mutate(FAS25 = lag(FAS25, default = 0))%>%
  mutate(FAS40 = ifelse(is.na(FAS_Years), NA,
                        rollmean(Sal40, k = FAS_Years, fill = 0, align = "right"))) %>% 
  mutate(FAS40 = lag(FAS40, default = 0))%>%
  mutate(FAS40 = case_when(
    YOS >= FAS_Years ~ FAS40,
    YOS < FAS_Years ~ 0
  ))


#Mortality table time - First is 1 year survival rate
mort2<- mort %>% 
  group_by(Mortality_Category) %>% 
  mutate(S1y = cumprod(1 - lag(Mortality, default = 0)))
#And Life Expectancy
mort3 <- mort2 %>% 
  group_by(Mortality_Category) %>% 
  mutate(LifeExp = rev(cumsum(rev(S1y)))/S1y)

M3 <- M2 %>%
  select(EquableClassID, Plan_FullName, Mortality_Category, Class_Name, Plan.Type, DiscRate, FAS_CompoundCOLA, FAS_COLARate)
#Now we specify the merge. Please note, it is by Mortality Category only.
M4 <- merge(M3, mort3, by = ("Mortality_Category"))
#Here we are panel-setting AND arranging the data, by Equable ID and Class for panel_set, and both as well as age for arrangement
M4 <- M4 %>%
  group_by(EquableClassID, Class_Name) %>%
  arrange(EquableClassID, Class_Name, Age)
#Now we generate a new dataframe that drops all duplicate rows
M5 <-M4[!duplicated(M4),]

#Drop M4 because it eats up a lot of memory (due to said duplicate rows)
remove(M4)

#Now we make discounted Survival rate. Its just the survival rate over the quantity of 1 + the Discount rate, raised to the power of a given Age value - 20
M5$Survive <- M5$S1y/((1+M5$DiscRate)^(M5$Age - 20))

M6 <- M5 %>% 
  group_by(EquableClassID) %>% 
  mutate(Survive_COLA = case_when(
    FAS_CompoundCOLA == 1 ~ Survive * (1 + FAS_COLARate)^(Age - 20),
    FAS_CompoundCOLA == 0 ~ Survive * (1 + FAS_COLARate*(Age - 20))
  ),
  #Then calculate the annuity factor depending on the COLA rule (compound or simple). The simple COLA case is a bit more complicated. 
  AF = case_when(
    FAS_CompoundCOLA == 1 ~ rev(cumsum(rev(Survive_COLA)))/Survive_COLA,
    FAS_CompoundCOLA == 0 ~ (rev(cumsum(rev(Survive_COLA))) - rev(cumsum(rev(Survive)))*FAS_COLARate*(Age - 20))/Survive  
  )
  )

M10 <- M2

#here we are generating a YOS40 variable, so that we can do all of the needed calculations for age 40 entrants using this variable instead of the YOS variable that is set up to work for 25 year old entrants.
M10$YOS40 <- 0
#Here we artificially generate an Age Variable for merging in the M8 dataframe. Then we replace the values in YOS40 with -15:30, and lag our Sal40 and FAS40 values so that they start at the correct entry Age for 40 year old entrants (40 years old), instead of at 25 years old. Then we set the Sal40 and FAS40 values equal to these lagged values.
#Please note, it is very important to panel-set the data before doing this (hence the group_by(EquableID))
M10 <- M10 %>%
  group_by(EquableClassID) %>%
  arrange(YOS, .by_group = TRUE) %>%
  mutate(Age = rep(25:70))%>%
  mutate(YOS40 = rep(-15:30))%>%
  mutate(Sal40_L = lag(Sal40, 15))%>%
  mutate(FAS40_L = lag(FAS40, 15))%>%
  mutate(Sal40 = Sal40_L)%>%
  mutate(FAS40 = FAS40_L)

#Some additional clean up. Here we say for every instance where YOS40 is greater than or equal to zero we set it equal to itself.  This makes the values that are less than zero NAs by coercion.  We can then update all of the NAs into 0s.  We also drop the Lagged versions of Sal40 and FAS40, as we have already used them for what we needed them for.  
M10 <- M10%>%
  mutate(YOS40 = case_when(
    YOS40 >= 0 ~ YOS40
  ))%>%
  select(-Sal40_L, -FAS40_L)

M10$YOS40[is.na(M10$YOS40)] <- 0


M11 <- merge(M10,M6, by = c("EquableClassID","Age"), all.x = TRUE, all.y =FALSE)

M11 <- M11 %>%
  select(-Plan_FullName.y,-Plan.Type.y,-Mortality_Category.y,-Class_Name.y,-DiscRate.y,-FAS_COLARate.y,-FAS_CompoundCOLA.y)
names(M11)[names(M11) == "Plan_FullName.x"] <- "Plan_FullName"
names(M11)[names(M11) == "Mortality_Category.x"] <- "Mortality_Category"
names(M11)[names(M11) == "Plan.Type.x"] <- "Plan.Type"
names(M11)[names(M11) == "DiscRate.x"] <- "DiscRate"
names(M11)[names(M11) == "FAS_CompoundCOLA.x"] <- "FAS_CompoundCOLA"
names(M11)[names(M11) == "FAS_COLARate.x"] <- "FAS_COLARate"
names(M11)[names(M11) == "Class_Name.x"] <- "Class_Name"

M11$FAS_EECont1[M11$FAS_EECont1 == ""] <- 0
M11$FAS_EECont1[M11$FAS_EECont1 == "0%"] <- 0
M11$FAS_EECont1 <- as.numeric(M11$FAS_EECont1)
#M11$FAS_EECont1[is.na(M11$FAS_EECont1)] <- 0
#Now we need to account for there being plans hat have multiple different employee contribution rates, dependent on YOS
M11 <- M11 %>%
  group_by(EquableClassID) %>%
  mutate(FAS_EmpRate25 = case_when(
    FAS_EECont1_YOS != 99 & YOS >= FAS_EECont1_YOS  ~ (FAS_EECont2),
    FAS_EECont1_YOS != 99 & YOS < FAS_EECont1_YOS~ (FAS_EECont1),
    FAS_EECont1_YOS == 99 ~ FAS_EECont1))%>%
  mutate(FAS_EmpRate40 = case_when(
    FAS_EECont1_YOS != 99 & YOS40 >= FAS_EECont1_YOS  ~ (FAS_EECont2),
    FAS_EECont1_YOS != 99 & YOS40 < FAS_EECont1_YOS~ (FAS_EECont1),
    FAS_EECont1_YOS == 99 ~ FAS_EECont1))

#Declare a new variable, set it equal to zero, update it with the Employee Contribution rate times the Salary for a 25 year old entrant.  This gives us Employee Contribution in Dollars
M11$EmpCont25 <- 0
M11$EmpCont25 <- M11$FAS_EmpRate25*M11$Sal25

#Create an Adjusted Employee Contribution Variable
M11$AdjEmpCont25 <- 0

#Update the values in the Adjusted Employer Contribution value such that when YOS = 0, it is also equal to 0, but when YOS is greater than 0, it is equal to the Lagged Adjusted Employee Contribution in dollars times the quantity of 1 plus the credited interest rate, and then plus the  non-adjusted Lagged employer Contribution.
for (i in 0:46) {
  M11 <- M11 %>%
    group_by(EquableClassID) %>%
    mutate(EmpContL25 = lag(EmpCont25))%>%
    mutate(AdjEmpContL25 = lag(AdjEmpCont25))%>%
    mutate(AdjEmpCont25 = case_when(
      YOS == 0 ~ 0,
      YOS > 0 ~ (AdjEmpContL25*(1+FAS_CreditingRate)) + EmpContL25))
}

#Now we repeat the process but for Employer Contributions
M11$ERCont25 <- 0
M11$ERCont25 <- M11$FAS_EMCont*M11$Sal25

M11$AdjER25 <- 0
for (i in 0:46) {
  M11 <- M11 %>%
    group_by(EquableClassID) %>%
    mutate(ERContL25 = lag(ERCont25))%>%
    mutate(AdjERL25 = lag(AdjER25))%>%
    mutate(AdjER25 = case_when(
      YOS == 0 ~ 0,
      YOS > 0 ~ (AdjERL25*(1+FAS_CreditingRate)) + ERContL25))
}

#Here we are calculating the present value of Employee and Employer Contributions, which are just the adjusted contribution values divided by the quantity of 1 + the assumed price inflation raised to the Age-25
M11$PVEmpCont25 <- 0
M11$PVERCont25 <- 0
M11 <- M11 %>%
  group_by(EquableClassID) %>%
  mutate(PVEmpCont25 = AdjEmpCont25/((1+Inflation)^(Age - 25)))%>%
  mutate(PVERCont25 = AdjER25/((1+Inflation)^(Age - 25)))

#Convert Multipliers that are not numeric to numeric  
#M11$FAS_Multiplier5 <- as.numeric(M11$FAS_Multiplier5)
#M11$FAS_Multiplier5_YOS <- as.numeric(M11$FAS_Multiplier5_YOS)
M11$FAS_Multiplier1[M11$FAS_Multiplier1 == ""] <- 0
M11$FAS_Multiplier1[M11$FAS_Multiplier1 == "1.85%"] <- 0.0185
M11$FAS_Multiplier1[M11$FAS_Multiplier1 == "2%"] <- 0.02
M11$FAS_Multiplier1[M11$FAS_Multiplier1 == "2.50%"] <- 0.0250
M11$FAS_Multiplier1 <- as.numeric(M11$FAS_Multiplier1)

M11$RepRate25 <- 0
#Multiple Multipliers nonsense (Additive and also retroactive)
M12 <- M11 %>%
  group_by(EquableClassID) %>%
  mutate(RepRate25r = case_when(
    YOS >= FAS_Vest & YOS < FAS_Multiplier5_YOS & YOS >= FAS_Multiplier4_YOS  & FAS_Additive_Multipliers == 0 ~ (YOS*FAS_Multiplier5),
    YOS >= FAS_Vest & YOS < FAS_Multiplier4_YOS & YOS >= FAS_Multiplier3_YOS & FAS_Additive_Multipliers == 0 ~ (YOS*FAS_Multiplier4),
    YOS >= FAS_Vest & YOS < FAS_Multiplier3_YOS & YOS >= FAS_Multiplier2_YOS & FAS_Additive_Multipliers == 0 ~ (YOS*FAS_Multiplier3),
    YOS >= FAS_Vest & YOS < FAS_Multiplier2_YOS & YOS >= FAS_Multiplier1_YOS  & FAS_Additive_Multipliers == 0~ (YOS*FAS_Multiplier2),
    YOS >= FAS_Vest & YOS < FAS_Multiplier1_YOS  & FAS_Additive_Multipliers == 0 ~ (YOS*FAS_Multiplier1),
    YOS < FAS_Vest ~ 0
  ))%>%
  mutate(RepRate25a = case_when(
    YOS >= FAS_Vest & YOS >= FAS_Multiplier4_YOS & YOS < FAS_Multiplier5_YOS & FAS_Additive_Multipliers == 1 ~ (FAS_Multiplier1 *(FAS_Multiplier1_YOS - 1)) + (FAS_Multiplier2 *(FAS_Multiplier2_YOS - FAS_Multiplier1_YOS)) + (FAS_Multiplier3 *(FAS_Multiplier3_YOS - FAS_Multiplier2_YOS)) + (FAS_Multiplier4 *(FAS_Multiplier4_YOS - FAS_Multiplier3_YOS)) + (FAS_Multiplier5*(YOS - (FAS_Multiplier4_YOS -1))),
    YOS >= FAS_Vest & YOS >= FAS_Multiplier3_YOS & YOS < FAS_Multiplier4_YOS & FAS_Additive_Multipliers == 1 ~ (FAS_Multiplier1 *(FAS_Multiplier1_YOS - 1)) +(FAS_Multiplier2 *(FAS_Multiplier2_YOS - FAS_Multiplier1_YOS)) + (FAS_Multiplier3 *(FAS_Multiplier3_YOS - FAS_Multiplier2_YOS)) + (FAS_Multiplier4*(YOS - (FAS_Multiplier3_YOS -1))),
    YOS >= FAS_Vest & YOS >= FAS_Multiplier2_YOS & YOS < FAS_Multiplier3_YOS & FAS_Additive_Multipliers == 1 ~ (FAS_Multiplier1 *(FAS_Multiplier1_YOS - 1)) +(FAS_Multiplier2 *(FAS_Multiplier2_YOS - FAS_Multiplier1_YOS)) + (FAS_Multiplier3*(YOS - (FAS_Multiplier2_YOS -1))),
    YOS >= FAS_Vest & YOS >= FAS_Multiplier1_YOS & YOS < FAS_Multiplier2_YOS & FAS_Additive_Multipliers == 1 ~ (FAS_Multiplier1 *(FAS_Multiplier1_YOS - 1)) + (FAS_Multiplier2*(YOS - (FAS_Multiplier1_YOS -1))),
    YOS >= FAS_Vest & YOS < FAS_Multiplier1_YOS & FAS_Additive_Multipliers == 1 ~ (YOS*FAS_Multiplier1),
    YOS < FAS_Vest ~ 0))%>%
  mutate(RepRate25 = case_when(
    FAS_Additive_Multipliers == 0 ~ RepRate25r,
    FAS_Additive_Multipliers == 1 ~ RepRate25a
  ))
#Specifically NY SLRS ERS General for 25 year old entrant
M12 <- M12 %>%
  group_by(EquableClassID) %>%
  mutate(RepRate25 = case_when(
    EquableClassID != 33010106 ~ RepRate25,
    EquableClassID == 33010106 & YOS < FAS_Vest ~ 0,
    EquableClassID == 33010106 & YOS < (FAS_Multiplier1_YOS - 1) ~ YOS*FAS_Multiplier1,
    EquableClassID == 33010106 & YOS == (FAS_Multiplier1_YOS - 1) ~ (YOS*FAS_Multiplier2),
    EquableClassID == 33010106 & YOS > (FAS_Multiplier1_YOS - 1) ~ (YOS*FAS_Multiplier2) + (FAS_Multiplier3*(YOS - (FAS_Multiplier1_YOS - 1)))
  ))

#And NY STRS 25 year old entrant  
M12 <- M12 %>%
  group_by(EquableClassID) %>%
  mutate(RepRate25 = case_when(
    EquableClassID != 33020106 ~ RepRate25,
    EquableClassID == 33020106 & YOS < FAS_Vest ~ 0,
    EquableClassID == 33020106 & YOS < (FAS_Multiplier1_YOS - 1) ~ YOS*FAS_Multiplier1,
    EquableClassID == 33020106 & YOS == (FAS_Multiplier1_YOS - 1) ~ (YOS*FAS_Multiplier2),
    EquableClassID == 33020106 & YOS > (FAS_Multiplier1_YOS - 1) ~ (YOS*FAS_Multiplier2) + (FAS_Multiplier3*(YOS - (FAS_Multiplier1_YOS - 1)))
  )) 


#Here we create the pension wealth (for 25 year old entrants) variable, and set it to zero when YOS are 0, or the RepRate we just generated times the FAS if YOS > 0
#Pension Wealth Time
M12$PWealth25 <- 0
M12 <- M12 %>%
  group_by(EquableClassID) %>%
  mutate(PWealth25 = case_when(
    YOS == 0 ~ 0,
    YOS > 0 ~ RepRate25*FAS25))

M12$STR25 <- 0
M12 <- M12 %>%
  group_by(EquableClassID) %>%
  mutate(STR25 = case_when(
    Age == FAS_Retire25 ~ S1y,
  ))%>%
  mutate(TSTR25 = case_when(
    is.na(STR25) ~ STR25[Age == FAS_Retire25],
    !is.na(STR25)~ STR25[Age == FAS_Retire25]))

#Create a Years until retirement Variable - its just Normal Retirement Age minus Age, at each different Age in the aframe.
M12$YTR25 <- 0
M12$YTR25 <- M12$FAS_Retire25 - M12$Age


M12 <- M12 %>% 
  group_by(EquableClassID) %>% 
  mutate(FAF25 = ifelse(YOS < FAS_Vest, 0,
                        case_when(
                          Age <= FAS_Retire25 ~ AF[Age == FAS_Retire25] * Survive[Age == FAS_Retire25] / Survive,
                          Age > FAS_Retire25 ~ AF
                        )))
M12$FAF25[is.na(M12$FAF25)] <- 0

#Lets Make Pension Wealth (Not Adjusted for Inflation, but yes annuitized) This is just the Pension Wealth for an age 25 entrant variable we made earlier (PWealth25) times the Final Annuity Factor for an Age 25 entrant (FAF25) we just made
M12$AdjPWealth25 <- M12$PWealth25*M12$FAF25
#Now we are creating a Final Pension Wealth for an Age 25 Entrant, PWealth25F.  It is equal to The Adjusted Employee Contribution for an Age 25 entrant if The Employee contribution is greater than the Adjusted Pension Wealth for an Age 25 entrant; OR It is equal to the Adjusted Pension wealth for an Age 25 entrant if it is greater than the Adjusted Employee Contribution
M12$PWealth25F <- 0
M12 <- M12 %>%
  group_by(EquableClassID) %>%
  mutate(PWealth25F = case_when( 
    AdjEmpCont25 > AdjPWealth25 ~ AdjEmpCont25,
    AdjEmpCont25 < AdjPWealth25 ~ AdjPWealth25
  ))

#Here we adjust out Final Pension Wealth  for Inflation
M12$AdjPW25 <- 0
M12$AdjPW25 <- M12$PWealth25F/((1 + M12$Inflation)^(M12$Age - 25))

#Create a new dataframe to be safe again
M13 <- M12 #%>%

#Here we set the Annuitization rate
M13$annrate <- ((1.036)/(1+M13$FAS_COLARate))-1

#This is how many payments do we expect a 25 year old entrant to get from their DB plan.  Its average life expectancy for an adult female (86 years old) minus the normal retirement age for a plan.
M13$Payment25 <- 86 - M13$FAS_Retire25

#now we just calculate the Annuity values for the 25 year old entrant using the annuitization rate and number of payments we just calculated.
M13$Annuity25 <- 0
M13$Annuity25 <- (M13$AdjPW25*M13$annrate)/(1-((1+M13$annrate)^(-1*M13$Payment25)))

#Now we do the same thing all over again, but for Age 40 plan entrants
#Declare a new variable, set it equal to zero, update it with the Employee Contribution rate times the Salary for a 40 year old entrant.  This gives us Employee Contribution in Dollars
M13$EmpCont40 <- 0
M13$EmpCont40 <- M13$FAS_EmpRate40*M13$Sal40

#Create an Adjusted Employee Contribution Variable
M13$AdjEmpCont40 <- 0

#Update the values in the Adjusted Employer Contribution value such that when YOS = 0, it is also equal to 0, but when YOS is greater than 0, it is equal to the Lagged Adjusted Employee Contribution in dollars times the quantity of 1 plus the credited interest rate, and then plus the  non-adjusted Lagged employer Contribution.
for (i in 0:31) {
  M13 <- M13 %>%
    group_by(EquableClassID) %>%
    mutate(EmpContL40 = lag(EmpCont40))%>%
    mutate(AdjEmpContL40 = lag(AdjEmpCont40))%>%
    mutate(AdjEmpCont40 = case_when(
      YOS40 == 0 ~ 0,
      YOS40 > 0 ~ (AdjEmpContL40*(1+FAS_CreditingRate)) + EmpContL40))
}

#Now we repeat the process but for Employer Contributions
M13$ERCont40 <- 0
M13$ERCont40 <- M13$FAS_EMCont*M13$Sal40

M13$AdjER40 <- 0
for (i in 0:31) {
  M13 <- M13 %>%
    group_by(EquableClassID) %>%
    mutate(ERContL40 = lag(ERCont40))%>%
    mutate(AdjERL40 = lag(AdjER40))%>%
    mutate(AdjER40 = case_when(
      YOS40 == 0 ~ 0,
      YOS40 > 0 ~ (AdjERL40*(1+FAS_CreditingRate)) + ERContL40))
}

#Here we are calculating the present value of Employee and Employer Contributions, which are just the adjusted contribution values divided by the quantity of 1 + the assumed price inflation raised to the Age-40
M13$PVEmpCont40 <- 0
M13$PVERCont40 <- 0
M13 <- M13 %>%
  group_by(EquableClassID) %>%
  mutate(PVEmpCont40 = AdjEmpCont40/((1+Inflation)^(Age - 40)))%>%
  mutate(PVERCont40 = AdjER40/((1+Inflation)^(Age - 40)))


#Here we create a variable to hold the replacement rate for 40 year old entrants.
M13$RepRate40 <- 0
M14 <- M13 %>%
  group_by(EquableClassID) %>%
  mutate(RepRate40r = case_when(
    YOS40 >= FAS_Vest & YOS40 < FAS_Multiplier5_YOS & YOS40 >= FAS_Multiplier4_YOS  & FAS_Additive_Multipliers == 0 ~ (YOS40*FAS_Multiplier5),
    YOS40 >= FAS_Vest & YOS40 < FAS_Multiplier4_YOS & YOS40 >= FAS_Multiplier3_YOS & FAS_Additive_Multipliers == 0 ~ (YOS40*FAS_Multiplier4),
    YOS40 >= FAS_Vest & YOS40 < FAS_Multiplier3_YOS & YOS40 >= FAS_Multiplier2_YOS & FAS_Additive_Multipliers == 0 ~ (YOS40*FAS_Multiplier3),
    YOS40 >= FAS_Vest & YOS40 < FAS_Multiplier2_YOS & YOS40 >= FAS_Multiplier1_YOS  & FAS_Additive_Multipliers == 0~ (YOS40*FAS_Multiplier2),
    YOS40 >= FAS_Vest & YOS40 < FAS_Multiplier1_YOS  & FAS_Additive_Multipliers == 0 ~ (YOS40*FAS_Multiplier1),
    YOS40 < FAS_Vest ~ 0
  ))%>%
  mutate(RepRate40a = case_when(
    YOS40 >= FAS_Vest & YOS40 >= FAS_Multiplier4_YOS & YOS40 < FAS_Multiplier5_YOS & FAS_Additive_Multipliers == 1 ~ (FAS_Multiplier1 *(FAS_Multiplier1_YOS - 1)) +(FAS_Multiplier2 *(FAS_Multiplier2_YOS - FAS_Multiplier1_YOS)) + (FAS_Multiplier3 *(FAS_Multiplier3_YOS - FAS_Multiplier2_YOS)) + (FAS_Multiplier4 *(FAS_Multiplier4_YOS - FAS_Multiplier3_YOS)) + (FAS_Multiplier5*(YOS40 - (FAS_Multiplier4_YOS -1))),
    YOS40 >= FAS_Vest & YOS40 >= FAS_Multiplier3_YOS & YOS40 < FAS_Multiplier4_YOS & FAS_Additive_Multipliers == 1 ~ (FAS_Multiplier1 *(FAS_Multiplier1_YOS - 1)) + (FAS_Multiplier2 *(FAS_Multiplier2_YOS - FAS_Multiplier1_YOS)) + (FAS_Multiplier3 *(FAS_Multiplier3_YOS - FAS_Multiplier2_YOS)) + (FAS_Multiplier4*(YOS40 - (FAS_Multiplier3_YOS -1))),
    YOS40 >= FAS_Vest & YOS40 >= FAS_Multiplier2_YOS & YOS40 < FAS_Multiplier3_YOS & FAS_Additive_Multipliers == 1 ~ (FAS_Multiplier1 *(FAS_Multiplier1_YOS - 1)) +(FAS_Multiplier2 *(FAS_Multiplier2_YOS - FAS_Multiplier1_YOS)) + (FAS_Multiplier3*(YOS40 - (FAS_Multiplier2_YOS -1))),
    YOS40 >= FAS_Vest & YOS40 >= FAS_Multiplier1_YOS & YOS40 < FAS_Multiplier2_YOS & FAS_Additive_Multipliers == 1 ~ (FAS_Multiplier1 *(FAS_Multiplier1_YOS - 1)) + (FAS_Multiplier2*(YOS40 - (FAS_Multiplier1_YOS -1))),
    YOS40 >= FAS_Vest & YOS40 < FAS_Multiplier1_YOS & FAS_Additive_Multipliers == 1 ~ (YOS40*FAS_Multiplier1),
    YOS40 < FAS_Vest ~ 0))%>%
  mutate(RepRate40 = case_when(
    FAS_Additive_Multipliers == 0 ~ RepRate40r,
    FAS_Additive_Multipliers == 1 ~ RepRate40a
  ))
#Specifically NY SLRS ERS General for 40 year old entrant
M14 <- M14 %>%
  group_by(EquableClassID) %>%
  mutate(RepRate40 = case_when(
    EquableClassID != 33010106 ~ RepRate40,
    EquableClassID == 33010106 & YOS40 < FAS_Vest ~ 0,
    EquableClassID == 33010106 & YOS40 < (FAS_Multiplier1_YOS - 1) ~ YOS40*FAS_Multiplier1,
    EquableClassID == 33010106 & YOS40 == (FAS_Multiplier1_YOS - 1) ~ (YOS40*FAS_Multiplier2),
    EquableClassID == 33010106 & YOS40 > (FAS_Multiplier1_YOS - 1) ~ (YOS40*FAS_Multiplier2) + (FAS_Multiplier3*(YOS40 - (FAS_Multiplier1_YOS - 1)))
  ))

#And NY STRS 40 year old entrant  
M14 <- M14 %>%
  group_by(EquableClassID) %>%
  mutate(RepRate40 = case_when(
    EquableClassID != 33020106 ~ RepRate40,
    EquableClassID == 33020106 & YOS40 < FAS_Vest ~ 0,
    EquableClassID == 33020106 & YOS40 < (FAS_Multiplier1_YOS - 1) ~ YOS40*FAS_Multiplier1,
    EquableClassID == 33020106 & YOS40 == (FAS_Multiplier1_YOS - 1) ~ (YOS40*FAS_Multiplier2),
    EquableClassID == 33020106 & YOS40 > (FAS_Multiplier1_YOS - 1) ~ (YOS40*FAS_Multiplier2) + (FAS_Multiplier3*(YOS40 - (FAS_Multiplier1_YOS - 1)))
  )) 

#Here we create the pension wealth (for 40 year old entrants) variable, and set it to zero when YOS are 0, or the RepRate we just generated times the FAS if YOS > 0
#Pension Wealth Time
M14$PWealth40 <- 0
M14 <- M14 %>%
  group_by(EquableClassID) %>%
  mutate(PWealth40 = case_when(
    YOS == 0 ~ 0,
    YOS > 0 ~ RepRate40*FAS40))


#Now we do survival to retire from age 40 until the Normal Retirement Age. We will Use this in the Final (adjusted) Annuity Factor Variable.  Please note, the TSTR40 variable is just the STR40 where Age is equal to the normal retirement age for all values.
M14$STR40 <- 0
M14 <- M14 %>%
  group_by(EquableClassID) %>%
  mutate(STR40 = case_when(
    Age == FAS_Retire40 ~ S1y,
  ))%>%
  mutate(TSTR40 = case_when(
    is.na(STR40) ~ STR40[Age == FAS_Retire40],
    !is.na(STR40)~ STR40[Age == FAS_Retire40]))

#Years until retirement Variable - its just Normal Retirement Age minus Age, at each different Age in the aframe.
M14$YTR40 <- 0
M14$YTR40 <- M14$FAS_Retire40 - M14$Age

M14 <- M14 %>% 
  group_by(EquableClassID) %>% 
  mutate(FAF40 = ifelse(YOS40 < FAS_Vest, 0,
                        case_when(
                          Age <= FAS_Retire40 ~ AF[Age == FAS_Retire40] * Survive[Age == FAS_Retire40] / Survive,
                          Age > FAS_Retire40 ~ AF
                        )))
M14$FAF40[is.na(M14$FAF40)] <- 0

M14$AdjPWealth40 <- M14$PWealth40*M14$FAF40
#Now we are creating a Final Pension Wealth for an Age 25 Entrant, PWealth25F.  It is equal to The Adjusted Employee Contribution for an Age 25 entrant if The Employee contribution is greater than the Adjusted Pension Wealth for an Age 25 entrant; OR It is equal to the Adjusted Pension wealth for an Age 25 entrant if it is greater than the Adjusted Employee Contribution
M14$PWealth40F <- 0
M14 <- M14 %>%
  group_by(EquableClassID) %>%
  mutate(PWealth40F = case_when( 
    AdjEmpCont40 > AdjPWealth40 ~ AdjEmpCont40,
    AdjEmpCont40 < AdjPWealth40 ~ AdjPWealth40
  ))

#Pension Wealth adjusted for Inflation
M14$AdjPW40 <- 0
M14$AdjPW40 <- M14$PWealth40F/((1 + M14$Inflation)^(M14$Age - 40))

#Create a new aframe to be safe again
M15 <- M14 #%>%

#Next Is Annuitization (Oh boy, here we go Annuitizing again!)
M15$annrate <- ((1.036)/(1+M15$FAS_COLARate))-1

#This is how many payments do we expect a 40 year old entrant to get from their DB plan.  Its average life expectancy for an adult female (86 years old) minus the normal retirement age for a plan.
M15$Payment40 <- 86 - M15$FAS_Retire40

#now we just calculate the Annuity values for the 25 year old entrant using the annuitization rate and number of payments we just calculated.
M15$Annuity40 <- 0
M15$Annuity40 <- (M15$AdjPW40*M15$annrate)/(1-((1+M15$annrate)^(-1*M15$Payment40)))



#Finally, a little prep work for later on, we need to inflation adjust out salary25 and 40 and then also inflation adjust the FAS25 and 40 as well


M15$Inf.Sal25 <- M15$Sal25/((1+M15$Inflation)^(M15$Age-25))
M15$Inf.Sal40 <- M15$Sal40/((1+M15$Inflation)^(M15$Age-40))

M15 <- M15 %>% 
  group_by(EquableClassID) %>% 
  mutate(Inf.FAS25 = ifelse(is.na(FAS_Years), NA,
                        rollmean(Inf.Sal25, k = FAS_Years, fill = 0, align = "right"))) %>% 
  mutate(Inf.FAS25 = lag(Inf.FAS25, default = 0))%>%
  mutate(Inf.FAS40 = ifelse(is.na(FAS_Years), NA,
                        rollmean(Inf.Sal40, k = FAS_Years, fill = 0, align = "right"))) %>% 
  mutate(Inf.FAS40 = lag(Inf.FAS40, default = 0))

M15[is.na(M15)] <- 0
#Additionally, we need to generate the FASTargets for a 25 and a 40 year old entrant
M15$targetFAS25 <- 0
M15$targetFAS40 <- 0
M15$FASTarget25 <- 0
M15$FASTarget40 <- 0

#Here we are just saying that the target FAS is the value of Inflation adjusted FAS for a given plan at Age 67, and then generates a column in which every cell is equal to that value. This is done independently for the 25 and 40 year old plan entrants. Please note, this is done on the Inflation Adjusted FAS, hence why we had to inflation adjust Sal and FAS earlier.
#25 year old entrants
M15 <- M15 %>%
  group_by(EquableClassID, Age) %>%
  mutate(targetFAS25 = case_when(
    Age == 67 ~ Inf.FAS25
  ))

M15 <- M15 %>%
  group_by(EquableClassID) %>%
  mutate(FASTarget25 = case_when(
    is.na(targetFAS25) ~ targetFAS25[Age == 67],
    !is.na(targetFAS25) ~ targetFAS25[Age == 67]
  ))%>%
  select(-targetFAS25)

#40 year old entrants
M15 <- M15 %>%
  group_by(EquableClassID, Age) %>%
  mutate(targetFAS40 = case_when(
    Age == 67 ~ Inf.FAS40
  ))

M15 <- M15 %>%
  group_by(EquableClassID) %>%
  mutate(FASTarget40 = case_when(
    is.na(targetFAS40) ~ targetFAS40[Age == 67],
    !is.na(targetFAS40) ~ targetFAS40[Age == 67]
  ))%>%
  select(-targetFAS40)

#Make a DB only Data Set
DB1 <- M15 %>%
  filter(Plan.Type == "FAS")

#Now a DC only Data Set
DC1 <- M15 %>%
  filter(Plan.Type == "DC")
DC1$dcInt <- (DC1$ARR -0.0050)
DC1$payments25 <- 19
DC1$payments40 <- 19 #Survival to age 86, retirement at 67
#We make this thresholdCOLA variable and set it equal to inflation. We will use it shortly in calculating the Annuitization rate
DC1$thresholdCOLA <- DC1$Inflation
#And Calculating the Annuitization rate
DC1$annrateT <- ((1.036)/(1 + DC1$thresholdCOLA))-1
DC1$Inf.Sal25 <- DC1$Sal25/((1+DC1$Inflation)^(DC1$Age-25))
DC1$Inf.Sal40 <- DC1$Sal40/((1+DC1$Inflation)^(DC1$Age-40))
#Here we are calculating the FASTargets for a 25 and a 40 year old entrant.  Once again, please notice the use of Inflation Adjusted Values here.  Also note that because DC plans do not have FAS, we use Salary here instead.
DC1$FASTarget25 <- 0
DC1$FASTarget40 <- 0
DC1 <- DC1%>%
  mutate(FASTarget25 = Inf.Sal25[Age == 67])%>%
  mutate(FASTarget40 = Inf.Sal40[Age == 67])
#This is set up for when there are plans that have multiple employee contribution rates. We aren't currently using it, so it is mostly commented out. 
DC1$DCEmpRate25 <- DC1$DCGR_EECont #There is only 1 employee rate for DC and GR plans, so we can just set the overall Employee rate for a given entry age as the first one.
DC1$DCEmpRate40 <- DC1$DCGR_EECont
DC1$DCERRate25 <- DC1$DCGR_ERCont1
DC1$DCERRate40 <- DC1$DCGR_ERCont1


#Attempt at model thing - 25 year old edition
DC1$RP <- 0
DC1$RP[DC1$Age == 25] <- (DC1$Sal25[DC1$Age==25]*(DC1$DCEmpRate25[DC1$Age==25] + DC1$DCERRate25[DC1$Age==25]))
DC1$RPg <- 0
DC1$RPg[DC1$Age == 25] <- DC1$RP[DC1$Age == 25]

for (i in 26:70) {  
DC1<- DC1 %>%
  group_by(EquableClassID)%>%
  mutate(RP = case_when(
    Age == 25 ~ RP,
    Age > 25 ~ (RP[Age== i-1]*(1+dcInt) + (Sal25[Age ==i]*(DCEmpRate25[Age==i] + DCERRate25[Age==i])))
    ))
DC1$RPg[DC1$Age == i] <- DC1$RP[DC1$Age==i]
}
#And let's assign the values in RPg to a new variable called DC_PW25 (DC Pension Wealth for a 25 year old plan entrant)
DC1$DC_PW25 <- DC1$RPg
#Attempt at model thing - 40 year old edition
DC1$RP <- 0
DC1$RP[DC1$Age == 40] <- (DC1$Sal40[DC1$Age==40]*(DC1$DCEmpRate40[DC1$Age==40] + DC1$DCERRate40[DC1$Age==40]))
DC1$RPg <- 0
DC1$RPg[DC1$Age == 40] <- DC1$RP[DC1$Age == 40]

for (i in 41:70) {  
  DC1<- DC1 %>%
    group_by(EquableClassID)%>%
    mutate(RP = case_when(
      Age == 40 ~ RP,
      Age > 40 ~ (RP[Age== i-1]*(1+dcInt) + (Sal40[Age ==i]*(DCEmpRate40[Age==i] + DCERRate40[Age==i])))
    ))
  DC1$RPg[DC1$Age == i] <- DC1$RP[DC1$Age==i]
}
#And let's assign the values in RPg to a new variable called DC_PW40 (DC Pension Wealth for a 40 year old plan entrant)
DC1$DC_PW40 <- DC1$RPg
#We ostensibly have Pension wealth for Age == 40 now for DC plans.  
#Now we want to adjust 
DC1$AdjDC_PW25 <- 0
DC1$AdjDC_PW40 <- 0

#And we are also going to make a Total Employer Contribution 25 and 40 Variable to use in adjusting the graded vesting pension wealth values
DC1$EmpTotCont25 <- 0
DC1$EmpTotCont40 <- 0
DC1$ERCVal25 <- DC1$DCERRate25*DC1$Sal25
DC1$ERCVal40 <- DC1$DCERRate40*DC1$Sal40

DC1 <- DC1 %>%
  group_by(EquableClassID)%>%
  mutate(EmpTotCont25 = cumsum(ERCVal25))%>%
  mutate(EmpTotCont40 = cumsum(ERCVal40))



#So when a plan does not have graded vesting, and the years of service is less than the Vesting Point, Pension wealth is equal to unadjusted pension wealth minus Employer Contributions.  Then when YOS is greater than or equal to the vesting point, adjusted pension wealth is equal to unadjusted pension wealth.  Finally, when YOS is less than the Vesting point, adjusted pension wealth is equal to unadjusted pension wealth times the particular grade adjustment.  Which grade adjustment is used is dependant on years of service.
DC1 <- DC1 %>%
  group_by(EquableClassID) %>%
  mutate(AdjDC_PW25 = case_when(
    YOS < DCGR_FullVest & YOS < DCGR_Grade10_YOS & YOS >= DCGR_Grade9_YOS ~ DC_PW25-((1-DCGR_Grade10)*EmpTotCont25),
    YOS < DCGR_FullVest & YOS < DCGR_Grade9_YOS & YOS >= DCGR_Grade8_YOS ~ DC_PW25-((1-DCGR_Grade9)*EmpTotCont25),
    YOS < DCGR_FullVest & YOS < DCGR_Grade8_YOS & YOS >= DCGR_Grade7_YOS ~ DC_PW25-((1-DCGR_Grade8)*EmpTotCont25),
    YOS < DCGR_FullVest & YOS < DCGR_Grade7_YOS & YOS >= DCGR_Grade6_YOS ~ DC_PW25-((1-DCGR_Grade7)*EmpTotCont25),
    YOS < DCGR_FullVest & YOS < DCGR_Grade6_YOS & YOS >= DCGR_Grade5_YOS ~ DC_PW25-((1-DCGR_Grade6)*EmpTotCont25),
    YOS < DCGR_FullVest & YOS < DCGR_Grade5_YOS & YOS >= DCGR_Grade4_YOS ~ DC_PW25-((1-DCGR_Grade5)*EmpTotCont25),
    YOS < DCGR_FullVest & YOS < DCGR_Grade4_YOS & YOS >= DCGR_Grade3_YOS ~ DC_PW25-((1-DCGR_Grade4)*EmpTotCont25),
    YOS < DCGR_FullVest & YOS < DCGR_Grade3_YOS & YOS >= DCGR_Grade2_YOS ~ DC_PW25-((1-DCGR_Grade3)*EmpTotCont25),
    YOS < DCGR_FullVest & YOS < DCGR_Grade2_YOS & YOS >= DCGR_Grade1_YOS  ~ DC_PW25-((1-DCGR_Grade2)*EmpTotCont25),
    YOS < DCGR_FullVest & YOS < DCGR_Grade1_YOS  ~ DC_PW25-((1-DCGR_Grade1)*EmpTotCont25),
    YOS >= DCGR_FullVest ~ DC_PW25,
    YOS < DCGR_FullVest & DCGR_GradeVest  == 0 ~ (DC_PW25 - (Sal25*DCERRate25))
  ))
DC1 <- DC1 %>%
  group_by(EquableClassID) %>%
  mutate(AdjDC_PW40 = case_when(
    YOS40 < DCGR_FullVest & YOS40 < DCGR_Grade10_YOS & YOS40 >= DCGR_Grade9_YOS ~ DC_PW40-((1-DCGR_Grade10)*EmpTotCont40),
    YOS40 < DCGR_FullVest & YOS40 < DCGR_Grade9_YOS & YOS40 >= DCGR_Grade8_YOS ~ DC_PW40-((1-DCGR_Grade9)*EmpTotCont40),
    YOS40 < DCGR_FullVest & YOS40 < DCGR_Grade8_YOS & YOS40 >= DCGR_Grade7_YOS ~ DC_PW40-((1-DCGR_Grade8)*EmpTotCont40),
    YOS40 < DCGR_FullVest & YOS40 < DCGR_Grade7_YOS & YOS40 >= DCGR_Grade6_YOS ~ DC_PW40-((1-DCGR_Grade7)*EmpTotCont40),
    YOS40 < DCGR_FullVest & YOS40 < DCGR_Grade6_YOS & YOS40 >= DCGR_Grade5_YOS ~ DC_PW40-((1-DCGR_Grade6)*EmpTotCont40),
    YOS40 < DCGR_FullVest & YOS40 < DCGR_Grade5_YOS & YOS40 >= DCGR_Grade4_YOS ~ DC_PW40-((1-DCGR_Grade5)*EmpTotCont40),
    YOS40 < DCGR_FullVest & YOS40 < DCGR_Grade4_YOS & YOS40 >= DCGR_Grade3_YOS ~ DC_PW40-((1-DCGR_Grade4)*EmpTotCont40),
    YOS40 < DCGR_FullVest & YOS40 < DCGR_Grade3_YOS & YOS40 >= DCGR_Grade2_YOS ~ DC_PW40-((1-DCGR_Grade3)*EmpTotCont40),
    YOS40 < DCGR_FullVest & YOS40 < DCGR_Grade2_YOS & YOS40 >= DCGR_Grade1_YOS  ~ DC_PW40-((1-DCGR_Grade2)*EmpTotCont40),
    YOS40 < DCGR_FullVest & YOS40 < DCGR_Grade1_YOS  ~ DC_PW40-((1-DCGR_Grade1)*EmpTotCont40),
    YOS40 >= DCGR_FullVest ~ DC_PW40,
    YOS40 < DCGR_FullVest & DCGR_GradeVest  == 0 ~ (DC_PW40 - (Sal40*DCERRate40))
  ))

#DC1T <- DC1 %>%
  #filter(DCGR_GradeVest == 1)
#Here we are adjusting the previously adjusted Pension Wealth values, this time for inflation.     
DC1$Inf.DC_PW25 <- DC1$AdjDC_PW25/((1+DC1$Inflation)^(DC1$Age-25))
DC1$Inf.DC_PW40 <- DC1$AdjDC_PW40/((1+DC1$Inflation)^(DC1$Age-40)) 


#Now it is time for the GR Plans
GR1 <- M15 %>%
  filter(Plan.Type == "GR")
#Here is the exactly one exception, we set the interest rate we use in calculating pension wealth equal to the the GR Credited Interest rate instead of the Assumed Rate of Return minus 50 basis points.
GR1$GRInt <- GR1$GR_CreditingRate
GR1$payments25 <- 19
GR1$payments40 <- 19 #Survival to age 86, retirement at 67
GR1$thresholdCOLA <- GR1$Inflation
GR1$annrateT <- ((1.036)/(1 + GR1$thresholdCOLA))-1
GR1$Inf.Sal25 <- GR1$Sal25/((1+GR1$Inflation)^(GR1$Age-25))
GR1$Inf.Sal40 <- GR1$Sal40/((1+GR1$Inflation)^(GR1$Age-40))
#Once again generating FAS Targets
GR1$FASTarget25 <- 0
GR1$FASTarget40 <- 0
GR1 <- GR1%>%
  mutate(FASTarget25 = Inf.Sal25[Age == 67])%>%
  mutate(FASTarget40 = Inf.Sal40[Age == 67])
#The code for multiple employer contribution rates.
GR1$GREmpRate25 <- GR1$DCGR_EECont
GR1$GREmpRate40 <- GR1$DCGR_EECont
GR1$GRERRate25 <- 0
GR1$GRERRate40 <- 0
GR1 <- GR1 %>%
  group_by(EquableClassID) %>%
  mutate(GRERRate25 = case_when(
    DCGR_ERCont1_YOS != 99 & YOS < DCGR_ERCont4_YOS & YOS >= DCGR_ERCont3_YOS ~ (DCGR_ERCont4),
    DCGR_ERCont1_YOS != 99 & YOS < DCGR_ERCont3_YOS & YOS >= DCGR_ERCont2_YOS ~ (DCGR_ERCont3),
    DCGR_ERCont1_YOS != 99 & YOS < DCGR_ERCont2_YOS & YOS >= DCGR_ERCont1_YOS  ~ (DCGR_ERCont2),
    DCGR_ERCont1_YOS != 99 & YOS <= DCGR_ERCont1_YOS ~ (DCGR_ERCont1),
    DCGR_ERCont1_YOS == 99 ~ (DCGR_ERCont1)
  ))
GR1 <- GR1 %>%
  group_by(EquableClassID) %>%
  mutate(GRERRate40 = case_when(
    DCGR_ERCont1_YOS != 99 & YOS40 < DCGR_ERCont4_YOS & YOS40 >= DCGR_ERCont3_YOS ~ (DCGR_ERCont4),
    DCGR_ERCont1_YOS != 99 & YOS40 < DCGR_ERCont3_YOS & YOS40 >= DCGR_ERCont2_YOS ~ (DCGR_ERCont3),
    DCGR_ERCont1_YOS != 99 & YOS40 < DCGR_ERCont2_YOS & YOS40 >= DCGR_ERCont1_YOS  ~ (DCGR_ERCont2),
    DCGR_ERCont1_YOS != 99 & YOS40 <= DCGR_ERCont1_YOS ~ (DCGR_ERCont1),
    DCGR_ERCont1_YOS == 99 ~ (DCGR_ERCont1)
  ))
#Attempt at model thing - 25 year old edition
GR1$RP <- 0
GR1$RP[GR1$Age == 25] <- (GR1$Sal25[GR1$Age==25]*(GR1$GREmpRate25[GR1$Age==25] + GR1$GRERRate25[GR1$Age==25]))
GR1$RPg <- 0
GR1$RPg[GR1$Age == 25] <- GR1$RP[GR1$Age == 25]

for (i in 26:70) {  
  GR1<- GR1 %>%
    group_by(EquableClassID)%>%
    mutate(RP = case_when(
      Age == 25 ~ RP,
      Age > 25 ~ (RP[Age== i-1]*(1+GRInt) + (Sal25[Age ==i]*(GREmpRate25[Age==i] + GRERRate25[Age==i])))
    ))
  GR1$RPg[GR1$Age == i] <- GR1$RP[GR1$Age==i]
}
#And let's assign the values in RPg to a new variable called GR_PW25 (GR Pension Wealth for a 25 year old plan entrant)
GR1$GR_PW25 <- GR1$RPg

#Attempt at model thing - 40 year old edition
GR1$RP <- 0
GR1$RP[GR1$Age == 40] <- (GR1$Sal40[GR1$Age==40]*(GR1$GREmpRate40[GR1$Age==40] + GR1$GRERRate40[GR1$Age==40]))
GR1$RPg <- 0
GR1$RPg[GR1$Age == 40] <- GR1$RP[GR1$Age == 40]

for (i in 41:70) {  
  GR1<- GR1 %>%
    group_by(EquableClassID)%>%
    mutate(RP = case_when(
      Age == 40 ~ RP,
      Age > 40 ~ (RP[Age== i-1]*(1+GRInt) + (Sal40[Age ==i]*(GREmpRate40[Age==i] + GRERRate40[Age==i])))
    ))
  GR1$RPg[GR1$Age == i] <- GR1$RP[GR1$Age==i]
}
#And let's assign the values in RPg to a new variable called GR_PW40 (GR Pension Wealth for a 40 year old plan entrant)
GR1$GR_PW40 <- GR1$RPg

GR1$AdjGR_PW25 <- 0
GR1$AdjGR_PW40 <- 0
#So when a plan does not have graded vesting, and the years of service is less than the Vesting Point, Pension wealth is equal to unadjusted pension wealth minus Employer Contributions.  Then when YOS is greater than or equal to the vesting point, adjusted pension wealth is equal to unadjusted pension wealth.  Finally, when YOS is less than the Vesting point, adjusted pension wealth is equal to unadjusted pension wealth times the particular grade adjustment.  Which grade adjustment is used is dependant on years of service.            
GR1 <- GR1 %>%
  group_by(EquableClassID) %>%
  mutate(AdjGR_PW25 = case_when(
    YOS >= DCGR_FullVest ~ GR_PW25,
    YOS < DCGR_FullVest ~ (GR_PW25 - (Sal25*GRERRate25))
  ))
GR1 <- GR1 %>%
  group_by(EquableClassID) %>%
  mutate(AdjGR_PW40 = case_when(
    YOS40 >= DCGR_FullVest ~ GR_PW40,
    YOS40 < DCGR_FullVest ~ (GR_PW40 - (Sal40*GRERRate40))
  ))

#Here we are adjusting the previously adjusted Pension Wealth values, this time for inflation. 
GR1$Inf.GR_PW25 <- GR1$AdjGR_PW25/((1+GR1$Inflation)^(GR1$Age-25))
GR1$Inf.GR_PW40 <- GR1$AdjGR_PW40/((1+GR1$Inflation)^(GR1$Age-40))    

#Hybrid Plans
Hy1 <- M15 %>%
  filter(Plan.Type == "Hybrid")


#Variables needed for both GR and DC Modeling, and also the separate credited interest rates
Hy1$DCInt <- (Hy1$ARR -0.0050)
Hy1$GRInt <- Hy1$GR_CreditingRate
Hy1$payments25 <- 19
Hy1$payments40 <- 19 #Survival to age 86, retirement at SSA Normal Retirement Age 67
Hy1$thresholdCOLA <- Hy1$Inflation
Hy1$annrateT <- ((1.036)/(1 + Hy1$thresholdCOLA))-1


#The code for multiple employer contribution rates.
Hy1$HyEmpRate25 <- Hy1$DCGR_EECont
Hy1$HyEmpRate40 <- Hy1$DCGR_EECont
Hy1$HyERRate25 <- 0
Hy1$HyERRate40 <- 0
Hy1 <- Hy1 %>%
  group_by(EquableClassID) %>%
  mutate(HyERRate25 = case_when(
    DCGR_ERCont1_YOS != 99 & YOS < DCGR_ERCont4_YOS & YOS >= DCGR_ERCont3_YOS ~ (DCGR_ERCont4),
    DCGR_ERCont1_YOS != 99 & YOS < DCGR_ERCont3_YOS & YOS >= DCGR_ERCont2_YOS ~ (DCGR_ERCont3),
    DCGR_ERCont1_YOS != 99 & YOS < DCGR_ERCont2_YOS & YOS >= DCGR_ERCont1_YOS  ~ (DCGR_ERCont2),
    DCGR_ERCont1_YOS != 99 & YOS <= DCGR_ERCont1_YOS ~ (DCGR_ERCont1),
    DCGR_ERCont1_YOS == 99 ~ (DCGR_ERCont1)
  ))

Hy1 <- Hy1 %>%
  group_by(EquableClassID) %>%
  mutate(HyERRate40 = case_when(
    DCGR_ERCont1_YOS != 99 & YOS40 < DCGR_ERCont4_YOS & YOS40 >= DCGR_ERCont3_YOS ~ (DCGR_ERCont4),
    DCGR_ERCont1_YOS != 99 & YOS40 < DCGR_ERCont3_YOS & YOS40 >= DCGR_ERCont2_YOS ~ (DCGR_ERCont3),
    DCGR_ERCont1_YOS != 99 & YOS40 < DCGR_ERCont2_YOS & YOS40 >= DCGR_ERCont1_YOS  ~ (DCGR_ERCont2),
    DCGR_ERCont1_YOS != 99 & YOS40 <= DCGR_ERCont1_YOS ~ (DCGR_ERCont1),
    DCGR_ERCont1_YOS == 99 ~ (DCGR_ERCont1)
  ))

#Here we make the Hybrid -DC 25 and 40 year old entrant models

Hy1$RP <- 0
Hy1$RP[Hy1$Age == 25] <- (Hy1$Sal25[Hy1$Age==25]*(Hy1$HyEmpRate25[Hy1$Age==25] + Hy1$HyERRate25[Hy1$Age==25]))
Hy1$RPg <- 0
Hy1$RPg[Hy1$Age == 25] <- Hy1$RP[Hy1$Age == 25]

for (i in 26:70) {  
  Hy1<- Hy1 %>%
    group_by(EquableClassID)%>%
    mutate(RP = case_when(
      Age == 25 ~ RP,
      Age > 25 ~ (RP[Age== i-1]*(1+DCInt) + (Sal25[Age ==i]*(HyEmpRate25[Age==i] + HyERRate25[Age==i])))
    ))
  Hy1$RPg[Hy1$Age == i] <- Hy1$RP[Hy1$Age==i]
}
#And let's assign the values in RPg to a new variable called Hy_PW25 (Hy Pension Wealth for a 25 year old plan entrant)
Hy1$Hy_PW25DC <- Hy1$RPg
#Attempt at model thing - 40 year old edition
Hy1$RP <- 0
Hy1$RP[Hy1$Age == 40] <- (Hy1$Sal40[Hy1$Age==40]*(Hy1$HyEmpRate40[Hy1$Age==40] + Hy1$HyERRate40[Hy1$Age==40]))
Hy1$RPg <- 0
Hy1$RPg[Hy1$Age == 40] <- Hy1$RP[Hy1$Age == 40]

for (i in 41:70) {  
  Hy1<- Hy1 %>%
    group_by(EquableClassID)%>%
    mutate(RP = case_when(
      Age == 40 ~ RP,
      Age > 40 ~ (RP[Age== i-1]*(1+DCInt) + (Sal40[Age ==i]*(HyEmpRate40[Age==i] + HyERRate40[Age==i])))
    ))
  Hy1$RPg[Hy1$Age == i] <- Hy1$RP[Hy1$Age==i]
}
#And let's assign the values in RPg to a new variable called Hy_PW40 (Hy Pension Wealth for a 40 year old plan entrant)
Hy1$Hy_PW40DC <- Hy1$RPg

#And here we do the same thing again, but for the Hybrid-GR plans
Hy1$RP <- 0
Hy1$RP[Hy1$Age == 25] <- (Hy1$Sal25[Hy1$Age==25]*(Hy1$HyEmpRate25[Hy1$Age==25] + Hy1$HyERRate25[Hy1$Age==25]))
Hy1$RPg <- 0
Hy1$RPg[Hy1$Age == 25] <- Hy1$RP[Hy1$Age == 25]

for (i in 26:70) {  
  Hy1<- Hy1 %>%
    group_by(EquableClassID)%>%
    mutate(RP = case_when(
      Age == 25 ~ RP,
      Age > 25 ~ (RP[Age== i-1]*(1+GRInt) + (Sal25[Age ==i]*(HyEmpRate25[Age==i] + HyERRate25[Age==i])))
    ))
  Hy1$RPg[Hy1$Age == i] <- Hy1$RP[Hy1$Age==i]
}
#And let's assign the values in RPg to a new variable called Hy_PW25 (Hy Pension Wealth for a 25 year old plan entrant)
Hy1$Hy_PW25GR <- Hy1$RPg
#Attempt at model thing - 40 year old edition
Hy1$RP <- 0
Hy1$RP[Hy1$Age == 40] <- (Hy1$Sal40[Hy1$Age==40]*(Hy1$HyEmpRate40[Hy1$Age==40] + Hy1$HyERRate40[Hy1$Age==40]))
Hy1$RPg <- 0
Hy1$RPg[Hy1$Age == 40] <- Hy1$RP[Hy1$Age == 40]

for (i in 41:70) {  
  Hy1<- Hy1 %>%
    group_by(EquableClassID)%>%
    mutate(RP = case_when(
      Age == 40 ~ RP,
      Age > 40 ~ (RP[Age== i-1]*(1+GRInt) + (Sal40[Age ==i]*(HyEmpRate40[Age==i] + HyERRate40[Age==i])))
    ))
  Hy1$RPg[Hy1$Age == i] <- Hy1$RP[Hy1$Age==i]
}
#And let's assign the values in RPg to a new variable called Hy_PW40 (Hy Pension Wealth for a 40 year old plan entrant)
Hy1$Hy_PW40GR <- Hy1$RPg


#Now we need to adjust both the DC and GR plan pension wealth values for graded vesting, for both Age 25 and Age 40 entrants.
Hy1$HyAdjDC_PW25 <- 0
Hy1$HyAdjDC_PW40 <- 0
Hy1$HyAdjGR_PW25 <- 0
Hy1$HyAdjGR_PW40 <- 0

#And we are also going to make a Total Employer Contribution 25 and 40 Variable to use in adjusting the graded vesting pension wealth values
Hy1$EmpTotCont25 <- 0
Hy1$EmpTotCont40 <- 0
Hy1$ERCVal25 <- DC1$DCERRate25*DC1$Sal25
Hy1$ERCVal40 <- DC1$DCERRate40*DC1$Sal40

DC1 <- DC1 %>%
  group_by(EquableClassID)%>%
  mutate(EmpTotCont25 = cumsum(ERCVal25))%>%
  mutate(EmpTotCont40 = cumsum(ERCVal40))


#This is for DC Plan Age 25 Entrants            
Hy1 <- Hy1 %>%
  group_by(EquableClassID) %>%
  mutate(HyAdjDC_PW25 = case_when(
    YOS < DCGR_FullVest & YOS < DCGR_Grade10_YOS & YOS >= DCGR_Grade9_YOS ~ Hy_PW25DC-((1-DCGR_Grade10)*EmpTotCont25),
    YOS < DCGR_FullVest & YOS < DCGR_Grade9_YOS & YOS >= DCGR_Grade8_YOS ~ Hy_PW25DC-((1-DCGR_Grade9)*EmpTotCont25),
    YOS < DCGR_FullVest & YOS < DCGR_Grade8_YOS & YOS >= DCGR_Grade7_YOS ~ Hy_PW25DC-((1-DCGR_Grade8)*EmpTotCont25),
    YOS < DCGR_FullVest & YOS < DCGR_Grade7_YOS & YOS >= DCGR_Grade6_YOS ~ Hy_PW25DC-((1-DCGR_Grade7)*EmpTotCont25),
    YOS < DCGR_FullVest & YOS < DCGR_Grade6_YOS & YOS >= DCGR_Grade5_YOS ~ Hy_PW25DC-((1-DCGR_Grade6)*EmpTotCont25),
    YOS < DCGR_FullVest & YOS < DCGR_Grade5_YOS & YOS >= DCGR_Grade4_YOS ~ Hy_PW25DC-((1-DCGR_Grade5)*EmpTotCont25),
    YOS < DCGR_FullVest & YOS < DCGR_Grade4_YOS & YOS >= DCGR_Grade3_YOS ~ Hy_PW25DC-((1-DCGR_Grade4)*EmpTotCont25),
    YOS < DCGR_FullVest & YOS < DCGR_Grade3_YOS & YOS >= DCGR_Grade2_YOS ~ Hy_PW25DC-((1-DCGR_Grade3)*EmpTotCont25),
    YOS < DCGR_FullVest & YOS < DCGR_Grade2_YOS & YOS >= DCGR_Grade1_YOS  ~ Hy_PW25DC-((1-DCGR_Grade2)*EmpTotCont25),
    YOS < DCGR_FullVest & YOS < DCGR_Grade1_YOS  ~ Hy_PW25DC-((1-DCGR_Grade1)*EmpTotCont25),
    YOS >= DCGR_FullVest ~ Hy_PW25DC,
    YOS < DCGR_FullVest & DCGR_GradeVest  == 0 ~ (Hy_PW25DC - (Sal25*HyERRate25))
  ))
#This is for DC Plan Age 40 Entrants
Hy1 <- Hy1 %>%
  group_by(EquableClassID) %>%
  mutate(HyAdjDC_PW40 = case_when(
    YOS40 < DCGR_FullVest & YOS40 < DCGR_Grade10_YOS & YOS40 >= DCGR_Grade9_YOS ~ Hy_PW40DC-((1-DCGR_Grade10)*EmpTotCont40),
    YOS40 < DCGR_FullVest & YOS40 < DCGR_Grade9_YOS & YOS40 >= DCGR_Grade8_YOS ~ Hy_PW40DC-((1-DCGR_Grade9)*EmpTotCont40),
    YOS40 < DCGR_FullVest & YOS40 < DCGR_Grade8_YOS & YOS40 >= DCGR_Grade7_YOS ~ Hy_PW40DC-((1-DCGR_Grade8)*EmpTotCont40),
    YOS40 < DCGR_FullVest & YOS40 < DCGR_Grade7_YOS & YOS40 >= DCGR_Grade6_YOS ~ Hy_PW40DC-((1-DCGR_Grade7)*EmpTotCont40),
    YOS40 < DCGR_FullVest & YOS40 < DCGR_Grade6_YOS & YOS40 >= DCGR_Grade5_YOS ~ Hy_PW40DC-((1-DCGR_Grade6)*EmpTotCont40),
    YOS40 < DCGR_FullVest & YOS40 < DCGR_Grade5_YOS & YOS40 >= DCGR_Grade4_YOS ~ Hy_PW40DC-((1-DCGR_Grade5)*EmpTotCont40),
    YOS40 < DCGR_FullVest & YOS40 < DCGR_Grade4_YOS & YOS40 >= DCGR_Grade3_YOS ~ Hy_PW40DC-((1-DCGR_Grade4)*EmpTotCont40),
    YOS40 < DCGR_FullVest & YOS40 < DCGR_Grade3_YOS & YOS40 >= DCGR_Grade2_YOS ~ Hy_PW40DC-((1-DCGR_Grade3)*EmpTotCont40),
    YOS40 < DCGR_FullVest & YOS40 < DCGR_Grade2_YOS & YOS40 >= DCGR_Grade1_YOS  ~ Hy_PW40DC-((1-DCGR_Grade2)*EmpTotCont40),
    YOS40 < DCGR_FullVest & YOS40 < DCGR_Grade1_YOS  ~ Hy_PW40DC-((1-DCGR_Grade1)*EmpTotCont40),
    YOS40 >= DCGR_FullVest ~ Hy_PW40DC,
    YOS40 < DCGR_FullVest & DCGR_GradeVest  == 0 ~ (Hy_PW40DC - (Sal25*HyERRate40))
  ))


#This is for GR Plan Age 25 Entrants            
Hy1 <- Hy1 %>%
  group_by(EquableClassID) %>%
  mutate(HyAdjGR_PW25 = case_when(
    YOS < DCGR_FullVest & YOS < DCGR_Grade10_YOS & YOS >= DCGR_Grade9_YOS ~ Hy_PW25GR-((1-DCGR_Grade10)*EmpTotCont25),
    YOS < DCGR_FullVest & YOS < DCGR_Grade9_YOS & YOS >= DCGR_Grade8_YOS ~ Hy_PW25GR-((1-DCGR_Grade9)*EmpTotCont25),
    YOS < DCGR_FullVest & YOS < DCGR_Grade8_YOS & YOS >= DCGR_Grade7_YOS ~ Hy_PW25GR-((1-DCGR_Grade8)*EmpTotCont25),
    YOS < DCGR_FullVest & YOS < DCGR_Grade7_YOS & YOS >= DCGR_Grade6_YOS ~ Hy_PW25GR-((1-DCGR_Grade7)*EmpTotCont25),
    YOS < DCGR_FullVest & YOS < DCGR_Grade6_YOS & YOS >= DCGR_Grade5_YOS ~ Hy_PW25GR-((1-DCGR_Grade6)*EmpTotCont25),
    YOS < DCGR_FullVest & YOS < DCGR_Grade5_YOS & YOS >= DCGR_Grade4_YOS ~ Hy_PW25GR-((1-DCGR_Grade5)*EmpTotCont25),
    YOS < DCGR_FullVest & YOS < DCGR_Grade4_YOS & YOS >= DCGR_Grade3_YOS ~ Hy_PW25GR-((1-DCGR_Grade4)*EmpTotCont25),
    YOS < DCGR_FullVest & YOS < DCGR_Grade3_YOS & YOS >= DCGR_Grade2_YOS ~ Hy_PW25GR-((1-DCGR_Grade3)*EmpTotCont25),
    YOS < DCGR_FullVest & YOS < DCGR_Grade2_YOS & YOS >= DCGR_Grade1_YOS  ~ Hy_PW25GR-((1-DCGR_Grade2)*EmpTotCont25),
    YOS < DCGR_FullVest & YOS < DCGR_Grade1_YOS  ~ Hy_PW25GR-((1-DCGR_Grade1)*EmpTotCont25),
    YOS >= DCGR_FullVest ~ Hy_PW25GR,
    YOS < DCGR_FullVest & DCGR_GradeVest  == 0 ~ (Hy_PW25GR - (Sal25*HyERRate25))
  ))
  
#This is for GR Plan Age 40 Entrants
Hy1 <- Hy1 %>%
  group_by(EquableClassID) %>%
  mutate(HyAdjGR_PW40 = case_when(
    YOS40 < DCGR_FullVest & YOS40 < DCGR_Grade10_YOS & YOS40 >= DCGR_Grade9_YOS ~ Hy_PW40GR-((1-DCGR_Grade10)*EmpTotCont40),
    YOS40 < DCGR_FullVest & YOS40 < DCGR_Grade9_YOS & YOS40 >= DCGR_Grade8_YOS ~ Hy_PW40GR-((1-DCGR_Grade9)*EmpTotCont40),
    YOS40 < DCGR_FullVest & YOS40 < DCGR_Grade8_YOS & YOS40 >= DCGR_Grade7_YOS ~ Hy_PW40GR-((1-DCGR_Grade8)*EmpTotCont40),
    YOS40 < DCGR_FullVest & YOS40 < DCGR_Grade7_YOS & YOS40 >= DCGR_Grade6_YOS ~ Hy_PW40GR-((1-DCGR_Grade7)*EmpTotCont40),
    YOS40 < DCGR_FullVest & YOS40 < DCGR_Grade6_YOS & YOS40 >= DCGR_Grade5_YOS ~ Hy_PW40GR-((1-DCGR_Grade6)*EmpTotCont40),
    YOS40 < DCGR_FullVest & YOS40 < DCGR_Grade5_YOS & YOS40 >= DCGR_Grade4_YOS ~ Hy_PW40GR-((1-DCGR_Grade5)*EmpTotCont40),
    YOS40 < DCGR_FullVest & YOS40 < DCGR_Grade4_YOS & YOS40 >= DCGR_Grade3_YOS ~ Hy_PW40GR-((1-DCGR_Grade4)*EmpTotCont40),
    YOS40 < DCGR_FullVest & YOS40 < DCGR_Grade3_YOS & YOS40 >= DCGR_Grade2_YOS ~ Hy_PW40GR-((1-DCGR_Grade3)*EmpTotCont40),
    YOS40 < DCGR_FullVest & YOS40 < DCGR_Grade2_YOS & YOS40 >= DCGR_Grade1_YOS  ~ Hy_PW40GR-((1-DCGR_Grade2)*EmpTotCont40),
    YOS40 < DCGR_FullVest & YOS40 < DCGR_Grade1_YOS  ~ Hy_PW40GR-((1-DCGR_Grade1)*EmpTotCont40),
    YOS40 >= DCGR_FullVest ~ Hy_PW40GR,
    YOS40 < DCGR_FullVest & DCGR_GradeVest  == 0 ~ (Hy_PW40GR - (Sal25*HyERRate40))
  ))
#Now let us get the overall hybrid Benefit Value. 
#Now we do a quick adjustment for plans such that we don't count them as both Hybrid DC and Hybrid GR.  At the moment, all of our plans are Hybrid DC plans, so we can just set our GR values to zero, however, in the event there are plans that have a GR component, we will need to do a case_when instead to selectively sum values such that we don't double count the YOS == 0 Pension wealth from both GR and DC, as there are no hybrids that have both a GR and a DC component in the database.     
Hy1$HybridBenefitValue25 <- 0 
Hy1$HybridBenefitValue40 <- 0 
#Then this needs to be done to account for the NA in the YOS == 0 row for PWealth25F and Pwealth40F, which will not be an NA for the DC or GR elements of the plan, and therefore need to be added to an actual number and not NA'd out. 
Hy1$PWealth25F[is.na(Hy1$PWealth25F)] <- 0
Hy1$PWealth40F[is.na(Hy1$PWealth40F)] <- 0
Hy1 <- Hy1 %>%
  group_by(EquableClassID) %>%
  mutate(HybridBenefitValue25  = case_when(
    is.na(GR_CreditingRate) ~ PWealth25F + HyAdjDC_PW25,
    !is.na(GR_CreditingRate) ~ PWealth25F + HyAdjGR_PW25 ))
Hy1 <- Hy1 %>%
  group_by(EquableClassID) %>%
  mutate(HybridBenefitValue40 = case_when(
    is.na(GR_CreditingRate) ~ PWealth40F + HyAdjDC_PW40,
    !is.na(GR_CreditingRate) ~ PWealth40F + HyAdjGR_PW40))
#Inflation Adjustment for Hybrid Benefits
Hy1$Inf.HBV25 <- Hy1$HybridBenefitValue25/((1+Hy1$Inflation)^(Hy1$Age-25))
Hy1$Inf.HBV40 <- Hy1$HybridBenefitValue40/((1+Hy1$Inflation)^(Hy1$Age-40))  
#Make merged dataframe
#First we are going to declare a new dataframe M16, by selecting only the variables we need from the DB1 dataframe
M16 <- DB1 %>%
  select(EquableClassID, Plan_FullName, Class_Name, Plan.Type, YOS, YOS40, AdjPW25, AdjPW40, FASTarget25, FASTarget40, Inflation, SSA_Enroll, FAS_Retire25, FAS_Retire40, Sal25, Sal40, FAS25, FAS40, Age, DiscRate, FAS_COLARate, FAS_Years)
#We then do the same thing with the DC dataframe, and then also the GR and Hybrid dataframes.
DC2 <- DC1 %>%
  select(EquableClassID, Plan_FullName, Class_Name, Plan.Type, YOS, YOS40, Inf.DC_PW25, Inf.DC_PW40,FASTarget25,FASTarget40, SSA_Enroll,Inflation, Sal25, Sal40, FAS25, FAS40, Age, DiscRate, FAS_COLARate, FAS_Years)

GR2 <- GR1 %>%
  select(EquableClassID, Plan_FullName, Class_Name, Plan.Type, YOS, YOS40, Inf.GR_PW25, Inf.GR_PW40,FASTarget25,FASTarget40, SSA_Enroll,Inflation, Sal25, Sal40, FAS25, FAS40, Age, DiscRate, FAS_COLARate, FAS_Years,FAS_Retire25, FAS_Retire40)

Hy2 <- Hy1 %>%
  select(EquableClassID, Plan_FullName, Class_Name, Plan.Type, YOS, YOS40, Inf.HBV25, Inf.HBV40, SSA_Enroll, Inflation, FASTarget25, FASTarget40, AdjPW25, AdjPW40, Sal25, Sal40, FAS25, FAS40, Age, DiscRate, FAS_COLARate, FAS_Years,FAS_Retire25, FAS_Retire40)

#Here we are merging M16 with DC2, by the Equable ID, Plan_FullName, YOS, YOS40, and Plan.Type categories. Adding Plan.Type here just makes it so that we won't have a duplicate Plan.Type variable after the merge (we would otherwise, due to the all.x and all.y being TRUE)
M17 <- merge(M16,DC2, by = c("EquableClassID","Plan_FullName","YOS","YOS40","Class_Name", "Plan.Type", "Inflation", "SSA_Enroll", "Sal25","Sal40","FAS25","FAS40", "Age","DiscRate", "FAS_COLARate", "FAS_Years"), all.x = TRUE, all.y = TRUE)
#Here we are renaming the FASTarget Variables for consistency and ease of use.
names(M17)[names(M17) == "FASTarget25.x"] <- "FASTarget25"
names(M17)[names(M17) == "FASTarget40.x"] <- "FASTarget40"

#Here we are updating the FASTarget25 and FASTarget40 variables so that the two different FAS Columns (From DB and DC) are merged into one.
M17 <- M17%>%
  group_by(EquableClassID)%>%
  mutate(FASTarget25 = case_when(
    !is.na(FASTarget25) ~ FASTarget25,
    is.na(FASTarget25) ~ FASTarget25.y
  ))%>%
  mutate(FASTarget40 = case_when(
    !is.na(FASTarget40) ~ FASTarget40,
    is.na(FASTarget40) ~ FASTarget40.y
  ))%>%
  select(-FASTarget25.y, -FASTarget40.y)

#Here we merge in the GR2 dataframe (i.e. the GR Plans), and then do the same cleaning and mutating we did for the DC plans
M18 <- merge(M17,GR2, by = c("EquableClassID","Plan_FullName","YOS","YOS40", "Class_Name", "Plan.Type", "Inflation", "SSA_Enroll", "Sal25","Sal40","FAS25","FAS40","Age","DiscRate", "FAS_COLARate", "FAS_Years", "FAS_Retire25", "FAS_Retire40"), all.x = TRUE, all.y = TRUE)

names(M18)[names(M18) == "FASTarget25.x"] <- "FASTarget25"
names(M18)[names(M18) == "FASTarget40.x"] <- "FASTarget40"

M18 <- M18%>%
  group_by(EquableClassID)%>%
  mutate(FASTarget25 = case_when(
    !is.na(FASTarget25) ~ FASTarget25,
    is.na(FASTarget25) ~ FASTarget25.y
  ))%>%
  mutate(FASTarget40 = case_when(
    !is.na(FASTarget40) ~ FASTarget40,
    is.na(FASTarget40) ~ FASTarget40.y
  ))%>%
  select(-FASTarget25.y, -FASTarget40.y)

#Finally we merge in our hybrid plans. Note, we don't need to do anything fancy with FASTargets here, as we can just use the DB FASTargets we generated back in the DB section
M19 <- merge(M18,Hy2, by = c("EquableClassID","Plan_FullName","YOS","YOS40","Class_Name", "Plan.Type", "Inflation", "SSA_Enroll", "Sal25","Sal40","FAS25","FAS40","Age","DiscRate", "FAS_COLARate", "FAS_Years", "FAS_Retire25", "FAS_Retire40"), all.x = TRUE, all.y = TRUE)

names(M19)[names(M19) == "FASTarget25.x"] <- "FASTarget25"
names(M19)[names(M19) == "FASTarget40.x"] <- "FASTarget40"

M19 <- M19%>%
  group_by(EquableClassID)%>%
  mutate(FASTarget25 = case_when(
    !is.na(FASTarget25) ~ FASTarget25,
    is.na(FASTarget25) ~ FASTarget25.y
  ))%>%
  mutate(FASTarget40 = case_when(
    !is.na(FASTarget40) ~ FASTarget40,
    is.na(FASTarget40) ~ FASTarget40.y
  ))%>%
  select(-FASTarget25.y, -FASTarget40.y)

names(M19)[names(M19) == "AdjPW25.x"] <- "AdjPW25"
names(M19)[names(M19) == "AdjPW40.x"] <- "AdjPW40"

M19 <- M19%>%
  group_by(EquableClassID)%>%
  mutate(AdjPW25 = case_when(
    !is.na(AdjPW25) ~ AdjPW25,
    is.na(AdjPW25) ~ AdjPW25.y
  ))%>%
  mutate(AdjPW40 = case_when(
    !is.na(AdjPW40) ~ AdjPW40,
    is.na(AdjPW40) ~ AdjPW40.y
  ))%>%
  select(-AdjPW25.y, -AdjPW40.y)



#Now we Calculate Total Benefit value (its just the sum of the inflation adjusted benefit values from each of the different plan types)  Note that we are fist setting out NA values to 0, so as to avoid R getting mad at us
M19[is.na(M19)] <- 0

M19 <- M19 %>%
  group_by(EquableClassID)%>%
  mutate(TotalBenefitsValue25 = case_when(
    Plan.Type != "Hybrid" ~ AdjPW25 + Inf.DC_PW25 + Inf.GR_PW25,
    Plan.Type == "Hybrid" ~ Inf.DC_PW25 + Inf.GR_PW25 + Inf.HBV25
  ))%>%
  mutate(TotalBenefitsValue40 = case_when(
    Plan.Type != "Hybrid" ~ AdjPW40 + Inf.DC_PW40 + Inf.GR_PW40,
    Plan.Type == "Hybrid" ~ Inf.DC_PW40 + Inf.GR_PW40 + Inf.HBV40))

#M19$TotalBenefitsValue25 <- M19$AdjPW25 + M19$Inf.DC_PW25 + M19$Inf.GR_PW25 + M19$Inf.HBV25
#M19$TotalBenefitsValue40 <- M19$AdjPW40 + M19$Inf.DC_PW40 + M19$Inf.GR_PW40 + M19$Inf.HBV40



#This is set up for annuitizing Total Benefits Value
M19$tCOLA <- (M19$Inflation)
M19$ARt <- ((1.036)/(1 + M19$tCOLA))-1

M19 <- M19 %>%
  group_by(EquableClassID)%>%
  mutate(payments25 = case_when(
    Plan.Type == "FAS" ~ 86 - FAS_Retire25,
    Plan.Type == "Hybrid" ~ 86 - FAS_Retire25,
    Plan.Type == "DC" ~ 19,
    Plan.Type == "GR" ~ 86 - FAS_Retire25
  ))

M19 <- M19 %>%
  group_by(EquableClassID)%>%
  mutate(payments40 = case_when(
    Plan.Type == "FAS" ~ 86 - FAS_Retire40,
    Plan.Type == "Hybrid" ~ 86 - FAS_Retire40,
    Plan.Type == "DC" ~ 19,
    Plan.Type == "GR" ~ 86 - FAS_Retire40
  ))

#Annuitize Total Benefits Value
M19$TotalAnnVal25 <- (M19$TotalBenefitsValue25*M19$ARt)/(1 - ((1+M19$ARt)^(-1*M19$payments25)))
M19$TotalAnnVal40 <- (M19$TotalBenefitsValue40*M19$ARt)/(1 - ((1+M19$ARt)^(-1*M19$payments40)))


#Need a block here that ensures that annuitized value does not decrease post normal retirement age

M19 <- M19
M19$AdjAnnVal25 <- 0
M19$AdjAnnVal40 <- 0

M19 <- M19 %>%
  group_by(EquableClassID)%>%
  mutate(AdjAnnVal25 = case_when(
    TotalAnnVal25 > dplyr::lag(TotalAnnVal25,n=1L, default = 0) ~ TotalAnnVal25,
    TotalAnnVal25 < dplyr::lag(TotalAnnVal25,n=1L, default = 0) ~ max(TotalAnnVal25)
  ))%>%
  mutate(AdjAnnVal40 = case_when(
    TotalAnnVal40 > dplyr::lag(TotalAnnVal40,n=1L, default = 0) ~ TotalAnnVal40,
    TotalAnnVal40 < dplyr::lag(TotalAnnVal40,n=1L, default = 0) ~ max(TotalAnnVal25)
  ))

M19[is.na(M19)] <- 0


#And now Replacement Rates (with and without Social Security)
M19$Replace25 <- M19$AdjAnnVal25/M19$FASTarget25
M19$Replace40 <- M19$AdjAnnVal40/M19$FASTarget40

#This will be added in after we discuss with Anthony. Mostly intending on using SSA for the threshold calculations, not so much for actually calculation replacement rates
#And Finally, add on the Social Security Adjustment for Plans that have social security coverage
M20<- M19

M21 <- M20 %>%
  select(EquableClassID, Plan_FullName, Class_Name, SSA_Enroll, YOS, YOS40, Plan.Type,TotalBenefitsValue25, TotalBenefitsValue40, AdjAnnVal25, AdjAnnVal40, Inflation, payments25, payments40, Sal25, Sal40, FAS25, FAS40, Age, DiscRate, FAS_COLARate, FAS_Years)

M21$dcInt <- 0.055
M21$ThreshCOLA <- M21$Inflation
#M21 <- M21 %>%
#group_by(EquableClassID)
#mutate(ThreshCOLA = case_when(
#is.na(FAS_Years) ~ Inflation,
#!is.na(FAS_Years) ~ FAS_COLARate
#))
#Create inflation adjusted Salary
M21$Inf.Sal25 <- M21$Sal25/((1+M21$Inflation)^(M21$Age-25))
M21$Inf.Sal40 <- M21$Sal40/((1+M21$Inflation)^(M21$Age-40))

library(zoo)
M21a <- M21 %>% 
  group_by(EquableClassID) %>%
  dplyr::filter(Plan.Type == "FAS" | Plan.Type== "Hybrid")%>%
  mutate(Inf.FAS25 = ifelse(is.na(FAS_Years), NA,
                        rollmean(Inf.Sal25, k = FAS_Years, fill = 0, align = "right"))) %>% 
  mutate(Inf.FAS25 = lag(Inf.FAS25, default = 0))%>%
  mutate(Inf.FAS40 = ifelse(is.na(FAS_Years), NA,
                        rollmean(Inf.Sal40, k = FAS_Years, fill = 0, align = "right"))) %>% 
  mutate(Inf.FAS40 = lag(Inf.FAS40, default = 0))%>%
  mutate(Inf.FAS40 = case_when(
    YOS40 >= FAS_Years ~ Inf.FAS40,
    YOS40 < FAS_Years ~ 0
  ))

M21b <- M21 %>%
  group_by(EquableClassID)%>%
  dplyr::filter(Plan.Type == "DC" | Plan.Type == "GR")
M21b$Inf.FAS25 <- 0
M21b$Inf.FAS40 <- 0
  
M22 <- rbind(M21a,M21b)
M22 <- M22 %>%
  group_by(EquableClassID)%>%
  arrange(EquableClassID,YOS)
#Make FASTargets
  M23 <- M22 %>%
  group_by(EquableClassID) %>%
  mutate(targetFAS25 = case_when(
    Age == 67 ~ Inf.FAS25
  ))%>%
  mutate(targetFAS40 = case_when(
    Age == 67 ~ Inf.FAS40
  ))%>%
  mutate(FASTarget25 = case_when(
    is.na(targetFAS25) ~ targetFAS25[Age == 67],
    !is.na(targetFAS25) ~ targetFAS25[Age == 67]
  ))%>%
  mutate(FASTarget40 = case_when(
    is.na(targetFAS40) ~ targetFAS40[Age == 67],
    !is.na(targetFAS40) ~ targetFAS40[Age == 67]
  ))%>%
  select(-targetFAS25, -targetFAS40)%>%
  mutate(Target25 = case_when(
    Plan.Type == "FAS" ~ FASTarget25,
    Plan.Type == "Hybrid" ~ FASTarget25,
    Plan.Type == "DC" ~ Inf.Sal25[Age == 67],
    Plan.Type == "GR" ~ Inf.Sal25[Age == 67]
  ))%>%
  mutate(Target40 = case_when(
    Plan.Type == "FAS" ~ FASTarget40,
    Plan.Type == "Hybrid" ~ FASTarget40,
    Plan.Type == "DC" ~ Inf.Sal40[Age == 67],
    Plan.Type == "GR" ~ Inf.Sal40[Age == 67]))
  
#Annual Rate  
M23$AnnRate <- ((1.036)/(1 + M23$ThreshCOLA))-1

M23a <- M23

M23$StartBen25 <- 0
M23$StartBen40 <- 0

M23 <- M23 %>%
  group_by(EquableClassID)%>%
  mutate(Target25F = case_when(
    SSA_Enroll == "No" ~ Target25*.7,
    SSA_Enroll == "Mixed" ~ Target25*.7,
    SSA_Enroll == "Yes" ~ Target25 * (.7-.33)
  ))
M23$shift25 <- (M23$Target25F- M23$StartBen25)/(67-25)

M23$Thresh25 <- 0
M23$T25 <- 0
for (i in 26:70) {  
  M23 <- M23%>%
    group_by(EquableClassID)%>%
    mutate(T25 = case_when(
      Age == 25 ~ StartBen25,
      Age > 25 ~ (T25[Age== i-1] + shift25[Age==i])
    ))
  M23$Thresh25[M23$Age == i] <- M23$T25[M23$Age==i]
}

M23$ThreshRep25 <- M23$Thresh25/M23$Target25

#40 Year old entrant
M23$startsaveT <- 0
M23<-M23%>%
  group_by(EquableClassID)%>%
  mutate(startsaveT = case_when(
    Age == 40 ~ ThreshRep25[Age == 40],
    Age != 40 ~ ThreshRep25[Age == 40]
  ))

M23 <- M23 %>%
  group_by(EquableClassID)%>%
  mutate(Target40F = case_when(
    SSA_Enroll == "No" ~ Target40*.7,
    SSA_Enroll == "Mixed" ~ Target40*.7,
    SSA_Enroll == "Yes" ~ Target40 * (.7-.33)
  ))%>%
  mutate(Target40F = Target40F - (startsaveT*Target40))

M23$shift40 <- (M23$Target40F - M23$StartBen40)/(67-40)
M23$Thresh40 <- 0
M23$T40 <- 0
for (i in 41:70) {  
  M23 <- M23%>%
    group_by(EquableClassID)%>%
    mutate(T40 = case_when(
      Age == 40 ~ StartBen40,
      Age > 40 ~ (T40[Age== i-1] + shift40[Age==i])
    ))
  M23$Thresh40[M23$Age == i] <- M23$T40[M23$Age==i]
}

M23$ThreshRep40 <- M23$Thresh40/M23$Target40

M24 <- M23%>%
  select(-T25,-T40)

#write.csv(M24, "/Users/David/Desktop/R things/AllPlanOutput_14May21.csv")

#M24t <- M24 %>%
  #filter(EquableClassID == 6010100| EquableClassID == 6010200)%>%
  #select(EquableClassID, Age, YOS, YOS40, TotalBenefitsValue25,TotalBenefitsValue40,TotalAnnVal25,TotalAnnVal40)
#M24t <- M24 %>%
  #filter(EquableClassID == 26010100)

#MODOTU <- ggplot(data = M24t, aes (x =Age, y = AdjAnnVal25)) +
  #geom_line() +
  #geom_point()

#MODOTU


#write.csv(M6t, "/Users/David/Desktop/R things/TwoPlanTest_22Apr21.csv")



#Bonus, 60% Thresholds
#25 year old entrant
M24a <- M24
M24 <- M24 %>%
  group_by(EquableClassID)%>%
  mutate(Target25F_60 = case_when(
    SSA_Enroll == "No" ~ Target25*.6,
    SSA_Enroll == "Mixed" ~ Target25*.6,
    SSA_Enroll == "Yes" ~ Target25 * (.6-.33)
  ))
M24$shift25_60 <- (M24$Target25F_60- M24$StartBen25)/(67-25)

M24$Thresh25_60 <- 0
M24$T25_60 <- 0
for (i in 26:70) {  
  M24 <- M24%>%
    group_by(EquableClassID)%>%
    mutate(T25_60 = case_when(
      Age == 25 ~ StartBen25,
      Age > 25 ~ (T25_60[Age== i-1] + shift25_60[Age==i])
    ))
  M24$Thresh25_60[M24$Age == i] <- M24$T25_60[M24$Age==i]
}

M24$ThreshRep25_60 <- M24$Thresh25_60/M24$Target25

#40 Year old Entrant
M24$startsaveT_60 <- 0
M24<-M24%>%
  group_by(EquableClassID)%>%
  mutate(startsaveT_60 = case_when(
    Age == 40 ~ ThreshRep25_60[Age == 40],
    Age != 40 ~ ThreshRep25_60[Age == 40]
  ))

M24 <- M24 %>%
  group_by(EquableClassID)%>%
  mutate(Target40F_60 = case_when(
    SSA_Enroll == "No" ~ Target40*.6,
    SSA_Enroll == "Mixed" ~ Target40*.6,
    SSA_Enroll == "Yes" ~ Target40 * (.6-.33)
  ))%>%
  mutate(Target40F_60 = Target40F_60 - (startsaveT_60*Target40))

M24$shift40_60 <- (M24$Target40F_60 - M24$StartBen40)/(67-40)
M24$Thresh40_60 <- 0
M24$T40_60 <- 0
for (i in 41:70) {  
  M24 <- M24%>%
    group_by(EquableClassID)%>%
    mutate(T40_60 = case_when(
      Age == 40 ~ StartBen40,
      Age > 40 ~ (T40_60[Age== i-1] + shift40_60[Age==i])
    ))
  M24$Thresh40_60[M24$Age == i] <- M24$T40_60[M24$Age==i]
}

M24$ThreshRep40_60 <- M24$Thresh40_60/M24$Target40

M25 <- M24%>%
  select(-T25_60,-T40_60)


#Bonus, 80% Thresholds
#25 year old entrant
M25a <- M25
M25 <- M25 %>%
  group_by(EquableClassID)%>%
  mutate(Target25F_80 = case_when(
    SSA_Enroll == "No" ~ Target25*.8,
    SSA_Enroll == "Mixed" ~ Target25*.8,
    SSA_Enroll == "Yes" ~ Target25 * (.8-.33)
  ))
M25$shift25_80 <- (M25$Target25F_80- M25$StartBen25)/(67-25)

M25$Thresh25_80 <- 0
M25$T25_80 <- 0
for (i in 26:70) {  
  M25 <- M25%>%
    group_by(EquableClassID)%>%
    mutate(T25_80 = case_when(
      Age == 25 ~ StartBen25,
      Age > 25 ~ (T25_80[Age== i-1] + shift25_80[Age==i])
    ))
  M25$Thresh25_80[M25$Age == i] <- M25$T25_80[M25$Age==i]
}

M25$ThreshRep25_80 <- M25$Thresh25_80/M25$Target25

#40 Year old Entrant
M25$startsaveT_80 <- 0
M25<-M25%>%
  group_by(EquableClassID)%>%
  mutate(startsaveT_80 = case_when(
    Age == 40 ~ ThreshRep25_80[Age == 40],
    Age != 40 ~ ThreshRep25_80[Age == 40]
  ))

M25 <- M25 %>%
  group_by(EquableClassID)%>%
  mutate(Target40F_80 = case_when(
    SSA_Enroll == "No" ~ Target40*.8,
    SSA_Enroll == "Mixed" ~ Target40*.8,
    SSA_Enroll == "Yes" ~ Target40 * (.8-.33)
  ))%>%
  mutate(Target40F_80 = Target40F_80 - (startsaveT_80*Target40))

M25$shift40_80 <- (M25$Target40F_80 - M25$StartBen40)/(67-40)
M25$Thresh40_80 <- 0
M25$T40_80 <- 0
for (i in 41:70) {  
  M25 <- M25%>%
    group_by(EquableClassID)%>%
    mutate(T40_80 = case_when(
      Age == 40 ~ StartBen40,
      Age > 40 ~ (T40_80[Age== i-1] + shift40_80[Age==i])
    ))
  M25$Thresh40_80[M25$Age == i] <- M25$T40_80[M25$Age==i]
}

M25$ThreshRep40_80 <- M25$Thresh40_80/M25$Target40

M26 <- M25%>%
  select(-T25_80,-T40_80)



