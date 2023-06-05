# brfss 2021 hpv vaccination
# project set up and recoding

#########################################################################################################

# install packages only once
install.packages("rio")
install.packages("tidyverse")
install.packages("janitor")
install.packages("survey")
install.packages("gtsummary")
install.packages("purrr")
install.packages("rlist")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("officer")
install.packages("forcats")
install.packages("scales")
install.packages("car")

# load libraries 
library(rio) # importing and exporting data
library(tidyverse) # data management and visualization
library(janitor) # data cleaning and tables
library(survey) # survey design: for survey functions
library(gtsummary) # data summary tables
library(purrr) # function to iterate 
library(rlist) # append lists function
library(dplyr) # data manipulation
library(ggplot2)
library(stringr) # working with characters  
library(forcats) # graphs
library(scales) # percent function in 
library(car) # glm customization

#########################################################################################################

# import data and remove after: 
raw_brfss21 <- import("LLCP2021.XPT")
# 438693 obs. of  303 variables
str(raw_brfss21)
# version 1
raw_brfss21v1 <- import("LLCP21V1.XPT")
# 438693 obs. of  303 variables
str(raw_brfss21v1)

#########################################################################################################

# check format of column names 
names(raw_brfss21)
# standardizes column names
raw_brfss21 <- raw_brfss21 %>% 
  clean_names()

names(raw_brfss21v1)
raw_brfss21v1 <- raw_brfss21v1 %>% 
  clean_names()

#########################################################################################################

# subset: states that completed HPV vaccination module for 2021
# Delaware=10, Georgia=13, Hawaii=15, Mississippi=28, New Jersey=34, Tennessee=47, West Virginia=54 
# [row, column]
states21 <- raw_brfss21[raw_brfss21$state %in% c(10, 13, 15, 28, 34, 47, 54), ]
# rename data file's weight _LLCPWT to finalwt
states21 <- rename(states21, finalwt = llcpwt)

# check: 43531 obs. of  303 variables
names(states21)
str(states21)


# Massachusetts=25
# data file's weight = lcpwtv1
# can also use: raw_statesv1 <- filter(raw_brfss21v1, state == 25)
states21v1 <- raw_brfss21v1[raw_brfss21v1$state %in% c(25), ]
states21v1 <- rename(states21v1, finalwt = lcpwtv1)
# 3566 obs. of  303 variables
names(states21v1)
str(states21v1)

#########################################################################################################

# Error in match.names(clabs, names(xi)) : names do not match previous names
# b/c clcwtv1 vs cllcpwt
# adjust code to fix 

##df1_names <- as.data.frame(colnames(raw_states))
##export(df1_names, "df1_names.csv")

# check: cllcpwt was replaced with clcwtv1 
##df3_names <- as.data.frame(colnames(raw_comb21))
##export(df3_names, "df3_names.csv")

#########################################################################################################

# combine all data sets
colnames(states21) = colnames(states21v1)
raw_comb21 <- rbind(states21, states21v1)

# 47097 obs. of  303 variables
str(raw_comb21)

#########################################################################################################

# give all variables and T/F for which vars have all NAs
colSums(is.na(raw_comb21)) == nrow(raw_comb21)
# create a dropvar object to store set of columns with 100% NAs
drop_na_vars21 <- which(colSums(is.na(raw_comb21)) == nrow(raw_comb21))

# [row, column] = 8 columns/vars dropped
raw_clean21 <- raw_comb21[, -drop_na_vars21]

# 47097 obs. of  295 variables
str(raw_clean21)

#########################################################################################################

# inclusion criteria for age: 18-29 only: groups 1 and 2 
# 18-24 = 2866, 25-29 = 2358 
count(raw_clean21, vars = ageg5yr)
# select rows & create object to capture them and then filter them out
which(raw_clean21$ageg5yr != 1 & raw_clean21$ageg5yr != 2)
exclude_agerows <- which(raw_clean21$ageg5yr != 1 & raw_clean21$ageg5yr != 2)
raw_group21 <- filter(raw_clean21[-exclude_agerows,])

# 5224 obs. of  295 variables
str(raw_group21)
count(raw_group21, vars = ageg5yr)

#########################################################################################################
 
# inclusion criteria: interview completed
# complete = 3954, partial = 1270
count(raw_group21, vars = dispcode)
exclude_partial <- which(raw_group21$dispcode != 1100)
raw_complete21 <- filter(raw_group21[-exclude_partial,])

# 3954 obs. of  295 variables
str(raw_complete21)
count(raw_complete21, vars = dispcode)

#########################################################################################################

# subset your data set with the variables you need for analysis
hpv_vars21 <- c(
  # survey design & weight vars & state
  "psu", "finalwt", "ststr",
  # demographic vars
  "sex", "marital", "educag", "race", "ageg5yr", "state",
  # health insurance
  "priminsr", "hlthpln", 
  # cervical screening 
  "cervscrn", "crvclcnc", 
  # pap test
  "crvclpap", 
  # hpv test 
  "crvclhpv", 
  # hpv vaccination
  "hpvadvc4", "hpvadsht",
  # routine doc check
  "checkup1", 
  # gen health rating
  "rfhlth")

# subset keep all rows and above columns
raw_hpv21 <- raw_complete21[, hpv_vars21]
# 3954 obs. of  19 variables
str(raw_hpv21)

#########################################################################################################

# create a copy to re-code and change class of variables
coded_hpv21 <- raw_hpv21

###########
# Male = 2023, Female = 1931
coded_hpv21$sex <- as.factor(
  recode(coded_hpv21$sex, 
         '1' = 'Male', 
         '2' = 'Female', 
         .missing = NULL,
         .default = NULL))
coded_hpv21$sex <- factor(coded_hpv21$sex, 
                              levels = c('Female', 'Male'))
class(coded_hpv21$sex)
count(coded_hpv21, vars = sex)
###########
# NA = 30, 1 = Relationship = 1011, 2 = Single = 2913
coded_hpv21$marital <- as.factor(
  recode(coded_hpv21$marital, 
         '1' = 'Relationship', 
         '2' = 'Single', 
         '3' = 'Single',
         '4' = 'Single', 
         '5' = 'Single',
         '6' = 'Relationship', 
         '9' = NULL, 
         .missing = NULL,
         .default = NULL))
coded_hpv21$marital <- factor(coded_hpv21$marital, 
                              levels = c('Relationship', 'Single'))
class(coded_hpv21$marital)
count(coded_hpv21, vars = marital)

###########
# NA = 11, 1 = Not HS Grad = 208, 2 = HS Grad = 1327, # 3 = Some College = 1192, 4 = College Grad = 1216
coded_hpv21$educag <- as.factor(
  recode(coded_hpv21$educag, 
         '1' = 'Not HS Grad', 
         '2' = 'HS Grad', 
         '3' = 'Some College',
         '4' = 'College Grad', 
         '9' = NULL,
         .missing = NULL,
         .default = NULL))
coded_hpv21$educag <- factor(coded_hpv21$educag, 
                             levels = c('Not HS Grad', 'HS Grad', 'Some College', 'College Grad'))
class(coded_hpv21$educag)
count(coded_hpv21, vars = educag)
###########
# NA = 64, 1 = Non-Hispanic White = 1924, 2 = Non-Hispanic Black/African American = 487, 3 = Non-Hispanic American Indian/Alaskan Native = 10, 
# 4 = Non-Hispanic Asian = 420, 5 = Non-Hispanic Native Hawaiian/Pacific Islander = 104, 
# 6 = Non-Hispanic Other = 30, 7 = Non-Hispanic Multiracial = 251, 8 = Hispanic = 664
coded_hpv21$race <- as.factor(
  recode(coded_hpv21$race, 
         '1' = 'Non-Hispanic White', 
         '2' = 'Non-Hispanic Black/African American', 
         '3' = 'Non-Hispanic American Indian/Alaskan Native',
         '4' = 'Non-Hispanic Asian', 
         '5' = 'Non-Hispanic Native Hawaiian/Pacific Islander',
         '6' = 'Non-Hispanic Other/Multiracial',
         '7' = 'Non-Hispanic Other/Multiracial',
         '8' = 'Hispanic',
         '9' = NULL,
         .missing = NULL,
         .default = NULL))
coded_hpv21$race <- factor(coded_hpv21$race, 
                           levels = c('Hispanic', 'Non-Hispanic White', 'Non-Hispanic Black/African American', 
                                      'Non-Hispanic Asian', 'Non-Hispanic American Indian/Alaskan Native',
                                      'Non-Hispanic Native Hawaiian/Pacific Islander', 
                                      'Non-Hispanic Other/Multiracial'))
class(coded_hpv21$race)
count(coded_hpv21, vars = race)
###########
# 1 = 18-24yrs = 2210, 2 = 25-29yrs = 1744
coded_hpv21$ageg5yr <- as.factor(
  recode(coded_hpv21$ageg5yr, 
         '1' = '18-24 yrs', 
         '2' = '25-29 yrs', 
         .missing = NULL,
         .default = NULL))
coded_hpv21$ageg5yr <- factor(coded_hpv21$ageg5yr, 
                             levels = c('18-24 yrs', '25-29 yrs'))
class(coded_hpv21$ageg5yr)
count(coded_hpv21, vars = ageg5yr)
###########
# NA = 340, 1 = Employer/Private = 2150, 2 = Federal/State = 867, 3 = Military = 126, 4 = Other = 3, 5 = No Coverage = 468
coded_hpv21$priminsr <- as.factor(
  recode(coded_hpv21$priminsr, 
         '1' = 'Employer/Private', 
         '2' = 'Employer/Private',
         '3' = 'Federal/State', 
         '4' = 'Federal/State', 
         '5' = 'Federal/State', 
         '6' = 'Federal/State', 
         '7' = 'Military', 
         '8' = 'Other', 
         '9' = 'Federal/State', 
         '10' = 'Federal/State', 
         '88' = 'No Coverage',
         '77' = NULL,
         '99' = NULL,
         .missing = NULL,
         .default = NULL))
coded_hpv21$priminsr <- factor(coded_hpv21$priminsr, 
                              levels = c('Employer/Private', 'Federal/State',
                                         'Military', 'Other', 'No Coverage'))
class(coded_hpv21$priminsr)
count(coded_hpv21, vars = priminsr)
###########
# NA = 340, 1 = Have Insurance = 3146, 2 = No Insurance  468
coded_hpv21$hlthpln <- as.factor(
  recode(coded_hpv21$hlthpln, 
         '1' = 'Have Insurance', 
         '2' = 'No Insurance',
         '9' = NULL,
         .missing = NULL,
         .default = NULL))
coded_hpv21$hlthpln <- factor(coded_hpv21$hlthpln, 
                               levels = c('Have Insurance','No Insurance'))
class(coded_hpv21$hlthpln)
count(coded_hpv21, vars = hlthpln)
###########
# NA = 3176, 1 = Had Cervical Cancer Screening = 216, 0 = Not Had CC Screening = 562
coded_hpv21$cervscrn <- as.factor(
  recode(coded_hpv21$cervscrn, 
         '1' = 'Yes', 
         '2' = 'No',
         '7' = NULL,
         '9' = NULL,
         .missing = NULL,
         .default = NULL))
coded_hpv21$cervscrn <- factor(coded_hpv21$cervscrn, 
                              levels = c('Yes','No'))
class(coded_hpv21$cervscrn)
count(coded_hpv21, vars = cervscrn)
###########
# NA = 3743, 1 = <12mths since last Cervical Cancer Screening = 117, 0 = >12mnths since last CC screening =94
coded_hpv21$crvclcnc <- as.factor(
  recode(coded_hpv21$crvclcnc, 
         '1' = 'Yes', 
         '2' = 'No',
         '3' = 'No',
         '4' = 'No',
         '5' = 'No',
         '7' = NULL,
         .missing = NULL,
         .default = NULL))
coded_hpv21$crvclcnc <- factor(coded_hpv21$crvclcnc, 
                               levels = c('Yes','No'))
class(coded_hpv21$crvclcnc)
count(coded_hpv21, vars = crvclcnc)
###########
# NA = 3742, 1 = Had Pap Test at CC Screening = 208, 0 = Not Had Pap Test at CC Screening = 4
coded_hpv21$crvclpap <- as.factor(
  recode(coded_hpv21$crvclpap, 
         '1' = 'Yes', 
         '2' = 'No',
         '7' = NULL,
         .missing = NULL,
         .default = NULL))
coded_hpv21$crvclpap <- factor(coded_hpv21$crvclpap, 
                               levels = c('Yes','No'))
class(coded_hpv21$crvclpap)
count(coded_hpv21, vars = crvclpap)
###########
# NA = 3787, 1 = Had HPV Test at CC Screening = 97, 0 = Not Had HPV Test at CC Screening = 70
coded_hpv21$crvclhpv <- as.factor(
  recode(coded_hpv21$crvclhpv, 
         '1' = 'Yes', 
         '2' = 'No',
         '7' = NULL,
         .missing = NULL,
         .default = NULL))
coded_hpv21$crvclhpv <- factor(coded_hpv21$crvclhpv, 
                               levels = c('Yes','No'))
class(coded_hpv21$crvclhpv)
count(coded_hpv21, vars = crvclhpv)
###########
# NA = 650, 1 = HPV Vaccinated = 1417, 0 = Not = 1887
coded_hpv21$hpvadvc4 <- as.factor(
  recode(coded_hpv21$hpvadvc4, 
         '1' = '1', 
         '2' = '0',
         '3' = '0',
         '7' = NULL,
         '9' = NULL, 
         .missing = NULL,
         .default = NULL))
class(coded_hpv21$hpvadvc4)
count(coded_hpv21, vars = hpvadvc4)
###########
# NA = 2838, 1 = 1-2 shots = 528, 2 = All shots = 588
coded_hpv21$hpvadsht <- as.factor(
  recode(coded_hpv21$hpvadsht, 
         '1' = '1', 
         '2' = '1',
         '3' = '2', 
         '77' = NULL, 
         '99' = NULL,
         .missing = NULL,
         .default = NULL))
class(coded_hpv21$hpvadsht)
count(coded_hpv21, vars = hpvadsht)
###########
# NA = 157, 1 = <12mnths since routine doc check = 2479, 2 = >12mnths since routine doc check = 1318
coded_hpv21$checkup1 <- as.factor(
  recode(coded_hpv21$checkup1, 
         '1' = 'Yes', 
         '2' = 'No',
         '3' = 'No', 
         '4' = 'No', 
         '7' = NULL, 
         '8' = NULL,
         '9' = NULL,
         .missing = NULL,
         .default = NULL))
coded_hpv21$checkup1 <- factor(coded_hpv21$checkup1, 
                               levels = c('Yes','No'))
class(coded_hpv21$checkup1)
count(coded_hpv21, vars = checkup1)
###########
# NA = 8, 1 = In Good/Better Health = 3636, 2 = In Fair/Poor Health = 310
coded_hpv21$rfhlth <- as.factor(
  recode(coded_hpv21$rfhlth, 
         '1' = 'Yes', 
         '2' = 'No',
         '9' = NULL,
         .missing = NULL,
         .default = NULL))
coded_hpv21$rfhlth <- factor(coded_hpv21$rfhlth, 
                               levels = c('Yes','No'))
class(coded_hpv21$rfhlth)
count(coded_hpv21, vars = rfhlth)
###########
# 10 = 273, 13 = 459, 15 = 692, 25 = 470, 28 = 434, 34 = 751, 47 = 384, 54 = 491
coded_hpv21$state <- as.factor(
  recode(coded_hpv21$state,
         '10' = 'Delaware',
         '13' = 'Georgia',
         '15' = 'Hawaii',
         '28' = 'Mississippi',
         '34' = 'New Jersey',
         '47' = 'Tennessee',
         '54' = 'West Virginia',
         '25' = 'Massachusetts ',
         '9' = NULL,
         .missing = NULL,
         .default = NULL))
class(coded_hpv21$state)
count(coded_hpv21, vars = state)
###########
# No = 1887, Yes 1-2 shots = 528, Yes, all shots = 588, NA = 951
coded_hpv21 <- coded_hpv21 %>%
  mutate(hpvstat = case_when(
    # case_when() evaluates in order (if, else if, else if, ... else)
    hpvadvc4 == "0" ~ 'No',
    hpvadsht == "1" ~ 'Yes, 1-2 shots', 
    hpvadsht == "2" ~ 'Yes, all shots',
    TRUE ~ NA
  ))
coded_hpv21$hpvstat <- as.factor(coded_hpv21$hpvstat)
coded_hpv21$hpvstat <- factor(coded_hpv21$hpvstat, 
                             levels = c('No','Yes, 1-2 shots', 'Yes, all shots'))
class(coded_hpv21$hpvstat)
count(coded_hpv21, vars = hpvstat)

#########################################################################################################

# shows original values 
str(raw_hpv21) 
# shows re-coded values
glimpse(coded_hpv21)

#########################################################################################################

## SURVEY PACKAGE set up
options( survey.lonely.psu = "adjust" )
# create survey design -- CDC complex sampling weights
design_hpv21 <- svydesign(
  # cluster psu
  # cdc suggested 1 over psu
  ids = ~1,
  # sampling was stratified by ststr
  strata = ~ststr,
  # weight variable 
  weights = ~finalwt,
  # have to specify data set
  data = coded_hpv21, 
  nest = TRUE)
# add error = clusters not nested in strata at top level --> nest = TRUE

# produce a summary of the design
summary(design_hpv21)

# number of clusters = 3954
coded_hpv21 %>%
  summarize(n_clusters = n_distinct(ststr, psu))

# sample size in clusters
coded_hpv21 %>%
  count(ststr, psu)

#########################################################################################################
#####################################################################
#####################################################################
#####################################################################
#####################################################################
#####################################################################
#####################################################################
#####################################################################
#####################################################################
#####################################################################
#####################################################################
#####################################################################
#####################################################################
#####################################################################
#####################################################################
#####################################################################
#####################################################################
#####################################################################
#####################################################################
#####################################################################

#########################################################################################################
# set reference values for logistic regression & odds ratios
logreg_hpv21 <- coded_hpv21 %>%
  select(hpvstat, sex, race, educag, hlthpln, checkup1, rfhlth, cervscrn, psu, finalwt, ststr)

# re-code to set reference levels
logreg_hpv21$hpvshot <- recode(logreg_hpv21$hpvstat, 'No' = 'ref')
logreg_hpv21$hpvshot <- factor(logreg_hpv21$hpvshot, 
                              levels = c('ref','Yes, 1-2 shots', 'Yes, all shots'))
count(logreg_hpv21, vars = hpvshot)

logreg_hpv21$female <- recode(logreg_hpv21$sex, 'Female' = 'ref')
logreg_hpv21$female <- factor(logreg_hpv21$female, 
                               levels = c('ref','Male'))
count(logreg_hpv21, vars = female)

logreg_hpv21$hisp <- recode(logreg_hpv21$race, 'Hispanic' = 'ref') 
logreg_hpv21$hisp <- factor(logreg_hpv21$hisp, 
                               levels = c('ref',
                                          'Non-Hispanic White', 
                                          'Non-Hispanic Black/African American',
                                          'Non-Hispanic Asian',
                                          'Non-Hispanic American Indian/Alaskan Native',
                                          'Non-Hispanic Native Hawaiian/Pacific Islander',
                                          'Non-Hispanic Other/Multiracial'))
count(logreg_hpv21, vars = hisp)


logreg_hpv21$educ <- recode(logreg_hpv21$educag, 'Not HS Grad' = 'ref')
logreg_hpv21$educ <- factor(logreg_hpv21$educ, 
                               levels = c('ref',
                                          'HS Grad',
                                          'Some College',
                                          'College Grad'))
count(logreg_hpv21, vars = educ)


logreg_hpv21$ins <- recode(logreg_hpv21$hlthpln, 'No Insurance' = 'ref')
logreg_hpv21$ins <- factor(logreg_hpv21$ins, 
                               levels = c('ref',
                                          'Have Insurance'))
count(logreg_hpv21, vars = ins)


logreg_hpv21$hlth <- recode(logreg_hpv21$rfhlth, 'No' = 'ref')
logreg_hpv21$hlth <- factor(logreg_hpv21$hlth, 
                               levels = c('ref','Yes'))
count(logreg_hpv21, vars = hlth)

logreg_hpv21$dochk <- recode(logreg_hpv21$checkup1,'No' = 'ref') 
logreg_hpv21$dochk <- factor(logreg_hpv21$dochk, 
                             levels = c('ref','Yes'))
count(logreg_hpv21, vars = dochk)


logreg_hpv21$cervs <- recode(logreg_hpv21$cervscrn, 'No' = 'ref')
logreg_hpv21$cervs  <- factor(logreg_hpv21$cervs , 
                               levels = c('ref','Yes'))
count(logreg_hpv21, vars = cervs)

# select new vars
logreg_hpv21 <- logreg_hpv21 %>%
  select(hpvshot, female, hisp, educ, ins, hlth, dochk, cervs, psu, finalwt, ststr)

# shows original values 
str(logreg_hpv21) 
# shows re-coded values
glimpse(logreg_hpv21)
