/*************
	Project: NHANES: Immigrant Status & Social Support Analysis 
	Data Cycle: 2007-08
	Author: Maneesha Muriki 
	Last Updated: 5/31/2023  
*************/

* assign permanent library;
libname nhanes 'C:\Users\mm00247\Documents\Research\NHANES_PROJECT';

* sort the different data modules;
proc sort data=nhanes.ssq_e; by seqn; run;
proc sort data=nhanes.demo_e; by seqn; run;
proc sort data=nhanes.hsq_e; by seqn; run;

* merge all data modules by seqn: 10149 obs and 76 vars;
data nhanes.merged ; 
* 4025 obs from ssq_e, 10149 obs from demo_e, 9307 obs from hsq_e;
merge nhanes.ssq_e nhanes.demo_e nhanes.hsq_e; 
by seqn; 
run;

proc contents data=nhanes.merged order=varnum; run;

* check output before collapsing missing variables;
proc freq data = nhanes.merged;
tables SSQ011 HSD010 DMDCITZN DMDBORN2 DMDEDUC2 DMDYRSUS DMDMARTL HSQ470 HSQ480 HSQ490 SSQ021A/list missing;
run;


* collapse refused/don't know values;; 
data nhanes.collapsed; 
set nhanes.merged;
	array recode(5) SSQ011 HSD010 DMDCITZN DMDBORN2 DMDEDUC2;
	do I = 1 to 5;
	if recode(i) in(7,9) then recode(I)=.;
	end;
	drop I;
	array recode2(6) DMDYRSUS DMDMARTL HSQ470 HSQ480 HSQ490 SSQ021A;
	do I = 1 to 6; 
	if recode2 (i) in(77,99) then recode2(I)=.;
	end;
	drop I;
run;


* recode variables for analysis:  10149 observations and 87 variables;  
data nhanes.recoded; 
set nhanes.collapsed;
	
	* RIAGENDR not recoded;

	* RIDAGEYR recoded to be used as sub-population group for analysis;
	if (18<=RIDAGEYR<=79) OR (RIDAGEYR=80) then AGEGRP=1;
	else AGEGRP=0;

	* RIDRETH1 not recoded;

	* DMDEDUC2 recode;
	if DMDEDUC2 in(1,2) then EDUCLVL=1;
	else if DMDEDUC2=3 then EDUCLVL=2;
	else if DMDEDUC2=4 then EDUCLVL=3;
	else if DMDEDUC2=5 then EDUCLVL=4; 

	* DMDMARTL recode;
	if DMDMARTL=5 then MRTLSTAT=1;
	else if DMDMARTL=6 then MRTLSTAT=2;
	else if DMDMARTL=1 then MRTLSTAT=3;
	else if DMDMARTL=2 then MRTLSTAT=4;
	else if DMDMARTL in(3,4) then MRTLSTAT=5;

	* DMDCITZN/DMDBORN2 recode;
	if (DMDCITZN=2 AND DMDBORN2 in(2,4,5)) then IMMIGRANT=1; 
	else IMMIGRANT=0;

	* DMDYRSUS recode;
	if DMDYRSUS in(1,2) then TIMEUS=1;
	else if DMDYRSUS=3 then TIMEUS=2;
	else if DMDYRSUS in(4,5) then TIMEUS=3; 
	else if DMDYRSUS in(6,7,8,9) then TIMEUS=4;
	
	* SSQ011 recode;
	if SSQ011=1 then EMOSPRT=1; 
	else EMOSPRT=0;

	* SSQ021A, SSQ021B, SSQ021C, SSQ021D, SSQ021E, SSQ021F, SSQ021G, SSQ021H, SSQ021I, SSQ021J, SSQ021K, SSQ021L, SSQ021M recode;
	if SSQ021A=10 then WHOSPRT=1;
	else if (SSQ021B=11 AND SSQ021C=12) THEN WHOSPRT=2;
	else if SSQ021D=13 then WHOSPRT=3;
	else if SSQ021E=14 then WHOSPRT=4;
	else if SSQ021F=15 then WHOSPRT=5;
	else if SSQ021L=21 then WHOSPRT=6;
	else if SSQ021G=16 OR SSQ021H=17 OR SSQ021I=18 OR SSQ021J=19 OR SSQ021K=20 OR SSQ021M=22 then WHOSPRT=7;

	* HSD010 recode;
	if HSD010 in(1,2) then GENHLTH=1;
	else if HSD010 in(3,4) then GENHLTH=2;
	else if HSD010=5 then GENHLTH=3;
	
	* HSQ470 recode;
	if 0<=HSQ470<=10 then BADHLTH_P=1;
	else if 11<=HSQ470<=20 then BADHLTH_P=2;
	else if 21<=HSQ470<30 then BADHLTH_P=3;
	else if HSQ470=30 then BADHLTH_P=4;

	* HSQ480 recode;
	if 0<=HSQ480<=10 then BADHLTH_M=1;
	else if 11<=HSQ480<=20 then BADHLTH_M=2;
	else if 21<=HSQ480<30 then BADHLTH_M=3;
	else if HSQ480=30 then BADHLTH_M=4;

	* HSQ490 recode;
	if 0<=HSQ490<=10 then INACTIVE=1;
	else if 11<=HSQ490<=20 then INACTIVE=2;
	else if 21<=HSQ490<30 then INACTIVE=3;
	else if HSQ490=30 then INACTIVE=4;
	
run; 


/* CHECK */
proc freq data = nhanes.recoded;
tables IMMIGRANT AGEGRP/list missing;
run;

* format values for new variables;
proc format;
value RIAGENDRf 1='Male' 2='Female';
value AGEGRPf 1='Adults(>=18 years)' AGEGRP 0='Not Adults(<18 years)';
value RIDRETH1f 1='Mexican American' 2='Other Hispanic' 3='Non-Hispanic White' 4='Non-Hispanic Black' 5='Other/Multi-race';
value EDUCLVLf 1='< High School' 2='HS Diploma/GED' 3='Some College/AA Degree' 4='College Grad/+';
value MRTLSTATf 1='Single' 2='Live-in w/ Partner' 3='Married' 4='Widowed' 5='Separated/Divorced';
value TIMEUSf 1='<5 years' 2='5-9 years' 3='10-19 years' 4='20+ years';

value IMMIGRANTf 1='Immigrant' 0='Not Immigrant';

*value SUBPOPf 1='Subpopulation' 0='Not Subpopulation' SUBPOP="Subpopulation (Immigrant >=18 years)";

value EMOSPRTf 1='Has Support' 0='No Support';
value WHOSPRTf 1='Spouse' 2='Children' 3='Siblings' 4='Parents' 5='Relatives' 6='Friends' 7='Others';

value GENHLTHf 1='Good' 2='Fair' 3='Bad';
value BADHLTH_Pf 1='Never' 2='Occasionally' 3='Often' 4='Always';
value BADHLTH_Mf 1='Never' 2='Occasionally' 3='Often' 4='Always';
value INACTIVEf 1='Never' 2='Occasionally' 3='Often' 4='Always';
run;



/*************
**************
Basic Stats
**************
*************/
/* 1. select variables to keep in final dataset & label them: 10149 observations and 21 variables */
data nhanes.analysis
(keep=SEQN WTINT2YR SDMVSTRA SDMVPSU
RIAGENDR AGEGRP RIDRETH1 EDUCLVL MRTLSTAT TIMEUS
IMMIGRANT
EMOSPRT WHOSPRT 
GENHLTH BADHLTH_P BADHLTH_M INACTIVE HSQ470 HSQ480 HSQ490);
set nhanes.recoded;
label AGEGRP="Adult Age Group (>=18 years)"
		EDUCLVL="Education Level Completed"
		MRTLSTAT="Marital Status"
		TIMEUS="Duration of residence in US"
		IMMIGRANT="Immigrant Status"
		EMOSPRT="Has someone for emotional support"
		WHOSPRT="Who provides emotional support"
		GENHLTH="Rating of general health"
		BADHLTH_P="How often physical heath was bad"
		BADHLTH_M="How often mental health was bad"
		INACTIVE="How often inactive due to bad health";
run;

/* check contents of final data analysis set */
proc contents data=nhanes.analysis order=varnum; run;
proc print data=nhanes.analysis (obs=5); run;

/* tally of obs & missing obs from final data set for analysis */
proc means data=nhanes.analysis n nmiss; 
title "Count of Responses & Missing Values Across Variables of Interest";
run; 

/* summary of survey design */
proc means data=nhanes.analysis n min mean max sum;
var WTINT2YR;
run;

/*************
**************
Descriptive Stats: chi-square
**************
*************/
/* Goodness-of-fit test of outcome variable */
proc surveyfreq data=nhanes.analysis nosummary;
strata SDMVSTRA; 
cluster SDMVPSU;
weight WTINT2YR;
tables GENHLTH / nowt chisq nototal row cl;
format GENHLTH GENHLTHf. ;
title "Chi-Square Goodness-of-Fit Test: General Health";
run;
/* Test of Independent Assocation */ 
proc surveyfreq data=nhanes.analysis nosummary; 
strata SDMVSTRA; 
cluster SDMVPSU;
weight WTINT2YR;
tables EMOSPRT*GENHLTH/nowt chisq nototal row cl;
format GENHLTH GENHLTHf. EMOSPRT EMOSPRTf. ;
title "Test of Independent Association: Health & Emotional Support";
run;





/*************
**************
Calculate Prevalance/Proportions
**************
*************/
/* METHOD 1: NHANES depression example analysis */
/* shows proportion for variable group only */
* nomcar = treating missing values as not missing completely at random for taylor series estimates;
proc surveymeans data=nhanes.analysis nomcar nobs plots=none;
strata SDMVSTRA; 
cluster SDMVPSU;
weight WTINT2YR;
domain IMMIGRANT IMMIGRANT*AGEGRP;
var RIAGENDR RIDRETH1 EDUCLVL MRTLSTAT DMDHHSIZ TIMEUS;
format RIAGENDR RIAGENDRf. AGEGRP AGEGRPf. RIDRETH1 RIDRETH1f. EDUCLVL EDUCLVLf. MRTLSTAT MRTLSTATf. 
IMMIGRANT IMMIGRANTf. TIMEUS TIMEUSf.;
title "Proportions of Immigrants Across Demographic Characteristics";
run; 
/* USED IN REPORT METHOD 2: NHANES hypertension example analysis */
/* for categorical variables: use class & var statements */
/* for continuous variables: use just var statement */
/* Unadjusted prevalence of bad mental heatlh days for immigrant, sex & time in us breakdown */
proc surveymeans data=nhanes.analysis plots=none noobs sum median mean; 
strata SDMVSTRA; 
cluster SDMVPSU;
weight WTINT2YR;
class GENHLTH;
domain IMMIGRANT;
var GENHLTH;
format IMMIGRANT IMMIGRANTf. GENHLTH GENHLTHf.;
title "Unadjusted Prevalence of Health Status of Immigrant Status Individuals";
run;
/* surveyfreq versin of surveymeans */
proc surveyfreq data=nhanes.analysis;
strata SDMVSTRA; 
cluster SDMVPSU;
weight WTINT2YR;
tables IMMIGRANT*GENHLTH;
format IMMIGRANT IMMIGRANTf. GENHLTH GENHLTHf.;
title "Unadjusted Prevalence of Health Status of Immigrant Status Individuals";
run;

/* METHOD 3: NHANES osteoporosis example analysis */
/* proportion of general health rating of immigrant status */
/* issue: can't specify agegrp in the domain statement */
/* log note: 
The input data set is subset by WHERE, OBS, or FIRSTOBS. This provides a completely separate
      analysis of the subset. It does not provide a statistically valid subpopulation or domain
      analysis, where the total number of units in the subpopulation is not known with certainty. If
      you want a domain analysis, you should include the domain variables in the TABLES request.
There is at least one stratum that contains only a single cluster for the table of RIAGENDR by
      GENHLTH controlling for AGEGRP. Single-cluster strata are not included in the variance
      estimates
*/
proc surveyfreq data=nhanes.analysis ;
strata SDMVSTRA; 
cluster SDMVPSU;
weight WTINT2YR;
table AGEGRP*GENHLTH/row col wchisq nostd chisq ;
where IMMIGRANT=1;
format RIAGENDR RIAGENDRf. AGEGRP AGEGRPf. IMMIGRANT IMMIGRANTf. GENHLTH GENHLTHf.;
title "Proportion of Immigrants x General Health Status";
run ;

/* METHOD 3: Applied Survey Data Analysis 2nd Ed Ch 6 */
proc surveyfreq data=nhanes.analysis ;
strata SDMVSTRA; 
cluster SDMVPSU;
weight WTINT2YR;
tables AGEGRP*IMMIGRANT / deff row cl ;
title "Estimating the Proportion of the U.S. Adult Population with Immigrant Status" ;
run ;

proc surveyfreq data=nhanes.analysis  ;
strata SDMVSTRA; 
cluster SDMVPSU;
weight WTINT2YR;
tables IMMIGRANT*RIDRETH1 / nowt nocellpercent row(deff cl ) ;
format RIDRETH1 RIDRETH1f. IMMIGRANT IMMIGRANTf.;
title "Estimating the Proportion of U.S. Immigrants by Race and Ethnicity" ;
run ;




/*************
**************
Graphs
**************
*************/
/* Method 2: bar graph*/
ods graphics on;
proc surveyfreq data = nhanes.analysis;
strata SDMVSTRA; 
cluster SDMVPSU;
weight WTINT2YR;
tables IMMIGRANT*EMOSPRT*TIMEUS / plots (only) = freqplot (scale=percent twoway=stacked)nowt;
format IMMIGRANT IMMIGRANTf. EMOSPRT EMOSPRTf. TIMEUS TIMEUSf. ;
title 'Distribution of Social Support by Time of Residence in the US (Immigrant Status)';
run;
ods graphics off;
/* Mosaic graph */
ods graphics on;
proc surveyfreq data = nhanes.analysis;
weight WTINT2YR;
cluster SDMVPSU;
strata SDMVSTRA;
tables IMMIGRANT*RIAGENDR*RIDRETH1 / plots (only) = mosaicplot ;
format RIAGENDR RIAGENDRf. RIDRETH1 RIDRETH1f. IMMIGRANT IMMIGRANTf.;
run;
ods graphics off;



/*************
**************
Logistic Regression
**************
*************/
proc surveylogistic data = nhanes.analysis; 
weight WTINT2YR;
cluster SDMVPSU;
strata SDMVSTRA;
class EMOSPRT (ref='No Support') 
IMMIGRANT (ref='Not Immigrant') 
RIAGENDR RIDRETH1 AGEGRP TIMEUS/param=ref; 
model GENHLTH = EMOSPRT IMMIGRANT AGEGRP RIAGENDR RIDRETH1 TIMEUS/vadjust=none;
format EMOSPRT EMOSPRTf. GENHLTH GENHLTHf. IMMIGRANT IMMIGRANTf. 
RIAGENDR RIAGENDRf. AGEGRP AGEGRPf. RIDRETH1 RIDRETH1f. TIMEUS TIMEUSf. ;
title 'Logistic Regression';
run;



/* CODE ATTEMPTS BELOW 
****************************************************************************************************************************
****************************************************************************************************************************
****************************************************************************************************************************
****************************************************************************************************************************
****************************************************************************************************************************
****************************************************************************************************************************
****************************************************************************************************************************
****************************************************************************************************************************
****************************************************************************************************************************
****************************************************************************************************************************
****************************************************************************************************************************
****************************************************************************************************************************
****************************************************************************************************************************
****************************************************************************************************************************
****************************************************************************************************************************
****************************************************************************************************************************
*/

/*************
**************
Check Outcome Variable(s) via one-way table: goodness-of-fit test ??????????
**************
*************/
proc surveyfreq data=nhanes.analysis nosummary ; 
strata SDMVSTRA;
cluster SDMVPSU;
weight WTINT2YR;
tables GENHLTH / cl nowt chisq ;
format GENHLTH GENHLTHf. ;
run; 
/*************
**************
Test of Independence: outcome & exposure variables ????????????????
**************
*************/
proc surveyfreq data=nhanes.analysis nosummary ; 
strata SDMVSTRA;
cluster SDMVPSU;
weight WTINT2YR;
tables IMMIGRANT*GENHLTH / row nowt nototal ;
format GENHLTH GENHLTHf. IMMIGRANT IMMIGRANTf.;
run; 


/* PROPORTION COUNT: breaks down values and shows distribution of values */
proc surveyfreq data = nhanes.analysis;
strata SDMVSTRA; 
cluster SDMVPSU;
weight WTINT2YR;
table RIAGENDR RIDRETH1 EDUCLVL MRTLSTAT DMDHHSIZ TIMEUS;
* cannot do age sub group??;
where IMMIGRANT=1;
format RIAGENDR RIAGENDRf. AGEGRP AGEGRPf. RIDRETH1 RIDRETH1f. EDUCLVL EDUCLVLf. MRTLSTAT MRTLSTATf. 
IMMIGRANT IMMIGRANTf. TIMEUS TIMEUSf.;
title "Table 1: Proportion of People of Immigrant Status by Demographic Characteristics (NHANES, 2007-08)";
run;

* surveymeans = proportions (cateogirical var) with domain analysis; 


* 2. domain analysis: immigrant status;
proc surveyfreq data=anlys_ss nosummary order=data; 
strata SDMVSTRA; 
cluster SDMVPSU;
weight WTINT2YR;
tables immigrant*socsprt/plots=wtfreqplot(type=dot scale=percent groupby=row);
title 'Social Support by Immigrant Status';
format adult adultf. immigrant immigrantf. socsprt socsprtf.;
run; quit;

* 3. domain analysis: time in the US;
proc surveyfreq data=anlys_ss order=intern nosummary; 
strata SDMVSTRA; 
cluster SDMVPSU;
weight WTINT2YR;
tables timeus*socsprt/plots=wtfreqplot(type=dot scale=percent groupby=row);
title 'Social Support by Time in the US';
format immigrant immigrantf. timeus timeusf. socsprt socsprtf.;
run; quit;

* 4. association between social support & time in the US;
proc surveyfreq data=anlys_ss nosummary order=data;
strata SDMVSTRA; 
cluster SDMVPSU;
weight WTINT2YR;
tables socsprt*timeus/row nowt chisq;
title 'Chi-Square Test of No Association';
format timeus timeusf. socsprt socsprtf.;
run; quit;

* association between immigrant status & race/ethnicity;
proc surveyfreq data=anlys_ss nosummary;
strata SDMVSTRA; 
cluster SDMVPSU;
weight WTINT2YR;
tables immigrant*RIDRETH1/row nowt wchisq;
format immigrant immigrantf. RIDRETH1 RIDRETH1f.;
run; quit;

* association between immigrant status & race/ethnicity;
proc surveyfreq data=anlys_ss nosummary;
strata SDMVSTRA; 
cluster SDMVPSU;
weight WTINT2YR;
tables immigrant*educ/row nowt wchisq;
format immigrant immigrantf. educ educf.;
run; quit;


* logistic regression: response var=independent var;
proc surveylogistic data=anlys_ss;
strata SDMVSTRA; 
cluster SDMVPSU;
weight WTINT2YR;
class RIAGENDR RIDRETH1 agegrp/ param=ref;
model socsprt (event='1') = timeus immigrant RIAGENDR RIDRETH1 ;
format RIAGENDR RIAGENDRf. american americanf. immigrant immigrantf. RIDRETH1 RIDRETH1f. timeus timeusf.;
run; quit;



* descriptive statistics of continuous variables;
proc surveymeans data=anlys_ss min mean max range;
weight WTINT2YR;
cluster SDMVPSU; 
strata SDMVSTRA; 
var RIDAGEYR ; 
run;
* descriptive statistics of binary variables;
proc surveyfreq data=anlys_ss;
weight WTINT2YR;
cluster SDMVPSU; 
strata SDMVSTRA; 
tables usborn/nowt nofreq cl;
run;
proc surveyfreq data=anlys_ss;
weight WTINT2YR;
cluster SDMVPSU; 
strata SDMVSTRA; 
tables usborn*SSQ011/nofreq nocellpercent row risk relrisk;
format SSQ011 SSQ011f.;
run;

proc surveyfreq data=anlys_ss;
weight WTINT2YR;
cluster SDMVPSU; 
strata SDMVSTRA; 
tables havsprt*(usborn RIAGENDR age); 
format RIAGENDR RIAGENDRf.;
run;
* descriptive statistics with plots;
proc surveymeans data=anlys_ss min mean max range;
domain usborn;
weight WTINT2YR;
cluster SDMVPSU; 
strata SDMVSTRA; 
var RIDAGEYR; 
run;
proc surveymeans data=anlys_ss plots=boxplot;
weight WTINT2YR;
cluster SDMVPSU; 
strata SDMVSTRA; 
var RIDAGEYR; 
run;


* surveyfreq = pecentages;
proc surveyfreq data=anlys_ss order=internal;
weight WTINT2YR;
cluster SDMVPSU; 
strata SDMVSTRA; 
tables usborn;
title 'descriptive stats for immigrant status';
run; quit;


