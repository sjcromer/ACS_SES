#NHANES-ACS_Area-Indiv-SES study
#ctrl+shift+F10 then restart R to restart work session
#use to clear global environment - do NOT run otherwise or you will have to reload everything :'(....rm(list=ls())
#update.packages()
library(tidycensus)
require(censusapi)
library(nhanesA)
library(progress)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(sociome)

library(ggpubr)
library(survey)
library(progress)
library(survival)
library(survminer)
library(Rcpp)
library(openxlsx)
library(phonTools)
library(lavaan)
library(tableone)
library(survey)
library(RColorBrewer)
library(getopt)

setwd('filepath')

#####import and combine data#####
census_api_key("yourapikeyhere", install=T)

#Decennial census data
state_abb <- c(state.abb,'DC')
state_abb
all_states <- c(Alabama='AL',Alaska='AK',Arizona='Az')
census_vars_all <- load_variables(2010, 'sf2', cache=T)
View(census_vars_all)
census_race_vars <-c(Hispanic = "P2_002N", White = "P2_005N", Black = "P2_006N", Asian = "P2_008N")
ct_race <- get_decennial(geography='tract', variables='P013001', state=state.abb, survey=sf2, year=2010)
dim(ct_race)
head(ct_race)
table(ct_race$variable)




#####American Community Survey 2015-2019 for NHANES 2015-3/2020#####
#ACS MOEs are at 90% confidence level; SE=MOE/1.645; CV=SE/estimate*100; Z value to compare geographies/times=[(estimate1 - estimate2)/sqrt(SE1^2 + SE2^2)]
#for collapsing categories (pg 68 of ACS documentation: https://www.census.gov/content/dam/Census/library/publications/2020/acs/acs_general_handbook_2020.pdf) - just add estimates, MOE = sqrt(SE1^2 + SE2^2 + ... + SEn^2)
#for collapsing categories (not just adding, also proportions/percents): https://www.census.gov/content/dam/Census/library/publications/2019/acs/acs_general_handbook_2019_ch08.pdf
#API user guide: https://www.census.gov/content/dam/Census/data/developers/api-user-guide/api-guide.pdf
#variable list: https://api.census.gov/data/2019/acs/acs5/variables.html
acs2019_vars_all <- load_variables(2019, 'acs5', cache=T)
acs2019_vars_simple <- acs2019_vars_all[acs2019_vars_all$label=='Estimate!!Total',]
View(acs2019_vars_all)
acs19_vars <-c(tot_pop='B01001_001',tot_pop_racetab='B03002_001',Hispanic = "B03002_012", White = "B03002_003", Black = "B03002_004", Asian = "B03002_006", 
                     MHI='B19013_001', tot_pop_FPL='B17026_001', FPL00.50='B17026_002', FPL50.74='B17026_003',FPL75.99='B17026_004', FPL100.124='B17026_005', 
                     FPL125.149='B17026_006', FPL150.184='B17026_007', FPL185.199='B17026_008', gini='B19083_001',
                     educ.pop_ge_25='B15003_001',educ.none='B15003_002', educ.prek='B15003_003', educ.kind='B15003_004', educ.1st='B15003_005', educ.2nd='B15003_006', 
                     educ.3rd='B15003_007', educ.4th='B15003_008', educ.5th='B15003_009', educ.6th='B15003_010', educ.7th='B15003_011', educ.8th='B15003_012',
                     educ.9th='B15003_013', educ.10th='B15003_014', educ.11th='B15003_015', educ.12th_nodegree='B15003_016',educ.HS='B15003_017', 
                     educ.ged='B15003_018', educ.some_coll1yr='B15003_019', educ.some_coll='B15003_020', educ.assocdeg='B15003_021',
                     educ.bach='B15003_022', educ.mast='B15003_023', educ.prof='B15003_024', educ.doct='B15003_025',
                     emp.pop_ge_16='B23025_001',  emp.civlabforce='B23025_003',  emp.unemp_civlabforce='B23025_005')

acs2019_raw <- get_acs(geography='tract', variables=acs19_vars, state=state_abb, year=2019, survey='acs5')
test <- get_acs(geography='tract',variables='B17026_001', state=state.abb, year=2019, survey='acs5')
test

table(acs2019_raw$variable)
acs2019_raw <- as.data.frame(acs2019_raw)
dim(acs2019_raw)
acs2019_raw2 <- acs2019_raw[,c('GEOID','variable','estimate','moe')]

acs2019_wide <- data.frame(reshape(acs2019_raw2, idvar=c('GEOID'), timevar='variable', direction='wide'))
acs2019_wide[1:5,]
dim(acs2019_wide)
acs2019_wide$estimate.fpl_lt100 <- acs2019_wide$estimate.FPL00.50 + acs2019_wide$estimate.FPL50.74 + acs2019_wide$estimate.FPL75.99
acs2019_wide$moe.fpl_lt100 <- sqrt((acs2019_wide$moe.FPL00.50)^2 + (acs2019_wide$moe.FPL50.74)^2 + (acs2019_wide$moe.FPL75.99)^2)

acs2019_wide$estimate.fpl_lt150 <- acs2019_wide$estimate.FPL00.50 + acs2019_wide$estimate.FPL50.74 + acs2019_wide$estimate.FPL75.99 + acs2019_wide$estimate.FPL100.124 + acs2019_wide$estimate.FPL125.149
acs2019_wide$moe.fpl_lt150 <- sqrt((acs2019_wide$moe.FPL00.50)^2 + (acs2019_wide$moe.FPL50.74)^2 + (acs2019_wide$moe.FPL75.99)^2 + (acs2019_wide$moe.FPL100.124)^2 + (acs2019_wide$moe.FPL125.149)^2)

acs2019_wide$estimate.fpl_lt200 <- acs2019_wide$estimate.FPL00.50 + acs2019_wide$estimate.FPL50.74 + acs2019_wide$estimate.FPL75.99 + acs2019_wide$estimate.FPL100.124 + acs2019_wide$estimate.FPL125.149 + acs2019_wide$estimate.FPL150.184 + acs2019_wide$estimate.FPL185.199
acs2019_wide$moe.fpl_lt200 <- sqrt((acs2019_wide$moe.FPL00.50)^2 + (acs2019_wide$moe.FPL50.74)^2 + (acs2019_wide$moe.FPL75.99)^2 + (acs2019_wide$moe.FPL100.124)^2 + (acs2019_wide$moe.FPL125.149)^2 + (acs2019_wide$moe.FPL150.184)^2 + (acs2019_wide$moe.FPL185.199)^2)

acs2019_wide$estimate.lths <- acs2019_wide$estimate.educ.none + acs2019_wide$estimate.educ.prek + acs2019_wide$estimate.educ.kind + acs2019_wide$estimate.educ.1st + acs2019_wide$estimate.educ.2nd + acs2019_wide$estimate.educ.3rd + acs2019_wide$estimate.educ.4th + acs2019_wide$estimate.educ.5th + acs2019_wide$estimate.educ.6th + acs2019_wide$estimate.educ.7th + acs2019_wide$estimate.educ.8th + acs2019_wide$estimate.educ.9th + acs2019_wide$estimate.educ.10th + acs2019_wide$estimate.educ.11th + acs2019_wide$estimate.educ.12th_nodegree
acs2019_wide$moe.lths <- sqrt((acs2019_wide$moe.educ.none)^2 + (acs2019_wide$moe.educ.prek)^2 + (acs2019_wide$moe.educ.kind)^2 + (acs2019_wide$moe.educ.1st)^2 + (acs2019_wide$moe.educ.2nd)^2 + (acs2019_wide$moe.educ.3rd)^2 + (acs2019_wide$moe.educ.4th)^2 + (acs2019_wide$moe.educ.5th)^2 + (acs2019_wide$moe.educ.6th)^2 + (acs2019_wide$moe.educ.7th)^2 + (acs2019_wide$moe.educ.8th)^2 + (acs2019_wide$moe.educ.9th)^2 + (acs2019_wide$moe.educ.10th)^2 + (acs2019_wide$moe.educ.11th)^2 + (acs2019_wide$moe.educ.12th_nodegree)^2)

acs2019_wide$estimate.ltcoll <- acs2019_wide$estimate.educ.none + acs2019_wide$estimate.educ.prek + acs2019_wide$estimate.educ.kind + acs2019_wide$estimate.educ.1st + acs2019_wide$estimate.educ.2nd + acs2019_wide$estimate.educ.3rd + acs2019_wide$estimate.educ.4th + acs2019_wide$estimate.educ.5th + acs2019_wide$estimate.educ.6th + acs2019_wide$estimate.educ.7th + acs2019_wide$estimate.educ.8th + acs2019_wide$estimate.educ.9th + acs2019_wide$estimate.educ.10th + acs2019_wide$estimate.educ.11th + acs2019_wide$estimate.educ.12th_nodegree + acs2019_wide$estimate.educ.HS + acs2019_wide$estimate.educ.ged + acs2019_wide$estimate.educ.some_coll1yr + acs2019_wide$estimate.educ.some_coll + acs2019_wide$estimate.educ.assocdeg
acs2019_wide$moe.ltcoll <- sqrt((acs2019_wide$moe.educ.none)^2 + (acs2019_wide$moe.educ.prek)^2 + (acs2019_wide$moe.educ.kind)^2 + (acs2019_wide$moe.educ.1st)^2 + (acs2019_wide$moe.educ.2nd)^2 + (acs2019_wide$moe.educ.3rd)^2 + (acs2019_wide$moe.educ.4th)^2 + (acs2019_wide$moe.educ.5th)^2 + (acs2019_wide$moe.educ.6th)^2 + (acs2019_wide$moe.educ.7th)^2 + (acs2019_wide$moe.educ.8th)^2 + (acs2019_wide$moe.educ.9th)^2 + (acs2019_wide$moe.educ.10th)^2 + (acs2019_wide$moe.educ.11th)^2 + (acs2019_wide$moe.educ.12th_nodegree)^2 + (acs2019_wide$estimate.educ.HS)^2 + (acs2019_wide$estimate.educ.ged)^2 + (acs2019_wide$estimate.educ.some_coll1yr)^2 + (acs2019_wide$estimate.educ.some_coll)^2 + (acs2019_wide$estimate.educ.assocdeg)^2)

#percents 
acs2019_wide$estimate.pct_white <- acs2019_wide$estimate.White / acs2019_wide$estimate.tot_pop #some NAs generated in tot_pop=0 locations
acs2019_wide$moe.pct_white <- ifelse((acs2019_wide$moe.White^2-((acs2019_wide$estimate.White / acs2019_wide$estimate.tot_pop)^2)*acs2019_wide$moe.tot_pop^2) <0, (1 / acs2019_wide$estimate.tot_pop) * (sqrt((acs2019_wide$moe.White^2) + ((acs2019_wide$estimate.White / acs2019_wide$estimate.tot_pop)^2)*acs2019_wide$moe.tot_pop^2)), (1 / acs2019_wide$estimate.tot_pop) * (sqrt((acs2019_wide$moe.White^2)-((acs2019_wide$estimate.White / acs2019_wide$estimate.tot_pop)^2)*acs2019_wide$moe.tot_pop^2)))
a <- acs2019_wide$moe.White^2-((acs2019_wide$estimate.White / acs2019_wide$estimate.tot_pop)^2)*acs2019_wide$moe.tot_pop^2
summary(a)
summary(acs2019_wide$estimate.pct_white)
summary(acs2019_wide$moe.pct_white)
View(acs2019_wide[,c('GEOID','estimate.tot_pop','moe.tot_pop','estimate.White','moe.White','estimate.pct_white','moe.pct_white')]) #n=10 census tracts with moe.pct_white >1
#5020 NAs - INSTRUCTIONS NOTE: "users should note that if the value under the square root is negative, then the formula for calculating the MOE of a ratio should be used instead, which substitutes a "plus" for the "minus" under the square root.": acs2019_wide$moe.pct_white <- 1/acs2019_wide$estimate.tot_pop * sqrt((acs2019_wide$moe.White)^2 - ((acs2019_wide$estimate.pct_white)^2 * (acs2019_wide$moe.tot_pop)^2))
acs2019_wide$estimate.pct_Black <- acs2019_wide$estimate.Black / acs2019_wide$estimate.tot_pop
acs2019_wide$moe.pct_Black <- ifelse(acs2019_wide$moe.Black^2-((acs2019_wide$estimate.Black / acs2019_wide$estimate.tot_pop)^2)*acs2019_wide$moe.tot_pop^2 <0, (1 / acs2019_wide$estimate.tot_pop) * (sqrt((acs2019_wide$moe.Black^2)+((acs2019_wide$estimate.Black / acs2019_wide$estimate.tot_pop)^2)*acs2019_wide$moe.tot_pop^2)), (1 / acs2019_wide$estimate.tot_pop) * (sqrt((acs2019_wide$moe.Black^2)-((acs2019_wide$estimate.Black / acs2019_wide$estimate.tot_pop)^2)*acs2019_wide$moe.tot_pop^2)))
acs2019_wide$estimate.pct_Asian <- acs2019_wide$estimate.Asian / acs2019_wide$estimate.tot_pop
acs2019_wide$moe.pct_Asian <- ifelse(acs2019_wide$moe.Asian^2-((acs2019_wide$estimate.Asian / acs2019_wide$estimate.tot_pop)^2)*acs2019_wide$moe.tot_pop^2 <0, (1 / acs2019_wide$estimate.tot_pop) * (sqrt((acs2019_wide$moe.Asian^2)+((acs2019_wide$estimate.Asian / acs2019_wide$estimate.tot_pop)^2)*acs2019_wide$moe.tot_pop^2)), (1 / acs2019_wide$estimate.tot_pop) * (sqrt((acs2019_wide$moe.Asian^2)-((acs2019_wide$estimate.Asian / acs2019_wide$estimate.tot_pop)^2)*acs2019_wide$moe.tot_pop^2)))
acs2019_wide$estimate.pct_Hispanic <- acs2019_wide$estimate.Hispanic / acs2019_wide$estimate.tot_pop
acs2019_wide$moe.pct_Hispanic <- ifelse(acs2019_wide$moe.Hispanic^2-((acs2019_wide$estimate.Hispanic / acs2019_wide$estimate.tot_pop)^2)*acs2019_wide$moe.tot_pop^2 <0, (1 / acs2019_wide$estimate.tot_pop) * (sqrt((acs2019_wide$moe.Hispanic^2)+((acs2019_wide$estimate.Hispanic / acs2019_wide$estimate.tot_pop)^2)*acs2019_wide$moe.tot_pop^2)), (1 / acs2019_wide$estimate.tot_pop) * (sqrt((acs2019_wide$moe.Hispanic^2)-((acs2019_wide$estimate.Hispanic / acs2019_wide$estimate.tot_pop)^2)*acs2019_wide$moe.tot_pop^2)))
acs2019_wide$estimate.pct_fpl100 <- acs2019_wide$estimate.fpl_lt100 / acs2019_wide$estimate.tot_pop_FPL
acs2019_wide$moe.pct_fpl100 <- ifelse(acs2019_wide$moe.fpl_lt100^2-((acs2019_wide$estimate.fpl_lt100 / acs2019_wide$estimate.tot_pop_FPL)^2)*acs2019_wide$moe.tot_pop^2 <0, (1 / acs2019_wide$estimate.tot_pop_FPL) * (sqrt((acs2019_wide$moe.fpl_lt100^2)+((acs2019_wide$estimate.fpl_lt100 / acs2019_wide$estimate.tot_pop_FPL)^2)*acs2019_wide$moe.tot_pop^2)), (1 / acs2019_wide$estimate.tot_pop_FPL) * (sqrt((acs2019_wide$moe.fpl_lt100^2)-((acs2019_wide$estimate.fpl_lt100 / acs2019_wide$estimate.tot_pop_FPL)^2)*acs2019_wide$moe.tot_pop^2)))
acs2019_wide$estimate.pct_fpl150 <- acs2019_wide$estimate.fpl_lt150 / acs2019_wide$estimate.tot_pop_FPL
acs2019_wide$moe.pct_fpl150 <- ifelse(acs2019_wide$moe.fpl_lt150^2-((acs2019_wide$estimate.fpl_lt150 / acs2019_wide$estimate.tot_pop_FPL)^2)*acs2019_wide$moe.tot_pop^2 <0, (1 / acs2019_wide$estimate.tot_pop_FPL) * (sqrt((acs2019_wide$moe.fpl_lt150^2)+((acs2019_wide$estimate.fpl_lt150 / acs2019_wide$estimate.tot_pop_FPL)^2)*acs2019_wide$moe.tot_pop^2)), (1 / acs2019_wide$estimate.tot_pop_FPL) * (sqrt((acs2019_wide$moe.fpl_lt150^2)-((acs2019_wide$estimate.fpl_lt150 / acs2019_wide$estimate.tot_pop_FPL)^2)*acs2019_wide$moe.tot_pop^2)))
acs2019_wide$estimate.pct_fpl200 <- acs2019_wide$estimate.fpl_lt200 / acs2019_wide$estimate.tot_pop_FPL
acs2019_wide$moe.pct_fpl200 <- ifelse(acs2019_wide$moe.fpl_lt200^2-((acs2019_wide$estimate.fpl_lt200 / acs2019_wide$estimate.tot_pop_FPL)^2)*acs2019_wide$moe.tot_pop^2 <0, (1 / acs2019_wide$estimate.tot_pop_FPL) * (sqrt((acs2019_wide$moe.fpl_lt200^2)+((acs2019_wide$estimate.fpl_lt200 / acs2019_wide$estimate.tot_pop_FPL)^2)*acs2019_wide$moe.tot_pop^2)), (1 / acs2019_wide$estimate.tot_pop_FPL) * (sqrt((acs2019_wide$moe.fpl_lt200^2)-((acs2019_wide$estimate.fpl_lt200 / acs2019_wide$estimate.tot_pop_FPL)^2)*acs2019_wide$moe.tot_pop^2)))
acs2019_wide$estimate.pct_unemp <- acs2019_wide$estimate.emp.unemp_civlabforce / acs2019_wide$estimate.emp.civlabforce
acs2019_wide$moe.pct_unemp <- ifelse(acs2019_wide$moe.emp.unemp_civlabforce^2-((acs2019_wide$estimate.emp.unemp_civlabforce / acs2019_wide$estimate.emp.civlabforce)^2)*acs2019_wide$moe.emp.civlabforce^2 <0, (1 / acs2019_wide$estimate.emp.civlabforce) * (sqrt((acs2019_wide$moe.emp.unemp_civlabforce^2)+((acs2019_wide$estimate.emp.unemp_civlabforce / acs2019_wide$estimate.emp.civlabforce)^2)*acs2019_wide$moe.emp.civlabforce^2)), (1 / acs2019_wide$estimate.emp.civlabforce) * (sqrt((acs2019_wide$moe.emp.unemp_civlabforce^2)-((acs2019_wide$estimate.emp.unemp_civlabforce / acs2019_wide$estimate.emp.civlabforce)^2)*acs2019_wide$moe.emp.civlabforce^2)))
acs2019_wide$estimate.pct_lths <- acs2019_wide$estimate.lths / acs2019_wide$estimate.educ.pop_ge_25
acs2019_wide$moe.pct_lths <- ifelse(acs2019_wide$moe.lths^2-((acs2019_wide$estimate.lths / acs2019_wide$estimate.educ.pop_ge_25)^2)*acs2019_wide$moe.educ.pop_ge_25^2 <0, (1 / acs2019_wide$estimate.educ.pop_ge_25) * (sqrt((acs2019_wide$moe.lths^2)+((acs2019_wide$estimate.lths / acs2019_wide$estimate.educ.pop_ge_25)^2)*acs2019_wide$moe.educ.pop_ge_25^2)), (1 / acs2019_wide$estimate.educ.pop_ge_25) * (sqrt((acs2019_wide$moe.lths^2)-((acs2019_wide$estimate.lths / acs2019_wide$estimate.educ.pop_ge_25)^2)*acs2019_wide$moe.educ.pop_ge_25^2)))
acs2019_wide$estimate.pct_ltcoll <- acs2019_wide$estimate.ltcoll / acs2019_wide$estimate.educ.pop_ge_25
acs2019_wide$moe.pct_ltcoll <- ifelse(acs2019_wide$moe.ltcoll^2-((acs2019_wide$estimate.ltcoll / acs2019_wide$estimate.educ.pop_ge_25)^2)*acs2019_wide$moe.educ.pop_ge_25^2 <0, (1 / acs2019_wide$estimate.educ.pop_ge_25) * (sqrt((acs2019_wide$moe.ltcoll^2)+((acs2019_wide$estimate.ltcoll / acs2019_wide$estimate.educ.pop_ge_25)^2)*acs2019_wide$moe.educ.pop_ge_25^2)), (1 / acs2019_wide$estimate.educ.pop_ge_25) * (sqrt((acs2019_wide$moe.ltcoll^2)-((acs2019_wide$estimate.ltcoll / acs2019_wide$estimate.educ.pop_ge_25)^2)*acs2019_wide$moe.educ.pop_ge_25^2)))
colnames(acs2019_wide)
head(acs2019_wide)
acs2019_short <- acs2019_wide[,c('GEOID','estimate.tot_pop','moe.tot_pop','estimate.MHI','moe.MHI','estimate.gini','moe.gini',
                                 'estimate.pct_white','moe.pct_white','estimate.pct_Black','moe.pct_Black',
                                 'estimate.pct_Asian','moe.pct_Asian','estimate.pct_Hispanic','moe.pct_Hispanic',
                                 'estimate.pct_fpl100','moe.pct_fpl100','estimate.pct_fpl150','moe.pct_fpl150','estimate.pct_fpl200','moe.pct_fpl200',
                                 'estimate.pct_unemp','moe.pct_unemp','estimate.pct_lths','moe.pct_lths','estimate.pct_ltcoll','moe.pct_ltcoll')]
colnames(acs2019_short)
write.csv(acs2019_short, 'filepath/ACSonly_2019.csv',row.names=TRUE)

#add adi, svi, sdi
adi2019_raw <- read.csv(file='filepath/ADI_2015_US_v4_0_1.csv')
summary(adi2019_raw$ADI_NATRANK, na.rm=T)
adi2019_raw$adi <- adi2019_raw$ADI_NATRANK
adi2019_raw$GEOID <- substr(as.character(adi2019_raw$FIPS),1,11)
adi2019 <- adi2019_raw[,c('GEOID','adi')]
head(adi2019)
rm(adi2019_raw)

svi2019_raw <- read.csv(file='filepath/SVI_2018_US.csv')
svi2019_raw$svi <- svi2019_raw$RPL_THEMES
svi2019_raw$GEOID <- svi2019_raw$FIPS
svi2019 <- svi2019_raw[,c('GEOID','svi')]
class(svi2019$GEOID) <- 'character'
head(svi2019)
rm(svi2019_raw)

sdi2019_raw <- read.csv(file='filepath/SDI_2015_2019_CT.csv')
sdi2019_raw$sdi <- sdi2019_raw$SDI_score
sdi2019_raw$GEOID <- sdi2019_raw$CENSUSTRACT_FIPS
class(sdi2019$GEOID) <- 'character'
sdi2019 <- sdi2019_raw[,c('GEOID','sdi')]
head(sdi2019_raw)
rm(sdi2019_raw)

write.csv(adi2019, 'filepath/ADI_2015.csv',row.names=TRUE)
write.csv(svi2019, 'filepath/SVI_2019.csv',row.names=TRUE)
write.csv(sdi2019, 'filepath/SDI_2019.csv',row.names=TRUE)


#merge
acs19_adi <- merge(acs2019_short, adi2019, by='GEOID', all.x=TRUE, all.y=FALSE, sort=TRUE)
dim(acs19_adi)
acs19_adi_sdi <- merge(acs19_adi, sdi2019, by='GEOID', all.x=TRUE, all.y=FALSE, sort=TRUE)
dim(acs19_adi_sdi)
acs19_final <- merge(acs19_adi_sdi, svi2019, by='GEOID', all.x=TRUE, all.y=FALSE, sort=TRUE)
dim(acs19_final)
colnames(acs19_final)
write.csv(acs19_final, 'filepath/ACS_2019.csv',row.names=TRUE)



####JUNE 2026 UPDATE####
#replace SVI given SVI removed from CDC site 2025 or so - need to import SVI 2018 from prior cleaned files, then merge
#STEPS = upload ACS 2019 merged file (created 2023), remove SVI column, upload previous ACS 2018 merged file and combine, isolate GEOID and svi columns, merge by GEOID, save
acs2019_short <- read.csv(file='filepath/ACS_2019.csv')
acs2019_short$GEOID_11 <- str_pad(as.character(acs2019_short$GEOID), 11, pad='0')
acs2019_short <- subset(acs2019_short, select = -c(sdi,svi,adi))

#SVI from:
acs2018_old1 <- read.csv(file='filepath/ACS20181.csv')
acs2018_old2 <- read.csv(file='filepath/ACS20182.csv')
acs2018_old <- rbind(acs2018_old1, acs2018_old2)
acs2018_old$GEOID_11 <- str_pad(as.character(acs2018_old$GEOID), 11, pad='0')
acs2018_old$svi <- ifelse(acs2018_old$svi <0, NA, acs2018_old$svi)
summary(acs2018_old$svi[acs2018_old$svi >-0.01])
sum(acs2018_old$svi < 0, na.rm=T)
acs2018_old_svionly <- subset(acs2018_old, select= c('GEOID_11', 'svi'))

#ADI from: 
#note, must drop final FIPS character to go from block group to tract; AVERAGE within tracts
adi2020 <- read.csv(file='filepath/nanda_ADI_std_blckgrp_2020_01P.csv')
colnames(adi2020)
adi2020$blkgrp_fips20[1:10]
adi2020$GEOID <- as.numeric(adi2020$blkgrp_fips20)
adi2020$GEOID_12 <- str_pad(as.character(adi2020$GEOID), 12, pad='0')
adi2020$GEOID_11 <- str_sub(adi2020$GEOID_12, end = -2)
adi2020$adi <- as.numeric(adi2020$adistd_ntl)
summary(as.numeric(adi2020$adi))
adi2020_ct <- adi2020 %>%
  group_by(GEOID_11) %>%
  summarize(adi = mean(adi, na.rm=T))
dim(adi2020_ct)
summary(adi2020_ct$adi)
#adi2020_ct <- subset(adi2020, select = c('GEOID_11','adi'))

#SDI from:
sdi2019_raw <- read.csv(file='filepath/SDI_2019.csv')
sdi2019_raw$GEOID_11 <- str_pad(as.character(sdi2019_raw$GEOID), 11, pad='0')
sdi2019 <- subset(sdi2019_raw, select = c('GEOID_11','sdi'))

#Merge
colnames(acs2019_short)
colnames(acs2018_old_svionly)
colnames(adi2020)
colnames(sdi2019)
acs2019_svi2018 <- merge(acs2019_short, acs2018_old_svionly, by='GEOID_11', all.x=TRUE, all.y=FALSE, sort=TRUE)
summary(acs2019_svi2018$svi[acs2019_svi2018$svi >-0.01])
sum(acs2019_svi2018$svi < 0, na.rm=T)
acs2019_svisdi <- merge(acs2019_svi2018, sdi2019, by='GEOID_11', all.x=TRUE, all.y=FALSE, sort=TRUE)
acs2019_svisdi[1:10,]
summary(acs2019_svisdi$sdi)
acs2019 <- merge(acs2019_svisdi, adi2015_ct, by='GEOID_11', all.x=TRUE, all.y=FALSE, sort=TRUE)
acs2019[1:10,]
summary(acs2019$adi)
colnames(acs2019)
acs2019[1:10,c("GEOID_11","adi","svi","sdi")]
class(acs2019$GEOID_11)

acs2019$FIPS_state <- substr(acs2019$GEOID_11, start=1, stop=2)
acs2019$FIPS_county <- substr(acs2019$GEOID_11, start=3, stop=5)
acs2019$GEOID_6 <- str_sub(acs2019$GEOID_11, -6)
acs2019 <- subset(acs2019, select = -c(X,NAME, ADI, Financial_Strength, Economic_Hardship_and_Inequality, Educational_Attainment))
dim(acs2019) #expect 34 cols
sum(is.na(acs2019$estimate.tot_pop), na.rm=T)
write.csv(acs2019, 'filepath/ACS_2019_FINAL.csv',row.names=TRUE)
acs2019_1 <- acs2019[1:40000,]
acs2019_2 <- acs2019[40001:73056,]
write.csv(acs2019_1, 'filepath/ACS_2019_01.csv',row.names=TRUE)
write.csv(acs2019_2, 'filepath/ACS_2019_02.csv',row.names=TRUE)







#####American Community Survey 2010-2014 for NHANES 2010-2014#####
acs2014_vars_all <- load_variables(2014, 'acs5', cache=T)
View(acs2014_vars_all)
acs14_vars <-c(tot_pop='B01003_001',tot_pop_racetab='B03002_001',Hispanic = "B03002_012", White = "B03002_003", Black = "B03002_004", Asian = "B03002_006", 
               MHI='B19013_001', tot_pop_FPL='C17002_001', FPL00.50='C17002_002', FPL50.99='C17002_003', FPL100.124='C17002_004', 
               FPL125.149='C17002_005', FPL150.184='C17002_006', FPL185.199='C17002_007', gini='B19083_001',
               educ.pop_ge_25='B15003_001',educ.none='B15003_002', educ.prek='B15003_003', educ.kind='B15003_004', educ.1st='B15003_005', educ.2nd='B15003_006', 
               educ.3rd='B15003_007', educ.4th='B15003_008', educ.5th='B15003_009', educ.6th='B15003_010', educ.7th='B15003_011', educ.8th='B15003_012',
               educ.9th='B15003_013', educ.10th='B15003_014', educ.11th='B15003_015', educ.12th_nodegree='B15003_016',educ.HS='B15003_017', 
               educ.ged='B15003_018', educ.some_coll1yr='B15003_019', educ.some_coll='B15003_020', educ.assocdeg='B15003_021',
               educ.bach='B15003_022', educ.mast='B15003_023', educ.prof='B15003_024', educ.doct='B15003_025',
               emp.pop_ge_16='B23025_001',  emp.civlabforce='B23025_003',  emp.unemp_civlabforce='B23025_005')

acs2014_raw <- get_acs(geography='tract', variables=acs14_vars, state=state_abb, year=2014, survey='acs5')
acs2014_raw <- as.data.frame(acs2014_raw)
acs2014_raw[1:50,]
dim(acs2014_raw)
class(acs2014_raw$GEOID)
acs2014_raw2 <- acs2014_raw[,c('GEOID','variable','estimate','moe')]
acs2014_wide <- data.frame(reshape(acs2014_raw2, idvar=c('GEOID'), timevar='variable', direction='wide'))
acs2014_wide[1:5,]
dim(acs2014_wide)

acs2014_wide$estimate.fpl_lt100 <- acs2014_wide$estimate.FPL00.50 + acs2014_wide$estimate.FPL50.99
acs2014_wide$moe.fpl_lt100 <- sqrt((acs2014_wide$moe.FPL00.50)^2 + (acs2014_wide$moe.FPL50.99)^2)

acs2014_wide$estimate.fpl_lt150 <- acs2014_wide$estimate.FPL00.50 + acs2014_wide$estimate.FPL50.99 + acs2014_wide$estimate.FPL100.124 + acs2014_wide$estimate.FPL125.149
acs2014_wide$moe.fpl_lt150 <- sqrt((acs2014_wide$moe.FPL00.50)^2 + (acs2014_wide$moe.FPL50.99)^2 + (acs2014_wide$moe.FPL100.124)^2 + (acs2014_wide$moe.FPL125.149)^2)

acs2014_wide$estimate.fpl_lt200 <- acs2014_wide$estimate.FPL00.50 + acs2014_wide$estimate.FPL50.99 + acs2014_wide$estimate.FPL100.124 + acs2014_wide$estimate.FPL125.149 + acs2014_wide$estimate.FPL150.184 + acs2014_wide$estimate.FPL185.199
acs2014_wide$moe.fpl_lt200 <- sqrt((acs2014_wide$moe.FPL00.50)^2 + (acs2014_wide$moe.FPL50.99)^2 + (acs2014_wide$moe.FPL100.124)^2 + (acs2014_wide$moe.FPL125.149)^2 + (acs2014_wide$moe.FPL150.184)^2 + (acs2014_wide$moe.FPL185.199)^2)

acs2014_wide$estimate.lths <- acs2014_wide$estimate.educ.none + acs2014_wide$estimate.educ.prek + acs2014_wide$estimate.educ.kind + acs2014_wide$estimate.educ.1st + acs2014_wide$estimate.educ.2nd + acs2014_wide$estimate.educ.3rd + acs2014_wide$estimate.educ.4th + acs2014_wide$estimate.educ.5th + acs2014_wide$estimate.educ.6th + acs2014_wide$estimate.educ.7th + acs2014_wide$estimate.educ.8th + acs2014_wide$estimate.educ.9th + acs2014_wide$estimate.educ.10th + acs2014_wide$estimate.educ.11th + acs2014_wide$estimate.educ.12th_nodegree
acs2014_wide$moe.lths <- sqrt((acs2014_wide$moe.educ.none)^2 + (acs2014_wide$moe.educ.prek)^2 + (acs2014_wide$moe.educ.kind)^2 + (acs2014_wide$moe.educ.1st)^2 + (acs2014_wide$moe.educ.2nd)^2 + (acs2014_wide$moe.educ.3rd)^2 + (acs2014_wide$moe.educ.4th)^2 + (acs2014_wide$moe.educ.5th)^2 + (acs2014_wide$moe.educ.6th)^2 + (acs2014_wide$moe.educ.7th)^2 + (acs2014_wide$moe.educ.8th)^2 + (acs2014_wide$moe.educ.9th)^2 + (acs2014_wide$moe.educ.10th)^2 + (acs2014_wide$moe.educ.11th)^2 + (acs2014_wide$moe.educ.12th_nodegree)^2)

acs2014_wide$estimate.ltcoll <- acs2014_wide$estimate.educ.none + acs2014_wide$estimate.educ.prek + acs2014_wide$estimate.educ.kind + acs2014_wide$estimate.educ.1st + acs2014_wide$estimate.educ.2nd + acs2014_wide$estimate.educ.3rd + acs2014_wide$estimate.educ.4th + acs2014_wide$estimate.educ.5th + acs2014_wide$estimate.educ.6th + acs2014_wide$estimate.educ.7th + acs2014_wide$estimate.educ.8th + acs2014_wide$estimate.educ.9th + acs2014_wide$estimate.educ.10th + acs2014_wide$estimate.educ.11th + acs2014_wide$estimate.educ.12th_nodegree + acs2014_wide$estimate.educ.HS + acs2014_wide$estimate.educ.ged + acs2014_wide$estimate.educ.some_coll1yr + acs2014_wide$estimate.educ.some_coll + acs2014_wide$estimate.educ.assocdeg
acs2014_wide$moe.ltcoll <- sqrt((acs2014_wide$moe.educ.none)^2 + (acs2014_wide$moe.educ.prek)^2 + (acs2014_wide$moe.educ.kind)^2 + (acs2014_wide$moe.educ.1st)^2 + (acs2014_wide$moe.educ.2nd)^2 + (acs2014_wide$moe.educ.3rd)^2 + (acs2014_wide$moe.educ.4th)^2 + (acs2014_wide$moe.educ.5th)^2 + (acs2014_wide$moe.educ.6th)^2 + (acs2014_wide$moe.educ.7th)^2 + (acs2014_wide$moe.educ.8th)^2 + (acs2014_wide$moe.educ.9th)^2 + (acs2014_wide$moe.educ.10th)^2 + (acs2014_wide$moe.educ.11th)^2 + (acs2014_wide$moe.educ.12th_nodegree)^2 + (acs2014_wide$estimate.educ.HS)^2 + (acs2014_wide$estimate.educ.ged)^2 + (acs2014_wide$estimate.educ.some_coll1yr)^2 + (acs2014_wide$estimate.educ.some_coll)^2 + (acs2014_wide$estimate.educ.assocdeg)^2)

#percents 
acs2014_wide$estimate.pct_white <- acs2014_wide$estimate.White / acs2014_wide$estimate.tot_pop
acs2014_wide$moe.pct_white <- ifelse(acs2014_wide$moe.White^2-((acs2014_wide$estimate.White / acs2014_wide$estimate.tot_pop)^2)*acs2014_wide$moe.tot_pop^2 <0, (1 / acs2014_wide$estimate.tot_pop) * (sqrt((acs2014_wide$moe.White^2)+((acs2014_wide$estimate.White / acs2014_wide$estimate.tot_pop)^2)*acs2014_wide$moe.tot_pop^2)), (1 / acs2014_wide$estimate.tot_pop) * (sqrt((acs2014_wide$moe.White^2)-((acs2014_wide$estimate.White / acs2014_wide$estimate.tot_pop)^2)*acs2014_wide$moe.tot_pop^2)))
summary(acs2014_wide$moe.pct_white)
acs2014_wide$estimate.pct_Black <- acs2014_wide$estimate.Black / acs2014_wide$estimate.tot_pop
acs2014_wide$moe.pct_Black <- ifelse(acs2014_wide$moe.Black^2-((acs2014_wide$estimate.Black / acs2014_wide$estimate.tot_pop)^2)*acs2014_wide$moe.tot_pop^2 <0, (1 / acs2014_wide$estimate.tot_pop) * (sqrt((acs2014_wide$moe.Black^2)+((acs2014_wide$estimate.Black / acs2014_wide$estimate.tot_pop)^2)*acs2014_wide$moe.tot_pop^2)), (1 / acs2014_wide$estimate.tot_pop) * (sqrt((acs2014_wide$moe.Black^2)-((acs2014_wide$estimate.Black / acs2014_wide$estimate.tot_pop)^2)*acs2014_wide$moe.tot_pop^2)))
acs2014_wide$estimate.pct_Asian <- acs2014_wide$estimate.Asian / acs2014_wide$estimate.tot_pop
acs2014_wide$moe.pct_Asian <- ifelse(acs2014_wide$moe.Asian^2-((acs2014_wide$estimate.Asian / acs2014_wide$estimate.tot_pop)^2)*acs2014_wide$moe.tot_pop^2 <0, (1 / acs2014_wide$estimate.tot_pop) * (sqrt((acs2014_wide$moe.Asian^2)+((acs2014_wide$estimate.Asian / acs2014_wide$estimate.tot_pop)^2)*acs2014_wide$moe.tot_pop^2)), (1 / acs2014_wide$estimate.tot_pop) * (sqrt((acs2014_wide$moe.Asian^2)-((acs2014_wide$estimate.Asian / acs2014_wide$estimate.tot_pop)^2)*acs2014_wide$moe.tot_pop^2)))
acs2014_wide$estimate.pct_Hispanic <- acs2014_wide$estimate.Hispanic / acs2014_wide$estimate.tot_pop
acs2014_wide$moe.pct_Hispanic <- ifelse(acs2014_wide$moe.Hispanic^2-((acs2014_wide$estimate.Hispanic / acs2014_wide$estimate.tot_pop)^2)*acs2014_wide$moe.tot_pop^2 <0, (1 / acs2014_wide$estimate.tot_pop) * (sqrt((acs2014_wide$moe.Hispanic^2)+((acs2014_wide$estimate.Hispanic / acs2014_wide$estimate.tot_pop)^2)*acs2014_wide$moe.tot_pop^2)), (1 / acs2014_wide$estimate.tot_pop) * (sqrt((acs2014_wide$moe.Hispanic^2)-((acs2014_wide$estimate.Hispanic / acs2014_wide$estimate.tot_pop)^2)*acs2014_wide$moe.tot_pop^2)))
acs2014_wide$estimate.pct_fpl100 <- acs2014_wide$estimate.fpl_lt100 / acs2014_wide$estimate.tot_pop_FPL
acs2014_wide$moe.pct_fpl100 <- ifelse(acs2014_wide$moe.fpl_lt100^2-((acs2014_wide$estimate.fpl_lt100 / acs2014_wide$estimate.tot_pop_FPL)^2)*acs2014_wide$moe.tot_pop^2 <0, (1 / acs2014_wide$estimate.tot_pop_FPL) * (sqrt((acs2014_wide$moe.fpl_lt100^2)+((acs2014_wide$estimate.fpl_lt100 / acs2014_wide$estimate.tot_pop_FPL)^2)*acs2014_wide$moe.tot_pop^2)), (1 / acs2014_wide$estimate.tot_pop_FPL) * (sqrt((acs2014_wide$moe.fpl_lt100^2)-((acs2014_wide$estimate.fpl_lt100 / acs2014_wide$estimate.tot_pop_FPL)^2)*acs2014_wide$moe.tot_pop^2)))
acs2014_wide$estimate.pct_fpl150 <- acs2014_wide$estimate.fpl_lt150 / acs2014_wide$estimate.tot_pop_FPL
acs2014_wide$moe.pct_fpl150 <- ifelse(acs2014_wide$moe.fpl_lt150^2-((acs2014_wide$estimate.fpl_lt150 / acs2014_wide$estimate.tot_pop_FPL)^2)*acs2014_wide$moe.tot_pop^2 <0, (1 / acs2014_wide$estimate.tot_pop_FPL) * (sqrt((acs2014_wide$moe.fpl_lt150^2)+((acs2014_wide$estimate.fpl_lt150 / acs2014_wide$estimate.tot_pop_FPL)^2)*acs2014_wide$moe.tot_pop^2)), (1 / acs2014_wide$estimate.tot_pop_FPL) * (sqrt((acs2014_wide$moe.fpl_lt150^2)-((acs2014_wide$estimate.fpl_lt150 / acs2014_wide$estimate.tot_pop_FPL)^2)*acs2014_wide$moe.tot_pop^2)))
acs2014_wide$estimate.pct_fpl200 <- acs2014_wide$estimate.fpl_lt200 / acs2014_wide$estimate.tot_pop_FPL
acs2014_wide$moe.pct_fpl200 <- ifelse(acs2014_wide$moe.fpl_lt200^2-((acs2014_wide$estimate.fpl_lt200 / acs2014_wide$estimate.tot_pop_FPL)^2)*acs2014_wide$moe.tot_pop^2 <0, (1 / acs2014_wide$estimate.tot_pop_FPL) * (sqrt((acs2014_wide$moe.fpl_lt200^2)+((acs2014_wide$estimate.fpl_lt200 / acs2014_wide$estimate.tot_pop_FPL)^2)*acs2014_wide$moe.tot_pop^2)), (1 / acs2014_wide$estimate.tot_pop_FPL) * (sqrt((acs2014_wide$moe.fpl_lt200^2)-((acs2014_wide$estimate.fpl_lt200 / acs2014_wide$estimate.tot_pop_FPL)^2)*acs2014_wide$moe.tot_pop^2)))
summary(acs2014_wide$moe.pct_fpl200)
acs2014_wide$estimate.pct_unemp <- acs2014_wide$estimate.emp.unemp_civlabforce / acs2014_wide$estimate.emp.civlabforce
acs2014_wide$moe.pct_unemp <- ifelse(acs2014_wide$moe.emp.unemp_civlabforce^2-((acs2014_wide$estimate.emp.unemp_civlabforce / acs2014_wide$estimate.emp.civlabforce)^2)*acs2014_wide$moe.emp.civlabforce^2 <0, (1 / acs2014_wide$estimate.emp.civlabforce) * (sqrt((acs2014_wide$moe.emp.unemp_civlabforce^2)+((acs2014_wide$estimate.emp.unemp_civlabforce / acs2014_wide$estimate.emp.civlabforce)^2)*acs2014_wide$moe.emp.civlabforce^2)), (1 / acs2014_wide$estimate.emp.civlabforce) * (sqrt((acs2014_wide$moe.emp.unemp_civlabforce^2)-((acs2014_wide$estimate.emp.unemp_civlabforce / acs2014_wide$estimate.emp.civlabforce)^2)*acs2014_wide$moe.emp.civlabforce^2)))
acs2014_wide$estimate.pct_lths <- acs2014_wide$estimate.lths / acs2014_wide$estimate.educ.pop_ge_25
acs2014_wide$moe.pct_lths <- ifelse(acs2014_wide$moe.lths^2-((acs2014_wide$estimate.lths / acs2014_wide$estimate.educ.pop_ge_25)^2)*acs2014_wide$moe.educ.pop_ge_25^2 <0, (1 / acs2014_wide$estimate.educ.pop_ge_25) * (sqrt((acs2014_wide$moe.lths^2)+((acs2014_wide$estimate.lths / acs2014_wide$estimate.educ.pop_ge_25)^2)*acs2014_wide$moe.educ.pop_ge_25^2)), (1 / acs2014_wide$estimate.educ.pop_ge_25) * (sqrt((acs2014_wide$moe.lths^2)-((acs2014_wide$estimate.lths / acs2014_wide$estimate.educ.pop_ge_25)^2)*acs2014_wide$moe.educ.pop_ge_25^2)))
acs2014_wide$estimate.pct_ltcoll <- acs2014_wide$estimate.ltcoll / acs2014_wide$estimate.educ.pop_ge_25
acs2014_wide$moe.pct_ltcoll <- ifelse(acs2014_wide$moe.ltcoll^2-((acs2014_wide$estimate.ltcoll / acs2014_wide$estimate.educ.pop_ge_25)^2)*acs2014_wide$moe.educ.pop_ge_25^2 <0, (1 / acs2014_wide$estimate.educ.pop_ge_25) * (sqrt((acs2014_wide$moe.ltcoll^2)+((acs2014_wide$estimate.ltcoll / acs2014_wide$estimate.educ.pop_ge_25)^2)*acs2014_wide$moe.educ.pop_ge_25^2)), (1 / acs2014_wide$estimate.educ.pop_ge_25) * (sqrt((acs2014_wide$moe.ltcoll^2)-((acs2014_wide$estimate.ltcoll / acs2014_wide$estimate.educ.pop_ge_25)^2)*acs2014_wide$moe.educ.pop_ge_25^2)))
colnames(acs2014_wide)
acs2014_short <- acs2014_wide[,c('GEOID','estimate.tot_pop','moe.tot_pop','estimate.MHI','moe.MHI','estimate.gini','moe.gini',
                                 'estimate.pct_white','moe.pct_white','estimate.pct_Black','moe.pct_Black',
                                 'estimate.pct_Asian','moe.pct_Asian','estimate.pct_Hispanic','moe.pct_Hispanic',
                                 'estimate.pct_fpl100','moe.pct_fpl100','estimate.pct_fpl150','moe.pct_fpl150','estimate.pct_fpl200','moe.pct_fpl200',
                                 'estimate.pct_unemp','moe.pct_unemp','estimate.pct_lths','moe.pct_lths','estimate.pct_ltcoll','moe.pct_ltcoll')]
colnames(acs2014_short)
dim(acs2014_short)
write.csv(acs2014_short, 'filepath/ACSonly_2014.csv',row.names=TRUE)

#adi, svi, sdi
adi2014_raw <- get_adi('tract', year=2014, raw_data_only=T) #dataset='acs5' is default
adi2014 <- calculate_adi(adi2014_raw)
adi2014$adi <- adi2014$ADI
adi2014_short <- adi2014[,c('GEOID','adi')]
colnames(adi2014_short)
rm(adi2014_raw)

svi2014_raw <- read.csv(file='filepath/SVI_2014_US.csv')
svi2014_raw$svi <- svi2014_raw$RPL_THEMES
svi2014_raw$GEOID <- svi2014_raw$FIPS
svi2014 <- svi2014_raw[,c('GEOID','svi')]
class(svi2014$GEOID) <- 'character'
dim(svi2014)
rm(svi2014_raw)

sdi2014_raw <- read.csv(file='filepath/SDI_2011_2015_CT.csv')
sdi2014_raw$sdi <- sdi2014_raw$SDI_score
sdi2014_raw$GEOID <- sdi2014_raw$CENSUSTRACT_FIPS
class(sdi2014$GEOID) <- 'character'
sdi2014 <- sdi2014_raw[,c('GEOID','sdi')]
class(sdi2014$GEOID) <- 'character'
head(sdi2014_raw)
dim(sdi2014)
rm(sdi2014_raw)

write.csv(adi2014, 'filepath/ADI_2014.csv',row.names=TRUE)
write.csv(svi2014, 'filepath/SVI_2014.csv',row.names=TRUE)
write.csv(sdi2014, 'filepath/SDI_2014.csv',row.names=TRUE)


#merge
acs14_adi <- merge(acs2014_short, adi2014_short, by='GEOID', all.x=TRUE, all.y=FALSE, sort=TRUE)
dim(acs14_adi)
acs14_adi_sdi <- merge(acs14_adi, sdi2014, by='GEOID', all.x=TRUE, all.y=FALSE, sort=TRUE)
dim(acs14_adi_sdi)
acs14_final <- merge(acs14_adi_sdi, svi2014, by='GEOID', all.x=TRUE, all.y=FALSE, sort=TRUE)
dim(acs14_final)
View(acs14_adi_sdi[is.na(acs14_adi_sdi$estimate.tot_pop)==TRUE,])
colnames(acs14_final)
write.csv(acs14_final, 'filepath/ACS_2014.csv',row.names=TRUE)



####JUNE 2026 UPDATE####
#replace SVI given SVI removed from CDC site 2025 or so - need to import SVI 2014 from prior cleaned files, then merge
#STEPS = upload ACS 2014 merged file, remove SVI column, upload previous ACS 2014 merged file and combine, isolate GEOID and svi columns, merge by GEOID, save
acs2014_short <- read.csv(file='filepath/ACSonly_2014.csv')
acs2014_short$GEOID_11 <- str_pad(as.character(acs2014_short$GEOID), 11, pad='0')
colnames(acs2014_short)
#acs2014_short <- subset(acs2014_short, select = -c(sdi,svi,adi))

#SVI from:
svi2014 <- read.csv(file='filepath/SVI_2014.csv')
colnames(svi2014)
svi2014$GEOID_11 <- str_pad(as.character(svi2014$GEOID), 11, pad='0')
svi2014$svi <- ifelse(svi2014$svi <0, NA, svi2014$svi)
summary(svi2014$svi[svi2014$svi >-0.01])
sum(svi2014$svi < 0, na.rm=T)
acs2014_old_svionly <- subset(svi2014, select= c('GEOID_11', 'svi'))

#ADI from: 
#note, must drop final FIPS character to go from block group to tract; AVERAGE within tracts
adi2015 <- read.csv(file='filepath/nanda_ADI_std_blckgrp_2015_01P.csv')
colnames(adi2015)
adi2015[1:10,]
adi2015$GEOID <- as.numeric(adi2015$blkgrp_fips10)
adi2015$GEOID_12 <- str_pad(as.character(adi2015$GEOID), 12, pad='0')
adi2015$GEOID_11 <- str_sub(adi2015$GEOID_12, end = -2)
adi2015$adi <- as.numeric(adi2015$adistd_ntl)
summary(as.numeric(adi2015$adi))
adi2015_ct <- adi2015 %>%
  group_by(GEOID_11) %>%
  summarize(adi = mean(adi, na.rm=T))
length(unique(adi2015$GEOID_11))
colnames(adi2015_ct)
dim(adi2015_ct)
summary(adi2015_ct$adi)
#adi2015_ct <- subset(adi2015, select = c('GEOID_11','adi'))

#SDI from:
sdi2014_raw <- read.csv(file='filepath/SDI_2014.csv')
sdi2014_raw$GEOID_11 <- str_pad(as.character(sdi2014_raw$GEOID), 11, pad='0')
sdi2014 <- subset(sdi2014_raw, select = c('GEOID_11','sdi'))

#Merge
colnames(acs2014_short)
colnames(acs2014_old_svionly)
colnames(adi2015_ct)
colnames(sdi2014)
acs2014_svi2014 <- merge(acs2014_short, acs2014_old_svionly, by='GEOID_11', all.x=TRUE, all.y=FALSE, sort=TRUE)
summary(acs2014_svi2014$svi[acs2014_svi2014$svi >-0.01])
sum(acs2014_svi2014$svi < 0, na.rm=T)
acs2014_svisdi <- merge(acs2014_svi2014, sdi2014, by='GEOID_11', all.x=TRUE, all.y=FALSE, sort=TRUE)
acs2014_svisdi[1:10,]
summary(acs2014_svisdi$svi)
acs2014 <- merge(acs2014_svisdi, adi2015_ct, by='GEOID_11', all.x=TRUE, all.y=FALSE, sort=TRUE)
acs2014[1:10,]
summary(acs2014$adi)

colnames(acs2014)
acs2014[1:10,c("GEOID_11","adi","svi","sdi")]
class(acs2014$GEOID_11)
acs2014$FIPS_state <- substr(acs2014$GEOID_11, start=1, stop=2)
acs2014$FIPS_county <- substr(acs2014$GEOID_11, start=3, stop=5)
acs2014$GEOID_6 <- str_sub(acs2014$GEOID_11, -6)
acs2014 <- subset(acs2014, select = -c(X))
sum(is.na(acs2014$estimate.tot_pop), na.rm=T)
write.csv(acs2014, 'filepath/ACS_2014_FINAL.csv',row.names=TRUE)
dim(acs2014)
acs2014_1 <- acs2014[1:40000,]
acs2014_2 <- acs2014[40001:73056,]

write.csv(acs2014_1, 'filepath/ACS_2014_01.csv',row.names=TRUE)
write.csv(acs2014_2, 'filepath/ACS_2014_02.csv.csv',row.names=TRUE)












 
#####American Community Survey 2005-2009 for NHANES 1999-2009#####
acs2009_vars_all <- load_variables(2009, 'acs5', cache=T)
View(acs2009_vars_all)
key_acs_vars2009 <-c(tot_pop='B01003_001',tot_pop_racetab='B03002_001',Hispanic = "B03002_012", White = "B03002_003", Black = "B03002_004", Asian = "B03002_006", 
                     MHI='B19013_001', tot_pop_FPL='C17002_001',FPL00.50='C17002_002', FPL50.99='C17002_003', FPL100.124='C17002_004', 
                     FPL125.149='C17002_005', FPL150.184='C17002_006', FPL185.199='C17002_007', #gini='B19083_001',
                     emp.pop_ge_16='C23002A_001',unemp16.64men='C23002A_008',  unemp16.64women='C23002A_021',unemp65men='C23002A_013',  unemp65women='C23002A_026',
                     educ.pop_ge_25='B15002_001',educ.male.pop_ge_25='B15002_002',educ.male.none='B15002_003', educ.male.lt5th='B15002_004', educ.male.56th='B15002_005', 
                     educ.male.78th='B15002_006', educ.male.9th='B15002_007', educ.male.10th='B15002_008', educ.male.11th='B15002_009', educ.male.12th_nodegree='B15002_010',
                     educ.male.HSged='B15002_011', educ.male.some_coll1yr='B15002_012', educ.male.some_coll='B15002_013', educ.male.assocdeg='B15002_014',
                     educ.male.bach='B15002_015', educ.male.mast='B15002_016', educ.male.prof='B15002_017', educ.male.doct='B15002_018',
                     educ.female.pop_ge_25='B15002_019',educ.female.none='B15002_020', educ.female.lt5th='B15002_021', educ.female.56th='B15002_022', 
                     educ.female.78th='B15002_023', educ.female.9th='B15002_024', educ.female.10th='B15002_025', educ.female.11th='B15002_026', educ.female.12th_nodegree='B15002_027',
                     educ.female.HSged='B15002_028', educ.female.some_coll1yr='B15002_029', educ.female.some_coll='B15002_030', educ.female.assocdeg='B15002_031',
                     educ.female.bach='B15002_032', educ.female.mast='B15002_033', educ.female.prof='B15002_034', educ.female.doct='B15002_035')
acs2009_test <- get_acs(geography='tract', variables=key_acs_vars2009, state='RI', year=2009, survey='acs5')
head(acs2009_test)
rm(acs2009_test)
acs2009_raw <- get_acs(geography='tract', variables=key_acs_vars2009, state=state_abb, year=2009, survey='acs5')
acs2009_raw2 <- acs2009_raw[,c('GEOID','variable','estimate','moe')]
head(acs2009_raw2)
length(unique(acs2009_raw2$GEOID))

##GINI not available until 2010 - adding here
####gini <- get_acs(geography='tract', variables='B19083_001', state=state_abb, year=2010, survey='acs5')
##GEOID alignment with acs 2009 is poor, likely redrawn CTs - merge with svi/sdi/adi first then with acs
####setdiff(acs2009_raw2$GEOID, gini$GEOID) #13k GEOIDs do not match (acs not found in gini)

acs2009_wide <- acs2009_raw2 %>% pivot_wider(id_cols=GEOID, names_from=variable, values_from=c(estimate, moe))
colnames(acs2009_wide)
head(acs2009_wide)

acs2009_wide$estimate_fpl_lt100 <- acs2009_wide$estimate_FPL00.50 + acs2009_wide$estimate_FPL50.99
acs2009_wide$moe_fpl_lt100 <- sqrt((acs2009_wide$moe_FPL00.50)^2 + (acs2009_wide$moe_FPL50.99)^2)

acs2009_wide$estimate_fpl_lt150 <- acs2009_wide$estimate_FPL00.50 + acs2009_wide$estimate_FPL50.99 + acs2009_wide$estimate_FPL100.124 + acs2009_wide$estimate_FPL125.149
acs2009_wide$moe_fpl_lt150 <- sqrt((acs2009_wide$moe_FPL00.50)^2 + (acs2009_wide$moe_FPL50.99)^2 + (acs2009_wide$moe_FPL100.124)^2 + (acs2009_wide$moe_FPL125.149)^2)

acs2009_wide$estimate_fpl_lt200 <- acs2009_wide$estimate_FPL00.50 + acs2009_wide$estimate_FPL50.99 + acs2009_wide$estimate_FPL100.124 + acs2009_wide$estimate_FPL125.149 + acs2009_wide$estimate_FPL150.184 + acs2009_wide$estimate_FPL185.199
acs2009_wide$moe_fpl_lt200 <- sqrt((acs2009_wide$moe_FPL00.50)^2 + (acs2009_wide$moe_FPL50.99)^2 + (acs2009_wide$moe_FPL100.124)^2 + (acs2009_wide$moe_FPL125.149)^2 + (acs2009_wide$moe_FPL150.184)^2 + (acs2009_wide$moe_FPL185.199)^2)

acs2009_wide$estimate_lths <- acs2009_wide$estimate_educ.male.none + acs2009_wide$estimate_educ.male.lt5th + acs2009_wide$estimate_educ.male.56th + acs2009_wide$estimate_educ.male.78th + acs2009_wide$estimate_educ.male.9th + acs2009_wide$estimate_educ.male.10th + acs2009_wide$estimate_educ.male.11th + acs2009_wide$estimate_educ.male.12th_nodegree +
  acs2009_wide$estimate_educ.female.none + acs2009_wide$estimate_educ.female.lt5th + acs2009_wide$estimate_educ.female.56th + acs2009_wide$estimate_educ.female.78th + acs2009_wide$estimate_educ.female.9th + acs2009_wide$estimate_educ.female.10th + acs2009_wide$estimate_educ.female.11th + acs2009_wide$estimate_educ.female.12th_nodegree
acs2009_wide$moe_lths <- + sqrt((acs2009_wide$moe_educ.male.none)^2 + (acs2009_wide$moe_educ.male.lt5th)^2 + (acs2009_wide$moe_educ.male.56th)^2 + (acs2009_wide$moe_educ.male.78th)^2 + (acs2009_wide$moe_educ.male.9th)^2 + (acs2009_wide$moe_educ.male.10th)^2 + (acs2009_wide$moe_educ.male.11th)^2 + (acs2009_wide$moe_educ.male.12th_nodegree)^2 +
  (acs2009_wide$moe_educ.female.none)^2 + (acs2009_wide$moe_educ.female.lt5th)^2 + (acs2009_wide$moe_educ.female.56th)^2 + (acs2009_wide$moe_educ.female.78th)^2 + (acs2009_wide$moe_educ.female.9th)^2 + (acs2009_wide$moe_educ.female.10th)^2 + (acs2009_wide$moe_educ.female.11th)^2 + (acs2009_wide$moe_educ.female.12th_nodegree)^2)

acs2009_wide$estimate_ltcoll <- acs2009_wide$estimate_educ.male.none + acs2009_wide$estimate_educ.male.lt5th + acs2009_wide$estimate_educ.male.56th + acs2009_wide$estimate_educ.male.78th + acs2009_wide$estimate_educ.male.9th + acs2009_wide$estimate_educ.male.10th + acs2009_wide$estimate_educ.male.11th + acs2009_wide$estimate_educ.male.12th_nodegree + acs2009_wide$estimate_educ.male.HSged + acs2009_wide$estimate_educ.male.some_coll1yr + acs2009_wide$estimate_educ.male.some_coll + acs2009_wide$estimate_educ.male.assocdeg + acs2009_wide$estimate_educ.female.none + acs2009_wide$estimate_educ.female.lt5th + acs2009_wide$estimate_educ.female.56th + acs2009_wide$estimate_educ.female.78th + acs2009_wide$estimate_educ.female.9th + acs2009_wide$estimate_educ.female.10th + acs2009_wide$estimate_educ.female.11th + acs2009_wide$estimate_educ.female.12th_nodegree + acs2009_wide$estimate_educ.female.HSged + acs2009_wide$estimate_educ.female.some_coll1yr + acs2009_wide$estimate_educ.female.some_coll + acs2009_wide$estimate_educ.female.assocdeg
acs2009_wide$moe_ltcoll <- sqrt((acs2009_wide$moe_educ.male.none)^2 + (acs2009_wide$moe_educ.male.lt5th)^2 + (acs2009_wide$moe_educ.male.56th)^2 + (acs2009_wide$moe_educ.male.78th)^2 + (acs2009_wide$moe_educ.male.9th)^2 + (acs2009_wide$moe_educ.male.10th)^2 + (acs2009_wide$moe_educ.male.11th)^2 + (acs2009_wide$moe_educ.male.12th_nodegree)^2 + (acs2009_wide$estimate_educ.male.HSged)^2 + (acs2009_wide$estimate_educ.male.some_coll1yr)^2 + (acs2009_wide$estimate_educ.male.some_coll)^2 + (acs2009_wide$estimate_educ.male.assocdeg)^2 + (acs2009_wide$moe_educ.female.none)^2 + (acs2009_wide$moe_educ.female.lt5th)^2 + (acs2009_wide$moe_educ.female.56th)^2 + (acs2009_wide$moe_educ.female.78th)^2 + (acs2009_wide$moe_educ.female.9th)^2 + (acs2009_wide$moe_educ.female.10th)^2 + (acs2009_wide$moe_educ.female.11th)^2 + (acs2009_wide$moe_educ.female.12th_nodegree)^2 + (acs2009_wide$estimate_educ.female.HSged)^2 + (acs2009_wide$estimate_educ.female.some_coll1yr)^2 + (acs2009_wide$estimate_educ.female.some_coll)^2 + (acs2009_wide$estimate_educ.female.assocdeg)^2)

#percents 
acs2009_wide$estimate_pct_white <- acs2009_wide$estimate_White / acs2009_wide$estimate_tot_pop
acs2009_wide$moe_pct_white <- ifelse(acs2009_wide$moe_White^2-((acs2009_wide$estimate_White / acs2009_wide$estimate_tot_pop)^2)*acs2009_wide$moe_tot_pop^2 <0, (1 / acs2009_wide$estimate_tot_pop) * (sqrt((acs2009_wide$moe_White^2)+((acs2009_wide$estimate_White / acs2009_wide$estimate_tot_pop)^2)*acs2009_wide$moe_tot_pop^2)), (1 / acs2009_wide$estimate_tot_pop) * (sqrt((acs2009_wide$moe_White^2)-((acs2009_wide$estimate_White / acs2009_wide$estimate_tot_pop)^2)*acs2009_wide$moe_tot_pop^2)))
summary(acs2009_wide$moe_pct_white)
acs2009_wide$estimate_pct_Black <- acs2009_wide$estimate_Black / acs2009_wide$estimate_tot_pop
acs2009_wide$moe_pct_Black <- ifelse(acs2009_wide$moe_Black^2-((acs2009_wide$estimate_Black / acs2009_wide$estimate_tot_pop)^2)*acs2009_wide$moe_tot_pop^2 <0, (1 / acs2009_wide$estimate_tot_pop) * (sqrt((acs2009_wide$moe_Black^2)+((acs2009_wide$estimate_Black / acs2009_wide$estimate_tot_pop)^2)*acs2009_wide$moe_tot_pop^2)), (1 / acs2009_wide$estimate_tot_pop) * (sqrt((acs2009_wide$moe_Black^2)-((acs2009_wide$estimate_Black / acs2009_wide$estimate_tot_pop)^2)*acs2009_wide$moe_tot_pop^2)))
acs2009_wide$estimate_pct_Asian <- acs2009_wide$estimate_Asian / acs2009_wide$estimate_tot_pop
acs2009_wide$moe_pct_Asian <- ifelse(acs2009_wide$moe_Asian^2-((acs2009_wide$estimate_Asian / acs2009_wide$estimate_tot_pop)^2)*acs2009_wide$moe_tot_pop^2 <0, (1 / acs2009_wide$estimate_tot_pop) * (sqrt((acs2009_wide$moe_Asian^2)+((acs2009_wide$estimate_Asian / acs2009_wide$estimate_tot_pop)^2)*acs2009_wide$moe_tot_pop^2)), (1 / acs2009_wide$estimate_tot_pop) * (sqrt((acs2009_wide$moe_Asian^2)-((acs2009_wide$estimate_Asian / acs2009_wide$estimate_tot_pop)^2)*acs2009_wide$moe_tot_pop^2)))
acs2009_wide$estimate_pct_Hispanic <- acs2009_wide$estimate_Hispanic / acs2009_wide$estimate_tot_pop
acs2009_wide$moe_pct_Hispanic <- ifelse(acs2009_wide$moe_Hispanic^2-((acs2009_wide$estimate_Hispanic / acs2009_wide$estimate_tot_pop)^2)*acs2009_wide$moe_tot_pop^2 <0, (1 / acs2009_wide$estimate_tot_pop) * (sqrt((acs2009_wide$moe_Hispanic^2)+((acs2009_wide$estimate_Hispanic / acs2009_wide$estimate_tot_pop)^2)*acs2009_wide$moe_tot_pop^2)), (1 / acs2009_wide$estimate_tot_pop) * (sqrt((acs2009_wide$moe_Hispanic^2)-((acs2009_wide$estimate_Hispanic / acs2009_wide$estimate_tot_pop)^2)*acs2009_wide$moe_tot_pop^2)))
acs2009_wide$estimate_pct_fpl100 <- acs2009_wide$estimate_fpl_lt100 / acs2009_wide$estimate_tot_pop_FPL
acs2009_wide$moe_pct_fpl100 <- ifelse(acs2009_wide$moe_fpl_lt100^2-((acs2009_wide$estimate_fpl_lt100 / acs2009_wide$estimate_tot_pop_FPL)^2)*acs2009_wide$moe_tot_pop^2 <0, (1 / acs2009_wide$estimate_tot_pop_FPL) * (sqrt((acs2009_wide$moe_fpl_lt100^2)+((acs2009_wide$estimate_fpl_lt100 / acs2009_wide$estimate_tot_pop_FPL)^2)*acs2009_wide$moe_tot_pop^2)), (1 / acs2009_wide$estimate_tot_pop_FPL) * (sqrt((acs2009_wide$moe_fpl_lt100^2)-((acs2009_wide$estimate_fpl_lt100 / acs2009_wide$estimate_tot_pop_FPL)^2)*acs2009_wide$moe_tot_pop^2)))
acs2009_wide$estimate_pct_fpl150 <- acs2009_wide$estimate_fpl_lt150 / acs2009_wide$estimate_tot_pop_FPL
acs2009_wide$moe_pct_fpl150 <- ifelse(acs2009_wide$moe_fpl_lt150^2-((acs2009_wide$estimate_fpl_lt150 / acs2009_wide$estimate_tot_pop_FPL)^2)*acs2009_wide$moe_tot_pop^2 <0, (1 / acs2009_wide$estimate_tot_pop_FPL) * (sqrt((acs2009_wide$moe_fpl_lt150^2)+((acs2009_wide$estimate_fpl_lt150 / acs2009_wide$estimate_tot_pop_FPL)^2)*acs2009_wide$moe_tot_pop^2)), (1 / acs2009_wide$estimate_tot_pop_FPL) * (sqrt((acs2009_wide$moe_fpl_lt150^2)-((acs2009_wide$estimate_fpl_lt150 / acs2009_wide$estimate_tot_pop_FPL)^2)*acs2009_wide$moe_tot_pop^2)))
acs2009_wide$estimate_pct_fpl200 <- acs2009_wide$estimate_fpl_lt200 / acs2009_wide$estimate_tot_pop_FPL
acs2009_wide$moe_pct_fpl200 <- ifelse(acs2009_wide$moe_fpl_lt200^2-((acs2009_wide$estimate_fpl_lt200 / acs2009_wide$estimate_tot_pop_FPL)^2)*acs2009_wide$moe_tot_pop^2 <0, (1 / acs2009_wide$estimate_tot_pop_FPL) * (sqrt((acs2009_wide$moe_fpl_lt200^2)+((acs2009_wide$estimate_fpl_lt200 / acs2009_wide$estimate_tot_pop_FPL)^2)*acs2009_wide$moe_tot_pop^2)), (1 / acs2009_wide$estimate_tot_pop_FPL) * (sqrt((acs2009_wide$moe_fpl_lt200^2)-((acs2009_wide$estimate_fpl_lt200 / acs2009_wide$estimate_tot_pop_FPL)^2)*acs2009_wide$moe_tot_pop^2)))
summary(acs2009_wide$estimate_pct_fpl200)
acs2009_wide$estimate_unemp <- acs2009_wide$estimate_unemp16.64men + acs2009_wide$estimate_unemp16.64women + acs2009_wide$estimate_unemp65men + acs2009_wide$estimate_unemp65women
acs2009_wide$moe_unemp <- sqrt((acs2009_wide$moe_unemp16.64men)^2 + (acs2009_wide$moe_unemp16.64women)^2 + (acs2009_wide$moe_unemp65men)^2 + (acs2009_wide$moe_unemp65women)^2)
acs2009_wide$estimate_pct_unemp <- acs2009_wide$estimate_unemp / acs2009_wide$estimate_emp.pop_ge_16
acs2009_wide$moe_pct_unemp <- ifelse(acs2009_wide$moe_unemp^2-((acs2009_wide$estimate_unemp / acs2009_wide$estimate_emp.pop_ge_16)^2)*acs2009_wide$moe_emp.pop_ge_16^2 <0, (1 / acs2009_wide$estimate_emp.pop_ge_16) * (sqrt((acs2009_wide$moe_unemp^2)+((acs2009_wide$estimate_unemp / acs2009_wide$estimate_emp.pop_ge_16)^2)*acs2009_wide$moe_emp.pop_ge_16^2)), (1 / acs2009_wide$estimate_emp.pop_ge_16) * (sqrt((acs2009_wide$moe_unemp^2)-((acs2009_wide$estimate_unemp / acs2009_wide$estimate_emp.pop_ge_16)^2)*acs2009_wide$moe_emp.pop_ge_16^2)))
acs2009_wide$estimate_pct_lths <- acs2009_wide$estimate_lths / acs2009_wide$estimate_educ.pop_ge_25
acs2009_wide$moe_pct_lths <- ifelse(acs2009_wide$moe_lths^2-((acs2009_wide$estimate_lths / acs2009_wide$estimate_educ.pop_ge_25)^2)*acs2009_wide$moe_educ.pop_ge_25^2 <0, (1 / acs2009_wide$estimate_educ.pop_ge_25) * (sqrt((acs2009_wide$moe_lths^2)+((acs2009_wide$estimate_lths / acs2009_wide$estimate_educ.pop_ge_25)^2)*acs2009_wide$moe_educ.pop_ge_25^2)), (1 / acs2009_wide$estimate_educ.pop_ge_25) * (sqrt((acs2009_wide$moe_lths^2)-((acs2009_wide$estimate_lths / acs2009_wide$estimate_educ.pop_ge_25)^2)*acs2009_wide$moe_educ.pop_ge_25^2)))
acs2009_wide$estimate_pct_ltcoll <- acs2009_wide$estimate_ltcoll / acs2009_wide$estimate_educ.pop_ge_25
acs2009_wide$moe_pct_ltcoll <- ifelse(acs2009_wide$moe_ltcoll^2-((acs2009_wide$estimate_ltcoll / acs2009_wide$estimate_educ.pop_ge_25)^2)*acs2009_wide$moe_educ.pop_ge_25^2 <0, (1 / acs2009_wide$estimate_educ.pop_ge_25) * (sqrt((acs2009_wide$moe_ltcoll^2)+((acs2009_wide$estimate_ltcoll / acs2009_wide$estimate_educ.pop_ge_25)^2)*acs2009_wide$moe_educ.pop_ge_25^2)), (1 / acs2009_wide$estimate_educ.pop_ge_25) * (sqrt((acs2009_wide$moe_ltcoll^2)-((acs2009_wide$estimate_ltcoll / acs2009_wide$estimate_educ.pop_ge_25)^2)*acs2009_wide$moe_educ.pop_ge_25^2)))
summary(acs2009_wide$moe_ltcoll)
colnames(acs2009_wide)
acs2009_short <- acs2009_wide[,c('GEOID','estimate_tot_pop','moe_tot_pop','estimate_MHI','moe_MHI',#'estimate_gini','moe_gini',
                                 'estimate_pct_white','moe_pct_white','estimate_pct_Black','moe_pct_Black',
                                 'estimate_pct_Asian','moe_pct_Asian','estimate_pct_Hispanic','moe_pct_Hispanic',
                                 'estimate_pct_fpl100','moe_pct_fpl100','estimate_pct_fpl150','moe_pct_fpl150','estimate_pct_fpl200','moe_pct_fpl200',
                                 'estimate_pct_unemp','moe_pct_unemp','estimate_pct_lths','moe_pct_lths','estimate_pct_ltcoll','moe_pct_ltcoll')]
acs2009_short <- as.data.frame(acs2009_short)
dim(acs2009_short)

write.csv(acs2009_short, 'filepath/acsonly_2009.csv',row.names=TRUE)
acs2009_short <- read.csv(file='filepath/acsonly_2009.csv')

#adi, svi, sdi
adi2009_raw <- get_adi('tract', year=2009, raw_data_only=T) #dataset='acs5' is default
adi2009 <- calculate_adi(adi2009_raw)
adi2009$adi <- adi2009$ADI
adi2009_short <- adi2009[,c('GEOID','adi')]
colnames(adi2009_short)
rm(adi2009_raw)

svi2009_raw <- read.csv(file='filepath/SVI_2010_US.csv')
svi2009_raw$svi <- svi2009_raw$R_PL_THEMES
svi2009_raw$GEOID <- svi2009_raw$FIPS
svi2009 <- svi2009_raw[,c('GEOID','svi')]
class(svi2009$GEOID) <- 'character'
dim(svi2009)
rm(svi2009_raw)

sdi2009_raw <- read.csv(file='filepath/SDI_2008_2012_CT.csv')
sdi2009_raw$sdi <- sdi2009_raw$SDI_score
sdi2009_raw$GEOID <- sdi2009_raw$CENSUSTRACT_FIPS
class(sdi2009$GEOID) <- 'character'
sdi2009 <- sdi2009_raw[,c('GEOID','sdi')]
class(sdi2009$GEOID) <- 'character'
head(sdi2009_raw)
dim(sdi2009)
rm(sdi2009_raw)

write.csv(adi2009_short, 'filepath/ADI_2009.csv',row.names=TRUE)
write.csv(svi2009, 'filepath/SVI_2009.csv',row.names=TRUE)
write.csv(sdi2009, 'filepath/SDI_2009.csv',row.names=TRUE)

#merge
acs09_adi <- merge(acs2009_short, adi2009_short, by='GEOID', all.x=TRUE, all.y=FALSE, sort=TRUE)
dim(acs09_adi)
acs09_adi_sdi <- merge(acs09_adi, sdi2009, by='GEOID', all.x=TRUE, all.y=FALSE, sort=TRUE)
dim(acs09_adi_sdi)
acs09_final <- merge(acs09_adi_sdi, svi2009, by='GEOID', all.x=TRUE, all.y=FALSE, sort=TRUE)
dim(acs09_final)
View(acs09_adi_sdi[is.na(acs09_adi_sdi$estimate.tot_pop)==TRUE,])
colnames(acs09_final)
write.csv(acs09_final, 'filepath/ACS_2009.csv',row.names=TRUE)



####JUNE 2026 UPDATE####
#replace SVI given SVI removed from CDC site 2025 or so - need to import SVI 2009 from prior cleaned files, then merge
#STEPS = upload ACS 2009 merged file, remove SVI column, upload previous ACS 2009 merged file and combine, isolate GEOID and svi columns, merge by GEOID, save
acs2009_short <- read.csv(file='filepath/acsonly_2009.csv')
colnames(acs2009_short)
acs2009_short$GEOID_11 <- str_pad(as.character(acs2009_short$GEOID), 11, pad='0')
#acs2009_short <- subset(acs2009_short, select = -c(sdi,svi,adi))

#SVI from:
svi2009 <- read.csv(file='filepath/SVI_2009.csv')
#acs2009_old <- rbind(acs2009_old1, acs2009_old2)
svi2009$GEOID_11 <- str_pad(as.character(svi2009$GEOID), 11, pad='0')
svi2009$svi <- ifelse(svi2009$svi <0, NA, svi2009$svi)
summary(svi2009$svi[svi2009$svi >-0.01])
sum(svi2009$svi < 0, na.rm=T)
svi2009 <- subset(svi2009, select= c('GEOID_11', 'svi'))

#ADI from: 
#note, must drop final FIPS character to go from block group to tract; AVERAGE within tracts
adi2015 <- read.csv(file='filepath/nanda_ADI_std_blckgrp_2015_01P.csv')
colnames(adi2015)
adi2015$blkgrp_fips20[1:10]
adi2015$GEOID <- as.numeric(adi2015$blkgrp_fips20)
adi2015$GEOID_12 <- str_pad(as.character(adi2015$GEOID), 12, pad='0')
adi2015$GEOID_11 <- str_sub(adi2015$GEOID_12, end = -2)
adi2015$adi <- as.numeric(adi2015$adistd_ntl)
summary(as.numeric(adi2015$adi))
adi2015_ct <- adi2015 %>%
  group_by(GEOID_11) %>%
  summarize(adi = mean(adi, na.rm=T))
dim(adi2015_ct)
summary(adi2015_ct$adi)
#adi2015_ct <- subset(adi2015, select = c('GEOID_11','adi'))

#SDI from:
sdi2009_raw <- read.csv(file='filepath/SDI_2009.csv')
sdi2009_raw$GEOID_11 <- str_pad(as.character(sdi2009_raw$GEOID), 11, pad='0')
sdi2009 <- subset(sdi2009_raw, select = c('GEOID_11','sdi'))

#Merge
colnames(acs2009_short)
colnames(svi2009)
colnames(adi2015_ct)
colnames(sdi2009)
acs2009_short[1:10,]

#explore why merge of svi introduces NAs - because some GEOIDs exist in ACS that do not exist in svi2009
sum(!(acs2009_short$GEOID_11 %in% svi2009$GEOID_11)) #13728 in acs are not in svi
mean_results <- acs2009_short %>% 
  summarise(
    mean_in_dataset = mean(estimate_tot_pop[GEOID_11 %in% svi2009$GEOID_11], na.rm = TRUE),
    mean_not_in_dataset = mean(estimate_tot_pop[!GEOID_11 %in% svi2009$GEOID_11], na.rm = TRUE)
  ) #total population is actually higher in those not in svi than in those in svi
setdiff(acs2009_short$GEOID_11, svi2009$GEOID_11)[1:20] #acs but not svi is not focal - exist across states
setdiff(svi2009$GEOID_11, acs2009_short$GEOID_11)[1:20] #svi but not acs not focal - exist across states
setdiff(svi2009$GEOID_11, sdi2009$GEOID_11) #svi and sdi almost entirely congruent
setdiff(svi2009$GEOID_11, adi2015_ct$GEOID_11) #svi and sdi almost entirely congruent

#updated merge procedure for 2009 - merge svi/sdi/adi first, then merge with acs (first by exact match, then by 10-digit geoid match, averaging svi/sdi/adi values if more than one row per 10-digit GEOID)
svisdi2009 <- merge(svi2009, sdi2009, by='GEOID_11', all.x=TRUE, all.y=FALSE, sort=TRUE)
svisdiadi2009 <- merge(svisdi2009, adi2015_ct, by='GEOID_11', all.x=TRUE, all.y=FALSE, sort=TRUE)
dim(svisdiadi2009)
colnames(svisdiadi2009)
svisdiadi2009[1:10,]
summary(svisdiadi2009[,c('svi','sdi','adi')], na.rm=T)
#NOTE gini is missing as only available 2010: adding here; see above re: GEOIDs match better with svi/sdi/adi than acs
gini <- get_acs(geography='tract', variables='B19083_001', state=state_abb, year=2010, survey='acs5')
gini$GEOID_11 <- str_pad(as.character(gini$GEOID), 11, pad='0')
gini$estimate.gini <- gini$estimate
gini$moe.gini <- gini$moe
gini_estmoe <- subset(gini, select = c(GEOID_11, estimate.gini, moe.gini))
summary(gini_estmoe$estimate.gini, na.rm=T)
scisdiadigini2009 <- merge(svisdiadi2009, gini_estmoe, by='GEOID_11', all.x=TRUE, all.y=FALSE, sort=TRUE)
summary(acs2009_wgini$estimate, na.rm=T)


# 1. Prepare df2: Average duplicate 10-digit IDs and retain unique 10-digit matches
df2_avg <- svisdiadi2009 %>%
  group_by(GEOID_10 = str_sub(GEOID_11, 1, 10)) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

# 2. Extract exact matches
df_exact <- acs2009_short %>%
  inner_join(svisdiadi2009, by = "GEOID_11") %>%
  mutate(match_type = "exact")

# 3. Extract unmatched rows from df1
df_unmatched <- acs2009_short %>%
  anti_join(df_exact, by = "GEOID_11") %>%
  mutate(GEOID_10 = str_sub(GEOID_11, 1, 10))

# 4. Merge unmatched rows with averaged df2 data
df_partial <- df_unmatched %>%
  left_join(df2_avg, by = "GEOID_10") %>%
  mutate(match_type = "partial") %>%
  select(-GEOID_10)

# 5. Combine everything
final_df <- bind_rows(df_exact, df_partial)

summary(final_df[,c('svi','sdi','adi')], na.rm=T)


# 1. Prepare df3: Average duplicate 10-digit IDs for partial matching
df3_avg <- gini_estmoe %>%
  group_by(GEOID_10 = str_sub(GEOID_11, 1, 10)) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop")

# 2. Match final_df with df3 exactly
df3_exact <- final_df %>%
  inner_join(gini_estmoe, by = "GEOID_11")

# 3. Separate unmatched rows from final_df
df3_unmatched <- final_df %>%
  anti_join(df3_exact, by = "GEOID_11") %>%
  mutate(GEOID_10 = str_sub(GEOID_11, 1, 10))

# 4. Match remaining rows using the 10-digit summary
df3_partial <- df3_unmatched %>%
  left_join(df3_avg, by = "GEOID_10") %>%
  select(-GEOID_10)

# 5. Bind exact and partial rows into the ultimate dataset
ultimate_df <- bind_rows(df3_exact, df3_partial)

ultimate_df[1:10,]
summary(ultimate_df[,c('svi','sdi','adi','estimate.gini')], na.rm=T)
acs2009 <- ultimate_df
colnames(acs2009)
colnames(acs2014)

acs2009$FIPS_state <- substr(acs2009$GEOID_11, start=1, stop=2)
acs2009$FIPS_county <- substr(acs2009$GEOID_11, start=3, stop=5)
acs2009$GEOID_6 <- str_sub(acs2009$GEOID_11, -6)
acs2009 <- subset(acs2009, select = -c(X,match_type))
dim(acs2009) #expect 34 cols

sum(is.na(acs2009$estimate.tot_pop), na.rm=T)
write.csv(acs2009, 'filepath/ACS_2009_FINAL.csv',row.names=TRUE)
acs2009_1 <- acs2009[1:40000,]
acs2009_2 <- acs2009[40001:65461,]
write.csv(acs2009_1, 'filepath/ACS_2009_01.csv',row.names=TRUE)
write.csv(acs2009_2, 'filepath/ACS_2009_02.csv',row.names=TRUE)




#####make data dictionary#####
read.table('filepath/ACS_keyvar_200509.csv', head=TRUE, nrows=1, sep=',')[-1,]
read.table('filepath/ACS_keyvar_201014.csv', head=TRUE, nrows=1, sep=',')[-1,]
read.table('filepath/ACS_keyvar_201418.csv', head=TRUE, nrows=1, sep=',')[-1,]



#####break data sets into smaller chunks for github#####
acs1 <- read.csv('filepath/ACS_2009.csv', head=TRUE, sep=',')
dim(acs1)
colnames(acs1) <- c("X.1","GEOID","X","estimate.tot_pop","moe.tot_pop","estimate.MHI","moe.MHI","estimate.pct_white","moe.pct_white",
                    "estimate.pct_Black","moe.pct_Black","estimate.pct_Asian","moe.pct_Asian","estimate.pct_Hispanic","moe.pct_Hispanic",
                    "estimate.pct_fpl100","moe.pct_fpl100","estimate.pct_fpl150","moe.pct_fpl150","estimate.pct_fpl200","moe.pct_fpl200",
                    "estimate.pct_unemp","moe.pct_unemp","estimate.pct_lths","moe.pct_lths","estimate.pct_ltcoll","moe.pct_ltcoll","adi","sdi","svi")
acs1.1 <- acs1[1:40000,]
acs1.2 <- acs1[40001:65461,]
write.csv(acs1.1, 'filepath/ACS_2009_1.csv',row.names=TRUE)
write.csv(acs1.2, 'filepath/ACS_2009_2.csv',row.names=TRUE)

acs2 <- read.csv('filepath/ACS_2014.csv', head=TRUE, sep=',')
dim(acs2)
acs2.1 <- acs2[1:40000,]
acs2.2 <- acs2[40001:73056,]
write.csv(acs2.1, 'filepath/ACS_2014_1.csv',row.names=TRUE)
write.csv(acs2.2, 'filepath/ACS_2014_2.csv',row.names=TRUE)

acs3 <- read.csv('filepath/ACS_2019.csv', head=TRUE, sep=',')
dim(acs3)
acs3.1 <- acs3[1:40000,]
acs3.2 <- acs3[40001:87700,]
write.csv(acs3.1, 'filepath/ACS_2019_1.csv',row.names=TRUE)
write.csv(acs3.2, 'filepath/ACS_2019_2.csv',row.names=TRUE)
