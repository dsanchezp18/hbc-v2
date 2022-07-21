# ==================================== LAPOP ECUADOR TABULATIONS  ========================================================

# Daniel Sánchez
# USFQ Economics Undergraduate Thesis
# June 2022
# Script for the calculations of survey-weighted tabulations of the LAPOP study
# Dependent on the data_manipulation.R script- it is sourced here.

# ================================================= Preliminaries =============================================================

# No working directory specification needed since an Rproject is being used

# Load libraries --------------------------------------------------------------------------------------------------------------

library(tidyverse) # Ggplot and dplyr
library(openxlsx) # Import and export XL files
library(survey) # For survey robust calculations. 

# Load the data ----------------------------------------------------------------------------------------------------------

source('scripts/data_manipulation.R')

# ================================================= Cross-year tabulations ================================================

# Corruption variables ---------------------------------------------------------------------------------------------------

# New corruption perceptions question (2016 and on)

corrpernew_time<-svyby(~ corrper_new, 
                       ~ year, 
                       design = lapop_des,
                       svymean, 
                       na.rm = T)

# Corruption as the most important problem in the country

corprob_time<-svyby(~ corprob, 
                    ~ year, 
                    design = lapop_des,
                    svymean, 
                    na.rm = T)

# Corruption tolerance (ctol)

ctol_time<-svyby(~ ctol, 
                 ~ year, 
                 design = lapop_des,
                 svymean, 
                 na.rm = T)

# Corruption Perceptions

corrper_time<-svyby(~ corrper,
                    ~ year,
                    design = lapop_des,
                    svymean,
                    na.rm = T)

# Economic indicators ----------------------------------------------------------------------------------------------------

# Economic situation

ecsit_time<-svyby(~ econ_sit, 
                  ~ year, 
                  design = lapop_des, 
                  svymean, 
                  na.rm = T)

# Unemployment rate (full)

unem_time<-svyby(~ unem2_4a,
                 ~ year, 
                 design = lapop_des, 
                 svymean, 
                 na.rm = T)

# Unemployment (complete)

unem_t<-svyby(~ unem2_4a, 
              ~ year, 
              design = lapop_des,
              svymean, 
              na.rm = T)

# Political opinion variables --------------------------------------------------------------------------------------------

# Job Approval Rating

japrov_time<-svyby(~ pres_aprov_dic, 
                   ~ year, 
                   design = lapop_des,
                   svymean, 
                   na.rm = T)

# Confidence in the president

pconf_time<-svyby(~ pres_conf_dic, 
                  ~ year, 
                  design = lapop_des,
                  svymean, 
                  na.rm = T)

# Interest in politics 

intpol_year<-svyby(~ pol_int, 
                   ~ year, 
                   design = lapop_des,
                   svymean, 
                   na.rm = T)

# Interest in politics (dichotomized)

intpol_dic_time<-svyby(~ pint_dic,
                       ~ year,
                       design = lapop_des,
                       svymean,
                       na.rm = T)

# Leftists

leftist_time <-svyby(~ leftist, 
                    ~ year, 
                    design = lapop_des,
                    svymean, 
                    na.rm = T)

# Rightists

rightist_time<-svyby(~ rightist, 
                     ~ year, 
                     design = lapop_des,
                     svymean, 
                     na.rm = T)

# Internal political efficiency

eff2_time<-svyby(~ knowspol, 
                 ~ year, 
                 design = lapop_des,
                 svymean, 
                 na.rm = T)

# ================================================= Cross-variable tabulations ================================================

# I cross-tabulate based on groups in this section.

# Corruption tolerance ---------------------------------------------------------------------------------------------------

# By unemployment status in 2014

ctol_byunem14<-svyby(~ ctol, 
                     ~ unem2_4a, 
                     design = lapop_des14,
                     svymean, 
                     na.rm = T)

# By unemployment status in 2016

ctol_byunem16<-svyby(~ ctol, 
                     ~ unem2_4a, 
                     design = lapop_des16,
                     svymean, 
                     na.rm = T)

# By confidence in the president in 2014

ctol_bypconf14<-svyby(~ ctol, 
                      ~ pres_conf_dic, 
                      design = lapop_des14,
                      svymean, 
                      na.rm = T)

# By confidence in the president in 2016

ctol_bypconf16<-svyby(~ ctol, 
                      ~ pres_conf_dic, 
                      design = lapop_des16,
                      svymean, 
                      na.rm = T)

# By job approval rating in 2014

ctol_byaprov14<-svyby(~ ctol, 
                      ~ pres_aprov_dic, 
                      design = lapop_des14,
                      svymean, 
                      na.rm = T)

# By job approval rating in 2016

ctol_byaprov16<-svyby(~ ctol, 
                      ~ pres_aprov_dic, 
                      design = lapop_des16,
                      svymean, 
                      na.rm = T)

# By political groups 2014

ctol_bypls14<-svyby(~ ctol, 
                    ~ pol_group, 
                    design = lapop_des14,
                    svymean, 
                    na.rm = T)

# By political groups 2016

ctol_bypls16<-svyby(~ ctol, 
                    ~ pol_group, 
                    design = lapop_des16,
                    svymean, 
                    na.rm = T)

