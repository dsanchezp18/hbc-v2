## ---- chunk-tabulations

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

# ================================================= Survey-weighted tabulations ================================================

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

# Interest in politics. 

intpol_year<-svyby(~ pol_int, 
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

