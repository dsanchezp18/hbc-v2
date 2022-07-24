# ==================================== LAPOP ECUADOR DATA ANALYSIS ========================================================

# Daniel Sánchez
# USFQ Economics Undergraduate Thesis
# June 2022
# Script for the running of modified regressions for the analysis. 

# ================================================= Preliminaries =============================================================

# No working directory specification needed since an Rproject is being used

# Load libraries --------------------------------------------------------------------------------------------------------------

library(tidyverse) # Ggplot and dplyr
library(survey) # Survey stuff
library(margins) # Average Partial Effects
library(marginaleffects) # APEs

# ================================================= Modified Regressions  =====================================================

# Logit ------------------------------------------------------------------------------------------------------------------

# Unemployment

log_unem_m<-svyglm(ctol~ year*(unem2_4a) + age + gndr + ed + ur + eff1 + eff2 + prot3 + pint_dic + corrper + corr_exp +
                   polscore + pres_aprov,
                   design = lapop_des46,
                   family = quasibinomial(link = 'logit'))

# Job Approval Rating

log_aprov_m<-svyglm(ctol~ year*(pres_aprov) + age + gndr + ed + ur + eff1 + eff2 + prot3 + pint_dic + corrper + corr_exp +
                      unem2_4a + polscore,
                    design = lapop_des46,
                    family = quasibinomial(link = 'logit'))

# Political Score

log_pols_m<-svyglm(ctol~ year*(polscore) + age + gndr + ed + ur + eff1 + eff2 + prot3 + pint_dic + corrper + corr_exp + 
                     unem2_4a + pres_aprov,
                   design = lapop_des46,
                   family = quasibinomial(link = 'logit'))

# List all models in a table

modified_models_log<- list(log_unem_m,
                           log_aprov_m,
                           log_pols_m)

# Probit -----------------------------------------------------------------------------------------------------------------

# Unemployment

prob_unem_m<-svyglm(ctol~ year*(unem2_4a) + age + gndr + ed + ur + eff1 + eff2 + prot3 + pint_dic + corrper + corr_exp +
                    polscore + pres_aprov,
                    design = lapop_des46,
                    family = quasibinomial(link = 'probit'))

# Job Approval Rating

prob_aprov_m<-svyglm(ctol~ year*(pres_aprov) + age + gndr + ed + ur + eff1 + eff2 + prot3 + pint_dic + corrper + corr_exp +
                     unem2_4a + polscore,
                     design = lapop_des46,
                     family = quasibinomial(link = 'probit'))

# Political Score

prob_pols_m<-svyglm(ctol~ year*(polscore) + age + gndr + ed + ur + eff1 + eff2 + prot3 + pint_dic + corrper + corr_exp + 
                    unem2_4a + pres_aprov,
                    design = lapop_des46,
                    family = quasibinomial(link = 'probit'))

# List all models in a table

modified_models_prob<- list(prob_unem_m,
                            prob_aprov_m,
                            prob_pols_m)

# LPM --------------------------------------------------------------------------------------------------------------------

# Unemployment

lpm_unem_m<-svyglm(ctol~ year*(unem2_4a) + age + gndr + ed + ur + eff1 + eff2 + prot3 + pint_dic + corrper + corr_exp +
                   polscore + pres_aprov,
                   design = lapop_des46)

# Job Approval Rating

lpm_aprov_m<-svyglm(ctol~ year*(pres_aprov) + age + gndr + ed + ur + eff1 + eff2 + prot3 + pint_dic + corrper + corr_exp +
                    unem2_4a + polscore,
                    design = lapop_des46)

# Political Score

lpm_pols_m<-svyglm(ctol~ year*(polscore) + age + gndr + ed + ur + eff1 + eff2 + prot3 + pint_dic + corrper + corr_exp + 
                   unem2_4a + pres_aprov,
                   design = lapop_des46)

# List all models in a table

modified_models_lpm<- list(lpm_unem_m,
                           lpm_aprov_m,
                           lpm_pols_m)

# ================================================= Baseline APEs =====================================================

# Logit ------------------------------------------------------------------------------------------------------------------

modified_logit_mfx<-lapply(modified_models_log, marginaleffects)

modified_probit_mfx<-lapply(modified_models_prob, marginaleffects)


