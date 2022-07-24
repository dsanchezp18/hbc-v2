# ==================================== LAPOP ECUADOR DATA ANALYSIS ========================================================

# Daniel Sánchez
# USFQ Economics Undergraduate Thesis
# June 2022
# Script for the running of baseline regressions for the analysis. 

# ================================================= Preliminaries =============================================================

# No working directory specification needed since an Rproject is being used

# Load libraries --------------------------------------------------------------------------------------------------------------

library(tidyverse) # Ggplot and dplyr
library(survey) # Survey stuff
library(marginaleffects) # APEs

# ================================================= Baseline Regressions =====================================================

# Logit models -----------------------------------------------------------------------------------------------------------

# Economic Situation

log_ecsit<-svyglm(ctol ~ year*econ_sit,
                  design = lapop_des46,
                  family = quasibinomial(link = 'logit'))

# Unemployment

log_unem<-svyglm(ctol ~ year*unem2_4a,
                 design = lapop_des46,
                 family = quasibinomial(link = 'logit'))

# Confidence in the President

log_pconf<-svyglm(ctol ~ year*pres_conf,
                  design = lapop_des46,
                  family = quasibinomial(link = 'logit'))

# President's Job Approval

log_aprov<-svyglm(ctol ~ year*pres_aprov,
                  design = lapop_des46,
                  family = quasibinomial(link = 'logit'))

# Political wings

log_polscore<-svyglm(ctol ~ year*polscore,
                     design = lapop_des46,
                     family = quasibinomial(link = 'logit'))

# Create the list of models:

base_models_log<-
  list(log_ecsit, 
       log_unem, 
       log_pconf, 
       log_aprov, 
       log_polscore)

# Probit models ----------------------------------------------------------------------------------------------------------

prob_ecsit<-svyglm(ctol ~ year*econ_sit,
                   design = lapop_des46,
                   family = quasibinomial(link = 'probit'))

# Unemployment

prob_unem<-svyglm(ctol ~ year*unem2_4a,
                 design = lapop_des46,
                 family = quasibinomial(link = 'probit'))

# Confidence in the President

prob_pconf<-svyglm(ctol ~ year*pres_conf,
                  design = lapop_des46,
                  family = quasibinomial(link = 'probit'))

# President's Job Approval

prob_aprov<-svyglm(ctol ~ year*pres_aprov,
                  design = lapop_des46,
                  family = quasibinomial(link = 'probit'))

# Political wings

prob_polscore<-svyglm(ctol ~ year*polscore,
                     design = lapop_des46,
                     family = quasibinomial(link = 'probit'))

# Create the list of models

base_models_prob<-
  list(prob_ecsit,
       prob_unem,
       prob_pconf,
       prob_aprov,
       prob_polscore)

# Linear Probability Models ----------------------------------------------------------------------------------------------

lpm_ecsit<-svyglm(ctol ~ year*econ_sit,
                  design = lapop_des46)

# Unemployment
lpm_unem<-svyglm(ctol ~ year*unem2_4a,
                 design = lapop_des46)

# Confidence in the President

lpm_pconf<-svyglm(ctol ~ year*pres_conf,
                  design = lapop_des46)

# President's Job Approval

lpm_aprov<-svyglm(ctol ~ year*pres_aprov,
                  design = lapop_des46)

# Political wings

lpm_polscore<-svyglm(ctol ~ year*polscore,
                     design = lapop_des46)

# Create the list of models

base_models_lpm<-
  list(lpm_ecsit,
       lpm_unem,
       lpm_pconf,
       lpm_aprov,
       lpm_polscore)

# ================================================= Baseline APEs =====================================================

# Logit ------------------------------------------------------------------------------------------------------------------

base_logit_mfx<-lapply(base_models_log, marginaleffects)

# Probit -----------------------------------------------------------------------------------------------------------------

base_probit_mfx<-lapply(base_models_prob, marginaleffects)

