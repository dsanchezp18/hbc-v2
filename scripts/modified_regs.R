# ==================================== LAPOP ECUADOR DATA MANIPULATION ========================================================

# Daniel Sánchez
# USFQ Economics Undergraduate Thesis
# June 2022
# Script for the running of baseline regressions for the analysis. 

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
