# ==================================== LAPOP ECUADOR DATA ANALYSIS ========================================================

# Daniel Sánchez
# USFQ Economics Undergraduate Thesis
# June 2022
# Script for the running of cross section regressions for the analysis. 

# ================================================= Preliminaries =============================================================

# No working directory specification needed since an Rproject is being used

# Load libraries --------------------------------------------------------------------------------------------------------------

library(tidyverse) # Ggplot and dplyr
library(survey) # Survey stuff
library(margins) # Average Partial Effects
library(marginaleffects) # APEs

# ================================================= Cross-Sectional Regressions  =====================================================

# Logit ------------------------------------------------------------------------------------------------------------------


log_14<-svyglm(ctol~ unem2_4a + age + gndr + ed + ur + eff1 + eff2 + prot3 + pint_dic + corrper + corr_exp + polscore +
                 pres_aprov,
               design = lapop_des14,
               family = quasibinomial(link = 'logit'))

log_16<-svyglm(ctol~ unem2_4a + age + gndr + ed + ur + eff1 + eff2 + prot3 + pint_dic + corrper + corr_exp + polscore +
                 pres_aprov,
               design = lapop_des16,
               family = quasibinomial(link = 'logit'))

# List of logit cross-sectional models

cross_sec_log<-list(log_14,
                    log_16)


# Probit -----------------------------------------------------------------------------------------------------------------

prob_14<-svyglm(ctol~ unem2_4a + age + gndr + ed + ur + eff1 + eff2 + prot3 + pint_dic + corrper + corr_exp + polscore +
                pres_aprov,
                design = lapop_des14,
                family = quasibinomial(link = 'probit'))

prob_16<-svyglm(ctol~ unem2_4a + age + gndr + ed + ur + eff1 + eff2 + prot3 + pint_dic + corrper + corr_exp + polscore +
                pres_aprov,
                design = lapop_des16,
                family = quasibinomial(link = 'probit'))

# List of logit cross-sectional models

cross_sec_prob<-list(prob_14,
                     prob_16)

# LPM -----------------------------------------------------------------------------------------------------------------

lpm_14<-svyglm(ctol~ unem2_4a + age + gndr + ed + ur + eff1 + eff2 + prot3 + pint_dic + corrper + corr_exp + polscore +
                  pres_aprov,
                design = lapop_des14)

lpm_16<-svyglm(ctol~ unem2_4a + age + gndr + ed + ur + eff1 + eff2 + prot3 + pint_dic + corrper + corr_exp + polscore +
                  pres_aprov,
                design = lapop_des16)

# List of logit cross-sectional models

cross_sec_lpm<-list(lpm_14,
                    lpm_16)

# ================================================= Cross-Sectional APEs =============================================================

# Logit ------------------------------------------------------------------------------------------------------------------

cross_sec_logit_mfx<-lapply(cross_sec_log, marginaleffects)
cross_sec_probit_mfx<-lapply(cross_sec_prob, marginaleffects)





