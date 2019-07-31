pacman::p_load(tidyverse, broom, argparse, AER, magrittr, lubridate, stargazer)

# Import
print("Import data...")
load("../output/clean_bail_data.RData")
clean_df <- filter(constrained, drop_me == FALSE)

##### OLS --------------
print("Running OLS...")
formula <- outcome ~ log_bond_amount + prior_FTAs +prior_Fs + prior_Ms + AGE + GENDER + RACE + offense_class


ols.full <- lm(formula, data=clean_df)
ols.pre <- lm(formula, filter(clean_df, pre_reform == T))
ols.post <- lm(formula, filter(clean_df, pre_reform == F))

stargazer(ols.full, ols.pre, ols.post,
          column.labels = c("Full", "Pre-Reform", "Post-Reform"),
          covariate.labels = c("Log Bond Amount", "Num. Prior Skipped Trial", "Prior Felonies", "Prior Misdemeanors", "Age"),
          dep.var.labels = "Skips Trial",
          title="OLS Estimates", align=TRUE, no.space=T,
          out= "../output/ols.tex",
          keep.stat = c("rsq","adj.rsq"),
          omit = c("offense_class","RACE","GENDER"))

#### IV -------------
print("Running IV...")

iv_df <- filter(clean_df,!is.na(log_leaveout_avg_fac_off_bond))

iv.full <-  ivreg(outcome ~ log_bond_amount + prior_FTAs + prior_Fs + prior_Ms + AGE + GENDER + RACE + offense_class
                  | year_log_leaveout_avg_fac_off_bond + . - log_bond_amount,
                  data=iv_df)
# Prereform
iv.pre <- ivreg(outcome ~ log_bond_amount + prior_FTAs + prior_Fs + prior_Ms + AGE + GENDER + RACE + offense_class
                | year_log_leaveout_avg_fac_off_bond + . - log_bond_amount,
                data=filter(iv_df, pre_reform == T))
# post
iv.post <- ivreg(outcome ~ log_bond_amount + prior_FTAs + prior_Fs + prior_Ms + AGE + GENDER + RACE + offense_class
                 | year_log_leaveout_avg_fac_off_bond + . - log_bond_amount,
                 data=filter(iv_df, pre_reform == F))

stargazer(iv.full, iv.pre, iv.post,
          column.labels = c("Full", "Pre-Reform", "Post-Reform"),
          covariate.labels = c("Log Bond Amount", "Num. Prior Skipped Trial", "Prior Felonies", "Prior Misdemeanors", "Age"),
          title="2SLS Estimates", align=TRUE, no.space=T,
          omit.stat = c("rsq", "adj.rsq"),
          dep.var.labels = "Skips Trial",
          out= "../output/iv1.tex",
          summary = FALSE,
          omit = c("offense_class","RACE","GENDER"),
          omit.labels = c("Offense Class FE?", "Race FE?", "Gender FE?"))


#### IV on Subsamples --------
print("IV for Subsamples...")
iv.postM <- ivreg(outcome ~ log_bond_amount + prior_FTAs + prior_Fs + prior_Ms + AGE + GENDER + RACE + OFFENSE
                  | year_log_leaveout_avg_fac_off_bond + . - log_bond_amount,
                  data=filter(iv_df, is_MV, pre_reform == F))

# Prereform
iv.postF <- ivreg(outcome ~ log_bond_amount + prior_FTAs + prior_Fs + prior_Ms + AGE + GENDER + RACE + OFFENSE
                  | year_log_leaveout_avg_fac_off_bond + . - log_bond_amount,
                  data=filter(iv_df, !is_MV, pre_reform == F))

iv.preM <- ivreg(outcome ~ log_bond_amount + prior_FTAs + prior_Fs + prior_Ms + AGE + GENDER + RACE + OFFENSE
                 | year_log_leaveout_avg_fac_off_bond + . - log_bond_amount,
                 data=filter(iv_df, is_MV, pre_reform == T))

iv.other <- ivreg(outcome ~ log_bond_amount + prior_FTAs + prior_Fs + prior_Ms + AGE + GENDER + RACE + OFFENSE
                  | year_log_leaveout_avg_fac_off_bond + . - log_bond_amount,
                  data=filter(iv_df, !is_MV | pre_reform == T))


stargazer(iv.postM, iv.postF, iv.preM, iv.other,
          column.labels = c("Post-Reform Minor Crime", "Post-Reform Major Crime", "Pre-Reform Minor Crime", "Not Post-Reform Minor Crime"),
          title="Flight Risk Estimates", align=TRUE, no.space=T,
          covariate.labels = c("Log Bond Amount", "Num. Prior Skipped Trial", "Prior Felonies", "Prior Misdemeanors", "Age"),
          omit.stat = c("rsq", "adj.rsq"),
          dep.var.labels = "Skips Trial",
          omit = c("OFFENSE","RACE","GENDER"),
          out="../output/iv2.tex",
          omit.labels = c("Offense FE?", "Race FE?", "Gender FE?"),
          float.env = "sidewaystable")
