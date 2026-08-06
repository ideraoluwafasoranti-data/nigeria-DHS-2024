# ==============================================================================
# Project: Maternal Autonomy, Dietary Diversity, and Child Wasting and 
#          Stunting in Nigeria
# Data:    2024 Nigeria Demographic and Health Survey (NDHS)
# Author:  Ideraoluwa Fasoranti
# Date:    2026
# Description: Survey-weighted analysis of associations between maternal 
#       autonomy, minimum dietary diversity, wasting and stunting among children
#       aged 6-23 months in Nigeria's six geopolitical zones
# See README.md for full project description and reproduction instructions
# ==============================================================================

# ==============================================================================
# LOAD LIBRARIES
# ==============================================================================
# All packages needed for this analysis are loaded here at the start so anyone 
# reproducing the analysis can see all dependencies in one place and install 
# anything missing before running the script.
#
# To install any missing package run:
# install.packages("package_name")
# The survey package is very important, without it the regression estimates will
# not be nationally representative.
# ==============================================================================

library(tidyverse)
library(haven)
library(janitor)
library(survey)
library(ggplot2)
library(gtsummary)

# ==============================================================================
# NOTE ON WORKING DIRECTORY
# ==============================================================================
# This script assumes you are working within the nigeria-dhs-2024 R Project. 
# If you are not using the R Project, set your working directory manually to the
# nigeria-dhs-2024 folder before running this script:
# setwd("path/to/nigeria-dhs-2024")
# ==============================================================================

# ==============================================================================
# STEP 1: LOAD AND MERGE KR AND IR FILES
# ==============================================================================
# I am using two files from the 2024 Nigeria DHS:
# - KR file (Kids Recode): contains child level data including anthropometry and 
#   IYCF feeding variables
# - IR file (Individual Recode): contains woman level data including 
#   decision-making autonomy and background
# - The two files are linked using caseid. This is a unique
#   identifier shared by each mother and her children.
# ==============================================================================

# ------------------------------------------------------------------------------
# 1.1 Load raw Stata files
# ------------------------------------------------------------------------------
kr_raw <- read_dta("data/NGKR8BFL.DTA")
ir_raw <- read_dta("data/NGIR8BFL.DTA")

# ------------------------------------------------------------------------------
# 1.2 Select variables and clean names
# ------------------------------------------------------------------------------
kr <- kr_raw %>%
  clean_names () %>%
  select(
    caseid, v003, 
    v001, v005, v021, v022, 
    hw1, b4, hw70, hw72, v404, v414a, v414b, v414c, v414d, v414e, v414f, v414g,
    v414h, v414i, v414j, v414k, v414l, v414m, v414n, v414o, v414p, v414v,
    v411, v411a, v413a
  )

ir <- ir_raw %>%
  clean_names() %>%
  select(
    caseid,
    v743a, v743b, v743d, v743f,
    v739, v149, v190, v024, v025, v013
  )

# ------------------------------------------------------------------------------
# 1.3 Merge
# ------------------------------------------------------------------------------
merged <- kr %>%
  left_join(ir, by = "caseid")

# ------------------------------------------------------------------------------
# 1.4 Integrity checks
# ------------------------------------------------------------------------------
# A quick check to confirm:
# - No rows were lost during the merge
# - The IR autonomy variables joined successfully
# - About 5% NA on v743a is expected. These are women not currently in union 
#   who were not asked autonomy questions
# Merge success rate approximately 95%. This is expected for DHS data where some 
# women in the IR file do not have corresponding children in the KR file and 
# viceversa.
# nrow(merged) should be approximately 27,783.
# ------------------------------------------------------------------------------
nrow(kr)
nrow(merged)
mean(is.na(merged$v743a))

glimpse(merged)

# ==============================================================================
# STEP 2: CONSTRUCT MINIMUM DIETARY DIVERSITY (MDD)
# ==============================================================================
# # MDD constructed following WHO 2021 updated IYCF indicators, threshold is 
# at least 5 of 8 food groups.
# MDD is a WHO indicator that tells us whether a child ate a wide enough variety 
# of foods in the previous day.
# A child meets MDD if they consumed at least 5 out of 8 defined food groups
# yesterday.
# The 8 food groups are:
# 1. Grains, roots and tubers
# 2. Legumes and nuts
# 3. Dairy products
# 4. Flesh foods (meat, fish, poultry, organ meats)
# 5. Eggs
# 6. Vitamin A rich fruits and vegetables
# 7. Other fruits and vegetables
# 8. Breast milk
#
# The DHS asks about individual food items. I combined the relevant items into 
# each food group manually, following WHO/UNICEF guidelines.
#
# Values of 8 (don't know) are treated as not consumed because they fail 
# the == 1 check. Missing/NA values are handled by the missing = 0 argument 
# in if_else. This follows standard DHS practice for MDD construction.
# ==============================================================================

merged <- merged %>%
  mutate(
    # -------------------- 8 food groups ---------------------------
    fg1_grains    = if_else(v414e  == 1 | v414f == 1, 1, 0, missing = 0),
    fg2_legumes   = if_else(v414c  == 1 | v414o == 1, 1, 0, missing = 0),
    fg3_dairy     = if_else(v411   == 1 | v411a == 1 | v413a == 1 |
                            v414p  == 1 | v414v == 1, 1, 0, missing = 0),
    fg4_flesh     = if_else(v414h  == 1 | v414m == 1 | v414n == 1 |
                            v414b  == 1 | v414d == 1, 1, 0, missing = 0),
    fg5_eggs      = if_else(v414g  == 1, 1, 0, missing = 0),
    fg6_vita      = if_else(v414i  == 1 | v414j == 1 | v414k == 1, 1, 0, missing = 0),
    fg7_otherfv   = if_else(v414a  == 1 | v414l == 1, 1, 0, missing = 0),
    fg8_breastmilk = if_else(v404  == 1, 1, 0, missing = 0),
    
    # ----------------- Total food groups consumed ---------------------
    food_group_count = fg1_grains + fg2_legumes + fg3_dairy + fg4_flesh +
      fg5_eggs + fg6_vita + fg7_otherfv + fg8_breastmilk,
    
    # ------------ MDD: 1 if >=5 food groups ---------------------
    mdd = if_else(food_group_count >= 5, 1, 0)
  )

# ---------------------- Check ---------------------------
table(merged$mdd, useNA = "always")
mean(merged$mdd, na.rm  = TRUE)

# Note: food_group_count is the continuous version of dietary diversity, the 
# total number of food groups consumed. It was constructed here but not included
# in the regression models. Binary MDD was used instead for consistency with 
# WHO/UNICEF reporting standards. Future analyses should examine whether the 
# continuous score produces stronger associations than the binary indicator. 
# This is acknowledged in the limitations.

# ------------------------------------------------------------------------------
# 2.2 Filter to analytical sample - children 6 - 23 months
# ------------------------------------------------------------------------------
df <- merged %>%
  filter(hw1 >= 6 & hw1 <= 23) 

#Confirm sample size
nrow(df)

# Confirm MDD in analytical sample
mean(df$mdd, na.rm = TRUE)

# Note: MDD estimate here is slightly higher than the NDHS final report figure. 
# This is because the DHS report restricted the sample to "youngest children age
# 6 - 23 months living with their mother" while this sample includes 
# all children age 6 - 23 months in the KR file.
# This minor difference is expected and does not affect the validity of 
# this analysis.

# ==============================================================================
# STEP 3: CONSTRUCT WASTING VARIABLE (PRIMARY OUTCOME)
# ==============================================================================
# Wasting is measured using the weight-for-height z-score (WHZ). A child is 
# classified as wasted if the child's WHZ falls below -2 standard deviations 
# from the WHO reference median.
#
# Things to note on DHS coding:
# The DHS stores z-scores multiplied by 100 to avoid decimal places. This means 
# a z-score of -2.0 SD is stored as -200 in the dataset. 
# As such, The wasting cutoff is HW72 < -200, not HW72 < -2
#
# Flagged and implausible values (codes >= 9996) are set to NA and excluded from 
# the analysis. These include:
# - 9996: height out of plausible limits
# - 9997: age in days out of plausible limits
# - 9998: flagged cases
# - 9999: missing
#
# I also used as.numeric() first to strip the haven labelled format from the 
# Stata file before applying the cleaning logic.
# ==============================================================================
df <- df%>%
  mutate(
    # Set flagged values to NA
    hw72_clean = if_else(hw72 >= 9996, NA_real_, as.numeric(hw72)),
    
    # Wasting: 1 if WHZ < -2 SD, 0 if >= -2 SD, NA if missing
    wasting = case_when(
      hw72_clean <  -200  ~ 1,
      hw72_clean >= -200  ~ 0,
      TRUE                ~ NA_real_
    )
  )
# -------------------- Check ---------------------------------------
table(df$wasting, useNA = "always")
mean(df$wasting, na.rm  = TRUE)

# Note: Wasting prevalence among children 6 - 23 months is higher than the 
# national under-5 figure reported in the 2024 NDHS. The report confirms wasting 
# is most prevalent in 6 - 11 month age group. The restricted age sample in this 
# analysis captures this high risk window, so a higher estimate is consistent
# with NDHS findings.

# ==============================================================================
# STEP 3B: CONSTRUCT STUNTING VARIABLE (SECONDARY OUTCOME)
# ==============================================================================
# Stunting is measured using the height-for-age z-score (HAZ). A child is 
# classified as stunted if the child's HAZ falls below -2 standard deviations 
# from the WHO reference median.
#
# The same coding logic applies as for wasting:
# - HAZ is stored multiplied by 100 in the DHS
# - Stunting cutoff is HW70 < -200
# - Flagged values (>= 9996) are set to NA
# - as.numeric() used first to strip haven labels
#
# Stunting indicates chronic undernutrition accumulated over time. It is a 
# secondary outcome in this analysis since IYCF practices are more immediately
# linked to acute malnutrition (wasting).
# Including stunting also allows me to compare whether the autonomy-diet pathway 
# operates differently for acute vs chronic malnutrition.
# ==============================================================================

df <- df%>%
  mutate(
    # First strip haven labels then clean
    hw70_num     = as.numeric(hw70),
    hw70_clean   = if_else(hw70 >= 9996, NA_real_, as.numeric(hw70)),
    stunting     = case_when(
      hw70_clean <  -200 ~ 1,
      hw70_clean >= -200 ~ 0,
      TRUE               ~ NA_real_
    )
  )

# Check
summary(df$hw70_clean)
table(df$stunting, useNA = "always")
mean(df$stunting, na.rm  = TRUE)

# Note: Stunting prevalence among children 6 - 23 months is slightly lower than  
# the national under-5 figure reported in the 2024 NDHS. The report confirms  
# stunting accumulates with age and peaks at 36 - 47 months. This sample of
# children aged 6-23 months captures children before stunting fully accumulates, 
# so a lower estimate is consistent with published final report.

# ==============================================================================
# STEP 4: CONSTRUCT MATERNAL AUTONOMY COMPOSITE SCORE
# =========================================================
# I constructed a composite autonomy score from four DHS decision-making 
# variables asking who usually makes decisions in the following areas:
# - V743A: respondent's own healthcare
# - V743B: large household purchases
# - V743D: visits to family or relatives
# - V743F: what to do with husband's earnings
#
# Each item was scored as follows:
# - Woman decides alone = 1 (full autonomy)
# - Joint decision = 0.5 (partial autonomy)
# - Husband or someone else decides = 0 (no autonomy)
#
# The composite score is the mean of the four items, ranging from 0 to 1. 
# Women were then categorised as:
# - Low autonomy: score < 0.33
# - Medium autonomy: score 0.33 to 0.67
# - High autonomy: score >= 0.67
#
# Two variables were excluded from the composite:
#
# V739 (who decides how to spend respondent's own money) was excluded because it 
# was only asked of women with personal earnings, resulting in 45% missing data.
# Including it would have biased the score toward employed women only. 
# This was confirmed by checking the Nigeria 2024 DHS questionnaire which shows 
# the question is conditional on the woman having earnings.
#
# V743E (who decides what food is cooked daily) was not administered in the 2024 
# Nigeria DHS. It was marked NA in the MAP file. This was the most relevant
# variable for my research question and its absence is acknowledged as 
# a key limitation.
# ==============================================================================

df <-  df %>%
  mutate(
    # Recode each autonomy variable:
    # 1   = woman decides alone (highest autonomy)
    # 0.5 = joint decision (partial autonomy)
    # 0   = husband/someone else decides (no autonomy)
    aut_a = case_when(
      v743a == 1 ~ 1,
      v743a %in% c(2, 3)    ~ 0.5,
      v743a %in% c(4, 5, 6) ~ 0,
      TRUE ~ NA_real_
    ),
    aut_b = case_when(
      v743b == 1 ~ 1,
      v743b %in% c(2, 3)    ~ 0.5,
      v743b %in% c(4, 5, 6) ~ 0,
      TRUE ~ NA_real_
    ),
    aut_d = case_when(
      v743d == 1 ~ 1,
      v743d %in% c(2, 3)    ~ 0.5,
      v743d %in% c(4, 5, 6) ~ 0,
      TRUE ~ NA_real_
    ),
    aut_f = case_when(
      v743f == 1 ~ 1,
      v743f %in% c(2, 3)    ~ 0.5,
      v743f %in% c(4, 5, 6) ~ 0,
      TRUE ~ NA_real_
    ),
    
    # ------------Composite score: average of 5 autonomy items ----------------
    # Range: 0 (no autonomy) to 1 (full autonomy)
    autonomy_score = rowMeans(
      cbind(aut_a, aut_b, aut_d, aut_f), 
      na.rm = TRUE
    ),
    
    # ------------ Categorise into low/medium/high autonomy ----------------
    autonomy_cat = case_when(
      autonomy_score < 0.33 ~ "Low",
      autonomy_score < 0.67 ~ "Medium",
      autonomy_score >= 0.67 ~ "High",
      TRUE ~ NA_character_
    )
  )
# ------------------- Check -----------------------------
summary(df$autonomy_score)
table(df$autonomy_cat, useNA = "always")

# Note: The Low/Medium/High autonomy category thresholds (0.33 and 0.67) divide 
# the 0-1 scale into approximate thirds. These are analytical categories created 
# fordescriptive purposes. The continuous autonomy score is used in regression 
# models to preserve statistical power and avoid information loss 
# from categorisation.

# Individual autonomy variable distributions were compared against Table 15.8 of 
# the 2024 NDHS final report for v743a, v743b and v743d. Results were
# consistent with published figures. Minor differences show this sample of 
# mothers of children aged 6-23 months who tend to be younger and have
# slightly less autonomy than all married women.
# v743f could not be validated as it was not published in the report. Its 
# validity is inferred from the accuracy of the three validated variables.
 
# ==============================================================================
# STEP 5: DESCRIPTIVE STATISTICS BY ZONE
# ==============================================================================
# Before running any regression models I wanted to understand how the key 
# variables vary across Nigeria's six geopolitical zones. Survey weights are 
# applied here so the estimates are nationally representative.
#
# Zones are coded as:
# 1 = North West
# 2 = North East
# 3 = North Central
# 4 = South East
# 5 = South South
# 6 = South West
# ==============================================================================

# ------------------------------------------------------------------------------
# 5.1 Define survey design object
# ------------------------------------------------------------------------------
# Before computing any weighted estimates I need to tell R about the complex 
# sampling structure of the DHS. This ensures all estimates properly account for
# clustering, stratification and probability weights. 
# DHS uses complex sampling, must account for:
# v021 = clustering - primary sampling unit (PSU)
# v022 = strata
# v005 = probability weights (divide by 1,000,000 per DHS convention)
# ------------------------------------------------------------------------------

dhs_design <- svydesign(
  id       = ~v021,
  strata   = ~v022,
  weights  = ~I(v005 / 1000000),
  data     = df,
  nest     = TRUE
)

# ------------------------------------------------------------------------------
# 5.2 Wasting prevalence by zone
# ------------------------------------------------------------------------------
# Weighted wasting prevalence for each of the six geopolitical zones. This gives 
# a picture of where acute malnutrition is most concentrated before running any 
# regression models.
# ------------------------------------------------------------------------------
svyby(~wasting, ~v024, dhs_design, svymean, na.rm = TRUE)

# ------------------------------------------------------------------------------
# 5.3 MDD prevalence by zone 
# ------------------------------------------------------------------------------
# Weighted MDD prevalence by zone. Comparing this to the wasting map helps
# see whether zones with lower dietary diversity also have higher wasting, which
# would support the research hypothesis.
# ------------------------------------------------------------------------------
svyby(~mdd, ~v024, dhs_design, svymean, na.rm = TRUE)

# ------------------------------------------------------------------------------
# 5.4 Mean autonomy score by zone
# ------------------------------------------------------------------------------
# Weighted mean autonomy score by zone. This shows how maternal decision-making 
# power varies across Nigeria's geopolitical zones before formally
# testing its relationship with diet and malnutrition.
# ------------------------------------------------------------------------------
svyby(~autonomy_score, ~v024, dhs_design, svymean, na.rm = TRUE)

# ------------------------------------------------------------------------------
# 5.5 Stunting prevalence by zone
# ------------------------------------------------------------------------------
# Added to complement the wasting estimates. Stunting shows a clearer 
# North-South gradient than wasting, which is consistent with the cumulative 
# nature of chronic undernutrition and known regional disparities in Nigeria.
# ------------------------------------------------------------------------------
svyby(~stunting, ~v024, dhs_design, svymean, na.rm = TRUE)

# ------------------------------------------------------------------------------
# 5.6 Descriptive statistics table
# ------------------------------------------------------------------------------
# A summary table showing key sample characteristics by zone. This gives readers
# a clear picture of the analytical sample before interpreting any regression
# results.
# ------------------------------------------------------------------------------

# Prepare labelled data for table
df_table <- df %>%
  mutate(
    zone = case_when(
      v024 == 1 ~ "North West",
      v024 == 2 ~ "North East",
      v024 == 3 ~ "North Central",
      v024 == 4 ~ "South East",
      v024 == 5 ~ "South South",
      v024 == 6 ~ "South West"
    ),
    wasting_label  = factor(wasting, 
                            levels = c(0,1), 
                            labels = c("No","Yes")),
    stunting_label = factor(stunting, 
                            levels = c(0,1), 
                            labels = c("No","Yes")),
    mdd_label      = factor(mdd, 
                            levels = c(0,1), 
                            labels = c("No","Yes")),
    autonomy_cat   = factor(autonomy_cat,
                            levels = c("Low","Medium","High")),
    sex            = factor(b4,
                            levels = c(1,2),
                            labels = c("Male","Female")),
    residence      = factor(v025,
                            levels = c(1,2),
                            labels = c("Urban","Rural")),
    education      = factor(v149,
                            levels = c(0,1,2,3,4,5),
                            labels = c("No education",
                                       "Incomplete primary",
                                       "Complete primary",
                                       "Incomplete secondary",
                                       "Complete secondary",
                                       "Higher")),
    wealth         = factor(v190,
                            levels = c(1,2,3,4,5),
                            labels = c("Poorest","Poorer",
                                       "Middle","Richer",
                                       "Richest"))
  )

# Build table
table1 <- df_table %>%
  select(zone, wasting_label, stunting_label, mdd_label,
         autonomy_cat, autonomy_score, hw1, sex,
         residence, education, wealth) %>%
  tbl_summary(
    by = zone,
    label = list(
      wasting_label  ~ "Wasting",
      stunting_label ~ "Stunting",
      mdd_label      ~ "Met MDD",
      autonomy_cat   ~ "Autonomy category",
      autonomy_score ~ "Autonomy score (mean)",
      hw1            ~ "Child age in months",
      sex            ~ "Child sex",
      residence      ~ "Residence",
      education      ~ "Maternal education",
      wealth         ~ "Wealth index"
    ),
    statistic = list(
      all_continuous()  ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    missing = "no"
  ) %>%
  add_overall() %>%
  modify_caption("**Table 1. Sample characteristics by 
                 geopolitical zone, Nigeria 2024 DHS**") %>%
  bold_labels()

# Print table
table1

# Save table as HTML for GitHub
table1 %>%
  as_gt() %>%
  gt::gtsave("outputs/table1_descriptive.html")

# Important findings: 
# 1. Autonomy and MDD follow the same North-South gradient, both
#    lowest in the North West/North East  and highest in South South/South West
# 2. South West has highest MDD and lowest wasting. This is consistent with the 
#    autonomy diet wasting pathway
# 3. South South has highest autonomy but also highest wasting, this suggests 
#    other factors beyond diet quality in the zone.This is discussed further 
#    in the README.
# 4. Stunting prevalence zone patterns are consistent with national 
#    distributions, with Northern zones showing higher stunting burden than 
#    Southern zones.
# 5. Autonomy is low in this sample, which is consistent with Nigeria's 
#    patriarchal household structure and findings from the 2024 NDHS women's
#    empowerment module.
# 6. Stunting prevalence in this 6-23 month sample is lower than the national 
#    under-5 figure (40%) reported in the 2024 NDHS. This is expected because 
#    the report confirms stunting increases with age, peaking at 36-47 months. 

# ==============================================================================
# ANALYTICAL APPROACH FOR REGRESSION MODELS
# ==============================================================================
# I estimated three survey-weighted logistic regression models to examine the 
# research questions. All models use the svyglm function from the survey package
# with quasibinomial family to account for the DHS complex sampling design.
#
# The three models are:
#
# Model 1 (Step 6): Does maternal autonomy predict dietary diversity?
# Outcome: MDD
# This model tests whether women with more household decision-making power are 
# more likely to feed their children a diverse diet.
#
# Model 2 (Step 7): Do autonomy and dietary diversity predict wasting?
# Outcome: Wasting (primary outcome)
# This model tests whether the autonomy-diet pathway results in better 
# acute nutritional outcomes. Wasting is the primary outcome of this analysis
# because it shows recent feeding adequacy and is most directly linked to IYCF
# practices in the 6-23 month window.
#
# Model 3 (Step 7): Do autonomy and dietary diversity predict stunting?
# Outcome: Stunting (secondary)
# This model tests the same pathway for chronic malnutrition to see whether the 
# relationship is different for acute vs chronic undernutrition.
#
# These three models follow:
# autonomy --> dietary diversity --> child malnutrition
# ==============================================================================

# ==============================================================================
# STEP 6: SURVEY-WEIGHTED LOGISTIC REGRESSION - MODEL 1
# Does maternal autonomy predict dietary diversity?
# Outcome: MDD (binary - met/not met)
# Main predictor: autonomy composite score
# Covariates: zone, maternal education, wealth index, urban/rural residence, 
#             maternal age group
# ==============================================================================

# ------------------------------------------------------------------------------
# 6.1 Convert haven labelled variables to factors
# ------------------------------------------------------------------------------
# Before running regression models, categorical variables need to be converted 
# to factors so R treats them correctly in the regression model. This produces 
# one coefficient per category level rather than treating the variable 
# as a continuous number.
#
# Direct conversion using as.factor() or as_factor() from haven failed because 
# variables retained their Stata dbl+lbl format. The fix was to use as.integer()
# first to strip the haven attributes completely, then as.factor() to convert 
# to a proper R factor.
# ------------------------------------------------------------------------------

df$v024     <-  as.factor(as.integer(df$v024))
df$v149     <-  as.factor(as.integer(df$v149))
df$v190     <-  as.factor(as.integer(df$v190))     
df$v025     <-  as.factor(as.integer(df$v025))     
df$v013     <-  as.factor(as.integer(df$v013))   
df$b4       <-  as.factor(as.integer(df$b4))
df$mdd      <-  as.factor(as.integer(df$mdd))
df$stunting <-  as.factor(as.integer(df$stunting))
df$wasting  <-  as.factor(as.integer(df$wasting))

# Verify conversion
class(df$v024)
levels(df$v024)
class(df$stunting)
levels(df$stunting)

# ------------------------------------------------------------------------------
# 6.2 Update survey design with corrected factors
# ------------------------------------------------------------------------------
# The survey design object must be rebuilt after factor conversion so it uses 
# the updated factor variables. This makes sure all regression estimates are 
# properly weighted for the DHS complex sampling design.
# DHS uses complex sampling design with:
# - clustering (PSU = v021)
# - Stratification (v022)
# - Probability weights (v005 divided by 1,000,000)
# All estimates must account for this design to be nationally representative
# ------------------------------------------------------------------------------

dhs_design <- svydesign(
  id       = ~v021,
  strata   = ~v022,
  weights  = ~I(v005 / 1000000),
  data     = df,
  nest     = TRUE
)

# ------------------------------------------------------------------------------
# 6.3 Run Model 1 - Autonomy predicts MDD
# ------------------------------------------------------------------------------
# quasibinomial family is used instead of binomial because it handles survey 
# weighted data better and accounts for overdispersion without requiring integer
# counts. This is standard practice for survey weighted logistic 
# regression in DHS analyses.
# ------------------------------------------------------------------------------

model1 <- svyglm(
  mdd ~ autonomy_score + v024 + v149 +v190 + v025 + v013,
  design = dhs_design,
  family = quasibinomial(link ="logit")
)

summary(model1)

# Model 1: Autonomy does not significantly predict MDD after controlling for 
# covariates. Household wealth is the strongest predictor of dietary 
# diversity. This suggests that even when mothers have decision-making power, 
# financial resources are the constraint on what they can actually feed their 
# children. 
# North East zone had significantly lower MDD than North West even after 
# controlling for autonomy and wealth.

# ==============================================================================
# STEP 7: SURVEY-WEIGHTED LOGISTIC REGRESSION
# ==============================================================================
# Models 2 and 3
# Does autonomy combined with dietary diversity predict child 
# malnutrition outcomes?
#
# Model 2 outcome: Wasting (primary outcome)
# Model 3 outcome: Stunting (secondary outcome)
#
# Main predictors: autonomy composite score, MDD
# Covariates: zone, maternal education, wealth index, urban/rural residence,
#             maternal age group, sex of child, child's age in months
# 
# Sex of child and child's age are added as additional covariates in these 
# models because wasting and stunting risk vary by age and sex within the
# 6-23 month window. This is standard controls in child nutrition 
# regression models. 
# ==============================================================================

# ------------------------------------------------------------------------------
# 7.1 Convert wasting to factor
# ------------------------------------------------------------------------------
# Wasting needs to be converted to factor separately here because it was 
# constructed as numeric in Step 3 and needs to be a binary factor for the 
# logistic regression model.
# ------------------------------------------------------------------------------
df$wasting <- as.factor(as.integer(df$wasting))

# Verify
class(df$wasting)
levels(df$wasting)

# ------------------------------------------------------------------------------
# 7.2 Update survey design
# ------------------------------------------------------------------------------
# Survey design object rebuilt to include wasting as a factor variable alongside 
# the other covariates already converted in Step 6.
# ------------------------------------------------------------------------------
dhs_design <- svydesign(
  id       = ~v021,
  strata   = ~v022,
  weights  = ~I(v005 / 1000000),
  data     = df,
  nest     = TRUE
)

# ------------------------------------------------------------------------------
# 7.3 Run Model 2 - Autonomy + MDD predicts Wasting
# ------------------------------------------------------------------------------
model2 <-  svyglm(
  wasting ~ autonomy_score + mdd + v024 + v149 + v190 + v025 + v013 + b4 + hw1,
  design = dhs_design,
  family = quasibinomial(link = "logit")
)

summary(model2)

# Model 2: Neither Autonomy nor MDD significantly predict MDD after controlling 
# for covariates. Significant predictors: maternal education (protective),
# South South zone (higher risk), female sex (lower risk), child's age
# (older = lower risk within 6 - 23 months).
# The null finding for autonomy and MDD may be because wasting is driven by acute
# illness and infection which this analysis did not capture. 
# This is acknowledged as a limitation.

# ------------------------------------------------------------------------------
# 7.4 Run Model 3 - Autonomy + MDD predicts Stunting
# ------------------------------------------------------------------------------
model3 <- svyglm(
  stunting ~ autonomy_score + mdd + v024 + v149 + v190 + v025 + v013 + b4 + hw1,
  design = dhs_design,
  family = quasibinomial(link = "logit")
)

summary(model3)

# Model 3: Autonomy and MDD do not significantly predict stunting.
# Key protective factors: maternal education, older maternal age, higher wealth, 
# South zone residence, female sex.
# Child age positively predicts stunting, this is consistent with cumulative 
# nature of chronic undernutrition.
# The counterintuitive positive direction of the autonomy coefficient may be as 
# a result of structural confounding. This is discussed further in the README 
# under the South South paradox section.

# ==============================================================================
# STEP 8: VISUALIZATION
# ==============================================================================
# Wasting and Stunting prevalence by MDD status and Zone
# I created five charts to tell the visual story of this analysis. The charts 
# are saved as PNG files in the outputs folder at 300 DPI for good print quality.
#
# The charts follow this narrative order:
# 1. Where is autonomy lowest? (autonomy by zone)
# 2. Does low autonomy mean poor diet? (MDD by autonomy)
# 3. Does poor diet mean more malnutrition?
#    (wasting and stunting by autonomy)
# 4. Does MDD status change wasting by zone?
#    (wasting by MDD and zone)
# 5. Does MDD status change stunting by zone?
#    (stunting by MDD and zone)
# ==============================================================================

 library(ggplot2)
 
# Create outputs folder if it does not already exist
 dir.create("outputs", showWarnings = FALSE)

# ------------------------------------------------------------------------------
# 8.1 Prepare data for plotting
# ------------------------------------------------------------------------------
# I summarised wasting and stunting prevalence by zone and MDD status from the 
# analytical dataset. These are unweighted prevalence figures used for 
# visualization purposes only. All formal estimates use survey weights
# in Steps 5, 6 and 7.
# ------------------------------------------------------------------------------
plot_data <- df %>%
  mutate(
    zone = case_when(
      v024 == 1 ~ "North West",
      v024 == 2 ~ "North East",
      v024 == 3 ~ "North Central",
      v024 == 4 ~ "South East",
      v024 == 5 ~ "South South",
      v024 == 6 ~ "South West"
    ), 
    mdd_label = case_when(
      mdd == 1 ~ "Met MDD",
      mdd == 0 ~ "Did Not Meet MDD"
    ),
    wasting = as.numeric(as.character(wasting)),
    stunting = as.numeric(as.character(stunting))
  ) %>%
  filter(!is.na(zone) & !is.na(mdd_label)) %>%
  group_by(zone, mdd_label) %>%
  summarise(
    wasting_prev  = mean(wasting, na.rm  = TRUE) * 100,
    stunting_prev = mean(stunting, na.rm = TRUE) * 100,
    .groups = "drop"
  )
nrow(plot_data)
head(plot_data)

# ------------------------------------------------------------------------------
# 8.2 Plot - Wasting prevalence by MDD status and Zone
# ------------------------------------------------------------------------------
# This chart shows whether children who met MDD had lower wasting prevalence 
# than those who did not, in each geopolitical zone.
# ------------------------------------------------------------------------------

library(ggplot2)
ggplot(plot_data, aes(x = zone, y = wasting_prev, fill = mdd_label)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Met MDD" = "#2166ac",
                               "Did Not Meet MDD" = "#d73027")) +
  labs(
    title    = "Wasting Prevalence by Dietary Diversity Status and Zone",
    subtitle = "Children 6-23 months, Nigeria 2024 DHS",
    x        = "Geopolitical Zone",
    y        = "Wasting Prevalence (%)",
    fill     = "MDD Status",
    caption  = "Source: 2024 Nigeria Demographic and Health Survey"
  ) +
  theme_minimal() +
  theme(
    plot.title      = element_text(face  = "bold", size = 11),
    axis.text.x     = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

# Save plot
ggsave("outputs/wasting_by_mdd_zone.png", 
       width = 10, height = 6, dpi = 300)

# ------------------------------------------------------------------------------
# 8.3 Plot - Stunting prevalence by MDD status and Zone
# ------------------------------------------------------------------------------
# Same as 8.2 but for stunting, which is the secondary outcome. Comparing the 
# two charts helps us see whether the MDD-malnutrition relationship differs for
# acute vs chronic undernutrition across zones.
# ------------------------------------------------------------------------------
library(ggplot2)
ggplot(plot_data, aes(x = zone, y = stunting_prev, fill = mdd_label)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Met MDD"          = "#2166ac",
                               "Did Not Meet MDD" = "#d73027")) +
  labs(
    title    = "Stunting Prevalence by Dietary Diversity Status and Zone",
    subtitle = "Children 6-23 months, Nigeria 2024 DHS",
    x        = "Geopolitical Zone",
    y        = "Stunting Prevalence (%)",
    fill     = "MDD Status",
    caption  = "Source: 2024 Nigeria Demographic and Health Survey"
  ) +
  theme_minimal() +
  theme(
    plot.title      = element_text(face = "bold", size = 11),
    axis.text.x     = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

# Save plot
ggsave("outputs/stunting_by_mdd_zone.png", 
       width = 10, height = 6, dpi = 300)

# ------------------------------------------------------------------------------
# 8.4 Plot — Mean Autonomy Score by Zone
# ------------------------------------------------------------------------------
# This chart shows the North-South autonomy gradient clearly. The zones are 
# ordered from lowest to highest autonomy score. The color gradient shows the
# direction of the relationship.
# ------------------------------------------------------------------------------
autonomy_zone <- df %>%
  mutate(
    zone = case_when(
      v024 == 1 ~ "North West",
      v024 == 2 ~ "North East",
      v024 == 3 ~ "North Central",
      v024 == 4 ~ "South East",
      v024 == 5 ~ "South South",
      v024 == 6 ~ "South West"
    )
  ) %>%
  filter(!is.na(zone)) %>%
  group_by(zone) %>%
  summarise(
    mean_autonomy = mean(autonomy_score, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(autonomy_zone, aes(x = reorder(zone, mean_autonomy), y = mean_autonomy, 
                          fill = mean_autonomy)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "#d73027", high = "#2166ac") +
  labs(
    title    = "Mean Maternal Autonomy Score by Geopolitical Zone",
    subtitle = "Children 6-23 months, Nigeria 2024 DHS",
    x        = "Geopolitical Zone",
    y        = "Mean Autonomy Score (0-1)",
    caption  = "Source: 2024 Nigeria Demographic and Health Survey"
  ) +
  theme_minimal() +
  theme(
    plot.title      = element_text(face  = "bold", size = 11),
    axis.text.x     = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

ggsave("outputs/autonomy_by_zone.png",
       width = 10, height = 6, dpi = 300)

# ------------------------------------------------------------------------------
# 8.5 Plot - MDD Prevalence by Autonomy Category
# ------------------------------------------------------------------------------
# This chart shows whether higher autonomy is associated with better dietary 
# diversity descriptively, before the formal regression in Model 1.
# ------------------------------------------------------------------------------
mdd_autonomy <- df %>%
  filter(!is.na(autonomy_cat)) %>%
  mutate(
    mdd_num = as.numeric(as.character(mdd))
  ) %>%
  group_by(autonomy_cat) %>%
  summarise(
    mdd_prev = mean(mdd_num, na.rm = TRUE) * 100,
    .groups  = "drop"
  ) %>%
  mutate(
    autonomy_cat = factor(autonomy_cat, 
                          levels = c("Low", "Medium", "High"))
  )

ggplot(mdd_autonomy, aes(x = autonomy_cat, y = mdd_prev, fill = autonomy_cat)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Low"    = "#d73027",
                               "Medium" = "#fee090",
                               "High"   = "#2166ac")) +
  labs(
    title    = "MDD Prevalence by Maternal Autonomy Category",
    subtitle = "Children 6-23 months, Nigeria 2024 DHS",
    x        = "Autonomy Category",
    y        = "MDD Prevalence (%)",
    caption  = "Source: 2024 Nigeria Demographic and Health Survey"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 11),
    legend.position = "none"
  )

ggsave("outputs/mdd_by_autonomy.png",
       width = 10, height = 6, dpi = 300)

# ------------------------------------------------------------------------------
# 8.6 Plot — Wasting and Stunting by Autonomy Category
# ------------------------------------------------------------------------------
# This chart shows whether higher autonomy is associated with lower malnutrition 
# prevalence descriptively, before the formal regression in Models 2 and 3.
# Both outcomes are shown side by side for easy comparison.
# ------------------------------------------------------------------------------
malnutrition_autonomy <- df %>%
  filter(!is.na(autonomy_cat)) %>%
  mutate(
    wasting_num  = as.numeric(as.character(wasting)),
    stunting_num = as.numeric(as.character(stunting))
  ) %>%
  group_by(autonomy_cat) %>%
  summarise(
    Wasting  = mean(wasting_num, na.rm = TRUE) * 100,
    Stunting = mean(stunting_num, na.rm = TRUE) * 100,
    .groups  = "drop"
  ) %>%
  mutate(
    autonomy_cat = factor(autonomy_cat,
                          levels = c("Low", "Medium", "High"))
  ) %>%
  pivot_longer(cols      = c(Wasting, Stunting),
               names_to  = "outcome",
               values_to = "prevalence")

ggplot(malnutrition_autonomy, aes(x = autonomy_cat, y = prevalence,
                                  fill = outcome)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Wasting"  = "#d73027",
                               "Stunting" = "#2166ac")) +
  labs(
    title    = "Wasting and Stunting Prevalence by Maternal Autonomy Category",
    subtitle = "Children 6-23 months, Nigeria 2024 DHS",
    x        = "Autonomy Category",
    y        = "Prevalence (%)",
    fill     = "Outcome",
    caption  = "Source: 2024 Nigeria Demographic and Health Survey"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 11),
    legend.position = "bottom"
  )

ggsave("outputs/malnutrition_by_autonomy.png",
       width = 10, height = 6, dpi = 300)

# =========================================================
# SESSION INFO
# =========================================================
# R version and package versions used in this analysis are recorded here for 
# reproducibility purposes.
# =========================================================
sessionInfo()
