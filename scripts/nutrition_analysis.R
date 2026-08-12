# ==============================================================================
# Project: Maternal Autonomy, Child Dietary Adequacy, and 
#          Malnutrition in Nigeria
# Data:    2024 Nigeria Demographic and Health Survey (NDHS)
# Author:  Ideraoluwa Fasoranti
# Date:    2026
# Description: Survey-weighted analysis of associations between maternal 
#       autonomy, minimum dietary diversity, minimum acceptable diet, wasting 
#       and stunting among children aged 6-23 months in Nigeria's 
#       six geopolitical zones. Includes WASH covariates and examines the 
#       relative contribution of household wealth alongside maternal autonomy.
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
library(webshot2)

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
#   IYCF feeding variables.
# - IR file (Individual Recode): contains woman level data including 
#   decision-making autonomy and background.
# - The two files are linked using caseid.
#
#   col_select loads only the columns needed for this analysis instead of the 
#   full dataset. This reduces memory usage significantly on low RAM computers.
# ==============================================================================

# ------------------------------------------------------------------------------
# 1.1 Load KR file - child level data
# ------------------------------------------------------------------------------
kr_raw <- read_dta("data/NGKR8BFL.DTA",
                   col_select = c(caseid, v003, v001, v005, v021, v022,
                                  hw1, b4, hw70, hw72, v404,
                                  v414a, v414b, v414c, v414d, v414e, v414f,
                                  v414g, v414h, v414i, v414j, v414k, v414l,
                                  v414m, v414n, v414o, v414p, v414v,
                                  v411, v411a, v413a,
                                  v113, v116, v465,
                                  m39, v469e, v469f)) %>%
  clean_names()

# ------------------------------------------------------------------------------
# 1.2 Load IR file - women level data
# ------------------------------------------------------------------------------

ir_raw <- read_dta("data/NGIR8BFL.DTA",
                   col_select = c(caseid, v743a, v743b, v743d, v743f,
                                  v739, v149, v190, v024, v025, v013)) %>%
  clean_names()

# ------------------------------------------------------------------------------
# 1.3 Merge
# ------------------------------------------------------------------------------
merged <- kr_raw %>%
  left_join(ir_raw, by = "caseid")

# ------------------------------------------------------------------------------
# 1.4 Integrity checks
# ------------------------------------------------------------------------------
# - About 5% NA on v743a is expected. These are women not currently in union 
#   who were not asked autonomy questions.
# - nrow(merged) should be approximately 27,783.
# ------------------------------------------------------------------------------
nrow(kr_raw)
nrow(merged)
mean(is.na(merged$v743a))
glimpse(merged) 

# ==============================================================================
# STEP 2: CONSTRUCT DIETARY INDICATORS 
# ==============================================================================
# I constructed three dietary indicators in this step:
# MDD, meal frequency and MAD. MDD and MAD are the primary dietary exposures 
# used in the regression models. Meal frequency is constructed here as a
# building block for MAD. It is not used as a standalone outcome.
#
# All dietary indicators are constructed on df, the filtered 6-23 month sample,
# not on merged. This is important because complementary feeding indicators are 
# only valid for this age range.
# ==============================================================================

# ------------------------------------------------------------------------------
# 2.1 Filter to analytical sample first
# ------------------------------------------------------------------------------
# Filter to sample to children 6-23 months. WHO and UNICEF recommend this age 
# range for all complementary feeding indicators, including MDD and MAD.
# ------------------------------------------------------------------------------

df <- merged %>%
  filter(hw1 >= 6 & hw1 <= 23)

nrow(df)

# ------------------------------------------------------------------------------
# 2.2 Construct MDD - Minimum Dietary Diversity
# ------------------------------------------------------------------------------
# MDD = 1 if child consumed at least 5 of 8 WHO food groups the previous day. 
# Food groups are constructed by combining individual DHS food item variables.
#
# The 8 food groups are:
# 1. Grains, roots and tubers - v414e, v414f
# 2. Legumes and nuts - v414c, v414o
# 3. Dairy - v411, v411a, v413a, v414p, v414v
# 4. Flesh foods - v414h, v414m, v414n, v414b, v414d
# 5. Eggs - v414g
# 6. Vitamin A rich fruits and vegetables - v414i, v414j, v414k
# 7. Other fruits and vegetables - v414a, v414l
# 8. Breast milk - v404
#
# Values of 8 (don't know) are treated as not consumed because they fail 
# the == 1 check. Missing values are handled by the missing = 0 argument in 
# if_else. Constructed following WHO 2021 updated IYCF indicators.
# ------------------------------------------------------------------------------

df <- df %>%
  mutate(
    # -------------------- 8 food groups ---------------------------
    fg1_grains    = if_else(v414e  == 1 | v414f == 1, 1, 0, missing = 0),
    fg2_legumes   = if_else(v414c  == 1 | v414o == 1, 1, 0, missing = 0),
    fg3_dairy     = if_else(v411   == 1 | v411a == 1 | v413a == 1 |
                            v414p  == 1 | v414v == 1, 1, 0, missing = 0),
    fg4_flesh     = if_else(v414h  == 1 | v414m == 1 | v414n == 1 |
                            v414b  == 1 | v414d == 1, 1, 0, missing = 0),
    fg5_eggs      = if_else(v414g  == 1, 1, 0, missing = 0),
    fg6_vita      = if_else(v414i  == 1 | v414j == 1 | 
                             v414k == 1, 1, 0, missing = 0),
    fg7_otherfv   = if_else(v414a  == 1 | v414l == 1, 1, 0, missing = 0),
    fg8_breastmilk = if_else(v404  == 1, 1, 0, missing = 0),
    
    mdd = if_else(fg1_grains + fg2_legumes + fg3_dairy +
                    fg4_flesh + fg5_eggs + fg6_vita +
                    fg7_otherfv + fg8_breastmilk >= 5, 1, 0)
  )

mean(df$mdd, na.rm = TRUE)

# ------------------------------------------------------------------------------
# 2.3 Construct meal frequency and milk feeds from m39, v469e and v469f
#-------------------------------------------------------------------------------
# M39 captures number of times child ate solid semi-solid or soft food the 
# previous day.
# V469E captures times child was given powdered/tinned/fresh milk, this is used 
# for non-breastfed MAD threshold.
# V469F captures times child was given infant formula, this is used for 
# non-breastfed MAD threshold.
# Values of 8 (don't know) and 9 (missing) set to NA.
# ------------------------------------------------------------------------------

df <- df %>%
  mutate(
    meal_freq = case_when(
      as.numeric(m39) == 8 ~ NA_real_,
      as.numeric(m39) == 9 ~ NA_real_,
      TRUE ~ as.numeric(m39)
    ),
    milk_feeds = case_when(
      as.numeric(v469e) == 8 ~ NA_real_,
      as.numeric(v469e) == 9 ~ NA_real_,
      as.numeric(v469f) == 8 ~ NA_real_,
      as.numeric(v469f) == 9 ~ NA_real_,
      TRUE ~ as.numeric(v469e) + as.numeric(v469f)
    )
  )

# Check distributions
table(df$meal_freq, useNA = "always")
table(df$milk_feeds, useNA = "always")

# ------------------------------------------------------------------------------
# 2.4 Construct MAD - Minimum Acceptable Diet
# ------------------------------------------------------------------------------
# Minimum Acceptable Diet combines dietary diversity and meal frequency.
# 
# Breastfed children: MDD >= 5 AND meal frequency >= 2
# Non-breastfed children: MDD >= 5 AND meal frequency >= 3
#
# MAD is a more complete IYCF indicator than MDD alone as it combines both 
# diet quality and feeding frequency.
# WHO recommends MAD as the primary complementary feeding indicator 
# alongside MDD.
# Constructed following WHO 2021 updated IYCF indicators.
# ------------------------------------------------------------------------------

df <- df %>%
  mutate(
    mad = case_when(
      v404 == 1 & mdd == 1 & meal_freq >= 2 ~ 1,
      v404 == 1 & (mdd == 0 | meal_freq < 2) ~ 0,
      v404 == 0 & mdd == 1 & meal_freq >= 3 &
        milk_feeds >= 2 ~ 1,
      v404 == 0 & (mdd == 0 | meal_freq < 3 |
                     milk_feeds < 2) ~ 0,
      TRUE ~ NA_real_
    )
  )

# Check MAD distribution
table(df$mad, useNA = "always")
mean(df$mad, na.rm = TRUE)

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
# the analysis. 
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

# ==============================================================================
# STEP 4: CONSTRUCT MATERNAL AUTONOMY COMPOSITE SCORE
# ==============================================================================
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
# The composite is the mean of the four items
# ranging from 0 (no autonomy) to 1 (full autonomy).
#
# Two variables were excluded:
# - V739: only asked of employed women - 45% missing
# - V743E: not administered in 2024 Nigeria DHS
#
# V743E - who decides what food is cooked daily. This was the most relevant 
# variable for this research question. Its absence is the primary limitation of 
# this analysis and is discussed further in the README.
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
    autonomy_score = rowMeans(
      cbind(aut_a, aut_b, aut_d, aut_f), 
      na.rm = TRUE
    ),
    
    # ------------ Categorise into low/medium/high autonomy ---------------- #
    autonomy_cat = case_when(
      autonomy_score < 0.33 ~ "Low",
      autonomy_score < 0.67 ~ "Medium",
      autonomy_score >= 0.67 ~ "High",
      TRUE ~ NA_character_
    )
  )
# ------------------- Check -----------------------------
summary(df$autonomy_score)
mean(df$autonomy_score, na.rm = TRUE)
table(df$autonomy_cat, useNA = "always")

# ==============================================================================
# STEP 5: DESCRIPTIVE STATISTICS BY ZONE
# ==============================================================================
# Before running any regression models I wanted to understand how the key 
# variables vary across Nigeria's six geopolitical zones. Survey weights are 
# applied here so the estimates are nationally representative.
#
# Zones are coded as:
# 1 = North West            2 = North East                     3 = North Central
# 4 = South East            5 = South South                    6 = South West
# ==============================================================================

# ------------------------------------------------------------------------------
# 5.1 Define survey design object
# ------------------------------------------------------------------------------
# Before computing any weighted estimates I need to tell R the DHS uses a 
# complex sampling design. This ensures all estimates properly account for
# clustering, stratification and probability weights. 
# Probability weights are divide by 1,000,000 per DHS convention.
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

svyby(~wasting, ~v024, dhs_design, svymean, na.rm = TRUE)

# ------------------------------------------------------------------------------
# 5.3 MDD prevalence by zone 
# ------------------------------------------------------------------------------

svyby(~mdd, ~v024, dhs_design, svymean, na.rm = TRUE)

# ------------------------------------------------------------------------------
# 5.4 MAD prevalence by zone
# ------------------------------------------------------------------------------

svyby(~mad, ~v024, dhs_design, svymean, na.rm = TRUE)

# ------------------------------------------------------------------------------
# 5.5 Mean autonomy score by zone
# ------------------------------------------------------------------------------

svyby(~autonomy_score, ~v024, dhs_design, svymean, na.rm = TRUE)

# ------------------------------------------------------------------------------
# 5.6 Stunting prevalence by zone
# ------------------------------------------------------------------------------

svyby(~stunting, ~v024, dhs_design, svymean, na.rm = TRUE)

# ------------------------------------------------------------------------------
# 5.7 Descriptive statistics table
# ------------------------------------------------------------------------------
# A summary table showing sample characteristics by zone.
# MAD and meal frequency are included alongside MDD to give a complete picture 
# of dietary adequacy.
# Saved as HTML to outputs folder.
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
    mad_label      = factor(mad,
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
  select(zone, wasting_label, stunting_label, mdd_label, mad_label,
         autonomy_cat, autonomy_score, hw1, sex,
         residence, education, wealth) %>%
  tbl_summary(
    by = zone,
    label = list(
      wasting_label  ~ "Wasting",
      stunting_label ~ "Stunting",
      mdd_label      ~ "Met MDD",
      mad_label      ~ "Met MAD",
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

# Save table as PNG for GitHub
table%>%
as_gf() %>%
gt::gtsave("outpits/tables1_descriptive.png")

# ==============================================================================
# STEP 5B: RECODE WASH VARIABLES INTO BINARY CATEGORIES
# ==============================================================================
# Three WASH variables are included as covariates in Models 2, 3, 5 and 6 to 
# control for the household disease environment. Unsafe water, poor sanitation
# and unhygienic stool disposal increase infection and diarrhea risk which 
# independently drive wasting and stunting regardless of diet quality.
#
# This recoding must happen BEFORE Step 6 factor conversion. Recoding after 
# factor conversion fails because as.integer() no longer returns original DHS
# numeric codes after conversion, producing mostly NA.
#
# Improved water sources (coded 1):
# Piped water, boreholes, protected wells and springs, rainwater, 
# packaged water (codes 11-14, 21, 31, 41, 51, 71, 72)
#
# Improved toilet facilities (coded 1):
# Flush toilet, ventilated improved pit latrine, composting 
# toilet (codes 11, 12, 21, 22, 41)
#
# Safe stool disposal (coded 1):
# Disposed in toilet or buried (codes 1, 2)
# ==============================================================================

df <- df %>%
  mutate(
    water_improved = case_when(
      as.numeric(v113) %in% c(11,12,13,14,21,31,
                              41,51,71,72) ~ 1,
      as.numeric(v113) %in% c(32,42,43,61,
                              62,96) ~ 0,
      TRUE ~ NA_real_
    ),
    toilet_improved = case_when(
      as.numeric(v116) %in% c(11,12,21,22,41) ~ 1,
      as.numeric(v116) %in% c(13,14,15,23,31,
                              42,43,96) ~ 0,
      TRUE ~ NA_real_
    ),
    stool_safe = case_when(
      as.numeric(v465) %in% c(1,2) ~ 1,
      as.numeric(v465) %in% c(3,4,5,6,7,
                              8,9,96) ~ 0,
      TRUE ~ NA_real_
    )
  )

# Check distributions
table(df$water_improved, useNA = "always")
table(df$toilet_improved, useNA = "always")
table(df$stool_safe, useNA = "always")

# ==============================================================================
# ANALYTICAL APPROACH FOR REGRESSION MODELS
# ==============================================================================
# I estimated six survey-weighted regression models organised in two blocks: 
# one using MDD and one using MAD as the dietary indicator. This allows me to 
# compare whether the autonomy-nutrition pathway holds regardless
# of how dietary adequacy is measured.
#
# All models use svyglm with quasibinomial family to account for the DHS 
# complex sampling design.
#
# BLOCK 1 - MDD as dietary indicator:
# Model 1: Does autonomy predict MDD?
# Model 2: Do autonomy, MDD and WASH predict wasting?
# Model 3: Do autonomy, MDD and WASH predict stunting?
#
# BLOCK 2 - MAD as dietary indicator:
# Model 4: Does autonomy predict MAD?
# Model 5: Do autonomy, MAD and WASH predict wasting?
# Model 6: Do autonomy, MAD and WASH predict stunting?
#
# Covariates in all models:
# Zone, maternal education, wealth index, urban/rural residence, 
# maternal age group
#
# Additional covariates in Models 2, 3, 5 and 6:
# Child sex, child age in months, water source, toilet type, stool disposal
#
# A secondary aim is to examine the relative contribution of household wealth 
# compared to maternal autonomy in predicting child dietary adequacy and 
# malnutrition outcomes, given existing evidence that economic resources may 
# constrain what autonomy can achieve in low-income settings.
# ==============================================================================

# ==============================================================================
# STEP 6: FACTOR CONVERSION AND SURVEY DESIGN
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
# properly.
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
df$water_improved  <- as.factor(df$water_improved)
df$toilet_improved <- as.factor(df$toilet_improved)
df$stool_safe      <- as.factor(df$stool_safe)

# ------------------------------------------------------------------------------
# 6.2 Rebuild survey design with corrected factors
# ------------------------------------------------------------------------------
# The survey design object must be rebuilt after factor conversion so it uses 
# the updated factor variables. This makes sure all regression estimates are 
# properly weighted for the DHS complex sampling design.
# DHS uses complex sampling design with:
# - clustering (PSU = v021)
# - Stratification (v022)
# - Probability weights (v005 divided by 1,000,000)
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

model1 <- svyglm(
  mdd ~ autonomy_score + v024 + v149 +v190 + v025 + v013,
  design = dhs_design,
  family = quasibinomial(link ="logit")
)

summary(model1)

# ==============================================================================
# STEP 7: SURVEY-WEIGHTED LOGISTIC REGRESSION
# BLOCK 1 CONTINUED - Models 2 and 3
# BLOCK 2 - Models 4, 5 and 6 
# ==============================================================================

# ==============================================================================
# BLOCK 1 CONTINUED - MDD AS DIETARY INDICATOR
# ==============================================================================

# ------------------------------------------------------------------------------
# 7.1 Confirm factor conversion for wasting and stunting.
# Already converted in Step 6 but confirmed here before running Models 2 and 3.
# ------------------------------------------------------------------------------
df$wasting  <- as.factor(as.integer(df$wasting))
df$stunting <- as.factor(as.integer(df$stunting))

# ------------------------------------------------------------------------------
# 7.2 Rebuild survey design
# ------------------------------------------------------------------------------

dhs_design <- svydesign(
  id       = ~v021,
  strata   = ~v022,
  weights  = ~I(v005 / 1000000),
  data     = df,
  nest     = TRUE
)

# ------------------------------------------------------------------------------
# 7.3 Run Model 2 - Do autonomy, MDD and WASH predict wasting?
# ------------------------------------------------------------------------------
# WASH covariates are included to control for the household disease environment 
# which independently drives wasting through infection and diarrhea.
# ------------------------------------------------------------------------------

model2 <-  svyglm(
  wasting ~ autonomy_score + mdd + v024 + v149 + 
            v190 + v025 + v013 + b4 + hw1 +
            water_improved + toilet_improved + stool_safe,
  design = dhs_design,
  family = quasibinomial(link = "logit")
)

summary(model2)

# ------------------------------------------------------------------------------
# 7.4 Run Model 3 - Do autonomy, MDD and WASH predict stunting?
# ------------------------------------------------------------------------------
# Stunting shows chronic undernutrition accumulated over time. It was included 
# as secondary outcome to compare whether the autonomy-diet pathway differs for
# acute vs chronic malnutrition.
# ------------------------------------------------------------------------------
model3 <- svyglm(
  stunting ~ autonomy_score + mdd + v024 + v149 +
    v190 + v025 + v013 + b4 + hw1 +
    water_improved + toilet_improved + stool_safe,
  design = dhs_design,
  family = quasibinomial(link = "logit")
)

summary(model3)

# ==============================================================================
# BLOCK 2 - MAD AS DIETARY INDICATOR
# ==============================================================================
# MAD is a more complete IYCF indicator than MDD. It combines dietary diversity
# and meal frequency. 
# Block 2 mirrors Block 1 exactly but uses MAD instead of MDD 
# as the dietary exposure.
# ==============================================================================

# ------------------------------------------------------------------------------
# 7.5 Run Model 4 - Does autonomy predict MAD?
# ------------------------------------------------------------------------------
model4 <- svyglm(
  mad ~ autonomy_score + v024 + v149 +
    v190 + v025 + v013,
  design = dhs_design,
  family = quasibinomial(link = "logit")
)

summary(model4)

# ------------------------------------------------------------------------------
# 7.6 Run Model 5 - Do autonomy, MAD and WASH predict wasting?
# ------------------------------------------------------------------------------

model5 <- svyglm(
  wasting ~ autonomy_score + mad + v024 + v149 +
    v190 + v025 + v013 + b4 + hw1 +
    water_improved + toilet_improved + stool_safe,
  design = dhs_design,
  family = quasibinomial(link = "logit")
)

summary(model5)

# ------------------------------------------------------------------------------
# 7.7 Run Model 6 - Do autonomy, MAD and WASH predict stunting?
# ------------------------------------------------------------------------------
model6 <- svyglm(
  stunting ~ autonomy_score + mad + v024 + v149 +
    v190 + v025 + v013 + b4 + hw1 +
    water_improved + toilet_improved + stool_safe,
  design = dhs_design,
  family = quasibinomial(link = "logit")
)

summary(model6)

# ==============================================================================
# STEP 8: VISUALIZATION
# ==============================================================================
# Four charts that tell the visual story of this analysis in a logical sequence:
# 1. Where is autonomy lowest across zones?
# 2. Where is dietary adequacy lowest across zones?
# 3. Does autonomy relate to dietary adequacy?
# 4. Does autonomy relate to malnutrition outcomes?
#
# Charts are saved as PNG files at 300 DPI in the outputs folder.
# ==============================================================================

dir.create("outputs", showWarnings = FALSE)

# ------------------------------------------------------------------------------
# 8.1 Prepare data for plotting
# ------------------------------------------------------------------------------
# Zone labels and autonomy categories added for readable chart axes. MDD and MAD
# converted back to numeric for calculating prevalence.
# ------------------------------------------------------------------------------

plot_data <- df %>%
  mutate(
    zone_label = case_when(
      v024 == 1 ~ "North West",
      v024 == 2 ~ "North East",
      v024 == 3 ~ "North Central",
      v024 == 4 ~ "South East",
      v024 == 5 ~ "South South",
      v024 == 6 ~ "South West"
    ),
    mdd_num     = as.numeric(as.character(mdd)),
    mad_num     = as.numeric(as.character(mad)),
    wasting_num = as.numeric(as.character(wasting)),
    stunting_num = as.numeric(as.character(stunting))
  )

# ------------------------------------------------------------------------------
# 8.2 Autonomy by zone
# ------------------------------------------------------------------------------
# Zones ordered from lowest to highest autonomy score. Color gradient reinforces
# the direction.
# ------------------------------------------------------------------------------

autonomy_plot <- plot_data %>%
  group_by(zone_label) %>%
  summarise(mean_autonomy = mean(autonomy_score,
                                 na.rm = TRUE),
            .groups = "drop")

ggplot(autonomy_plot,
       aes(x = reorder(zone_label, mean_autonomy),
           y = mean_autonomy,
           fill = mean_autonomy)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "#E74C3C",
                      high = "#2ECC71") +
  labs(
    title = "Mean Maternal Autonomy Score by Zone",
    subtitle = "2024 Nigeria Demographic and Health Survey",
    x = "Geopolitical Zone",
    y = "Mean Autonomy Score (0-1)",
    fill = "Autonomy"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold"))

ggsave("outputs/autonomy_by_zone.png",
       width = 10, height = 6, dpi = 300)

# ------------------------------------------------------------------------------
# # 8.3 MDD and MAD prevalence by zone
# ------------------------------------------------------------------------------
# Both dietary indicators shown side by side by zone. This allows comparison of 
# dietary adequacy patterns across zones using both indicators simultaneously.
# ------------------------------------------------------------------------------

diet_zone_plot <- plot_data %>%
  group_by(zone_label) %>%
  summarise(
    MDD = mean(mdd_num, na.rm = TRUE) * 100,
    MAD = mean(mad_num, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(MDD, MAD),
               names_to = "Indicator",
               values_to = "Prevalence")

ggplot(diet_zone_plot,
       aes(x = reorder(zone_label, Prevalence),
           y = Prevalence,
           fill = Indicator)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("#3498DB", "#2ECC71")) +
  labs(
    title = "MDD and MAD Prevalence by Zone",
    subtitle = "2024 Nigeria Demographic and Health Survey",
    x = "Geopolitical Zone",
    y = "Prevalence (%)",
    fill = "Indicator"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold"))

ggsave("outputs/mdd_mad_by_zone.png",
       width = 10, height = 6, dpi = 300)

# ------------------------------------------------------------------------------
# 8.4 MDD and MAD prevalence by autonomy category
# ------------------------------------------------------------------------------
# Shows whether higher autonomy is associated with better dietary adequacy 
# descriptively, before the formal regression in Models 1 and 4.
# Both indicators shown side by side for comparison.
# ------------------------------------------------------------------------------

diet_autonomy_plot <- plot_data %>%
  filter(!is.na(autonomy_cat)) %>%
  group_by(autonomy_cat) %>%
  summarise(
    MDD = mean(mdd_num, na.rm = TRUE) * 100,
    MAD = mean(mad_num, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(MDD, MAD),
               names_to = "Indicator",
               values_to = "Prevalence") %>%
  mutate(autonomy_cat = factor(autonomy_cat,
                               levels = c("Low",
                                          "Medium",
                                          "High")))

ggplot(diet_autonomy_plot,
       aes(x = autonomy_cat,
           y = Prevalence,
           fill = Indicator)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("#3498DB", "#2ECC71")) +
  labs(
    title = "MDD and MAD Prevalence by Autonomy Category",
    subtitle = "2024 Nigeria Demographic and Health Survey",
    x = "Autonomy Category",
    y = "Prevalence (%)",
    fill = "Indicator"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

ggsave("outputs/mdd_mad_by_autonomy.png",
       width = 8, height = 6, dpi = 300)

# ------------------------------------------------------------------------------
# 8.5 Wasting and stunting by autonomy category
# ------------------------------------------------------------------------------
# Shows whether higher autonomy is associated with lower malnutrition prevalence
# descriptively, before the formal regression in Models 2, 3, 5 and 6.
# Both outcomes shown side by side for easy comparison.
# ------------------------------------------------------------------------------

malnutrition_plot <- plot_data %>%
  filter(!is.na(autonomy_cat)) %>%
  group_by(autonomy_cat) %>%
  summarise(
    Wasting  = mean(wasting_num, na.rm = TRUE) * 100,
    Stunting = mean(stunting_num, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(Wasting, Stunting),
               names_to = "Outcome",
               values_to = "Prevalence") %>%
  mutate(autonomy_cat = factor(autonomy_cat,
                               levels = c("Low",
                                          "Medium",
                                          "High")))

ggplot(malnutrition_plot,
       aes(x = autonomy_cat,
           y = Prevalence,
           fill = Outcome)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("#E74C3C", "#3498DB")) +
  labs(
    title = "Wasting and Stunting by Autonomy Category",
    subtitle = "2024 Nigeria Demographic and Health Survey",
    x = "Autonomy Category",
    y = "Prevalence (%)",
    fill = "Outcome"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

ggsave("outputs/malnutrition_by_autonomy.png",
       width = 8, height = 6, dpi = 300)

# ==============================================================================
# SESSION INFO
# ==============================================================================
# R version and package versions used in this analysis are recorded here for 
# reproducibility purposes.
# ==============================================================================
sessionInfo()
