# Maternal Autonomy, Dietary Diversity, and Child Wasting and Stunting in Nigeria: A Survey-Weighted Analysis of the 2024 NDHS

-----------------------------------------------------------------------------

## About This Project

This is an independent secondary data analysis I conducted using the 2024 
Nigeria Demographic and Health Survey (NDHS). 
The project looks at whether maternal decision-making autonomy and dietary 
diversity are associated with wasting and stunting among children aged 6-23 
months in Nigeria's six geopolitical zones.

My interest in this topic comes from my background in community 
nutrition and working with mothers in Ibadan, Oyo State during my National 
Youth Service Corps (NYSC) year. I saw that mothers often 
knew what to feed their children but they faced barriers at the household level 
to act on that knowledge. This analysis is my attempt to understand that gap 
using nationally representative data.

---

## Research Questions

1. Does maternal decision-making autonomy predict minimum dietary diversity 
   among children aged 6-23 months?
2. Do autonomy and dietary diversity predict wasting after controlling for key 
   sociodemographic factors?
3. Do autonomy and dietary diversity predict stunting after controlling for key 
   sociodemographic factors?

---

## Data

**Source:** 2024 Nigeria Demographic and Health Survey (NDHS)
**Files:** Kids Recode (KR) and Individual Recode (IR). Stata format
**Access:** Requested and got through dhsprogram.com for academic 
research purposes
**Sample:** The full KR file contained 27,783 child records. 
The analytical sample was restricted to 3,100 children aged 6-23 months with 
resident mothers. Note that our sample includes all children aged 6-23 months 
in the KR file, while the 2024 NDHS report restricted to the youngest child per 
mother. This explains the minor difference between our MDD estimate (13%) and 
the report figure (12%).

---

## Methods

**Software:** R
**Packages:** tidyverse, haven, janitor, survey, ggplot2, gtsummary, webshot2

### Outcome Variables
- **Wasting** - weight-for-height z-score < -2 SD (stored as HW72 < -200
in the DHS dataset), primary outcome
- **Stunting** - height-for-age z-score < -2 SD (stored as HW70 < -200
in the DHS dataset), secondary outcome

### Exposure Variables

**Minimum Dietary Diversity (MDD)**
MDD was constructed following the WHO 2021 updated Indicators for Assessing 
Infant and Young Child Feeding Practices.
Constructed using the WHO 8 food group framework from 24-hour dietary recall 
items in the KR file. A child met MDD if they consumed at least 5 of 8 food 
groups the previous day. The 8 groups are: breast milk; grains roots and tubers;
legumes and nuts; dairy; flesh foods; eggs; vitamin A rich fruits and 
vegetables; other fruits and vegetables.

**Maternal Autonomy Composite Score**
Constructed from four DHS decision-making variables:
- V743A - who decides on respondent's healthcare
- V743B - who decides on large household purchases
- V743D - who decides on visits to family
- V743F - who decides on husband's earnings

Each item scored: decides alone = 1, jointly = 0.5, husband or someone else = 0.
A score of 0 means the husband or someone else makes all decisions. A score 
of 1 means the woman makes all decisions alone. The composite is the mean of the
four items. Women were categorised as Low (<0.33), Medium (0.33-0.67), or 
High (>=0.67) for descriptive purposes. The continuous score was used in 
regression models.

**Covariates:** zone, maternal education, wealth index, urban/rural residence, 
maternal age group, child sex, child age in months

### Statistical Approach
Survey-weighted logistic regression using the survey package to account for DHS 
complex sampling design. Survey weights are applied to ensure estimates are 
nationally representative and account for the unequal probability of selection 
inherent in the DHS sampling design. Three models were estimated:
- Model 1: Autonomy predicting MDD
- Model 2: Autonomy + MDD predicting Wasting
- Model 3: Autonomy + MDD predicting Stunting

- **Wasting** - weight-for-height z-score < -2 SD (stored as HW72 < -200 
in the DHS dataset), primary outcome
- **Stunting** - height-for-age z-score < -2 SD (stored as HW70 < -200 
in the DHS dataset), secondary outcome

---

## Important Findings

### Descriptive
- 13% of children met MDD. This is consistent with the 2024 NDHS report 
  figure of 12%
- Wasting prevalence was 14.2%. This is higher than the national under-5 average 
  (8%), which is expected because wasting peaks in the 6–11 month age group
- Stunting prevalence was 34%. This is slightly lower than the national under-5 
  average (40%) because stunting accumulates with age and peaks later
- Only 5% of mothers had high autonomy. 49% fell in the low autonomy category
- Both autonomy and MDD followed a clear North-South gradient. Lowest in the 
  North, highest in the South
  - Autonomy lowest in North West (0.16) and highest in South South (0.49)
  - MDD lowest in North East (2.9%) and highest in South West (25.1%)
  - Wasting highest in South South (19.6%) and lowest in South West (12.0%)
  - Stunting highest in North East (50.4%) and lowest in South East (20.7%)

### Regression
**Model 1 - Autonomy predicts MDD:**
- Maternal autonomy did not significantly predict MDD after controlling for 
  covariates (p=0.347)
- Household wealth was the strongest predictor of dietary diversity. Children in
  richer households were significantly more likely to meet MDD across all 
  wealth quintiles (p<0.001)
- North East zone had significantly lower MDD than North West even after 
  controlling for autonomy and wealth (p<0.001)

**Model 2 - Autonomy + MDD predicts Wasting:**
- Neither autonomy nor MDD significantly predicted wasting after controlling for 
  covariates
- Maternal education was protective, children of mothers with incomplete 
  secondary or higher education had significantly lower risk of wasting compared 
  to children of mothers with no education (p<0.05)
- South South zone had significantly higher wasting risk than North West after 
  controlling for all covariates (p=0.044)
- Girls had significantly lower risk of wasting than boys (p=0.046)
- Older children within the 6-23 month window had lower risk of wasting (p=0.002)

**Model 3 Autonomy + MDD predicts Stunting:**
- Neither autonomy nor MDD significantly predicted stunting after controlling 
  for covariates
- Maternal education was strongly protective across multiple levels. Children of
  mothers with higher education had significantly lower risk of stunting (p<0.001)
- Older maternal age was protective, mothers aged 20 and above had 
  significantly lower risk of having a stunted child compared to 
  teenage mothers (p<0.05)
- Household wealth was protective at the highest quintile (p<0.001)
- Girls had significantly lower risk of stunting than boys (p<0.001)
- Child age positively predicted stunting. This is consistent with the 
  cumulative nature of chronic undernutrition (p<0.001)

### A Note on the South South Paradox

One unexpected finding in this analysis was that South South zone had the highest
maternal autonomy score (0.49) and relatively high dietary diversity, yet also 
had the highest wasting prevalence (19.6%) in the sample. This was also 
significant in the regression model for wasting.

I do not have a definitive explanation for this finding. It may be as a result 
of factors beyond diet quality that drive wasting in South South, such as 
disease burden, water and sanitation conditions, or healthcare access, which 
this analysis did not capture. It could also be because sample size limitations 
within the zone. This finding needs further investigation in future research.

This may also explain the counterintuitive positive direction of the autonomy 
coefficient in Model 3, where higher autonomy was weakly associated with higher 
stunting after controlling for other factors. This likely shows the same 
structural confounding rather than a true biological relationship.

---

## Limitations

I want to be transparent about the limitations of this analysis:

- **Key variable not administered:** V743E: who decides what food is cooked 
  daily, was not administered in the 2024 Nigeria DHS. This was the most 
  directly relevant autonomy variable for my research question. The composite 
  uses four general household decision variables as proxies.

- **V739 excluded:** Who decides how to spend the respondent's own money was 
  excluded from the composite due to 45% missingness. This variable was only 
  asked of women with personal earnings This makes it unrepresentative of 
  the full sample.

- **Continuous dietary diversity not examined:** MDD was used as a binary 
  indicator. The continuous food group count score was constructed but not 
  included in regression models. Future analyses should examine whether a 
  continuous score produces stronger associations with malnutrition outcomes.

- **Unhealthy food consumption not examined:** The 2024 Nigeria DHS contains 
  variables on consumption of unhealthy foods (V414R, V414T, V414W, V413C, 
  V413D, V409A). Constructing an unhealthy food consumption indicator alongside 
  MDD would provide a more complete picture of diet quality.

- **WASH variables:** I attempted to include water source and toilet type as 
  covariates but encountered incompatibility issues during recoding after factor 
  conversion. Future analyses should incorporate WASH indicators properly.

- **MDD is a single-day recall:** It captures what a child ate yesterday, 
  which may not show habitual feeding patterns.

- **Cross-sectional design:** This analysis cannot establish causality.

---

## What I Would Do Differently

- Include WASH indicators properly from the start
- Use Minimum Acceptable Diet (MAD) alongside MDD
- Apply a formal mediation framework to test whether MDD mediates the 
  autonomy-malnutrition pathway
- Investigate the South South paradox. High autonomy and dietary diversity 
  but also the highest wasting prevalence in the sample
- Construct an unhealthy food consumption indicator using available DHS 
  variables (V414R, V414T, V414W, V413C, V413D, V409A) to examine whether 
  unhealthy food consumption modifies the autonomy-malnutrition relationship
- Use the continuous food group count score alongside binary MDD in 
  regression models

---

## Visualizations

All outputs are in the `/outputs` folder:

1. `table1_descriptive.png` — descriptive statistics table showing sample
    characteristics by zone
2. `table1_descriptive.html` - descriptive statistics table showing sample 
    characteristics by zone
3. `wasting_by_mdd_zone.png` - wasting prevalence by MDD status and zone
4. `stunting_by_mdd_zone.png` - stunting prevalence by MDD status and zone
5. `autonomy_by_zone.png` - mean maternal autonomy score by zone
6. `mdd_by_autonomy.png` - MDD prevalence by autonomy category
7. `malnutrition_by_autonomy.png` - wasting and stunting by autonomy category
   
---

## Repository Structure

```
nigeria-dhs-2024/
├── .gitignore
├── README.md
├── nigeria-dhs-2024.Rproj
├── data/
│   └── README_data.txt
├── scripts/
│   └── nutrition_analysis.R
└── outputs/
    ├── table1_descriptive.png
    ├── table1_descriptive.html
    ├── wasting_by_mdd_zone.png
    ├── stunting_by_mdd_zone.png
    ├── autonomy_by_zone.png
    ├── mdd_by_autonomy.png
    └── malnutrition_by_autonomy.png
```

---

## How to Reproduce

1. Visit https://dhsprogram.com and create a free account
2. Request access to the 2024 Nigeria DHS dataset, access is usually granted 
   within 24-48 hours
3. Download the KR and IR Stata files and place them in the `/data` folder
4. Open `nigeria-dhs-2024.Rproj` in RStudio
5. Install required packages if needed:
   install.packages(c("tidyverse", "haven", "janitor", 
                      "survey", "ggplot2", "gtsummary", "webshot2"))
6. Run `scripts/nutrition_analysis.R` from top to bottom

**Important:** DHS microdata cannot be shared publicly under the DHS data access
agreement. Raw data files are not included in this repository and must be 
requested directly from dhsprogram.com.

---

## Author

**Ideraoluwa Fasoranti**
Nutrition and Dietetics Graduate | Independent Researcher

[![LinkedIn](https://img.shields.io/badge/LinkedIn-Connect-blue)]
(https://www.linkedin.com/in/ideraoluwa-fasoranti-)
