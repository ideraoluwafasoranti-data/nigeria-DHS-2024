# Maternal Autonomy, Child Dietary Adequacy, and  Malnutrition in Nigeria: A Survey-Weighted Analysis of the 2024 DHS

-----------------------------------------------------------------------------

## About This Project

This is an independent secondary data analysis I conducted using the 2024 
Nigeria Demographic and Health Survey (NDHS). 
The project looks at whether maternal decision-making autonomy and dietary 
diversity are associated with wasting and stunting among children aged 6-23 
months in Nigeria's six geopolitical zones. A secondary aim is to examine the 
relative contribution of household wealth compared to maternal autonomy, given 
existing evidence that economic resources may constrain what autonomy can achieve in 
low-income settings.

My interest in this topic comes from my background in community 
nutrition and working with mothers in Ibadan, Oyo State during my National 
Youth Service Corps (NYSC) year. I saw that mothers often 
knew what to feed their children but they faced barriers at the household level 
to act on that knowledge. This analysis is my attempt to understand that gap 
using nationally representative data.

---

## Research Questions

1. Does maternal decision-making autonomy predict minimum dietary diversity (MDD)
   among children aged 6-23 months?
2. Does maternal decision-making autonomy predict minimum acceptable diet (MAD)?
3. Do autonomy, MDD and WASH predict wasting and stunting after controlling for
   relevant sociodemographic factors?
4. Do autonomy, MAD and WASH predict wasting and stunting after controlling for
   relevant sociodemographic factors?

**Secondary aim:** To examine the relative contribution of household wealth
compared to maternal autonomy in predicting child dietary adequacy and malnutrition 
outcomes in Nigeria.

---

## Data

**Source:** 2024 Nigeria Demographic and Health Survey (NDHS)

**Files:** Kids Recode (KR) and Individual Recode (IR). Stata format

**Access:** Requested and obtained through dhsprogram.com for academic 
research purposes

**Sample:** The full KR file contained 27,783 child records. 
The analytical sample here was restricted to 3,100 children aged 6-23 months with 
resident mothers. Note that this sample includes all children aged 6-23 months 
in the KR file, while the 2024 NDHS report restricted to the youngest child per 
mother. This explains the minor difference between the MDD estimate (13%) here and 
the NDHS report figure (12%).

**Ethics:** Data was obtained under the DHS Program data access agreement and used
strictly for academic research purposes. No individual or household can be identified 
from the data used in this analysis.

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

**Minimum Acceptable Diet (MAD)**
A more complete IYCF indicator than MDD alone. MAD combines dietary diversity and 
meal frequency:
- Breastfed children: MDD >= 5 food groups AND meal frequency >= 2
- Non-breastfed children: MDD >= 5 food groups AND meal frequency ≥ 3 AND
  milk feeds >= 2

Note: Milk feed variables (V469E and V469F) are only administered to 
non-breastfeeding children in the DHS. Since most children in this sample 
were breastfed, MAD estimates for non-breastfed children are based on a 
small subsample and should be interpreted cautiously.
MAD was constructed following WHO 2021 updated IYCF indicators.

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

**WASH Covariates**
Three household sanitation indicators were included as covariates in 
Models 2, 3, 5 and 6 to control for the household disease environment:
- Water source — improved vs unimproved (V113)
- Toilet type — improved vs unimproved (V116)
- Stool disposal — safe vs unsafe (V465)

**Covariates:** zone, maternal education, urban/rural residence, 
maternal age group, child sex, child age in months

**Key covariate of interest:** Household wealth index (DHS wealth quintiles 1-5). 
This was included as a covariate in all models and examined for its relative 
contribution alongside autonomy as part of the secondary aim.

### Statistical Approach
Survey-weighted logistic regression using the survey package to account for DHS 
complex sampling design. Survey weights are applied to ensure estimates are 
nationally representative and account for the unequal probability of selection 
inherent in the DHS sampling design. 
Six models were estimated:
Six models were estimated in two blocks:

**Block 1 - MDD as dietary indicator:**
- Model 1: Does autonomy predict MDD?
- Model 2: Do autonomy, MDD and WASH predict wasting?
- Model 3: Do autonomy, MDD and WASH predict stunting?

**Block 2 - MAD as dietary indicator:**
- Model 4: Does autonomy predict MAD?
- Model 5: Do autonomy, MAD and WASH predict wasting?
- Model 6: Do autonomy, MAD and WASH predict stunting?

Using both MDD and MAD allows comparison of whether the autonomy-nutrition 
pathway holds regardless of how dietary adequacy is measured.

- **Wasting** - weight-for-height z-score < -2 SD (stored as HW72 < -200 
in the DHS dataset), primary outcome
- **Stunting** - height-for-age z-score < -2 SD (stored as HW70 < -200 
in the DHS dataset), secondary outcome

---

## Results

### Validation
The prevalence estimates were compared against the 2024 NDHS final report. 
MDD (13% vs report 12%), wasting (14.2% vs report 17% for 6-11 months) and 
stunting (34% vs report 40% for under-5) are consistent with published figures. 
Minor differences show the broader sample definition which includes all children 
6-23 months rather than the youngest child per mother only.

### Descriptive
- 13% of children met MDD and 7.7% met MAD. MAD is lower as expected since it
  is a stricter combined indicator requiring both dietary diversity and adequate 
  meal frequency.
- Wasting prevalence was 14.2%. This is higher than the national under-5 average 
  (8%), which is expected because wasting peaks in the 6-11 month age group
- Stunting prevalence was 34%. This is slightly lower than the national under-5 
  average (40%) because stunting accumulates with age and peaks later
- Only 5% of mothers had high autonomy. 49% fell in the low autonomy category
- A clear North-South gradient. Lowest in the North, highest in the South
  - Autonomy lowest in North West (0.16) and highest in South South (0.49)
  - MDD lowest in North East (2.9%) and highest in South West (25.1%)
  - MAD lowest in North East (1.5%) and highest in South South (11.0%)
  - Wasting highest in South South (19.6%) and lowest in South West (12.0%)
  - Stunting highest in North East (50.4%) and lowest in South East (20.7%)

**Block 1 - MDD:**

### Regression
**Model 1 - Does autonomy predicts MDD?:**
- Maternal autonomy did not significantly predict MDD after controlling for 
  covariates (p=0.347)
- Household wealth was the strongest predictor of dietary diversity. Children in
  richer households were significantly more likely to meet MDD across all 
  wealth quintiles (p<0.001)
- North East zone had significantly lower MDD than North West even after 
  controlling for autonomy and wealth (p<0.001)

**Model 2 - Do autonomy, MDD and WASH predict wasting?:**
- Neither autonomy (p=0.827) nor MDD (p=0.409) significantly predicted wasting
  after controlling for covariates. WASH variables were also not significant.
- Maternal education was protective, children of mothers with incomplete 
  secondary or higher education had significantly lower risk of wasting compared 
  to children of mothers with no education (p<0.05). Older children within the 
  6–23 month window had lower risks of wasting (p<0.001).
- South South zone had significantly higher wasting risk than North West after 
  controlling for all covariates (p=0.015)

**Model 3 - Do autonomy, MDD and WASH predict stunting?:**
- Neither autonomy (p=0.147) nor MDD (p=0.095) significantly predicted stunting
  after controlling for covariates. WASH variables were not significant.
- Maternal education was strongly protective across multiple levels (p<0.001)
- Household wealth was protective at the highest quintile (p<0.001)
- Girls had significantly lower risk of stunting than boys (p<0.001)
- Child age positively predicted stunting. This is consistent with the 
  cumulative nature of chronic undernutrition (p<0.001)

**Block 2 - MAD:**

**Model 4 - Does autonomy predict MAD?**
Maternal autonomy did not significantly predict MAD (p=0.739). Household wealth remained 
the strongest predictor. This is consistent with Model 1.

**Model 5 - Do autonomy, MAD and WASH predict wasting?**
Neither autonomy (p=0.621) nor MAD (p=0.840) significantly predicted wasting. 
This is consistent with Model 2.

**Model 6 - Do autonomy, MAD and WASH predict stunting?**
Neither autonomy (p=0.133) nor MAD (p=0.427) significantly predicted stunting. 
This is consistent with Model 3.

### Conclusion

Maternal autonomy did not significantly predict child dietary adequacy or malnutrition 
outcomes in any of the six models, regardless of whether MDD or MAD was used as 
the dietary indicator.

But household wealth was a dominant and most consistent predictor inall models. 
Children in richer households were significantly more likely to meet MDD 
and MAD regardless of their mother's autonomy level.

These findings should be interpreted because of a key measurement limitation. 
The variable most relevant to food-specific autonomy, V743E - who decides what 
food is cooked daily) was not administered in the 2024 Nigeria DHS. Thus, autonomy 
composite captures general household decision-making rather than food-specific autonomy. 

What we can say with confidence from this analysis is that regardless of a mother's 
general decision-making power, household wealth is the strongest predictor of whether 
her child receives a diverse and adequate diet. Economic empowerment alongside autonomy 
interventions may be important for improving child nutrition outcomes in Nigeria.

### A Note on the South South Paradox

One unexpected finding in this analysis was that South South zone had the highest
maternal autonomy score (0.49) and high child dietary diversity, yet also 
had the highest wasting prevalence (19.6%) in the sample. This was also 
significant in the regression model for wasting, even after controlling for WASH 
indicators, maternal education, wealth and other sociodemographic factors

I do not have a definitive explanation for this finding. It may be as a result 
of factors beyond diet quality that drive wasting in South South, such as 
acute illness burden, healthcare access, or other unmeasured factors specific 
to South South, which this analysis did not capture. This finding needs further 
investigation in future research.

This may also explain the counterintuitive positive direction of the autonomy 
coefficient in Model 3, where higher autonomy was weakly associated with higher 
stunting after controlling for other factors. This likely shows the same 
structural confounding rather than a true biological relationship.

---

## Why I Believe AAutonomy Was Not Significant - A Measurement Note

The most relevant variable for my research question, and to food-specific 
autonomy, V743E - who decides what food is cooked daily was not administered in 
the 2024 Nigeria DHS. The autonomy composite captures general household 
decision-making rather than food-specific autonomy, which limits my ability to 
properly test the autonomy-IYCF pathway.
It is possible that food-specific autonomy would show significant associations 
with child dietary adequacy and malnutrition outcomes that general household autonomy 
could not detect.

Reinstating V743E in future Nigeria DHS surveys and collecting primary data with 
food-specific autonomy instruments would enable more direct testing of this 
pathway.

---

## Limitations

I want to be transparent about the limitations of this analysis:

- **Key variable not administered:** V743E: who decides what food is cooked 
  daily, was not administered in the 2024 Nigeria DHS. The composite 
  uses four general household decision variables as proxies.

- **V739 excluded:** Who decides how to spend the respondent's own money was 
  excluded from the composite due to 45% missingness. This variable was only 
  asked of women with personal earnings, which makes it unrepresentative of 
  the full sample.

- **MAD for non-breastfed children:** Milk feed variables (V469E and V469F) are only 
  administered to non-breastfeeding children in the DHS. Since most children in our
  sample were breastfed, MAD estimates for non-breastfed children are based on a small 
  subsample and should be interpreted cautiously.

- **MDD and MAD are single-day recall indicators:** 
  They capture what a child ate yesterday which may not show habitual feeding patterns.

- **Cross-sectional design:** This analysis cannot establish causality.

---

## What I Would Do Differently

- Use food-specific autonomy measures, especially V743E if reinstated in future
  DHS surveys, or collect primary data using validated food-specific 
  autonomy instruments.
- Incorporate household food security measures which was not available in this dataset.
- Apply a formal mediation framework to test whether dietary diversity mediates the
  autonomy-malnutrition pathway.
- Investigate the South South paradox further using mixed methods or qualitative approaches.
- Extend the analysis to older age groups to examine whether the autonomy-nutrition pathway
  operates differently beyond the 6–23 month complementary feeding window.

---

## Visualizations

All outputs are in the `/outputs` folder:

1. `table1_descriptive.png` - descriptive statistics table showing sample
    characteristics by zone
2. `table1_descriptive.html` - descriptive statistics table showing sample 
    characteristics by zone
3. `mdd_mad_by_zone.png` - MDD and MAD prevalence by zone
4. `mdd_mad_by_autonomy.png` - MDD and MAD prevalence by autonomy category
5. `malnutrition_by_autonomy.png` - wasting and stunting by autonomy category
   
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
    ├── mdd_mad_by_zone.png
    ├── mdd_mad_by_autonomy.png
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

**Ideraoluwa J. Fasoranti**
Nutrition and Dietetics Graduate | Independent Researcher

[![LinkedIn](https://img.shields.io/badge/LinkedIn-Connect-blue)]
(https://www.linkedin.com/in/ideraoluwa-fasoranti-)
