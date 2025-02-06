################################################################################
# purpose: establish and apply weights to survey dataset
# last edited: feb 5, 2025
# reference: https://www.census.gov/quickfacts/fact/table/oaklandcitycalifornia/PST045223
# methodology note on raking: https://www.pewresearch.org/methods/2018/01/26/for-weighting-online-opt-in-samples-what-matters-most/
################################################################################

#### context ####
# Due to factors ranging from outreach channels to trust in government, some 
# communities are disproportionately represented in the responses.
# We take measures to mitigate disproportionate representation by weighting the 
# survey responses. Responses are first disaggregated by subgroups (“strata”) and 
# then the weights of those subgroup responses are set to create a distribution 
# that mirrors the overall population.
# 
# In our analysis, we’ll stratify by demographic factors where the distribution 
# of the overall pool of respondents are not representative of Oakland as a whole 
# (according to U.S. Bureau of Census data). Specifically:
# 
# Education level: Oakland is 48% bachelor’s or higher vs. 86.8% of survey respondents
# 
# Gender: Oakland is 50.5% female vs. 59% of survey respondents    
#     
# Racial/Ethnic Identity: Oakland is:
#     31% Caucasian vs.56% of survey respondents
#     29% Latinx vs.6.9% of survey respondents
#     21% Black vs.9.6% of survey respondents
#     16% Asian vs. 10.5% of survey respondents
#     12% Multi-racial vs. 9.8% of survey respondents
#     1% American Indian or Alaskan Native vs.0.4% of survey respondents
#     <0.5% Native Hawaiian or Pacific Islander vs.0.3% of survey respondents
# 
# Note that the proportion of residents should be roughly equal across districts, 
# i.e. 14.3% between 7 districts). D1 and D4 are were particularly responsive
# while D3, D5, D6 and D7 are underrepresented.

#### load packages and data ####
# load packages
library(tidyverse)
library(skimr)
library(anesrake)

# load data
load("data/2025/processed/survey_processed.Rdata")

#### use raking method to develop weights ####
# convert weighting variables from character to factor
d_survey <- d_survey %>%
    mutate(
        across(district:education, as_factor),
        # reorder factor levels to match what's available in census
        district = fct_relevel(district, sort),
        age = fct_relevel(age, sort),
        age_group = fct_collapse(
            age,
            "Young Adult (<18)" = c("0-17"),
            "Adult (18-64)" = c("18-24", "20-24", "25-29", "30-34", "35-39",
                "40-44", "45-49", "50-54", "55-59", "60-64"),
            "Older Adult (65+)" = c("65-74", "75 and older"),
            "No Response" = c("No Response")
        ),
        gender = fct_relevel(gender, 
            c("Male", "Female", "Nonbinary", "Other Response", "No Response")),
        race_ethnicity = fct_relevel(race_ethnicity, 
            d_rmap$r_en[d_rmap$question == "q17"]),
        race_group = fct_collapse(
            race_ethnicity,
            "White and MENA" = c("White", "Middle Eastern or North African"),
            "Other Response" = c("American Indian or Alaska Native",
                "Native Hawaiian or Pacific Islander", "Other Response"),
            "No Response" = c("No Response")
        ) %>% fct_relevel("Other Response", after = 5),
        education = fct_relevel(education, 
            d_rmap$r_en[d_rmap$question == "q19"]),
        education_group = fct_collapse(
            education, 
            "Baccalaureate and advanced" = c("Four-year college or bachelor’s degree",
                "Graduate school or advanced degree"),
            "Sub-baccalaureate" = c("First through 11th grade",
                "High school graduate", 
                "Vocational/technical school",
                "Some college, no degree",
                "Associate degree"),
            "No Response" = c("No Response")
        )
    )

# check factor level orders
d_survey %>% map(levels) %>% compact()

# create respondent-level dataset
d_respondents <- d_survey %>%
    select(respondent_id, district, age_group, gender, race_group, education_group) %>%
    distinct()

# specify population distribution using census jul 2023 estimates and 
# 1% estimate for population data missingness
district <- c(rep(0.99/7, 7), 0.01)
names(district) <- levels(d_respondents$district)
    
age_group <- c(0.193, 0.654, 0.143, 0.01)
names(age_group) <- levels(d_respondents$age_group)

race_group <- c(0.155, 0.211, 0.289, 0.279, 0.116, 0.016, 0.01)
names(race_group) <- levels(d_respondents$race_group)

gender <- c(0.47, 0.505, 0.01, 0.005, 0.01)
names(gender) <- levels(d_respondents$gender)

education_group <- c(0.511, 0.479, 0.01)
names(education_group) <- levels(d_respondents$education_group)

distributions <- list(district, age_group, race_group, gender, education_group)
names(distributions) <- c("district", "age_group", "race_group", "gender", 
    "education_group")

# rake
raking <- anesrake(
    inputter = distributions, 
    dataframe = as.data.frame(d_respondents), 
    caseid = as.data.frame(d_respondents)$respondent_id,
    cap = 5, 
    type = "pctlim",
    pctlim = 5,
    choosemethod = "total",
    force1 = TRUE
)

raking_summary <- summary(raking); raking_summary

#### add weights to survey data ####
# create dataframe of weights
d_weights <- enframe(raking$weightvec, name = "respondent_id", value = "wt")

# join weights
d_respondents <- d_respondents %>%
    left_join(d_weights, by = "respondent_id")

d_survey <- d_survey %>%
    left_join(d_weights, by = "respondent_id")

# save data
save(d_survey, d_respondents, d_place_to_live, d_approval,d_qmap, d_rmap,
     file = "data/2025/processed/survey_weighted.RData")
write_csv(d_survey, file = "data/2025/processed/survey_weighted.csv")