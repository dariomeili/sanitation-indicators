# Date: 6.2.2021
# R Script producing all results of the article "Indicators for the quality of sanitation in poor urban settlements"
#
# Need to install the following packages before running the script
#install.packages(c("tidyverse", "magrittr", "sjmisc", "wesanderson", "ggthemes", "knitr", "kableExtra", "estimatr", "clubSandwich", "extrafont", "Cairo", "gt", "gtsummary", "texreg"))
#
# load packages
options(scipen = 999)
# general
library(tidyverse)
library(magrittr) # for %<>% operator
library(sjmisc) # frq() etc..
# graphics
library(wesanderson) # wes anderson color palette
library(ggthemes) # for different ggplot themes
library(extrafont) # font for figures
library(Cairo) # eps output
library(ggpubr)
#tables
library(knitr) 
library(kableExtra) 
library(gt) 
library(gtsummary)
library(texreg) 
# estimation
library(estimatr) # for robust regressions
library(clubSandwich) # for cluster robsut vcov matrix
#
# read data
df <- read_csv("data/data_pooled.csv")
#
# run functions script
source("code/functions.R")
# recode some variables for preparation
df %<>%
  mutate(
    id_country = fct_relevel(as_factor(id_country),
                             "Kenya", "Ghana", "Bangladesh"),
    full_clogged = as_factor(toilet_fullclogged),
    wall = fct_recode(
      privacy_wall,
      "Low quality" = "Not solid",
      "High quality" = "Solid"
    ),
    floor = case_when(
      floor_holes == "Yes" |
        floor_obs %in% c("Natural", "Rudimentary") ~ "Low quality",
      floor_holes == "No" &
        floor_obs == "Finished" ~ "High quality",
      TRUE ~ NA_character_
    ),
    roof = fct_recode(
      roof_solid,
      "Low quality" = "Not solid",
      "High quality" = "Solid"
    ),
    use_night = fct_recode(
      use_night,
      "No use at night" = "No",
      "Use at night" = "Yes"
    ),
    lighting = fct_recode(nightlight_rep,
                          "No lighting" = "No",
                          "Lighting" = "Yes"),
    gender_separated = fct_recode(
      gender_separated,
      "Private" = "Private toilet",
      "No" = "No",
      "Yes" = "Yes"
    ),
    relationship = recode_factor(
      relationship,
      "Only relatives" = "Only relatives",
      "Close neighbors" = "Close neighbors/relatives",
      "Other" = "Others"
    ),
    hh_cubicle_cat = case_when(
      hh_cubicle_num == 1 ~ "1 HH",
      hh_cubicle_num %in% 2:4 ~ "2-4 HH",
      hh_cubicle_num %in% 5:7 ~ "5-7 HH", 
      hh_cubicle_num %in% 8:10 ~ "8-10 HH", 
      hh_cubicle_num == 11 ~ ">10 HH", 
      TRUE ~ as.character(hh_cubicle)
    ),
    hh_cubicle_cat = fct_relevel(hh_cubicle_cat,
                                 "1 HH",
                                 "2-4 HH",
                                 "5-7 HH",
                                 "8-10 HH",
                                 ">10 HH"
    ),
    cleaning_rota = fct_recode(
      cleaning_arrangement,
      "Private" = "Not shared",
      "No" = "No",
      "Yes" = "Yes"
    ),
    interface_pool = fct_recode(
      interface_rep,
      "Flush" = "Flush",
      "Flush" = "Pour-flush",
      "Pit latrine (with slab)" = "Improved pit latrine", 
      "Pit latrine (no slab)/other" = "Container based",
      "Pit latrine (no slab)/other" = "Unimproved pit latrine",
      "Pit latrine (no slab)/other" = "Other"
    ),
    interface_ctry = case_when(
      id_country %in% c("Kenya", "Ghana") ~ as.character(interface_pool),
      id_country == "Bangladesh" & interface_pool %in% c("Pit latrine (with slab)", "Pit latrine (no slab)/other") ~ "Pit latrine (no slab)/other",
      id_country == "Bangladesh" & interface_pool == "Flush" ~ "Flush",
      TRUE ~ as.character(interface_rep)),
    outflow_pool = fct_recode(outflow_rep,
                              "Piped sewer" = "Piped sewer system",
                              "Piped sewer" = "Septic tank",
                              "Pit" = "Single pit",
                              "Pit" = "Twin pit",
                              "Pit" = "Container based",
                              "Elsewhere" = "Don't know",
                              "Elsewhere" = "Open drain",
                              "Elsewhere" = "Other"
    ),
    technology_rep = recode_factor(D0,
                                   "Flush to piped sewer system" = "Flush to sewer/septic/pit",
                                   "Flush to septic tank" = "Flush to sewer/septic/pit",
                                   "Flush to pit latrine" = "Flush to sewer/septic/pit",
                                   "Flush to open drain" = "Flush to elsewhere",
                                   "Flush to don't know where" = "Flush to elsewhere",
                                   "Pour flush to piped sewer system" = "Flush to sewer/septic/pit",
                                   "Pour flush to septic tank" = "Flush to sewer/septic/pit",
                                   "Pour flush to pit latrine" = "Flush to sewer/septic/pit",
                                   "Pour flush to open drain" = "Flush to elsewhere",
                                   "Pour flush to don't know where" = "Flush to elsewhere",
                                   "Pit latrine with slab (no water)" = "Pit latrine (with slab)",
                                   "Twin pit with slab" = "Pit latrine (with slab)",
                                   "Pit latrine without slab / open pit (no water)" = "Pit latrine (no slab)/other",
                                   "Twin pit without slab" = "Pit latrine (no slab)/other",
                                   "Container based sanitation" = "Pit latrine (no slab)/other",
                                   "Hanging toilet / hanging latrine" = "Pit latrine (no slab)/other",
                                   "Bucket" = "Pit latrine (no slab)/other"
    ),
    improved_rep = fct_recode(technology_rep,
                              "No" ="Flush to elsewhere",
                              "No" = "Pit latrine (no slab)/other",
                              "Yes"="Flush to sewer/septic/pit",
                              "Yes" = "Pit latrine (with slab)",
    ),
    age_toilet = fct_relevel(
      age_toilet,
      "<1 Year", 
      "1-3 years",
      "4-6 years",
      "7-9 years",
      ">9 years / don't know"
    ),
    toilet_clean = fct_recode(clean_comp,
                              "No" = "Not clean",
                              "Yes" = "Clean"),
    handwashing = fct_recode(handwashing_obs,
                             "No" = "No facility with soap",
                             "Yes" = "Facility with soap"),
    location = fct_relevel(location, 
                           "Inside dwelling",
                           "Inside compound/on plot",
                           "Elsewhere"),
    landlord_on_plot = fct_recode(landlord_on_plot,
                                  "No"="landlord not on plot",
                                  "Yes"="landlord on plot"),
    bin_inside = fct_recode(bin_inside, 
                            "No" = "No bin inside cubicle",
                            "Yes" = "Bin inside cubicle"),
    lighting = fct_recode(lighting, 
                          "No"="No lighting",
                          "Yes" = "Lighting"),
    lighting = nightlight_rep,
    tiling = fct_recode(tiling, 
                        "No" = "No tiling",
                        "Yes" = "Tiling"))


#################################
# Source run Multiple correspondence analysis
source("code/mca.R")
# recode country id
df %<>% 
  mutate(id_country = fct_relevel(
    id_country,
    "Kenya",
    "Ghana",
    "Bangladesh"
  ))
#
#################################
# load fonts for figures
loadfonts()
#################################
# Source sanitation levels across the world figure
source("code/levels_world.R")

# Correlation plots between different SQI measures
## plot correlation between sqi pooled and by country
sqi_corplot <- df %>% 
  ggplot(aes(sqi, sqi_ctry, color = id_country, fill = id_country)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  labs(x = "pooled SQI", y="SQI by country", color = "Country", fill = "Country") +
  scale_colour_viridis_d(end = 0.9) +
  theme_minimal() +
  theme(text = element_text(size=10, family="LM Roman 10")) +
  ggsave("figures/sqi_corr.eps", device = cairo_ps, width = 119, height = 73, units = "mm")

# plot correletation bewteen additive and weighted SQI
## First, by pooled SQI
df %>% 
  ggplot(aes(sqi, sqi_add, color = id_country, fill = id_country)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  labs(x = "pooled SQI", y = "Additive quality score", color = "Country", fill = "Country" ) +
  scale_colour_viridis_d(end = 0.9) +
  theme_minimal() +
  theme(text = element_text(size=10, family="LM Roman 10")) +
  ggsave("figures/cor_additive.eps", device = cairo_ps, width = 119, height = 73, units = "mm")
## Second, by separately calculated SQI
df %>% 
  ggplot(aes(sqi_ctry, sqi_add, color = id_country, fill = id_country)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  labs(x = "SQI by country", y = "Additive quality score", color = "Country", fill = "Country" ) +
  scale_colour_viridis_d(end = 0.9) +
  theme_minimal() +
  theme(text = element_text(size=10, family="LM Roman 10")) +
  ggsave("figures/cor_add_ctry.eps", device = cairo_ps, width = 119, height = 73, units = "mm")

###########################
# Table of sampling results
df %>% 
  select(id_country, 
         spotcheck,
         gender_respondent,
         gender_hh, 
         hh_size, 
         hhmember_rooms, 
         B11_income_ppp,
         educ_hh, 
         tenancy,
         electricity) %>% 
  mutate(spotcheck = fct_recode(spotcheck, "First" = "Yes", "Second" = "No"),
         tenancy = fct_relevel(tenancy, 
                               "Owner", "Free rent", "Tenant (formal)", "Tenant (informal)"),
         gender_respondent = fct_relevel(gender_respondent, "Male", "Female"),
         gender_hh = fct_relevel(gender_hh, "Male", "Female"),
         electricity = fct_relevel(electricity, "No", "Yes"),
         educ_hh = fct_relevel(educ_hh, 
                               "None", "Primary", "Secondary", "Tertiary", "Don't know"))  %>% 
  tbl_summary(
    by = id_country, 
    label = list(
      spotcheck ~ "Respondent",
      gender_respondent ~ "Gender=female (respondent)",
      gender_hh ~ "Gender=female (HH head)",
      hh_size ~ "HH size", 
      hhmember_rooms ~ "HH members/room",
      B11_income_ppp ~ "Income (monthly, USD ppp)",
      educ_hh ~ "Education (HH head)", 
      tenancy ~ "House ownership", 
      electricity ~ "Electricity"),
    missing = "no",
    statistic = all_continuous() ~ "{mean} ({sd})",
    type = list(
      gender_respondent ~ "dichotomous",
      gender_hh ~ "dichotomous"),
    value = list(
      gender_respondent = "Female",
      gender_hh = "Female")) %>% 
  add_overall() %>% 
  modify_footnote(update = everything() ~ NA) %>%
  as_kable_extra(format = "latex",
                 caption = "Sample characteristics",
                 label = "sampling",
                 booktabs = TRUE,
                 position = "ht",
                 linesep = "") %>% 
  kable_styling(latex_options = c("scale_down")) %>%
  cat(file = "./tables/sampling.tex")
#
########################
# descriptive statistics of toilet characteristics
df %>% 
  mutate(id_country = id_country,
         gender_separated = fct_recode(gender_separated,
                                       "No" = "Private"),
         cleaning_rota = fct_recode(cleaning_rota,
                                    "No" = "Private"),
         hh_cubicle_cat = fct_recode(hh_cubicle_cat,
                                     "more than 10 HH" = ">10 HH"),
         age_toilet = fct_recode(age_toilet,
                                 "less than 1 year" = "<1 Year",
                                 "more than 9 years/don't know" = ">9 years / don't know")) %>% 
  select(id_country,
         technology_rep,
         hh_cubicle_cat,
         location,
         water,
         lighting, 
         lock,
         tiling,
         gender_separated,
         cleaning_rota,
         relationship,
         age_toilet,
         landlord_on_plot,
         bin_inside) %>% 
  tbl_summary(
    by = id_country, 
    label = list(
      technology_rep ~ "Toilet technology",
      hh_cubicle_cat ~ "Sharing HHs/cubicle",
      location ~ "Location",
      water ~ "Improved water on premises",
      lighting ~ "Lighting", 
      lock ~ "Lockable door",
      tiling ~ "Tiling (floor)",
      gender_separated ~ "Gender separated",
      cleaning_rota ~ "Cleaning rota",
      relationship ~ "User relationship",
      age_toilet ~ "Age of toilet",
      landlord_on_plot ~ "Landlord on plot",
      bin_inside ~ "Bin inside cubicle"
    ),
    missing = "no",
    statistic = all_continuous() ~ "{mean} ({sd})",
    type = list(water ~ "dichotomous"),
    value = list(water ~ "Improved on premises")
  ) %>% 
  add_overall() %>% 
  modify_footnote(update = everything() ~ NA) %>% 
  as_kable_extra(format = "latex",
                 caption = "Reported toilet characteristics",
                 label = "descriptive",
                 booktabs = TRUE,
                 position = "ht",
                 linesep = "") %>%
  footnote(alphabet = c("Flush includes both flush and pour-flush;",
                        "Flush to elsewhere contains both flush to don’t know where as well as flush to open drain; ",
                        "We assume toilets draining elsewhere involve an unsafe conveyance system.",
                        "The share of households using private toilets is not representative for the samples as these households\\\\newline(and toilets) were purposively sampled;"),
           footnote_order = c("number", "alphabet", "symbol", "general"),
           threeparttable = TRUE, 
           escape = F) %>% 
  kable_styling(latex_options = c("scale_down")) %>% 
  cat(file = "./tables/descriptive.tex")
#
# average number of HH per cubicle (excluding single toilets)
mean_hhcubicle <- df %>% 
  filter(hh_cubicle_num != 1) %>% 
  group_by(id_country) %>% 
  summarise(mean = mean(hh_cubicle_num, na.rm = T))
# descriptive statistics for outcome vars
df %>% select(id_country,
              sqi,
              sqi_ctry,
              toilet_clean,
              visible_feces,
              insects_obs,
              solidwaste_obs,
              full_clogged,
              wall,
              floor,
              roof,
              handwashing) %>%
  tbl_summary(by = id_country,
              label = list(sqi ~ "Pooled SQI",
                           sqi_ctry ~ "Country SQI",
                           toilet_clean ~ "Toilet clean (composite)",
                           visible_feces ~ "Visible feces",
                           insects_obs ~ "Insects",
                           solidwaste_obs ~ "Solid waste",
                           full_clogged ~ "Toilet full or clogged",
                           wall ~ "Wall material (high quality)",
                           floor ~ "Floor material (high quality)",
                           roof ~ "Roof material (high quality)" ,
                           handwashing ~ "Handwashing station (with soap)"),
              missing = "no",
              statistic = all_continuous() ~ "{mean} ({sd})",
              digits = list(
                sqi ~ c(1,1),
                sqi_ctry ~ c(1,1)),
              type = list(
                wall ~ "dichotomous",
                floor ~ "dichotomous",
                roof ~ "dichotomous"),
              value = list(
                wall ~ "High quality",
                floor ~ "High quality",
                roof ~ "High quality")) %>% 
  add_overall() %>% 
  modify_footnote(update = everything() ~ NA) %>% 
  as_kable_extra(format = "latex",
                 caption = "Toilet quality outcomes (observed)",
                 label = "outcomes",
                 booktabs = TRUE,
                 position = "ht",
                 linesep = "") %>% 
  footnote(alphabet = c("The toilet is considered clean if no visible feces, insects, and solid waste were observed."),
           threeparttable = T) %>% 
  kable_styling(latex_options = c("scale_down")) %>% 
  cat(file = "./tables/outcomes.tex")

###########################################
# distribution of quality index scores by country
df %>% 
  select(id_country,sqi,sqi_ctry) %>%
  pivot_longer(-id_country) %>% 
  mutate(bins = case_when(
    value < 20 ~ "0-20",
    value >= 20 & value < 40 ~ "21-40",
    value >= 40 & value < 60 ~ "41-60",
    value >= 60 & value < 80 ~ "61-80",
    value >= 80 & value <= 100 ~ "81-100",
    TRUE ~ NA_character_
  )) %>%
  count(id_country, bins, name) %>% 
  group_by(id_country, name) %>% 
  mutate(pct = n / sum(n),
         name = fct_recode(name,
                           "SQI pooled" = "sqi",
                           "SQI by country" = "sqi_ctry")) %>% 
  ggplot(aes(
    x = id_country,
    y = pct,
    fill = fct_rev(bins))) +
  geom_col() + 
  scale_fill_viridis_d(direction=-1) +
  labs(x = "Country",
       y = "Share",
       fill = "SQI score"
  ) +
  theme_minimal()+ 
  theme(text = element_text(size=10, family="LM Roman 10")) +
  facet_grid(.~name)+
  ggsave("figures/sqi_ctry.eps", device = cairo_ps, width = 119, height = 73, units = "mm")

######################################
# Regression Analysis
#define variables for full model
df %<>% 
  mutate(interface_pool = fct_relevel(
    interface_pool,
    "Flush", "Pit latrine (with slab)", "Pit latrine (no slab)/other"
  ),
  outflow_pool = fct_relevel(outflow_pool,
                             "Piped sewer", "Pit", "Elsewhere"),
  hh_cubicle_fct = fct_relevel(hh_cubicle_fct,
                               "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "more than 10"),
  water = fct_relevel(water,
                      "Unimproved/not on premises", "Improved on premises"),
  gender_separated = fct_recode(gender_separated,
                                "No" = "Private",
                                "Yes" = "Yes"),
  cleaning_rota = fct_recode(cleaning_rota,
                             "No" = "Private",
                             "Yes" = "Yes"),
  bin_inside = fct_relevel(bin_inside,
                           "No", "Yes"),
  age_toilet = fct_relevel(age_toilet,
                           "<1 Year", "1-3 years", "4-6 years", "7-9 years", ">9 years / don't know"),
  improved_rep = fct_relevel(improved_rep,
                             "No", "Yes")) 

variables <- c("interface_pool",
               "outflow_pool",
               "hh_cubicle_fct",
               "location",
               "water",
               "lighting",
               "lock",
               "tiling",
               "gender_separated",
               "cleaning_rota",
               "relationship_ctry",
               "age_toilet",
               "landlord_on_plot",
               "bin_inside"
)

# fit regression models for pooled sample
fit1_all <- lm_robust(sqi ~ improved_rep + sharing + id_country,
                      data = df, clusters = id_compound, 
                      se_type = "stata")
fit2_all <- lm_robust(sqi ~ interface_pool + outflow_pool + hh_cubicle_fct + id_country, 
                      data = df, clusters = id_compound, 
                      se_type = "stata")
# save variables in a formula
fmla <- as.formula(paste("sqi ~ ", paste(append(variables, "id_country"), collapse= "+")))
df %<>%
  mutate(relationship_ctry = fct_recode(relationship,
                                        "Close neighbors/landlord" = "Only relatives"))
fit3_all <- lm_robust(fmla, 
                      data = df, 
                      clusters = id_compound, 
                      se_type = "stata")

#########################
# Countrywise regressions 
# redefine variables
variables_ct <- c("interface_ctry",
                  "outflow_pool",
                  "hh_cubicle_fct",
                  "location",
                  "water",
                  "lighting",
                  "lock",
                  "tiling",
                  "gender_separated",
                  "cleaning_rota",
                  "relationship_ctry",
                  "age_toilet",
                  "landlord_on_plot",
                  "bin_inside"
)

# save variables in a formula
fmla_ct <- as.formula(paste("sqi_ctry ~ ", paste(variables_ct, collapse= "+")))
model1_ct <- function(.data) lm_robust(sqi_ctry ~ improved_rep + sharing, data = .data, clusters = .data$id_compound, se_type = "stata")
model2_ct <- function(.data) lm_robust(sqi_ctry ~ interface_ctry + outflow_pool + hh_cubicle_fct, data = .data, clusters = .data$id_compound, se_type = "stata")
model3_ct <- function(.data) lm_robust(fmla_ct, data = .data, clusters = .data$id_compound, se_type = "stata")

# fit all regression models using predefined functions
fit1_ctry <- df %>%
  group_by(id_country) %>%
  nest() %>%
  mutate(reg = map(data, model1_ct))

fit2_ctry <- df %>%
  group_by(id_country) %>%
  nest() %>%
  mutate(reg = map(data, model2_ct))

fit3_ctry <- df %>%
  group_by(id_country) %>%
  nest() %>%
  mutate(reg = map(data, model3_ct))

# render regression tables in latex
texreg(
  lapply(list(fit1_all, fit1_ctry$reg[[1]], fit1_ctry$reg[[2]], fit1_ctry$reg[[3]], 
              fit2_all, fit2_ctry$reg[[1]], fit2_ctry$reg[[2]], fit2_ctry$reg[[3]]),
         elm),
  custom.header = list("Pooled SQI"=1, "Country SQI"=2:4, "Pooled SQI"=5, "Country SQI"=6:8),
  custom.model.names = rep(c("All", "Kenya", "Ghana", "Bangladesh"), 2),
  custom.coef.map = list(
    "improved_repYes"="Improved technology (ref=\\textit{No})",
    "sharingShared"="Shared cubicle (ref=\\textit{No})",
    "interface_ctryPit latrine (with slab)" = "Pit latrine (with slab)",
    "interface_ctryPit latrine (no slab)/other" = "Pit latrine (no slab)/other",
    "interface_poolPit latrine (with slab)" = "Pit latrine (with slab)",
    "interface_poolPit latrine (no slab)/other" = "Pit latrine (no slab)/other",
    "outflow_poolPit" = "Pit",                       
    "outflow_poolElsewhere" = "Elsewhere",
    "hh_cubicle_fct2" = "2 HH",
    "hh_cubicle_fct3" = "3 HH",
    "hh_cubicle_fct4" = "4 HH",
    "hh_cubicle_fct5" = "5 HH",
    "hh_cubicle_fct6" = "6 HH",
    "hh_cubicle_fct7" = "7 HH",
    "hh_cubicle_fct8" = "8 HH",
    "hh_cubicle_fct9" = "9 HH",
    "hh_cubicle_fct10" = "10 HH",
    "hh_cubicle_fctmore than 10" = ">10 HH",
    "id_countryGhana" = "Ghana FE",
    "id_countryBangladesh" = "Bangladesh FE",
    "(Intercept)" = NA
  ),
  custom.note = paste("\\item[\\hspace{-5mm}] Standard errors are clustered on the compound level;",
                      "\\item[\\hspace{-5mm}] All pooled regressions include country fixed effects;",
                      "\\item[\\hspace{-5mm}] %stars."),
  groups = list("Technology (ref=\\textit{Flush})" = 3:4,
                "Outflow (ref=\\textit{Piped sewer/septic tank})" = 5:6,
                "Sharing HHs (ref=\\textit{1 HH})" = 7:16),
  caption = "OLS regression results of SQI on common indicators",
  caption.above = T,
  label = "tab:reg_tech_hh",
  use.packages = F,
  #fontsize = "tiny",
  booktabs = T,
  no.margin = T, 
  threeparttable = T,
  table = F
) %>% 
  cat(file = "./tables/reg_current.tex")

# regression table for all indicators
texreg(
  lapply(
    list(fit3_all, fit3_ctry$reg[[1]], fit3_ctry$reg[[2]], fit3_ctry$reg[[3]]),
    elm),
  custom.header = list("Pooled SQI"=1, "Country SQI"=2:4),
  custom.model.names = c("All", "Kenya", "Ghana", "Bangladesh"),
  custom.coef.map = list(
    "interface_poolPit latrine (with slab)" = "Pit latrine (with slab)",
    "interface_poolPit latrine (no slab)/other" = "Pit latrine (no slab)/other",
    "interface_ctryPit latrine (with slab)" = "Pit latrine (with slab)",
    "interface_ctryPit latrine (no slab)/other" = "Pit latrine (no slab)/other",
    "outflow_poolPit" = "Pit",                       
    "outflow_poolElsewhere" = "Elsewhere",
    "hh_cubicle_fct2" = "2 HH",
    "hh_cubicle_fct3" = "3 HH",
    "hh_cubicle_fct4" = "4 HH",
    "hh_cubicle_fct5" = "5 HH",
    "hh_cubicle_fct6" = "6 HH",
    "hh_cubicle_fct7" = "7 HH",
    "hh_cubicle_fct8" = "8 HH",
    "hh_cubicle_fct9" = "9 HH",
    "hh_cubicle_fct10" = "10 HH",
    "hh_cubicle_fctmore than 10" = ">10 HH",
    "locationInside compound/on plot" = "Inside compound",
    "locationElsewhere" = "Outside compound",
    "waterImproved on premises" = "Water on premises (ref=\\textit{No})",
    "lightingYes" = "Lighting (ref=\\textit{No})",
    "lockOnly inside" = "Only inside",
    "lockOnly outside" = "Only outside",
    "lockOutside and inside" = "Outside and inside",
    "tilingYes" = "Tiling (ref=\\textit{No})",
    "gender_separatedYes" = "Gender separated (ref=\\textit{No})",
    "cleaning_rotaYes" = "Cleaning rota (ref=\\textit{No})",
    "relationship_ctryOther tenants/unknown" = "Other tenants/unknown",
    "age_toilet1-3 years" = "1-3",
    "age_toilet4-6 years" = "4-6",
    "age_toilet7-9 years" = "7-9",
    "age_toilet>9 years / don't know" = ">9",
    "landlord_on_plotYes" = "Landlord on plot (ref=\\textit{No})",
    "bin_insideYes" = "Bin inside cubicle (ref=\\textit{No})",
    "id_countryGhana" = "Ghana FE",
    "id_countryBangladesh" = "Bangladesh FE",
    "(Intercept)" = NA
  ),
  custom.note = paste("\\item[\\hspace{-5mm}] Standard errors are clustered on the compound level.",
                      "\\item[\\hspace{-5mm}] All pooled regressions include country fixed effects.",
                      "\\item[\\hspace{-5mm}] %stars."),
  groups = list("Technology (ref=\\textit{Flush})" = 1:2,
                "Outflow (ref=\\textit{Piped sewer/septic tank})" = 3:4,
                "Sharing HHs (ref=\\textit{1 HH})" = 5:14,
                "Location (ref=\\textit{Inside dwelling})" = 15:16,
                "Lockable door (ref=\\textit{Not lockable})" = 19:21,
                "User relationship (ref=\\textit{Relatives/close neighbors})" = 25,
                "Toilet age, years (ref=\\textit{<1})" = 26:29
  ),
  caption = "OLS regression results of SQI on common and alternative indicators",
  caption.above = T,
  label = "tab:reg_all",
  use.packages = F,
  fontsize = "scriptsize",
  booktabs = T,
  longtable = T,
  threeparttable = T,
  no.margin = T) %>% 
  cat(file = "./tables/reg_all.tex")

##########################
# New service level tables
#define new sanitation levels
df %<>% 
  mutate(
    level_standard = case_when(
      hh_cubicle_num < 2 & technology_rep %in% c("Flush to sewer/septic/pit",
                                                 "Pit latrine (with slab)") ~ "Basic",
      hh_cubicle_num >= 2 & technology_rep %in% c("Flush to sewer/septic/pit",
                                                  "Pit latrine (with slab)") ~ "Limited",
      technology_rep %in% c("Flush to elsewhere",
                            "Pit latrine (no slab)/other") ~ "Unimproved",
      TRUE ~ NA_character_
    ),
    level_tech = case_when(
      hh_cubicle_num < 2 & technology_rep %in% c("Flush to sewer/septic/pit",
                                                 "Flush to elsewhere") ~ "Basic",
      hh_cubicle_num >= 2 & technology_rep %in% c("Flush to sewer/septic/pit",
                                                  "Flush to elsewhere") ~ "Limited",
      technology_rep %in% c("Pit latrine (with slab)",
                            "Pit latrine (no slab)/other") ~ "Unimproved",
      TRUE ~ NA_character_
    ),
    level_hh4 = case_when( # new level
      hh_cubicle < 4 & technology_rep %in% c("Flush to sewer/septic/pit",
                                             "Pit latrine (with slab)") ~ "Basic",
      hh_cubicle >= 4 & technology_rep %in% c("Flush to sewer/septic/pit",
                                              "Pit latrine (with slab)") ~ "Limited",
      technology_rep %in% c("Flush to elsewhere",
                            "Pit latrine (no slab)/other") ~ "Unimproved",
      TRUE ~ NA_character_
    ),
    level_hh4_tech = case_when( # new level
      hh_cubicle < 4 & technology_rep %in% c("Flush to sewer/septic/pit",
                                             "Flush to elsewhere") ~ "Basic",
      hh_cubicle >= 4 & technology_rep %in% c("Flush to sewer/septic/pit",
                                              "Flush to elsewhere") ~ "Limited",
      technology_rep %in% c("Pit latrine (with slab)",
                            "Pit latrine (no slab)/other") ~ "Unimproved",
      TRUE ~ NA_character_
    ),
    level_alt = case_when(
      technology_rep %in% c("Flush to sewer/septic/pit",
                            "Pit latrine (with slab)") & (lock != "No lock" & location != "Elsewhere" & nightlight_rep =="Yes") ~ "Basic",
      technology_rep %in% c("Flush to sewer/septic/pit",
                            "Pit latrine (with slab)") & (lock == "No lock" | location == "Elsewhere" | nightlight_rep =="No") ~ "Limited",
      technology_rep %in% c("Flush to elsewhere",
                            "Pit latrine (no slab)/other") ~ "Unimproved",
      TRUE ~ NA_character_
    ),
    level_alt_tech = case_when(
      technology_rep %in% c("Flush to sewer/septic/pit",
                            "Flush to elsewhere") & (lock != "No lock" & location != "Elsewhere" & nightlight_rep =="Yes") ~ "Basic",
      technology_rep %in% c("Flush to sewer/septic/pit",
                            "Flush to elsewhere") & (lock == "No lock" | location == "Elsewhere" | nightlight_rep =="No") ~ "Limited",
      technology_rep %in% c("Pit latrine (with slab)",
                            "Pit latrine (no slab)/other") ~ "Unimproved",
      TRUE ~ NA_character_
    )
  )

# frequency by level and country
newlevels <- df %>% 
  select(id_country, 
         level_standard,
         level_tech,
         level_hh4,
         level_hh4_tech,
         level_alt,
         level_alt_tech,
         sqi) %>% 
  pivot_longer(cols=-c("id_country", "sqi"), names_to = "Specification", values_to = "Level") %>% 
  group_by(id_country, Specification, Level) %>% 
  summarise(mean_sqi = round(mean(sqi), 1), n = n(), .groups = "keep") %>%  
  group_by(id_country, Specification) %>% 
  mutate(prop = round(n/sum(n)*100, 0)) %>% 
  select(-n) %>%  
  pivot_wider(names_from = id_country, values_from = c("mean_sqi","prop")) %>%
  ungroup() %>% 
  mutate_at(vars(starts_with("prop")), ~ paste0("(", ., "\\%)")) %>% 
  unite("Kenya", ends_with("Kenya"), sep = " ") %>% 
  unite("Ghana", ends_with("Ghana"), sep = " ") %>% 
  unite("Bangladesh", ends_with("Bangladesh"), sep = " ") %>% 
  mutate(techornah = case_when(
    Specification %in% c("level_standard", "level_hh4", "level_alt") ~ "Standard",
    TRUE ~ "Tech"
  ),
  Specification = recode_factor(Specification, 
                                level_standard = "JMP indicators",
                                level_tech = "JMP indicators",
                                level_hh4 = "Expanded JMP (3HH)",
                                level_hh4_tech = "Expanded JMP (3HH)",
                                level_alt = "Location + Lock + Lighting (LLL)",
                                level_alt_tech = "Location + Lock + Lighting (LLL)"
  )
  ) %>% 
  pivot_wider(names_from = "techornah", values_from = c("Kenya", "Ghana", "Bangladesh")) %>% 
  arrange(Specification) %>% 
  select(-Specification) %>% 
  kable(format = "latex", 
        align =  "lcccccc",
        col.names = c("Level specification", "Standard Technology", "Alternative Technology", "Standard Technology", "Alternative Technology", "Standard Technology", "Alternative Technology"),
        caption = "Mean SQI and frequency by sanitation level specification",
        label = "newlevels",
        escape = FALSE,
        booktabs = TRUE,
        position = "ht"
  ) %>% 
  kable_styling(latex_options = "scale_down") %>% 
  add_header_above(c(" " = 1,  "Kenya" = 2, "Ghana" = 2, "Bangladesh" = 2)) %>% 
  pack_rows(index = c("JMP indicators" = 3, 
                      "Expanded JMP (3HH)" = 3,
                      "Location + Lock + Lighting (LLL)" = 3))
cat(newlevels, file = "./tables/freqlevels.tex")

# Graphically show the resuls from this table
alt_levels <- df %>% 
  select(id_country, 
         level_standard,
         level_tech,
         level_hh4,
         level_hh4_tech,
         level_alt,
         level_alt_tech,
         sqi_ctry) %>% 
  pivot_longer(cols=-c("id_country", "sqi_ctry"), names_to = "spec_raw", values_to = "Level") %>% 
  group_by(id_country, spec_raw, Level) %>% 
  summarise(mean_sqi = round(mean(sqi_ctry), 1), freq = n(), ssd=sd(sqi_ctry), .groups="keep") %>%
  mutate(se_sqi = ssd / sqrt(freq),
         lower_ci = lower_ci(mean_sqi, se_sqi, freq),
         upper_ci = upper_ci(mean_sqi, se_sqi, freq)) %>% 
  group_by(id_country, spec_raw) %>% 
  mutate(prop = freq/sum(freq)*100,
         Specification = case_when(
           spec_raw %in% c("level_tech",
                           "level_hh4_tech",
                           "level_alt_tech") ~ "Alternative Technology",
           spec_raw %in% c("level_standard",
                           "level_hh4",
                           "level_alt") ~ "Standard Technology"
         ), 
         spec_raw = recode_factor(spec_raw, 
                                  level_standard = "JMP indicators",
                                  level_tech = "JMP indicators",
                                  level_hh4 = "3HH",
                                  level_hh4_tech = "3HH",
                                  level_alt = "LLL",
                                  level_alt_tech = "LLL")) 

alt_levels %>% 
  ggplot() +
  geom_point(aes(x=fct_rev(spec_raw), 
                 y=mean_sqi, 
                 shape=Level,
                 colour=Level), size = 2) +
  geom_linerange(aes(x=fct_rev(spec_raw), 
                     ymax=upper_ci, 
                     ymin=lower_ci)) +
  labs(x = "Specification", 
       y="Mean Country SQI", 
       colour ="Service Level", 
       shape = "Service Level") +
  facet_grid(fct_rev(Specification) ~ id_country) +
  scale_colour_viridis_d(direction = -1)  +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(size=10, family="LM Roman 10")) +
  ggsave("figures/newlevels.eps", device = cairo_ps, width = 119, height = 73, units = "mm")

alt_levels %>%   
  ggplot() +
  geom_col(aes(x=fct_rev(spec_raw),
               y=prop,
               fill = Level,)) +
  labs(x = "Specification", 
       y="Percent of facilities", 
       fill="Service Level") +
  facet_grid(fct_rev(Specification) ~ id_country) +
  scale_fill_viridis_d(direction = -1) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(size=10, family="LM Roman 10")) +
  ggsave("figures/freqlevels.eps", device = cairo_ps, width = 119, height = 73, units = "mm")    

# Make description table of alternative specification of levels
tibble(
  Specification = c(rep("JMP indicators",5), 
                    rep("Expanded JMP (3HH)", 5), 
                    rep("Location + Lock + Lighting (LLL)", 9)),
  Level = c(rep(c("Basic", "", "Limited", "", "Unimproved"), 2), "Basic", "", "", "", "Limited", "", "", "", "Unimproved"),
  "Standard Technology" = c("Improved  technology$^\\text{a}$", "1 HH",
                            "Improved technology", "$>$ 1 HH",
                            "Unimproved technology$^\\text{b}$",
                            "Improved technology", "1-3 HH",
                            "Improved technology", "$>$ 3 HH",
                            "Unimproved technology",
                            "Improved technology", "Inside/next to compound","Outside and/or inside lock","Lighting",
                            "Improved technology", "Elsewhere \\textit{or}","No lock \\textit{or}","No lighting",
                            "Unimproved technology"),
  "Alternative Technology" = c("Flush/pour-flush", "1 HH",
                               "Flush/pour-flush", "$>$ 1 HH",
                               "Pit latrine/other",
                               "Flush/pour-flush", "1-3 HH",
                               "Flush/pour-flush", "$>$ 3 HH",
                               "Pit latrine/other",
                               "Flush/pour-flush", "Inside/next to compound","Outside and/or inside lock","Lighting",
                               "Flush/pour-flush", "Elsewhere \\textit{or}","No lock \\textit{or}","No lighting",
                               "Pit latrine/other")
) %>% 
  select(Level, "Standard Technology", "Alternative Technology") %>% 
  kbl(format = "latex",
      digits = 2,
      row.names = F,
      align = "lll",
      caption = "Specifications of current and refined sanitation service levels",
      label = "spec_levels",
      booktabs = T,
      position = "ht", 
      linesep = "",
      escape = F) %>% 
  kable_styling(latex_options = "striped", stripe_index = c(1,2, 5, 8:9, 11:14, 19)) %>% 
  pack_rows("JMP indicators", 1, 5) %>%
  pack_rows("Expanded JMP (3HH)", 6, 10) %>%
  pack_rows("Location + Lock + Lighting (LLL)", 11, 19) %>% 
  footnote(alphabet = c("Flush to sewer/septic/pit, pit latrine (with slab);",
                        "Flush to elsewhere, pit latrine (no slab)/other")) %>% 
  cat(file = "./tables/spec_levels.tex")


#########################
# Appendix
########################
# Figure of reliability of coding data
df %>%  
  mutate(across(starts_with("cleanliness_"), 
                ~fct_recode(.x, 
                            "Very clean" = "Very clean", 
                            "Clean" = "Clean", 
                            "Neither/nor" = "Neither clean nor dirty", 
                            "Dirty" = "Dirty",
                            "Very dirty" = "Very dirty")
  ),
  across(starts_with("cleanliness_"),
         ~ fct_relevel(.x, 
                       "Very dirty", "Dirty", "Neither/nor", "Clean", "Very clean")
  )) %>% 
  select(cleanliness_rep, "Observed" = cleanliness_obs, "Remote"= cleanliness_cod) %>% 
  pivot_longer(cols = -cleanliness_rep) %>% 
  count(cleanliness_rep, name, value) %>%
  filter(!is.na(value)) %>% 
  group_by(cleanliness_rep, name) %>% 
  mutate(pct = n/sum(n)) %>% 
  ggplot(aes(x=cleanliness_rep, y=pct ,fill=factor(value)))+
  geom_col()+
  facet_grid(.~name) +
  scale_fill_viridis_d() +
  labs(x = "Reported cleanliness", y = "Proportion", fill = "Observed/remote cleanliness") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(size=10, family="LM Roman 10")) +
  ggsave("figures/reliability.eps", device = cairo_ps, width = 119, height = 73, units = "mm")

#########################
# Robustness checks for SQI vs additive measure

# save variables in a formula
fmla_add_all <- as.formula(paste("sqi_add ~ ", paste(append(variables, "id_country"), collapse= "+")))
fmla_add <- as.formula(paste("sqi_add ~ ", paste(variables_ct, collapse= "+")))
model1_add <- function(.data) lm_robust(sqi_add ~ improved_rep + sharing, data = .data, clusters = .data$id_compound, se_type = "stata")
model2_add <- function(.data) lm_robust(sqi_add ~ interface_ctry + outflow_pool + hh_cubicle_fct, data = .data, clusters = .data$id_compound, se_type = "stata")
model3_add <- function(.data) lm_robust(fmla_add, data = .data, clusters = .data$id_compound, se_type = "stata")

# fit all regression models using predefined functions
fit3_add_all <- lm_robust(fmla_add_all, 
                          data = df, 
                          clusters = id_compound, 
                          se_type = "stata")
fit3_add <- df %>%
  group_by(id_country) %>%
  nest() %>%
  mutate(reg = map(data, model3_add))

# regression table for all indicators
texreg(
  lapply(
    list(fit3_add_all, fit3_add$reg[[1]], fit3_add$reg[[2]], fit3_add$reg[[3]]),
    elm),
  custom.header = list("Additive SQI"=1:4),
  custom.model.names = c("All", "Kenya", "Ghana", "Bangladesh"),
  custom.coef.map = list(
    "interface_poolPit latrine (with slab)" = "Pit latrine (with slab)",
    "interface_poolPit latrine (no slab)/other" = "Pit latrine (no slab)/other",
    "interface_ctryPit latrine (with slab)" = "Pit latrine (with slab)",
    "interface_ctryPit latrine (no slab)/other" = "Pit latrine (no slab)/other",
    "outflow_poolPit" = "Pit",                       
    "outflow_poolElsewhere" = "Elsewhere",
    "hh_cubicle_fct2" = "2 HH",
    "hh_cubicle_fct3" = "3 HH",
    "hh_cubicle_fct4" = "4 HH",
    "hh_cubicle_fct5" = "5 HH",
    "hh_cubicle_fct6" = "6 HH",
    "hh_cubicle_fct7" = "7 HH",
    "hh_cubicle_fct8" = "8 HH",
    "hh_cubicle_fct9" = "9 HH",
    "hh_cubicle_fct10" = "10 HH",
    "hh_cubicle_fctmore than 10" = ">10 HH",
    "locationInside compound/on plot" = "Inside compound",
    "locationElsewhere" = "Outside compound",
    "waterImproved on premises" = "Water on premises (ref=\\textit{No})",
    "lightingYes" = "Lighting (ref=\\textit{No})",
    "lockOnly inside" = "Only inside",
    "lockOnly outside" = "Only outside",
    "lockOutside and inside" = "Outside and inside",
    "tilingYes" = "Tiling (ref=\\textit{No})",
    "gender_separatedYes" = "Gender separated (ref=\\textit{No})",
    "cleaning_rotaYes" = "Cleaning rota (ref=\\textit{No})",
    "relationship_ctryOther tenants/unknown" = "Other tenants/unknown",
    "age_toilet1-3 years" = "1-3",
    "age_toilet4-6 years" = "4-6",
    "age_toilet7-9 years" = "7-9",
    "age_toilet>9 years / don't know" = ">9",
    "landlord_on_plotYes" = "Landlord on plot (ref=\\textit{No})",
    "bin_insideYes" = "Bin inside cubicle (ref=\\textit{No})",
    "id_countryGhana" = "Ghana FE",
    "id_countryBangladesh" = "Bangladesh FE",
    "(Intercept)" = NA
  ),
  custom.note = paste("\\item[\\hspace{-5mm}] Standard errors are clustered on the compound level.",
                      "\\item[\\hspace{-5mm}] Pooled regression includes country fixed effects.",
                      "\\item[\\hspace{-5mm}] %stars."),
  groups = list("Technology (ref=\\textit{Flush})" = 1:2,
                "Outflow (ref=\\textit{Piped sewer/septic tank})" = 3:4,
                "Sharing HHs (ref=\\textit{1 HH})" = 5:14,
                "Location (ref=\\textit{Inside dwelling})" = 15:16,
                "Lockable door (ref=\\textit{Not lockable})" = 19:21,
                "User relationship (ref=\\textit{Relatives/close neighbors})" = 25,
                "Toilet age, years (ref=\\textit{<1})" = 26:29),
  caption = "OLS regression results of additive SQI on indicators",
  caption.above = T,
  label = "tab:reg_add_ctry",
  use.packages = F,
  fontsize = "footnotesize",
  booktabs = T,
  longtable = T,
  threeparttable = T,
  no.margin = T) %>% 
  cat(file = "./tables/reg_robust.tex")

# Figure of reliability of coding data
df %>%  
  mutate(across(starts_with("cleanliness_"), 
                ~fct_recode(.x, 
                            "Very clean" = "Very clean", 
                            "Clean" = "Clean", 
                            "Neither/nor" = "Neither clean nor dirty", 
                            "Dirty" = "Dirty",
                            "Very dirty" = "Very dirty")
  ),
  across(starts_with("cleanliness_"),
         ~ fct_relevel(.x, 
                       "Very dirty", "Dirty", "Neither/nor", "Clean", "Very clean")
  )) %>% 
  select(cleanliness_rep, "Observed" = cleanliness_obs, "Remote"= cleanliness_cod) %>% 
  pivot_longer(cols = -cleanliness_rep) %>% 
  count(cleanliness_rep, name, value) %>%
  filter(!is.na(value)) %>% 
  group_by(cleanliness_rep, name) %>% 
  mutate(pct = n/sum(n)) %>% 
  ggplot(aes(x=cleanliness_rep, y=pct ,fill=factor(value)))+
  geom_col()+
  facet_grid(.~name) +
  scale_fill_viridis_d() +
  labs(x = "Reported cleanliness", y = "Proportion", fill = "Observed/remote cleanliness") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(size=10, family="LM Roman 10")) +
  ggsave("figures/reliability.eps", device = cairo_ps, width = 119, height = 73, units = "mm")

# MCA RESULTS for appendix
tibble(Variable = names(weights), 
       Overall = weights, 
       Kenya = df_nest$weights[[1]], 
       Ghana = df_nest$weights[[2]], 
       Bangladesh = df_nest$weights[[3]]) %>% 
  mutate(Variable = recode(Variable,
                           "visible_feces_Yes" = "Visible feces: yes",
                           "visible_feces_No" = "Visible feces: no",
                           "insects_obs_Yes" = "Insects: yes",
                           "insects_obs_No" = "Insects: no",
                           "solidwaste_obs_Yes" = "Solid waste: yes",
                           "solidwaste_obs_No" = "Solid waste: no",
                           "floor_Low quality" = "Floor material: low quality",
                           "floor_High quality" = "Floor material: high quality",
                           "roof_Low quality" = "Roof material: low quality" ,
                           "roof_High quality" = "Roof material: high quality",
                           "wall_Low quality" = "Wall material: low quality",
                           "wall_High quality" = "Wall material: high quality",
                           "full_clogged_Yes" = "Toilet full/clogged: yes",
                           "full_clogged_No" = "Toilet full/clogged: no",
                           "No facility with soap" = "Handwashing station with soap: no",
                           "Facility with soap" = "Handwashing station with soap: yes")) %>% 
  kbl(format = "latex",
      digits = 2,
      row.names = F,
      align = "lcccc",
      caption = "Statistical weights for the construction of the SQI",
      label = "weights",
      booktabs = T,
      position = "ht", 
      linesep = "") %>% 
  cat(file = "./tables/weights.tex")


# screeplots of MCA
scree1 <- fviz_screeplot(mca_out, ylim = c(0, 40)) + 
  labs(x = "Dimensions", y = "Percentage of explained variance", title = "Pooled sample") +
  theme(text = element_text(size=10, family="LM Roman 10")) 
scree2 <- fviz_screeplot(df_nest$mca.out[[1]], ylim = c(0, 40)) + 
  labs(x = "Dimensions", y = "Percentage of explained variance", title = "Kenya") +
  theme(text = element_text(size=10, family="LM Roman 10")) 
scree3 <- fviz_screeplot(df_nest$mca.out[[2]], ylim = c(0, 40)) + 
  labs(x = "Dimensions", y = "Percentage of explained variance", title = "Ghana") +
  theme(text = element_text(size=10, family="LM Roman 10")) 
scree4 <- fviz_screeplot(df_nest$mca.out[[3]], ylim = c(0, 40)) + 
  labs(x = "Dimensions", y = "Percentage of explained variance", title = "Bangladesh") +
  theme(text = element_text(size=10, family="LM Roman 10"))

ggarrange(scree1, scree2, scree3, scree4,  ncol = 2, nrow = 2) +
  ggsave("figures/scree_pooled.eps", device = cairo_ps,  width = 119, height = 119, units = "mm")


