##################################################################
#
# Quantifying the global burden of mental illness and its economic value
<<<<<<< HEAD
# March 24, 2022
=======
# October 28, 2021
>>>>>>> 53996c3571acf22ca7c251859011dc4222c2d3fa
#
##################################################################

######################
##   HOUSEKEEPING   ##
######################

# Loading libraries
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(sp)
library(broom)
library(sf)
library(scales)
library(viridis)
library(data.table)
library(ggpubr)
library(rnaturalearth)
library(rnaturalearthdata)

# Setting directories - change these to your local directory
<<<<<<< HEAD
path <- "C:/Users/danie/Dropbox (Harvard University)/Important Files/3. PhD/WQE/WQE II/gmh_econ/"
=======
path <- "~/3. PhD/WQE/WQE II/gmh_econ"
>>>>>>> 53996c3571acf22ca7c251859011dc4222c2d3fa
datapath <- paste(path,'data', sep = "/")
resultspath <- paste(path,'results', sep = "/")

######################
##       DATA       ##
######################

# Loading IHME Global Burden of Disease Data

# The data are available for download at http://ghdx.healthdata.org/gbd-results-tool
# Data should be downloaded for DALYs, YLLs, YLDs, deaths, and prevalence by cause and geographies
# Save the file as IHME-GBD_2019_DATA-2019.csv
# If prevalence data are saved separately, save it as IHME-GBD_2019_DATA-2019-prev.csv

data <- read.csv(file = file.path(datapath,"IHME-GBD_2019_DATA-2019.csv"))

# Adding alpha-3 codes from ISO 3166-1 (i.e., World Bank Codes)
ihme_crosswalk <- read_excel(path = file.path(datapath, "ihme_crosswalk.xlsx")) %>% 
  select(-c("location_name"))
data <- left_join(data, ihme_crosswalk, by = "location_id")

# Adding World Bank income classifications
income_level <- read_excel(path = file.path(datapath, "OGHIST.xls"), sheet = 2) %>% 
  select(-(c(2:34))) %>% 
  rename("income_level" = "2019")
data <- left_join(data, income_level, by = "iso_code")

# Adding population (backed out using IHME rate data for consistency)
data <- data %>% unique() %>% gather("numeric_name", "est", "val":"lower") 
data_number <- data %>% filter(metric_id == 1) %>% rename("number" = "est")
data_rate <- data %>% filter(metric_id == 3) %>% select("measure_id","location_id", "sex_id", "age_id", "cause_id", "year", "numeric_name", "est") %>% rename("rate" = "est")
data <- left_join(data_number, data_rate, by = c("measure_id","location_id", "sex_id", "age_id", "cause_id", "year", "numeric_name")) %>% 
  select(!c("metric_id", "metric_name")) %>%
  unique()
data$population_100k <- data$number/data$rate
data$population <- data$population_100k  * 100000

# Removing unused columns
data <- data %>% select(!c("rate"))  %>% 
  select(!c("sex_id", "sex_name", "age_id", "age_name", "year")) %>%
  filter(measure_id != 5)

# Clean up
rm(ihme_crosswalk, income_level, data_number, data_rate)


#######################################################
##  ORIGINAL AND 2016 REALLOCATION APPROACH  ##
##################################################

# VIGO EL AL. 2016 WEIGHTS: ALLOCATION OF THE FOLLOWING TO MENTAL HEALTH 
# (PERCENTS DENOTE AMOUNT REALLOCATED TO MENTAL HEALTH)

# Dementia - 100%
# Epilepsy - 100%
# Migraine - 100%
# Tension-type headache - 100%
# Self-harm - 100%
# Chronic pain syndrome currently attributed to musculoskeletal disorders - 33%


# Naming relevant IHME cause IDs
mental_disorder_cause_id <- 558
dementia_cause_id1 <- 543 # Alzheimer's disease and other dementias
dementia_cause_id2 <- 544 # Parkinson's disease
epilepsy_cause_id <- 545 # Idiopathic epilepsy
migraine_cause_id <- 547
tension_type_headache_cause_id <- 548
self_harm_cause_id <- 718
alcohol_use_disorders_cause_id <- 560
drug_use_disorders_cause_id <- 561
low_back_pain_cause_id <- 630
neck_pain_cause_id <- 631
other_musculoskeletal_cause_id <- 639
all_cause_id <- 294


# Original allocation

# Cause ID 558 reflects the GBD 2019 mental disorder allocation
# These lines of code retain this allocation and save it under "original value"
cause_id <- c(mental_disorder_cause_id)
original_weights <- c(1)
original_weights_df <- data.frame(cause_id, original_weights)
data_rev <- left_join(data, original_weights_df, by = c("cause_id"))
data_rev$original_weights[is.na(data_rev$original_weights)] <- 0
data_rev$original_value <- data_rev$number * data_rev$original_weights  

# 2016 reallocation

cause_id <- c(mental_disorder_cause_id,
              dementia_cause_id1,
              dementia_cause_id2,
              epilepsy_cause_id,
              migraine_cause_id,
              tension_type_headache_cause_id,
              self_harm_cause_id,
              alcohol_use_disorders_cause_id,
              drug_use_disorders_cause_id,
              low_back_pain_cause_id,
              neck_pain_cause_id,
              other_musculoskeletal_cause_id,
              mental_disorder_cause_id,
              dementia_cause_id1,
              dementia_cause_id2,
              epilepsy_cause_id,
              migraine_cause_id,
              tension_type_headache_cause_id,
              self_harm_cause_id,
              alcohol_use_disorders_cause_id,
              drug_use_disorders_cause_id,
              low_back_pain_cause_id,
              neck_pain_cause_id,
              other_musculoskeletal_cause_id,
              mental_disorder_cause_id,
              dementia_cause_id1,
              dementia_cause_id2,
              epilepsy_cause_id,
              migraine_cause_id,
              tension_type_headache_cause_id,
              self_harm_cause_id,
              alcohol_use_disorders_cause_id,
              drug_use_disorders_cause_id,
              low_back_pain_cause_id,
              neck_pain_cause_id,
              other_musculoskeletal_cause_id)

numeric_name <- c("val", "val", "val", "val", "val","val", "val", "val", "val", "val", "val","val",
                  "lower","lower","lower","lower","lower","lower","lower","lower","lower","lower","lower","lower",
                  "upper","upper","upper","upper","upper","upper","upper","upper","upper","upper","upper","upper")


# Allocations of low back pain, neck pain, and other musculoskeletal disorders is based on
# the following section of Vigo et al. 2016

# "Considering that a fraction of low back, neck pain,
# and 50% of other musculoskeletal pain potentially
# corresponds to chronic pain syndromes, and for the
# purposes of producing a more accurate estimation and
# stimulating debate, we assume given the limited data
# that one third (rather than zero percent, as it is now) of
# the disease burden of these pain syndromes is
# potentially attributable to mental disorders and explore
#  the effect on mental illness burden calculations..."

rev_2016_weights <- c(1,
                      1,
                      1,
                      1,
                      1,
                      1,
                      1,
                      1,
                      1,
                      2/6, # Allocating 1/3
                      2/6,
                      1/2*2/6, 
                      
                      1,
                      1,
                      1,
                      1,
                      1,
                      1,
                      1,
                      1,
                      1,
                      1/6, # Allocating 1/6
                      1/6,
                      1/2*1/6, 
                      
                      1,
                      1,
                      1,
                      1,
                      1,
                      1,
                      1,
                      1,
                      1,
                      3/6, # Allocating 1/2 
                      3/6,
                      1/2*3/6) 


rev_2016_weights_df <- data.frame(cause_id,numeric_name, rev_2016_weights)
data_rev <- left_join(data_rev, rev_2016_weights_df, by = c("cause_id", "numeric_name"))

rm(cause_id, numeric_name, rev_2016_weights)
data_rev$rev_2016_weights[is.na(data_rev$rev_2016_weights)] <- 0
data_rev$rev_2016_value <- data_rev$number * data_rev$rev_2016_weights  


############################
##  COMPOSITE APPROACH  ##
############################

# Due to the distributive property of the Population Attributable Fraction, a category-specific attribution fraction is estimated
# using the formula from Rockhill et al, 1998:https://pubmed.ncbi.nlm.nih.gov/9584027/  

natural_cause_id <- 409 #NCDs
unnatural_cause_id <- 687 # Injuries


cause_id <- c(natural_cause_id,
              unnatural_cause_id,
              natural_cause_id,
              unnatural_cause_id,
              natural_cause_id,
              unnatural_cause_id)


numeric_name <- c("val", "val", 
                  "lower","lower",
                  "upper","upper")

relative_risk <- c(1.8,
                   7.22,
                   1.71,
                   6.43,
                   1.88,
                   8.12)

relative_risk_df <- data.frame(cause_id,numeric_name, relative_risk)

data_rev <- left_join(data_rev, relative_risk_df, by = c("cause_id", "numeric_name"))

data_prev <- read.csv(file = file.path(datapath,"IHME-GBD_2019_DATA-2019-prev.csv"))
data_prev <- data_prev %>%
  gather("numeric_name", "prevalence", "val":"lower")  %>% 
  select(!c("sex_id", "sex_name", "age_id", "age_name", "year")) %>% filter(cause_id == mental_disorder_cause_id) %>% select("location_id", "numeric_name", "prevalence")
data_rev  <- left_join(data_rev, data_prev, by = c("location_id", "numeric_name"))

# Two approaches to calculating the PAF are presented here. Later, only one will be retained for the creation of graphs, tables, and charts.
# In Walker et al., the PAF formula numbered as formula 4 in Rockhill et al. is used, in which prevalence is meant to be the
# prevalence of mental disorders among cases (i.e., the deceased). The conventional formula, formula 2, takes prevalence as 
# the prevalence of mental disorders in the general population, but is not internally valid in cases of confounding.

data_rev$paf2 <-     (data_rev$prevalence  * ((data_rev$relative_risk - 1))) /   ((data_rev$prevalence  * ((data_rev$relative_risk - 1))+1))
data_rev$paf4 <-     data_rev$prevalence  * ((data_rev$relative_risk - 1) /    data_rev$relative_risk)

data_rev <- data_rev%>% gather("paf_formula", "paf", "paf2":"paf4") 
data_rev$paf[is.na(data_rev$paf)] <- 0
data_rev$rev_2015_value <- data_rev$number * data_rev$paf

# Combine original and re-allocated values by location, measure, PAF formula used, and numeric value (point estimate, and upper or lower bounds)
data_rev <- data_rev %>% 
  group_by(location_id, measure_id, numeric_name, paf_formula) %>% 
  mutate(rev_2016_value_agg = sum(rev_2016_value),  rev_2015_value_agg = sum(rev_2015_value),original_value_agg = sum(original_value)) %>%
  rename("measure_total" = number) %>%
  ungroup %>%
  filter(cause_id == "294") %>% select(!c("original_weights":"relative_risk", "paf":"rev_2015_value"))

# Clean up
rm(data_prev, relative_risk_df, original_weights_df, rev_2016_weights_df)


############################
##  COMPOSITE METHOD  ##
############################

# This method pulls YLLs from Walker et al. and YLDs from Vigo et al. together to re-estimate 
# DALYs attributable to mental disorders. 

# Draw in YLDs from Vigo et al.
data_rev$composite_ylds <-ifelse(data_rev$measure_id == 3, data_rev$rev_2016_value_agg, 0)

# Draw in YLLs from Walker et al.
data_rev$composite_ylls <-ifelse(data_rev$measure_id == 4, data_rev$rev_2015_value_agg, 0)

# Combine to calculate DALYs
data_rev_composite <- data_rev %>% select(location_id, numeric_name, paf_formula, measure_id, composite_ylds, composite_ylls) %>%
  group_by(location_id, numeric_name, paf_formula) %>%
  mutate(composite_dalys = sum(composite_ylds, composite_ylls))
data_rev_composite <- data_rev_composite %>% filter(measure_id == 2) %>% ungroup() %>% unique() %>% select("location_id", "numeric_name", "paf_formula", "composite_dalys") 
data_rev <- left_join(data_rev, data_rev_composite, by = c("location_id", "numeric_name", "paf_formula"))
data_rev$composite_dalys[data_rev$measure_id != 2] <- 0
data_rev$composite_ylds[data_rev$measure_id != 3] <- 0
data_rev$composite_ylls[data_rev$measure_id != 4] <- 0

# Draw in deaths from Walker et al.
data_rev$composite_deaths <-ifelse(data_rev$measure_id == 1, data_rev$rev_2015_value_agg, 0)
data_rev$composite_deaths[data_rev$measure_id != 1] <- 0


# Combined columns, clean up, re-organizing, tidying, and labeling
data_rev$composite <- data_rev$composite_ylds + data_rev$composite_ylls + data_rev$composite_dalys + data_rev$composite_deaths
data_rev <- data_rev %>% select(!c("composite_ylds":"composite_deaths")) %>%  relocate(original_value_agg, .after = paf_formula) %>%
  relocate(numeric_name, .after = prevalence) %>%  
  rename("Composite method" = composite,
         "PAF method" = rev_2015_value_agg,
         "2016 reallocation method" = rev_2016_value_agg,
         "GBD 2019" = original_value_agg)

data_rev$cause_name <- "Mental disorders"
data_rev <- data_rev %>% gather("estimate", "number", "GBD 2019":"Composite method")

# Clean up
rm(data_rev_composite,
   natural_cause_id,
   unnatural_cause_id,
   mental_disorder_cause_id,
   dementia_cause_id1,
   dementia_cause_id2,
   epilepsy_cause_id,
   migraine_cause_id,
   tension_type_headache_cause_id,
   self_harm_cause_id,
   other_musculoskeletal_cause_id,
   all_cause_id,
   cause_id,
   numeric_name,
   original_weights,
   alcohol_use_disorders_cause_id,
   drug_use_disorders_cause_id,
   neck_pain_cause_id,
   low_back_pain_cause_id,
   relative_risk)

data_rev$estimate_id <- ifelse(data_rev$estimate == "Composite method", 4, 
                               ifelse(data_rev$estimate == "PAF method", 3,
                                      ifelse(data_rev$estimate == "2016 reallocation method", 2, 1)))
data_rev <- data_rev %>% relocate(estimate_id, .after = estimate)

# Calculate rates and percent of burden
data_rev <- data_rev %>% relocate (population_100k, .after = population)
data_rev$rate_per_100k <- data_rev$number/data_rev$population_100k
data_rev$percent <- data_rev$number/data_rev$measure_total*100


############################
##        COSTING         ##
############################

# Import GDP
gdp_data <- read.csv(file = file.path(datapath,# "wdi_gdp_ppp.csv"  # Using PPP
                                 "wdi_gdp.csv"                  # Not using PPP
                                 )) %>%
  select(-(c(3:62))) %>% 
  rename("gdp" = "X2019")
gdp <- gdp_data %>% select(c(iso_code, gdp))
data_rev <- left_join(data_rev, gdp, by = "iso_code")
data_rev <- data_rev %>% relocate(gdp, .after = iso_code)
data_rev$gdp[data_rev$location_id == 1] <- gdp_data$gdp[gdp_data$iso_code == "WLD"] # GDP for the world

# Calculate GDP per capita, with reference to IHME population values, for consistency
data_rev$gdp_per_capita <- data_rev$gdp/data_rev$population
data_rev <- data_rev %>% relocate (gdp_per_capita, .after = gdp)

# Clean up
rm(gdp)

# Import GDP PPP
gdp_data <- read.csv(file = file.path(datapath, "wdi_gdp_ppp.csv"  # Using PPP
                                      #"wdi_gdp.csv"                  # Not using PPP
)) %>%
  select(-(c(3:62))) %>% 
  rename("gdp_ppp" = "X2019")
gdp <- gdp_data %>% select(c(iso_code, gdp_ppp))
data_rev <- left_join(data_rev, gdp, by = "iso_code")
data_rev <- data_rev %>% relocate(gdp_ppp, .after = iso_code)
data_rev$gdp_ppp[data_rev$location_id == 1] <- gdp_data$gdp_ppp[gdp_data$iso_code == "WLD"] # GDP for the world

# Calculate GDP per capita, with reference to IHME population values, for consistency
data_rev$gdp_ppp_per_capita <- data_rev$gdp_ppp/data_rev$population
data_rev <- data_rev %>% relocate (gdp_ppp_per_capita, .after = gdp_ppp)

# Clean up
rm(gdp)

############################
# Cost per DALY

# Method 1: Copenhagen Consensus 1 - $1,000
data_rev$cost_cc1 <- ifelse(data_rev$measure_id == 2,
                            data_rev$number * 1000, 0)

# Method 2: Copenhagen Consensus 2 - $5,000
data_rev$cost_cc2 <- data_rev$cost_cc1 * 5

# Method 3: WHO - GDP/capita
data_rev$cost_who1 <-ifelse(data_rev$measure_id == 2,
                            data_rev$number * data_rev$gdp_per_capita, 0)

# Method 4: WHO - GDP/capita * 3
data_rev$cost_who2 <- data_rev$cost_who1 * 3

<<<<<<< HEAD
# Method 5: WHO - GDP PPP/capita
data_rev$cost_who3 <-ifelse(data_rev$measure_id == 2,
                            data_rev$number * data_rev$gdp_ppp_per_capita, 0)

# Method 6: WHO - GDP PPP/capita * 3
data_rev$cost_who4 <- data_rev$cost_who3 * 3

##################################
##         PAF FORMULA          ##
##################################

=======
##################################
##         PAF FORMULA          ##
##################################

>>>>>>> 53996c3571acf22ca7c251859011dc4222c2d3fa
# Select which of the PAF formulae to use for the remainder of the analysis

data_rev <- data_rev %>% filter(paf_formula == "paf2") # Conventional formula
# data_rev <- data_rev %>% filter(paf_formula == "paf4") # Case prevalence formula


################################
##         MAP PREP           ##
################################

<<<<<<< HEAD
data_rev_gbd <- data_rev %>% filter(estimate_id == 1) %>% 
  select (c(measure_id, location_id, numeric_name, paf_formula, number:cost_who4)) %>%
=======
data_rev_gbd <- data_rev %>% filter(estimate_id == 1) %>% select (c(measure_id, location_id, numeric_name, paf_formula, number:cost_who2)) %>%
>>>>>>> 53996c3571acf22ca7c251859011dc4222c2d3fa
  rename(
    "number_gbd" = "number",
    "rate_per_100k_gbd" = "rate_per_100k",
    "percent_gbd" = "percent",
    "cost_cc1_gbd" = "cost_cc1",
    "cost_cc2_gbd" = "cost_cc2",
    "cost_who1_gbd" = "cost_who1",
<<<<<<< HEAD
    "cost_who2_gbd" = "cost_who2",
    "cost_who3_gbd" = "cost_who3",
    "cost_who4_gbd" = "cost_who4")
=======
    "cost_who2_gbd" = "cost_who2"    )
>>>>>>> 53996c3571acf22ca7c251859011dc4222c2d3fa

data_rev <- left_join(data_rev, data_rev_gbd, by = c("location_id", "measure_id", "numeric_name", "paf_formula"))

data_rev <- data_rev %>% mutate(
  number_diff = number - number_gbd,
  rate_per_100k_diff = rate_per_100k - rate_per_100k_gbd,
  percent_diff = percent - percent_gbd,
  cost_cc1_diff = cost_cc1 - cost_cc1_gbd,
  cost_cc2_diff = cost_cc2 - cost_cc2_gbd,
  cost_who1_diff = cost_who1 - cost_who1_gbd,
<<<<<<< HEAD
  cost_who2_diff = cost_who2 - cost_who2_gbd,
  cost_who3_diff = cost_who3 - cost_who3_gbd,
  cost_who4_diff = cost_who4 - cost_who4_gbd)
=======
  cost_who2_diff = cost_who2 - cost_who2_gbd)
>>>>>>> 53996c3571acf22ca7c251859011dc4222c2d3fa

# Import regional data
region <- read_excel(path = file.path(datapath, "regions.xlsx"))

# Merge
region <- region %>% select (iso_code, who_region, ihme_region)
data_rev <- full_join(data_rev, region, by = "iso_code")

# WHO region map prep

data_rev <- data_rev %>% group_by(estimate_id, measure_id, numeric_name, paf_formula, who_region) %>% 
  mutate(who_region_pop100k = sum(population_100k),
         who_region_number = sum(number),
         who_region_measure_total = sum(measure_total),
<<<<<<< HEAD
         who_region_gdp = sum(gdp, na.rm=TRUE),
         who_region_gdp_ppp = sum(gdp_ppp, na.rm=TRUE))%>%
=======
         who_region_gdp = sum(gdp, na.rm=TRUE))%>%
>>>>>>> 53996c3571acf22ca7c251859011dc4222c2d3fa
  mutate(who_region_rate_per_100k = who_region_number/who_region_pop100k,
         who_region_percent = who_region_number/who_region_measure_total*100,
         who_region_population = who_region_pop100k * 100000) %>%
  mutate(who_region_gdp_per_capita = who_region_gdp/who_region_population,
<<<<<<< HEAD
         who_region_gdp_ppp_per_capita = who_region_gdp_ppp/who_region_population,
=======
>>>>>>> 53996c3571acf22ca7c251859011dc4222c2d3fa
         who_region_cost_cc1 = ifelse(measure_id == 2,
                                      who_region_number * 1000, 0),
         who_region_cost_cc2 = who_region_cost_cc1 * 5,
         who_region_cost_who1 = ifelse(measure_id == 2,
                                       who_region_number * who_region_gdp_per_capita, 0),
<<<<<<< HEAD
         who_region_cost_who2 = who_region_cost_who1 * 3,
         who_region_cost_who3 = ifelse(measure_id == 2,
                                       who_region_number * who_region_gdp_ppp_per_capita, 0),
         who_region_cost_who4 = who_region_cost_who3 * 3) %>%
  ungroup()

data_rev_gbd_who_region<- data_rev %>% filter(estimate_id == 1) %>% 
  select (c(measure_id, location_id, numeric_name, paf_formula, who_region_number:who_region_cost_who4)) %>%
=======
         who_region_cost_who2 = who_region_cost_who1 * 3) %>%
  ungroup()

data_rev_gbd_who_region<- data_rev %>% filter(estimate_id == 1) %>% 
  select (c(measure_id, location_id, numeric_name, paf_formula, who_region_number:who_region_cost_who2)) %>%
>>>>>>> 53996c3571acf22ca7c251859011dc4222c2d3fa
  rename(
    "who_region_number_gbd" = "who_region_number",
    "who_region_rate_per_100k_gbd" = "who_region_rate_per_100k",
    "who_region_percent_gbd" = "who_region_percent",
    "who_region_cost_cc1_gbd" = "who_region_cost_cc1",
    "who_region_cost_cc2_gbd" = "who_region_cost_cc2",
    "who_region_cost_who1_gbd" = "who_region_cost_who1",
<<<<<<< HEAD
    "who_region_cost_who2_gbd" = "who_region_cost_who2",
    "who_region_cost_who3_gbd" = "who_region_cost_who3",
    "who_region_cost_who4_gbd" = "who_region_cost_who4"
  )  %>%
  select(measure_id, location_id, numeric_name, paf_formula, who_region_number_gbd, who_region_rate_per_100k_gbd, who_region_percent_gbd,
         who_region_cost_cc1_gbd, who_region_cost_cc2_gbd, who_region_cost_who1_gbd, 
         who_region_cost_who2_gbd, who_region_cost_who3_gbd, who_region_cost_who4_gbd)
=======
    "who_region_cost_who2_gbd" = "who_region_cost_who2"
  )  %>%
  select(measure_id, location_id, numeric_name, paf_formula, who_region_number_gbd, who_region_rate_per_100k_gbd, who_region_percent_gbd,
         who_region_cost_cc1_gbd, who_region_cost_cc2_gbd, who_region_cost_who1_gbd, who_region_cost_who2_gbd)
>>>>>>> 53996c3571acf22ca7c251859011dc4222c2d3fa

data_rev <- left_join(data_rev, data_rev_gbd_who_region, by = c("location_id", "measure_id", "numeric_name", "paf_formula"))

data_rev <- data_rev %>% mutate(
  who_region_number_diff = who_region_number - who_region_number_gbd,
  who_region_rate_per_100k_diff = who_region_rate_per_100k - who_region_rate_per_100k_gbd,
  who_region_percent_diff = who_region_percent - who_region_percent_gbd,
  who_region_cost_cc1_diff = who_region_cost_cc1 - who_region_cost_cc1_gbd,
  who_region_cost_cc2_diff = who_region_cost_cc2 - who_region_cost_cc2_gbd,
  who_region_cost_who1_diff = who_region_cost_who1 - who_region_cost_who1_gbd,
<<<<<<< HEAD
  who_region_cost_who2_diff = who_region_cost_who2 - who_region_cost_who2_gbd,
  who_region_cost_who3_diff = who_region_cost_who3 - who_region_cost_who3_gbd,
  who_region_cost_who4_diff = who_region_cost_who4 - who_region_cost_who4_gbd)
=======
  who_region_cost_who2_diff = who_region_cost_who2 - who_region_cost_who2_gbd
)
>>>>>>> 53996c3571acf22ca7c251859011dc4222c2d3fa

rm(data_rev_gbd_who_region)

# IHME region  map prep

data_rev <- data_rev %>% group_by(estimate_id, measure_id, numeric_name, paf_formula, ihme_region) %>% 
  mutate(ihme_region_pop100k = sum(population_100k),
         ihme_region_number = sum(number),
         ihme_region_measure_total = sum(measure_total),
<<<<<<< HEAD
         ihme_region_gdp = sum(gdp, na.rm=TRUE),
         ihme_region_gdp_ppp = sum(gdp_ppp, na.rm=TRUE))%>%
=======
         ihme_region_gdp = sum(gdp, na.rm=TRUE)
  ) %>%
>>>>>>> 53996c3571acf22ca7c251859011dc4222c2d3fa
  mutate(ihme_region_rate_per_100k = ihme_region_number/ihme_region_pop100k,
         ihme_region_percent = ihme_region_number/ihme_region_measure_total*100,
         ihme_region_population = ihme_region_pop100k * 100000) %>%
  mutate(ihme_region_gdp_per_capita = ihme_region_gdp/ihme_region_population,
<<<<<<< HEAD
         ihme_region_gdp_ppp_per_capita = ihme_region_gdp_ppp/ihme_region_population,
         ihme_region_cost_cc1 = ifelse(measure_id == 2,
                                      ihme_region_number * 1000, 0),
         ihme_region_cost_cc2 = ihme_region_cost_cc1 * 5,
         ihme_region_cost_who1 = ifelse(measure_id == 2,
                                       ihme_region_number * ihme_region_gdp_per_capita, 0),
         ihme_region_cost_who2 = ihme_region_cost_who1 * 3,
         ihme_region_cost_who3 = ifelse(measure_id == 2,
                                       ihme_region_number * ihme_region_gdp_ppp_per_capita, 0),
         ihme_region_cost_who4 = ihme_region_cost_who3 * 3) %>%
  ungroup()

data_rev_gbd_ihme_region<- data_rev %>% filter(estimate_id == 1) %>% 
  select (c(measure_id, location_id, numeric_name, paf_formula, ihme_region_number:ihme_region_cost_who4)) %>%
=======
         ihme_region_cost_cc1 = ifelse(measure_id == 2,
                                       ihme_region_number * 1000, 0),
         ihme_region_cost_cc2 = ihme_region_cost_cc1 * 5,
         ihme_region_cost_who1 = ifelse(measure_id == 2,
                                        ihme_region_number * ihme_region_gdp_per_capita, 0),
         ihme_region_cost_who2 = ihme_region_cost_who1 * 3) %>%
  ungroup()


data_rev_gbd_ihme_region<- data_rev %>% filter(estimate_id == 1) %>% 
  select (c(measure_id, location_id, numeric_name, paf_formula, ihme_region_number:ihme_region_cost_who2)) %>%
>>>>>>> 53996c3571acf22ca7c251859011dc4222c2d3fa
  rename(
    "ihme_region_number_gbd" = "ihme_region_number",
    "ihme_region_rate_per_100k_gbd" = "ihme_region_rate_per_100k",
    "ihme_region_percent_gbd" = "ihme_region_percent",
    "ihme_region_cost_cc1_gbd" = "ihme_region_cost_cc1",
    "ihme_region_cost_cc2_gbd" = "ihme_region_cost_cc2",
    "ihme_region_cost_who1_gbd" = "ihme_region_cost_who1",
<<<<<<< HEAD
    "ihme_region_cost_who2_gbd" = "ihme_region_cost_who2",
    "ihme_region_cost_who3_gbd" = "ihme_region_cost_who3",
    "ihme_region_cost_who4_gbd" = "ihme_region_cost_who4"
  )  %>%
  select(measure_id, location_id, numeric_name, paf_formula, ihme_region_number_gbd, ihme_region_rate_per_100k_gbd, ihme_region_percent_gbd,
         ihme_region_cost_cc1_gbd, ihme_region_cost_cc2_gbd, ihme_region_cost_who1_gbd, 
         ihme_region_cost_who2_gbd, ihme_region_cost_who3_gbd, ihme_region_cost_who4_gbd)
=======
    "ihme_region_cost_who2_gbd" = "ihme_region_cost_who2"
  )  %>%
  select(measure_id, location_id, numeric_name, paf_formula, ihme_region_number_gbd, ihme_region_rate_per_100k_gbd, ihme_region_percent_gbd,
         ihme_region_cost_cc1_gbd, ihme_region_cost_cc2_gbd, ihme_region_cost_who1_gbd, ihme_region_cost_who2_gbd)
>>>>>>> 53996c3571acf22ca7c251859011dc4222c2d3fa

data_rev <- left_join(data_rev, data_rev_gbd_ihme_region, by = c("location_id", "measure_id", "numeric_name", "paf_formula"))

data_rev <- data_rev %>% mutate(
  ihme_region_number_diff = ihme_region_number - ihme_region_number_gbd,
  ihme_region_rate_per_100k_diff = ihme_region_rate_per_100k - ihme_region_rate_per_100k_gbd,
  ihme_region_percent_diff = ihme_region_percent - ihme_region_percent_gbd,
  ihme_region_cost_cc1_diff = ihme_region_cost_cc1 - ihme_region_cost_cc1_gbd,
  ihme_region_cost_cc2_diff = ihme_region_cost_cc2 - ihme_region_cost_cc2_gbd,
  ihme_region_cost_who1_diff = ihme_region_cost_who1 - ihme_region_cost_who1_gbd,
<<<<<<< HEAD
  ihme_region_cost_who2_diff = ihme_region_cost_who2 - ihme_region_cost_who2_gbd,
  ihme_region_cost_who3_diff = ihme_region_cost_who3 - ihme_region_cost_who3_gbd,
  ihme_region_cost_who4_diff = ihme_region_cost_who4 - ihme_region_cost_who4_gbd)
=======
  ihme_region_cost_who2_diff = ihme_region_cost_who2 - ihme_region_cost_who2_gbd
)
>>>>>>> 53996c3571acf22ca7c251859011dc4222c2d3fa

rm(data_rev_gbd_ihme_region)

# Import map data
world <- ne_countries(scale = "medium", returnclass = "sf")
world$iso_code <- world$gu_a3
world$iso_code[world$adm0_a3 == "SDS"] <- "SSD"

# Re-order estimates: original, Vigo, Walker, composite

<<<<<<< HEAD
data_rev <- data_rev %>% 
  mutate(estimate_newlab = recode(estimate, 
    'GBD 2019' = "Original approach",                
    '2016 reallocation method' = "Reallocation method",
    'PAF method' = "PAF method",              
    'Composite method'= "Composite approach"  
  ))

data_rev$estimate_newlab <- factor(data_rev$estimate_newlab ,      # Reordering group factor levels
                                levels = c("Original approach",
                                           "Reallocation method",
                                           "PAF method",
                                           "Composite approach"))
=======
data_rev$estimate <- factor(data_rev$estimate,      # Reordering group factor levels
                                levels = c("GBD 2019",
                                           "2016 reallocation method",
                                           "PAF method",
                                           "Composite method"))
>>>>>>> 53996c3571acf22ca7c251859011dc4222c2d3fa
# Merging map and dataframe
data_rev_map <- full_join(world, data_rev, by = "iso_code") %>% filter(numeric_name == "val")

# Clean up
rm(world, data_rev_gbd)


# Standard subtitles, and captions
subtitle_1 <- "Value per DALY: $1,000"
subtitle_2 <- "Value per DALY: $5,000"
subtitle_3 <- "Value per DALY: GDP/capita"
subtitle_4 <- "Value per DALY: 3 X GDP/capita"
<<<<<<< HEAD
#caption <- "Source: Global Burden of Disease Study. Reallocation method from Vigo et al. 2016."
=======
caption <- "Source: Global Burden of Disease Study. Reallocation method from Vigo et al. 2016."
>>>>>>> 53996c3571acf22ca7c251859011dc4222c2d3fa

###############################
##        NATIONAL           ##
###############################

# Loops are used to create consistent maps at the national and regional levels

n = 1
mapping <- c("who_region", "ihme_region")
<<<<<<< HEAD
measures <- c(1,2,3,4)
=======
measures <- c(1,2)
>>>>>>> 53996c3571acf22ca7c251859011dc4222c2d3fa
metric <- c("rate_per_100k", "percent")
monetaryvalue <- c("cost_cc1", "cost_cc2", "cost_who1", "cost_who2")
comparison <- c("abs", "diff")

<<<<<<< HEAD

  for(k in metric){
    for (m in comparison){
      for(j in measures){
        
=======
for(k in metric){
  for(j in measures){
    for (m in comparison){
      
>>>>>>> 53996c3571acf22ca7c251859011dc4222c2d3fa
      data_rev_map %>% 
        filter(measure_id== j) %>% 
        filter(estimate_id %in%  c(ifelse(m =="diff","",1), 2,4)) %>% 
        ggplot() +
        geom_sf(mapping = aes(fill = get(paste0(k,ifelse(m =="diff", "_diff","")))), color = "black", size = 0.01) +
        theme(panel.grid.major = element_blank(), 
              panel.background = element_blank(),
              axis.title = element_blank(), 
              axis.text = element_blank(),
              axis.ticks = element_blank()) +
<<<<<<< HEAD
        labs(title=paste0(
          ifelse(m =="diff" && j==1 && k == "rate_per_100k", "Difference in number of deaths due to mental disorders per 100,000 capita, relative to GBD 2019",
                 ifelse(m =="diff" && j==2 && k == "rate_per_100k", "Difference in number of DALYs due to mental disorders per 100,000 capita, relative to GBD 2019",                      
                        ifelse(m =="diff" && j==3 && k == "rate_per_100k", "Difference in number of YLDs due to mental disorders per 100,000 capita, relative to GBD 2019",
                               ifelse(m =="diff" && j==4 && k == "rate_per_100k", "Difference in number of YLLs due to mental disorders per 100,000 capita, relative to GBD 2019",
                                      ifelse(m =="diff" && j==1 && k != "rate_per_100k", "Percentage point difference in percent of deaths due to mental disorders, relative to GBD 2019",
                                             ifelse(m =="diff" && j==2 && k != "rate_per_100k", "Percentage point difference in percent of DALYs due to mental disorders, relative to GBD 2019",                      
                                                    ifelse(m =="diff" && j==3 && k != "rate_per_100k", "Percentage point difference in percent of YLDs due to mental disorders, relative to GBD 2019",
                                                           ifelse(m =="diff" && j==4 && k != "rate_per_100k", "Percentage point difference in percent of YLLs due to mental disorders, relative to GBD 2019",
                                                                  paste0(ifelse(j==1,"Deaths",
                                                                                ifelse(j==2, "DALYs",
                                                                                       ifelse(j==3, "YLDs" ,"YLLs")))," due to mental disorders", paste0(ifelse(k=="rate_per_100k"," per 100,000 capita", ", % of total")))))))))))), 
             subtitle="2019",
             # caption=caption,
             fill=
               ifelse(j == 1,ifelse(k == "rate_per_100k", "Deaths", "% of deaths"),
                      ifelse(j == 2, ifelse(k == "rate_per_100k", "DALYs", "% of DALYs"),
                             ifelse(j == 3, ifelse(k == "rate_per_100k", "YLDs", "% of YLDs"), 
                                    ifelse(k == "rate_per_100k", "YLLs", "% of YLLs"))))) +
        scale_fill_distiller(palette = ifelse(k == "rate_per_100k","RdYlBu", "YlGn"),
                             direction = ifelse(k == "rate_per_100k",-1, 1)) +
        coord_sf(ndiscr = F) + 
        facet_grid(~estimate_newlab) 
      
      
      ggsave(filename = paste0("fig", n,"_",m,"_",j,".jpg"), plot = last_plot(), 
=======
        labs(title=paste0(ifelse(m =="diff" && j==1 && k == "rate_per_100k", 
                                 "Difference in number of deaths due to mental disorders per 100,000, relative to GBD 2019",
                                 ifelse(m =="diff" && j==2 && k == "rate_per_100k", 
                                        "Difference in number of DALYs due to mental disorders per 100,000, relative to GBD 2019",
                                        ifelse(m =="diff" && j==1 && k != "rate_per_100k", 
                                               "Percentage point difference in percent of deaths due to mental disorders, relative to GBD 2019",
                                               ifelse(m =="diff" && j==2 && k != "rate_per_100k",
                                                      "Percentage point difference in percent of DALYs due to mental disorders, relative to GBD 2019",
                                                      paste0(ifelse(j==1,"Deaths", "DALYs")," due to mental disorders", paste0(ifelse(k=="rate_per_100k"," per 100,000", ", % of total")))))))), 
             subtitle="2019",
             caption=caption,
             fill=
               ifelse(j == 1, ifelse(k == "rate_per_100k", "Deaths", "% of deaths"),
                      ifelse(k == "percent", "% of DALYs", "DALYs"))) +
        scale_fill_distiller(palette = ifelse(k == "rate_per_100k","RdYlBu", "YlGn"),
                             direction = ifelse(k == "rate_per_100k",-1, 1)) +
        coord_sf(ndiscr = F) + 
        facet_grid(~estimate) 
      
      
      ggsave(filename = paste0("fig", n,"_",m,".jpg"), plot = last_plot(), 
>>>>>>> 53996c3571acf22ca7c251859011dc4222c2d3fa
             path = resultspath,
             width = 10,
             height = 4)
      
    }
<<<<<<< HEAD

=======
    
>>>>>>> 53996c3571acf22ca7c251859011dc4222c2d3fa
    n <- n + 1
    
  }
}

<<<<<<< HEAD
for (m in comparison) {
  for (l in monetaryvalue) {
=======
n = 5

for (l in monetaryvalue) {
  for (m in comparison) {
>>>>>>> 53996c3571acf22ca7c251859011dc4222c2d3fa
    
    data_rev_map %>%
      filter(measure_id== 2) %>% 
      filter(estimate_id %in%  c(ifelse(m =="diff","",1), 2,4)) %>% 
      ggplot() +
      geom_sf(mapping = aes(fill = get(paste0(l, ifelse(m =="diff", "_diff","")))/ gdp * 100), color = "black", size = 0.01) +
      theme(panel.grid.major = element_blank(), 
            panel.background = element_blank(),
            axis.title = element_blank(), 
            axis.text = element_blank(),
            axis.ticks = element_blank()) +
      labs(title=ifelse(m =="diff", "Difference in value of DALYs due to mental disorders in current USD, percentage points of GDP", 
                        "Value of DALYs due to mental disorders in current USD, % of GDP"),
           subtitle=get(paste0("subtitle_",(n-4))),
<<<<<<< HEAD
           # caption=caption,
=======
           caption=caption,
>>>>>>> 53996c3571acf22ca7c251859011dc4222c2d3fa
           fill="% of GDP") +
      scale_fill_distiller(palette = "Reds", direction = 1,
                           limits = c(0,20),
                           oob = squish) +
<<<<<<< HEAD
      facet_grid(~estimate_newlab)
    
    ggsave(filename = paste0("fig", n,"_",m,"_",l,".jpg"), plot = last_plot(), 
=======
      facet_grid(~estimate)
    
    ggsave(filename = paste0("fig", n,"_",m,".jpg"), plot = last_plot(), 
>>>>>>> 53996c3571acf22ca7c251859011dc4222c2d3fa
           path = resultspath,
           width = 10,
           height = 4)
    
<<<<<<< HEAD
  n <- n + 1
    
  }
  
  n = 5
=======
    
  }
  n <- n + 1
>>>>>>> 53996c3571acf22ca7c251859011dc4222c2d3fa
}


###############################
##        REGIONAL           ##
###############################

n = 1

for(k in metric){
<<<<<<< HEAD
  for (m in comparison){
    for(j in measures){
=======
  for(j in measures){
    for (m in comparison){
>>>>>>> 53996c3571acf22ca7c251859011dc4222c2d3fa
      for (i in mapping) {
        
        data_rev_map %>% 
          filter(measure_id== j) %>% 
          filter(estimate_id %in%  c(ifelse(m =="diff","",1), 2,4)) %>% 
          ggplot() +
          geom_sf(mapping = aes(fill = get(paste0(i,"_",k,ifelse(m =="diff", "_diff","")))), color = "white", size = 0.01) +
          geom_sf(data = . %>%   group_by(get(i)) %>% st_set_precision(1e4) %>%
<<<<<<< HEAD
                    summarize(geometry = st_combine(geometry)), fill = "transparent", color = 'black', size = 0.01) +
=======
                    summarize(geometry = st_union(geometry)), fill = "transparent", color = 'black', size = 0.01) +
>>>>>>> 53996c3571acf22ca7c251859011dc4222c2d3fa
          theme(panel.grid.major = element_blank(), 
                panel.background = element_blank(),
                axis.title = element_blank(), 
                axis.text = element_blank(),
<<<<<<< HEAD
                axis.ticks = element_blank())  + 
            labs(title=paste0(
            ifelse(m =="diff" && j==1 && k == "rate_per_100k", "Difference in number of deaths due to mental disorders per 100,000 capita, relative to GBD 2019",
                   ifelse(m =="diff" && j==2 && k == "rate_per_100k", "Difference in number of DALYs due to mental disorders per 100,000 capita, relative to GBD 2019",                      
                          ifelse(m =="diff" && j==3 && k == "rate_per_100k", "Difference in number of YLDs due to mental disorders per 100,000 capita, relative to GBD 2019",
                                 ifelse(m =="diff" && j==4 && k == "rate_per_100k", "Difference in number of YLLs due to mental disorders per 100,000 capita, relative to GBD 2019",
                                        ifelse(m =="diff" && j==1 && k != "rate_per_100k", "Percentage point difference in percent of deaths due to mental disorders, relative to GBD 2019",
                                               ifelse(m =="diff" && j==2 && k != "rate_per_100k", "Percentage point difference in percent of DALYs due to mental disorders, relative to GBD 2019",                      
                                                      ifelse(m =="diff" && j==3 && k != "rate_per_100k", "Percentage point difference in percent of YLDs due to mental disorders, relative to GBD 2019",
                                                             ifelse(m =="diff" && j==4 && k != "rate_per_100k", "Percentage point difference in percent of YLLs due to mental disorders, relative to GBD 2019",
                                                                    paste0(ifelse(j==1,"Deaths",
                                                                                  ifelse(j==2, "DALYs",
                                                                                         ifelse(j==3, "YLDs" ,"YLLs")))," due to mental disorders", paste0(ifelse(k=="rate_per_100k"," per 100,000 capita", ", % of total")))))))))))), 
            subtitle="2019",
               # caption=caption,
            fill=
              ifelse(j == 1,ifelse(k == "rate_per_100k", "Deaths", "% of deaths"),
                     ifelse(j == 2, ifelse(k == "rate_per_100k", "DALYs", "% of DALYs"),
                            ifelse(j == 3, ifelse(k == "rate_per_100k", "YLDs", "% of YLDs"), 
                                   ifelse(k == "rate_per_100k", "YLLs", "% of YLLs"))))) +
          scale_fill_distiller(palette = ifelse(k == "rate_per_100k","RdYlBu", "YlGn"),
                               direction = ifelse(k == "rate_per_100k",-1, 1)) +
          coord_sf(ndiscr = F) + 
          facet_grid(~estimate_newlab) 
        
        ggsave(filename = paste0("fig", n,"_",i,"_",m,"_",j,".jpg"), plot = last_plot(), 
=======
                axis.ticks = element_blank()) +
          labs(title=paste0(ifelse(m =="diff" && j==1 && k == "rate_per_100k", 
                                   "Difference in number of deaths due to mental disorders per 100,000, relative to GBD 2019",
                                   ifelse(m =="diff" && j==2 && k == "rate_per_100k", 
                                          "Difference in number of DALYs due to mental disorders per 100,000, relative to GBD 2019",
                                          ifelse(m =="diff" && j==1 && k != "rate_per_100k", 
                                                 "Percentage point difference in percent of deaths due to mental disorders, relative to GBD 2019",
                                                 ifelse(m =="diff" && j==2 && k != "rate_per_100k",
                                                        "Percentage point difference in percent of DALYs due to mental disorders, relative to GBD 2019",
                                                        paste0(ifelse(j==1,"Deaths", "DALYs")," due to mental disorders", paste0(ifelse(k=="rate_per_100k"," per 100,000", ", % of total")))))))), 
               subtitle="2019",
               caption=caption,
               fill=
                 ifelse(j == 1, ifelse(k == "rate_per_100k", "Deaths", "% of deaths"),
                        ifelse(k == "percent", "% of DALYs", "DALYs"))) +
          scale_fill_distiller(palette = ifelse(k == "rate_per_100k","RdYlBu", "YlGn"),
                               direction = ifelse(k == "rate_per_100k",-1, 1)) +
          coord_sf(ndiscr = F) + 
          facet_grid(~estimate) 
        
        ggsave(filename = paste0("fig", n,"_",i,"_",m,".jpg"), plot = last_plot(), 
>>>>>>> 53996c3571acf22ca7c251859011dc4222c2d3fa
               path = resultspath,
               width = 10,
               height = 4)
        
      }
<<<<<<< HEAD
  }
  
    n <- n + 1
  
}
}

for (m in comparison) {
  for (l in monetaryvalue) {
=======
    }
    n <- n + 1
  }
}

n = 5

for (l in monetaryvalue) {
  for (m in comparison) {
>>>>>>> 53996c3571acf22ca7c251859011dc4222c2d3fa
    for (i in mapping) {
      
      data_rev_map %>%
        filter(measure_id== 2) %>% 
        filter(estimate_id %in%  c(ifelse(m =="diff","",1), 2,4)) %>% 
        ggplot() +
        geom_sf(mapping = aes(fill = get(paste0(i,"_",l, ifelse(m =="diff", "_diff","")))/ get(paste0(i,"_","gdp")) * 100), color = "white", size = 0.01) +
        geom_sf(data = . %>%   group_by(get(i)) %>% st_set_precision(1e4) %>%
<<<<<<< HEAD
                  summarize(geometry = st_combine(geometry)), fill = "transparent", color = 'black', size = 0.01) +
=======
                  summarize(geometry = st_union(geometry)), fill = "transparent", color = 'black', size = 0.01) +
>>>>>>> 53996c3571acf22ca7c251859011dc4222c2d3fa
        theme(panel.grid.major = element_blank(), 
              panel.background = element_blank(),
              axis.title = element_blank(), 
              axis.text = element_blank(),
              axis.ticks = element_blank()) +
        labs(title=ifelse(m =="diff", "Difference in value of DALYs due to mental disorders in current USD, percentage points of GDP, relative to GBD 2019", 
                          "Value of DALYs due to mental disorders in current USD, % of GDP"),
             subtitle=get(paste0("subtitle_",(n-4))),
<<<<<<< HEAD
             # caption=caption,
=======
             caption=caption,
>>>>>>> 53996c3571acf22ca7c251859011dc4222c2d3fa
             fill="% of GDP") +
        scale_fill_distiller(palette = "Reds", direction = 1,
                             limits = c(0,20),
                             oob = squish) +
<<<<<<< HEAD
        facet_grid(~estimate_newlab)
=======
        facet_grid(~estimate)
>>>>>>> 53996c3571acf22ca7c251859011dc4222c2d3fa
      
      ggsave(filename = paste0("fig", n,"_",i,"_",m,".jpg"), plot = last_plot(), 
             path = resultspath,
             width = 10,
             height = 4)
      
    }
<<<<<<< HEAD
    
    n <- n + 1
    
  }
  
  n = 5
  

=======
  }
  n <- n + 1
>>>>>>> 53996c3571acf22ca7c251859011dc4222c2d3fa
}

rm(data_rev_map)

####################################
##       REGIONAL CHARTS          ##
####################################

who_region_positions <- c("African Region", "Eastern Mediterranean Region", "European Region",
                          "Region of the Americas", "South-East Asia Region", "Western Pacific Region",  "#NA")

who_region_pal <-  c("#E74C3C", "#E67E22" , "#F4D03F", "#45B39D", "#5499C7", "#AF7AC5")

ihme_region_positions <- c("East Asia", "Southeast Asia", "Oceania", "Central Asia", "Eastern Europe", "Central Europe",
                           "Caribbean", "Central Latin America", "Tropical Latin America", "Andean Latin America", "North Africa and Middle East",
                           "Southern Sub-Saharan Africa", "Western Sub-Saharan Africa", "Central Sub-Saharan Africa", "Eastern Sub-Saharan Africa",
                           "South Asia", "Southern Latin America", "Western Europe", "High-income North America",
                           "Australasia", "High-income Asia Pacific", "#NA")

ihme_pal_new <- c("#002673", "#005CE6", "#73DFFF", "#E54800", "#E69800", "#FFD37F",
                  "#216F00", "#33A600", "#4CE600", "#A3FF73", "#4E4E4E", 
                  "#730000", "#FF0000", "#FF7F7F", "#FFBEBE", 
                  "#E6E600", "#73004C", "#A80084", "#C500FF", "#E600A9", "#FFBEE8", "#7F7F7F")

ihme_region_pal <- c("#8C95F1", "#D7ECFC" , "#B6CDD4", "#EAA144", "#F3DA89", "#F9EFC2", "#B1F585", "#B6F2D5",
                     "#E3FD8C", "#E2FDD7", "#99ACA4", "#9D5F56", "#E989A9", "#F5DBD8", "#F8EEEF",
                     "#F4FF7C", "#C42AF1", "#E986F8", "#D7C2FC", "#DD9BE9", "#FAE5FE", "#7F7F7F")

n = 9

<<<<<<< HEAD
  for (m in comparison){
    for (i in mapping) {
      for(j in measures){
        
      
      data_rev %>% 
        filter(numeric_name == "val") %>%
        {if(i == "who_region") select(.,measure_id,estimate, estimate_newlab, estimate_id, who_region, who_region_pop100k:who_region_cost_who2_diff)
          else select(.,measure_id, estimate, estimate_newlab, estimate_id, ihme_region, ihme_region_pop100k:ihme_region_cost_who2_diff)} %>% unique %>%
=======
for(j in measures){
  for (m in comparison){
    for (i in mapping) {
      
      data_rev %>% 
        filter(numeric_name == "val") %>%
        {if(i == "who_region") select(.,measure_id,estimate, estimate_id, who_region, who_region_pop100k:who_region_cost_who2_diff)
          else select(.,measure_id, estimate, estimate_id, ihme_region, ihme_region_pop100k:ihme_region_cost_who2_diff)} %>% unique %>%
>>>>>>> 53996c3571acf22ca7c251859011dc4222c2d3fa
        filter(measure_id== j) %>% 
        filter(estimate_id %in%  c(ifelse(m =="diff","",1), 2,4)) %>% 
        filter(!is.na(get(i))) %>%
        mutate(region = factor(get(i) , levels =  get(paste0(i,"_positions")))) %>%
        ggplot(aes(x = region, fill=region, y=get(paste0(i,"_percent",ifelse(m =="diff", "_diff",""))))) +
        geom_bar(stat="identity") +
<<<<<<< HEAD
        labs(title=paste0(
          ifelse(m =="diff" && j==1 && k == "rate_per_100k", "Difference in number of deaths due to mental disorders per 100,000 capita, relative to GBD 2019",
                 ifelse(m =="diff" && j==2 && k == "rate_per_100k", "Difference in number of DALYs due to mental disorders per 100,000 capita, relative to GBD 2019",                      
                        ifelse(m =="diff" && j==3 && k == "rate_per_100k", "Difference in number of YLDs due to mental disorders per 100,000 capita, relative to GBD 2019",
                               ifelse(m =="diff" && j==4 && k == "rate_per_100k", "Difference in number of YLLs due to mental disorders per 100,000 capita, relative to GBD 2019",
                                      ifelse(m =="diff" && j==1 && k != "rate_per_100k", "Percentage point difference in percent of deaths due to mental disorders, relative to GBD 2019",
                                             ifelse(m =="diff" && j==2 && k != "rate_per_100k", "Percentage point difference in percent of DALYs due to mental disorders, relative to GBD 2019",                      
                                                    ifelse(m =="diff" && j==3 && k != "rate_per_100k", "Percentage point difference in percent of YLDs due to mental disorders, relative to GBD 2019",
                                                           ifelse(m =="diff" && j==4 && k != "rate_per_100k", "Percentage point difference in percent of YLLs due to mental disorders, relative to GBD 2019",
                                                                  paste0(ifelse(j==1,"Deaths",
                                                                                ifelse(j==2, "DALYs",
                                                                                       ifelse(j==3, "YLDs" ,"YLLs")))," due to mental disorders", paste0(ifelse(k=="rate_per_100k"," per 100,000 capita", ", % of total")))))))))))), 
          subtitle="2019",
          # caption=caption,
          fill=
            ifelse(j == 1,ifelse(k == "rate_per_100k", "Deaths", "% of deaths"),
                   ifelse(j == 2, ifelse(k == "rate_per_100k", "DALYs", "% of DALYs"),
                          ifelse(j == 3, ifelse(k == "rate_per_100k", "YLDs", "% of YLDs"), 
                                 ifelse(k == "rate_per_100k", "YLLs", "% of YLLs"))))) +
        facet_grid(~estimate_newlab) + 
=======
        labs(title=paste0(ifelse(m =="diff" && j==1 && k == "rate_per_100k", 
                                 "Difference in number of deaths due to mental disorders per 100,000, relative to GBD 2019",
                                 ifelse(m =="diff" && j==2 && k == "rate_per_100k", 
                                        "Difference in number of DALYs due to mental disorders per 100,000, relative to GBD 2019",
                                        ifelse(m =="diff" && j==1 && k != "rate_per_100k", 
                                               "Percentage point difference in percent of deaths due to mental disorders, relative to GBD 2019",
                                               ifelse(m =="diff" && j==2 && k != "rate_per_100k",
                                                      "Percentage point difference in percent of DALYs due to mental disorders, relative to GBD 2019",
                                                      paste0(ifelse(j==1,"Deaths", "DALYs")," due to mental disorders", paste0(ifelse(k=="rate_per_100k"," per 100,000", ", % of total")))))))), 
             subtitle="2019",
             caption=caption,
             fill=
               ifelse(j == 1, ifelse(k == "rate_per_100k", "Deaths", "% of deaths"),
                      ifelse(k == "percent", "% of DALYs", "DALYs"))) +
        facet_grid(~estimate) + 
>>>>>>> 53996c3571acf22ca7c251859011dc4222c2d3fa
        theme(panel.grid.major.y =  element_blank(), 
              panel.background = element_blank(),
              axis.title = element_blank(), 
              axis.text.x = element_blank(),
<<<<<<< HEAD
              axis.ticks.x = element_blank(),
              legend.position = "bottom")  + 
        geom_text(aes(label=round(get(paste0(i,"_percent",ifelse(m =="diff", "_diff",""))),1)), vjust=1.6, color="black", size=ifelse(i =="who_region",2.5, 1.5)) +
        scale_fill_manual(name = "Region", values = get(paste0(i,"_pal")))
      
      ggsave(filename = paste0("fig", n,"_",i,"_",m,"_", j,".jpg"), plot = last_plot(), 
             path = resultspath,
             width = 10,
             height = 6)
      }
      
      n <- n + 1
  }
 
  }

n = 13
=======
              axis.ticks.x = element_blank())  + 
        geom_text(aes(label=round(get(paste0(i,"_percent",ifelse(m =="diff", "_diff",""))),1)), vjust=1.6, color="black", size=ifelse(i =="who_region",2.5, 1.5)) +
        scale_fill_manual(name = "Region", values = get(paste0(i,"_pal")))
      
      ggsave(filename = paste0("fig", n,"_",i,"_",m,".jpg"), plot = last_plot(), 
             path = resultspath,
             width = 10,
             height = 4)
    }
  }
  n <- n + 1
}

data_rev$

n = 11
>>>>>>> 53996c3571acf22ca7c251859011dc4222c2d3fa

for (l in monetaryvalue) {
  for (m in comparison) {
    for (i in mapping) {
    
      data_rev %>% 
        filter(numeric_name == "val") %>%
<<<<<<< HEAD
        {if(i == "who_region") select(.,measure_id,estimate, estimate_id,estimate_newlab,  who_region, who_region_pop100k:who_region_cost_who2_diff)
          else select(.,measure_id, estimate, estimate_id, estimate_newlab, ihme_region, ihme_region_pop100k:ihme_region_cost_who2_diff)} %>% unique %>%
=======
        {if(i == "who_region") select(.,measure_id,estimate, estimate_id, who_region, who_region_pop100k:who_region_cost_who2_diff)
          else select(.,measure_id, estimate, estimate_id, ihme_region, ihme_region_pop100k:ihme_region_cost_who2_diff)} %>% unique %>%
>>>>>>> 53996c3571acf22ca7c251859011dc4222c2d3fa
        filter(measure_id== "2") %>% 
        filter(estimate_id %in%  c(ifelse(m =="diff","",1), 2,4)) %>% 
        filter(!is.na(get(i))) %>%
        mutate(region = factor(get(i) , levels =  get(paste0(i,"_positions")))) %>%
        ggplot(aes(x = region, fill=region, y=get(paste0(i,"_",l,ifelse(m =="diff", "_diff","")))/ get(paste0(i,"_","gdp")) * 100)) +
        geom_bar(stat="identity") +
        labs(title=ifelse(m =="diff", "Difference in value of DALYs due to mental disorders in current USD, percentage points of GDP, relative to GBD 2019", 
                          "Value of DALYs due to mental disorders in current USD, % of GDP"),
<<<<<<< HEAD
             subtitle=get(paste0("subtitle_",(n-12))),
             # caption=caption,
             fill="% of GDP") +
        facet_grid(~estimate_newlab) + 
=======
             subtitle=get(paste0("subtitle_",(n-4-6))),
             caption=caption,
             fill="% of GDP") +
        facet_grid(~estimate) + 
>>>>>>> 53996c3571acf22ca7c251859011dc4222c2d3fa
        theme(panel.grid.major.y =  element_blank(), 
              panel.background = element_blank(),
              axis.title = element_blank(), 
              axis.text.x = element_blank(),
<<<<<<< HEAD
              legend.position = "bottom")  + 
=======
              axis.ticks.x = element_blank())  + 
>>>>>>> 53996c3571acf22ca7c251859011dc4222c2d3fa
        geom_text(aes(label=round(get(paste0(i,"_",l,ifelse(m =="diff", "_diff","")))/ get(paste0(i,"_","gdp")) * 100,1)), vjust=1.6, color="black", size=ifelse(i =="who_region",2.5, 1.5)) +
        scale_fill_manual(name = "Region", values = get(paste0(i,"_pal")))
      
      ggsave(filename = paste0("fig", n,"_",i,"_",m,".jpg"), plot = last_plot(), 
             path = resultspath,
             width = 10,
<<<<<<< HEAD
             height = 6)
=======
             height = 4)
>>>>>>> 53996c3571acf22ca7c251859011dc4222c2d3fa
    }
  }
  n <- n + 1
}


######################
# Figure of GBD Inputs by IHME region
######################

data_rev_ihme_inputs <- read.csv(file = file.path(datapath,"IHME-GBD_2019_DATA-2019-inputs.csv"))
data_rev_ihme_inputs <- data_rev_ihme_inputs %>% gather(inputs_mental:inputs_all,key = "input", value = "count")

chart_4_gbdinputs_ihme_region_percent <- 
  data_rev_ihme_inputs %>% 
  filter(!ihme_region %in% c("Global", "Other/Duplicates")) %>%
  filter(input != "inputs_all") %>%
  mutate(ihme_region = factor(ihme_region, levels = ihme_region_positions)) %>%
  ggplot(aes(x = input, fill=ihme_region, y=count)) +
  geom_bar(position = "fill", stat="identity")+
  theme(panel.grid.major.y =  element_blank(), 
        panel.background = element_blank(),
        axis.title = element_blank(), 
        axis.ticks.x = element_blank()) +
  ggtitle("Share of cause-specific inputs") +
  scale_fill_manual(name = "Region", values = ihme_region_pal) + 
  scale_x_discrete(limit = c("inputs_mental", "inputs_maternal"),
                   labels = c(
                     
                     expression(paste("Mental disorders \n         (n=2870)" )),
                     expression(paste("Maternal and neonatal disorders \n                    (n=6149)" ))))

chart_4_gbdinputs_ihme_region_stacked <- 
  data_rev_ihme_inputs %>% 
  filter(!ihme_region %in% c("Global", "Other/Duplicates")) %>%
  filter(input != "inputs_all") %>%
  mutate(ihme_region = factor(ihme_region, levels = ihme_region_positions)) %>%
  ggplot(aes(x = input, fill=ihme_region, y=count)) +
  geom_bar(position = "stack", stat="identity")+
  theme(panel.grid.major.y =  element_blank(), 
        panel.background = element_blank(),
        axis.title = element_blank(), 
        axis.ticks.x = element_blank()) +
  ggtitle("Number of cause-specific inputs") +
  scale_fill_manual(name = "Region", values = ihme_region_pal) + 
  scale_x_discrete(limit = c("inputs_mental", "inputs_maternal"),
                   labels = c(
                     
                     expression(paste("Mental disorders \n         (n=2870)" )),
                     expression(paste("Maternal and neonatal disorders \n                    (n=6149)" ))))

combo_chart_4_gbdinputs_ihme_region <- ggarrange(chart_4_gbdinputs_ihme_region_stacked,
                                                 chart_4_gbdinputs_ihme_region_percent,
                                                 common.legend = TRUE,
                                                 legend = "bottom",
                                                 ncol = 2, nrow = 1)


<<<<<<< HEAD
ggsave(filename = "fig17_ihme_region.jpg", plot = last_plot(), 
=======
ggsave(filename = "fig15_ihme_region.jpg", plot = last_plot(), 
>>>>>>> 53996c3571acf22ca7c251859011dc4222c2d3fa
       path = resultspath,
       width = 13,
       height = 8)

############################
##        TABLES          ##
############################

# Table 1: Burden of disease due to mental disorders

data_global <- data_rev %>% filter(location_id == 1)  %>% filter(estimate_id != 3)  %>% select(location_name, measure_name, numeric_name, estimate, number, percent) %>%
  mutate(number = round(number/1000000, 1),
         percent = round(percent, 1))

data_regional <- data_rev  %>% filter(estimate_id != 3)  %>% 
<<<<<<< HEAD
  group_by(ihme_region,measure_name, numeric_name, estimate) %>% mutate(region_total = sum(measure_total),
                                                                       region_number = sum(number)) %>%
  mutate(region_percent = round(region_number / region_total * 100, 1)) %>% select(ihme_region, measure_name, numeric_name, estimate, region_number, region_percent) %>%
  rename("number" = "region_number",
         "percent" = "region_percent",
         "location_name" = "ihme_region") %>%
=======
  group_by(who_region,measure_name, numeric_name, estimate) %>% mutate(region_total = sum(measure_total),
                                                                       region_number = sum(number)) %>%
  mutate(region_percent = round(region_number / region_total * 100, 1)) %>% select(who_region, measure_name, numeric_name, estimate, region_number, region_percent) %>%
  rename("number" = "region_number",
         "percent" = "region_percent",
         "location_name" = "who_region") %>%
>>>>>>> 53996c3571acf22ca7c251859011dc4222c2d3fa
  ungroup() %>%
  unique() %>% filter(!is.na(location_name)) %>%
  mutate(number = round(number/1000000, 1)) %>% arrange((location_name))

data_income <- data_rev  %>% filter(estimate_id != 3)  %>% 
  group_by(income_level,measure_name, numeric_name, estimate) %>% mutate(region_total = sum(measure_total),
                                                                         region_number = sum(number)) %>%
  mutate(region_percent = round(region_number / region_total * 100, 1)) %>% select(income_level, measure_name, numeric_name, estimate, region_number, region_percent) %>%
  rename("number" = "region_number",
         "percent" = "region_percent",
         "location_name" = "income_level") %>%
  ungroup() %>%
  unique() %>% filter(!is.na(location_name)) %>%
  mutate(number = round(number/1000000, 1))

location_name <- c("H","UM","LM","L")
rank <- c(1,2,3,4)
fullname <- c("High income",
              "Upper-middle income",
              "Lower-middle income",
              "Low income")


incomelevels <- data.frame(location_name,rank, fullname)

data_income <- left_join(data_income, incomelevels, by = "location_name") %>% arrange(rank) %>% select(!c(location_name, rank)) %>% rename("location_name" = "fullname")

data_table1 <- rbind(data_global, data_income, data_regional) %>% 
  gather("metric","point","number":"percent" ) %>%  
  pivot_wider(names_from = c(numeric_name, metric, estimate), values_from = point)

rm(data_global, data_regional, data_income, incomelevels, location_name, rank, fullname)

col_order <- c(
  "measure_name",
  "location_name",                                    
  "val_number_GBD 2019",                             
  "lower_number_GBD 2019",                            
  "upper_number_GBD 2019",   
  "val_percent_GBD 2019",                            
  "lower_percent_GBD 2019",  
  "upper_percent_GBD 2019",                           
  "val_number_2016 reallocation method",   
  "lower_number_2016 reallocation method",  
  "upper_number_2016 reallocation method",  
  "val_percent_2016 reallocation method",  
  "lower_percent_2016 reallocation method", 
  "upper_percent_2016 reallocation method", 
  "val_number_Composite method",           
  "lower_number_Composite method",          
  "upper_number_Composite method",          
  "val_percent_Composite method",          
  "lower_percent_Composite method",  
  "upper_percent_Composite method")

data_table1 <- data_table1[, col_order]
rm(col_order)

data_table1 <- data_table1 %>% arrange(measure_name)

<<<<<<< HEAD
write.csv(data_table1, file = paste0(resultspath, "/table1.csv"))

# Table 2: Value of economic welfare estimates 

data_global <- data_rev %>% 
  filter(location_id == 1)  %>% 
  filter(measure_id == 2) %>%
  filter(estimate_id != 3)  %>% 
  select(location_name, numeric_name, 
         estimate, cost_cc1, cost_cc2, cost_who1, cost_who2, cost_who3, cost_who4)

data_global$location_name <- "Global"


data_regional <- data_rev  %>%   
  filter(measure_id == 2) %>%
  filter(estimate_id != 3)  %>% 
  select(ihme_region,numeric_name, estimate, ihme_region_cost_cc1,
           ihme_region_cost_cc2, ihme_region_cost_who1, ihme_region_cost_who2, ihme_region_cost_who3, ihme_region_cost_who4) %>%
  unique() %>%
  rename("location_name" =ihme_region,
         "cost_cc1" = ihme_region_cost_cc1,
         "cost_cc2" = ihme_region_cost_cc2,
         "cost_who1" = ihme_region_cost_who1,
         "cost_who2" = ihme_region_cost_who2,
         "cost_who3" = ihme_region_cost_who3,
         "cost_who4" = ihme_region_cost_who4) %>%
  filter(!is.na(location_name)) %>% 
  arrange((location_name))

data_income <- data_rev  %>% 
  filter(measure_id == 2) %>%
  filter(estimate_id != 3)  %>%  
  select(income_level, estimate, numeric_name, cost_cc1, cost_cc2, cost_who1, cost_who2,  cost_who3, cost_who4) %>%
  pivot_longer(cols = cost_cc1:cost_who4) %>%
  group_by(income_level,numeric_name, name, estimate) %>% 
  mutate(cost_total = sum(value, na.rm = TRUE)) %>%
  ungroup() %>%
  select(income_level, numeric_name, name, cost_total, estimate) %>% unique() %>%
  rename("location_name" = income_level)

location_name <- c("H","UM","LM","L")
rank <- c(1,2,3,4)
fullname <- c("High income",
              "Upper-middle income",
              "Lower-middle income",
              "Low income")

incomelevels <- data.frame(location_name,rank, fullname)

data_income <- left_join(data_income, incomelevels, by = "location_name") %>% 
  arrange(rank) %>% select(!c(location_name, rank)) %>% rename("location_name" = "fullname")

data_income <- data_income %>% 
  pivot_wider(values_from = cost_total, names_from = name) %>%
  filter(!is.na(location_name)) 
  
#### Pivot wider, change column names, drop NA for income levels

data_table2 <- rbind(data_global, data_income, data_regional) %>% 
  arrange(factor(numeric_name, levels = c("val", "lower", "upper"))) %>%
  arrange(factor(estimate, levels = c("GBD 2019", "2016 reallocation method", "Composite method"))) %>%
  pivot_longer(cols = cost_cc1:cost_who4) %>%
  mutate(value = round(value/1000000000000,2)) %>%
  pivot_wider(names_from = c(numeric_name, estimate), values_from = value) %>%
  arrange(name) %>%
  relocate(name, .before = "location_name")

write.csv(data_table2, file = paste0(resultspath, "/table2.csv"))

# Appendix 2: Value of economic welfare estimates (CC)

appendix_table <- data_rev %>% filter(location_id == 1) %>%  
  filter(measure_id == 2) %>% filter(estimate_id != 3) %>% 
  select(estimate, numeric_name, cost_cc1, cost_cc2, cost_who1, cost_who2, cost_who3, cost_who4)
appendix_table$cost_cc1 <- round(appendix_table$cost_cc1/1000000000000,2)
appendix_table$cost_cc2 <- round(appendix_table$cost_cc2/1000000000000,2)
appendix_table$cost_who1 <- round(appendix_table$cost_who1/1000000000000,2)
appendix_table$cost_who2 <- round(appendix_table$cost_who2/1000000000000,2)
appendix_table$cost_who3 <- round(appendix_table$cost_who3/1000000000000,2)
appendix_table$cost_who4 <- round(appendix_table$cost_who4/1000000000000,2)
appendix_table <- appendix_table %>% select(!c(cost_who1, cost_who2)) %>% arrange(estimate, factor(numeric_name, levels = c("val", "lower", "upper")))
appendix_table_t <- transpose(appendix_table)

colnames(appendix_table_t) <- rownames(appendix_table)
rownames(appendix_table_t) <- colnames(appendix_table)

write.csv(appendix_table_t, file = paste0(resultspath, "/table_appendix.csv"))

############################
##        VALUES          ##
############################

# Key values and calulcations for Abstract and Results

# Abstract

abstract1 <- data_table1 %>% 
  filter(measure_name=="DALYs (Disability-Adjusted Life Years)") %>% 
  filter(location_name =="Global") %>% 
  select("val_number_Composite method") %>% 
  unique()
abstract1 <- as.numeric(abstract1)
abstract1 <- round(abstract1, 0)

abstract2 <- data_table1 %>% 
  filter(measure_name=="DALYs (Disability-Adjusted Life Years)") %>% 
  filter(location_name =="Global") %>% 
  select("val_percent_Composite method") %>% 
  unique()
abstract2 <- as.numeric(abstract2)
abstract2 <- floor(abstract2)

abstract3 <- data_table1 %>% 
  filter(measure_name=="DALYs (Disability-Adjusted Life Years)") %>% 
  filter(location_name =="Global") %>% 
  select("val_number_GBD 2019") %>% 
  unique()
abstract3 <- as.numeric(abstract3)
abstract3 <- round(abstract3, 0)

abstract_sentence1 <- paste0("Using an estimation approach that accounts for premature mortality due to mental disorders and additional sources of morbidity, we estimate that ",
       abstract1,
       " million disability-adjusted life years (DALYs) were attributable to mental disorders in 2019 (",
       abstract2,
       "% of global DALYs)",
       ifelse(abstract1/abstract3>3," more than ","n approximate "), 
       "three-fold increase compared to conventional estimates.")

abstract4 <- data_table2%>% 
  filter(name == "cost_who1") %>% 
  filter(location_name == "Global") %>%
  select(`val_Composite method`) %>%
  unique()
abstract4 <- as.numeric(abstract4)
abstract4 <- floor(abstract4*10)/10

abstract_sentence2 <- paste0("The economic value associated with this burden is estimated to exceed USD ",
       abstract4,
       " trillion. ")

table3 <- data_rev %>% 
  filter(numeric_name == "val") %>%
  select(measure_id, estimate, estimate_id, ihme_region, ihme_region_cost_who1, ihme_region_gdp) %>% 
  unique %>%
  filter(measure_id== "2") %>% 
  filter(estimate_id== "4") %>% mutate(
    regional_percent = ihme_region_cost_who1 / ihme_region_gdp *100)

abstract5 <- table3$ihme_region[table3$regional_percent == max(table3$regional_percent)]
abstract6 <- round(table3$regional_percent[table3$regional_percent == max(table3$regional_percent)],1)

abstract7 <- table3$ihme_region[table3$regional_percent == min(table3$regional_percent)]
abstract8 <- round(table3$regional_percent[table3$regional_percent == min(table3$regional_percent)],1)

abstract_sentence3 <- paste0("At a regional level, the losses account for between ",
       abstract8,
       "% of gross domestic product in ",
       abstract7,
       " and ",
       abstract6,
       "% in ",
       abstract5,
       ".")

# Results

results1 <- data_table1 %>% 
  filter(measure_name=="DALYs (Disability-Adjusted Life Years)") %>% 
  filter(location_name =="Global") %>% 
  select("val_number_GBD 2019") %>% 
  unique()
results1 <- as.numeric(results1)
results1 <- floor(results1)

results2 <- data_table1 %>% 
  filter(measure_name=="DALYs (Disability-Adjusted Life Years)") %>% 
  filter(location_name =="Global") %>% 
  select("val_percent_GBD 2019") %>% 
  unique()
results2 <- as.numeric(results2)
results2 <- round(results2,0)

results3 <- data_table1 %>% 
  filter(measure_name=="DALYs (Disability-Adjusted Life Years)") %>% 
  filter(location_name =="Global") %>% 
  select("val_percent_2016 reallocation method") %>% 
  unique()
results3 <- as.numeric(results3)
results3 <- floor(results3)

results4 <- data_table1 %>% 
  filter(measure_name=="DALYs (Disability-Adjusted Life Years)") %>% 
  filter(location_name =="Global") %>% 
  select("val_number_2016 reallocation method") %>% 
  unique()
results4 <- as.numeric(results4)
results4 <- floor(results4)

results5 <- data_table1 %>% 
  filter(measure_name=="DALYs (Disability-Adjusted Life Years)") %>% 
  filter(location_name =="High income") %>% 
  select("val_number_Composite method") %>% 
  unique()
results5 <- as.numeric(results5)
results5 <- round(results5, 0)

results6 <- data_table1 %>% 
  filter(measure_name=="DALYs (Disability-Adjusted Life Years)") %>% 
  filter(location_name =="Low income") %>% 
  select("val_number_Composite method") %>% 
  unique()
results6 <- as.numeric(results6)
results6 <- round(results6, 0)

results_sentence1 <- paste0(
"Under GBD 2019, over ",
results1,
" million DALYs were attributed to mental disorders, or roughly ",
results2,
"% of the global burden. After including alcohol and drug use, neurological disorders, chronic pain, suicide, and self-harm, the share of morbidity and mortality due to mental disorders rose to ",
results3,
"% of global DALYs (approximately ",
results4,
" million DALYs). Under the composite approach to further capture premature mortality, an additional ",
abstract1-results4,
" million DALYs were attributed to mental disorders, encompassing, in total, over ",
abstract2,
"% of global DALYs. Under all three methods, the burden of mental disorders (in DALYs) exhibited a country-income gradient, with mental disorders comprising ",
ifelse(results5/results6 > 2.4, "over twice", ifelse(results5/results6> 2, "roughly twice", "approximately twice")),
" the burden of disease in high-income countries compared to low-income countries.")


results7 <- data_table2%>% 
  filter(name == "cost_who1") %>% 
  filter(location_name == "Global") %>%
  select(`val_Composite method`) %>%
  unique()
results7 <- as.numeric(results7)

results8 <- data_table2%>% 
  filter(name == "cost_who1") %>% 
  filter(location_name == "Global") %>%
  select(`val_2016 reallocation method`) %>%
  unique()
results8 <- as.numeric(results8)

results9 <- data_table2%>% 
  filter(name == "cost_who1") %>% 
  filter(location_name == "Global") %>%
  select(`val_GBD 2019`) %>%
  unique()
results9 <- as.numeric(results9)

results_sentence2 <- paste0(
"Using GDP per capita as a proxy for the value per DALY, economic losses due to mental disorders were estimated at ",
abstract4,
" trillion USD using our composite approach. This estimate is ",
floor((results7 - results8)*10)/10,
" trillion USD larger than that reached using the 2016 reallocation approach and over ",
floor((results7 - results9)*10)/10,
" trillion USD larger than that reached from the unadjusted GBD 2019 estimates.")

results_sentence3 <- paste0(
"Under the relative, GDP-per-capita values, the economic burden due to mental disorders account for between ",
abstract8,
       "% of gross domestic product in ",
       abstract7,
       " and ",
       abstract6,
       "% in ",
       abstract5,
       " under our composite estimation approach.")

# Discussion

discussion_sentence1 <- paste0(
"Ultimately, this approach suggests that the global DALYs attributable to mental disorders exceed ",
abstract1,
" million per year, or ",
abstract2,
"% of the total burden. If this estimate is taken to approximate the true burden of mental disorders, the current GBD classification would underestimate the burden by as many as ",
abstract1-results1,
" million DALYs, or ",
round((abstract1-results1)/abstract1*100),
"% of the current estimate.")


discussion1 <- data_table2%>% 
  filter(name == "cost_who1") %>% 
  filter(location_name == "Global") %>%
  select(`lower_Composite method`) %>%
  unique()
discussion1 <- as.numeric(discussion1)


discussion2 <- data_table2%>% 
  filter(name == "cost_who1") %>% 
  filter(location_name == "Global") %>%
  select(`upper_Composite method`) %>%
  unique()
discussion2 <- as.numeric(discussion2)

discussion_sentence2 <- paste0(
  "When applied against an economic value per DALY of one times GDP per capita, this approach further suggests that the per year losses associated with this burden exceeded ",
abstract4,
" trillion USD in 2019. This is over ",
floor((abstract4 - 2.9)*10)/10,
" trillion USD greater than Bloom et al.'s global welfare loss projection for 2030 (2.9 trillion USD, adjusted to 2019), using the same value per DALY approach. When adjusting for the wide uncertainty in estimates of the attributable burden of disease, the losses could range from ",
floor((discussion1)*10)/10,
" trillion to more than ",
floor((discussion2)*10)/10,
" trillion USD.")


abstract_sentence1
abstract_sentence2
abstract_sentence3
results_sentence1
results_sentence2
results_sentence3
discussion_sentence1
discussion_sentence2


#########################################
##        SENSITIVTY ANALYSIS          ##
#########################################

set.seed(82420212)
prev.sample <- rnorm(n = 1000, mean = .1304, sd = ((.1402 - .1212) / 3.92))
rr.sample <- rnorm (n = 1000,  mean = 2.22, sd = ((2.33 - 2.12) / 3.92))
=======
write.csv(data_table1, file = "results/table1.csv")

# Table 2: Value of economic welfare estimates 

table2 <- data_rev %>% filter(location_id == 1) %>%  filter(measure_id == 2) %>% filter(estimate_id != 3) %>% select(estimate, numeric_name, cost_cc1, cost_cc2, cost_who1, cost_who2)
table2$cost_cc1 <- round(table2$cost_cc1/1000000000000,2)
table2$cost_cc2 <- round(table2$cost_cc2/1000000000000,2)
table2$cost_who1 <- round(table2$cost_who1/1000000000000,2)
table2$cost_who2 <- round(table2$cost_who2/1000000000000,2)
table2 <- table2 %>% select(!c(cost_cc1, cost_cc2)) %>% arrange(estimate, factor(numeric_name, levels = c("val", "lower", "upper")))
table2_t <- transpose(table2)

colnames(table2_t) <- rownames(table2)
rownames(table2_t) <- colnames(table2)

write.csv(table2_t, file = "results/table2.csv")

# Appendix 2: Value of economic welfare estimates (CC)

appendix_table <- data_rev %>% filter(location_id == 1) %>%  filter(measure_id == 2) %>% filter(estimate_id != 3) %>% select(estimate, numeric_name, cost_cc1, cost_cc2, cost_who1, cost_who2)
appendix_table$cost_cc1 <- round(appendix_table$cost_cc1/1000000000000,2)
appendix_table$cost_cc2 <- round(appendix_table$cost_cc2/1000000000000,2)
appendix_table$cost_who1 <- round(appendix_table$cost_who1/1000000000000,2)
appendix_table$cost_who2 <- round(appendix_table$cost_who2/1000000000000,2)
appendix_table <- appendix_table %>% select(!c(cost_who1, cost_who2)) %>% arrange(estimate, factor(numeric_name, levels = c("val", "lower", "upper")))
appendix_table_t <- transpose(appendix_table)

colnames(appendix_table_t) <- rownames(appendix_table)
rownames(appendix_table_t) <- colnames(appendix_table)

write.csv(appendix_table_t, file = "results/table_appendix.csv")

############################
##        VALUES          ##
############################

# Key values and calulcations for Abstract and Results

# Abstract

abstract1 <- data_table1 %>% 
  filter(measure_name=="DALYs (Disability-Adjusted Life Years)") %>% 
  filter(location_name =="Global") %>% 
  select("val_number_Composite method") %>% 
  unique()
abstract1 <- as.numeric(abstract1)
abstract1 <- round(abstract1, 0)

abstract2 <- data_table1 %>% 
  filter(measure_name=="DALYs (Disability-Adjusted Life Years)") %>% 
  filter(location_name =="Global") %>% 
  select("val_percent_Composite method") %>% 
  unique()
abstract2 <- as.numeric(abstract2)
abstract2 <- floor(abstract2)

abstract3 <- data_table1 %>% 
  filter(measure_name=="DALYs (Disability-Adjusted Life Years)") %>% 
  filter(location_name =="Global") %>% 
  select("val_number_GBD 2019") %>% 
  unique()
abstract3 <- as.numeric(abstract3)
abstract3 <- round(abstract3, 0)

abstract_sentence1 <- paste0("Using an estimation approach that accounts for premature mortality due to mental disorders and additional sources of morbidity, we estimate that ",
       abstract1,
       " million disability-adjusted life years (DALYs) were attributable to mental disorders in 2019 (",
       abstract2,
       "% of global DALYs)—a",
       ifelse(abstract1/abstract3>3," more than ","n approximate "), 
       "three-fold increase compared to conventional estimates.")

abstract4 <- table2 %>% 
  filter(estimate=="Composite method") %>% 
  filter(numeric_name =="val") %>% 
  select("cost_who1") %>% 
  unique()
abstract4 <- as.numeric(abstract4)
abstract4 <- floor(abstract4*10)/10

abstract_sentence2 <- paste0("The economic value associated with this burden is estimated to exceed USD ",
       abstract4,
       " trillion. ")

table3 <- data_rev %>% 
  filter(numeric_name == "val") %>%
  select(measure_id, estimate, estimate_id, ihme_region, ihme_region_cost_who1, ihme_region_gdp) %>% 
  unique %>%
  filter(measure_id== "2") %>% 
  filter(estimate_id== "4") %>% mutate(
    regional_percent = ihme_region_cost_who1 / ihme_region_gdp *100)

abstract5 <- table3$ihme_region[table3$regional_percent == max(table3$regional_percent)]
abstract6 <- round(table3$regional_percent[table3$regional_percent == max(table3$regional_percent)],1)

abstract7 <- table3$ihme_region[table3$regional_percent == min(table3$regional_percent)]
abstract8 <- round(table3$regional_percent[table3$regional_percent == min(table3$regional_percent)],1)

abstract_sentence3 <- paste0("At a regional level, the losses account for between ",
       abstract8,
       "% of gross domestic product in ",
       abstract7,
       " and ",
       abstract6,
       "% in ",
       abstract5,
       ".")

# Results

results1 <- data_table1 %>% 
  filter(measure_name=="DALYs (Disability-Adjusted Life Years)") %>% 
  filter(location_name =="Global") %>% 
  select("val_number_GBD 2019") %>% 
  unique()
results1 <- as.numeric(results1)
results1 <- floor(results1)

results2 <- data_table1 %>% 
  filter(measure_name=="DALYs (Disability-Adjusted Life Years)") %>% 
  filter(location_name =="Global") %>% 
  select("val_percent_GBD 2019") %>% 
  unique()
results2 <- as.numeric(results2)
results2 <- round(results2,0)

results3 <- data_table1 %>% 
  filter(measure_name=="DALYs (Disability-Adjusted Life Years)") %>% 
  filter(location_name =="Global") %>% 
  select("val_percent_2016 reallocation method") %>% 
  unique()
results3 <- as.numeric(results3)
results3 <- floor(results3)

results4 <- data_table1 %>% 
  filter(measure_name=="DALYs (Disability-Adjusted Life Years)") %>% 
  filter(location_name =="Global") %>% 
  select("val_number_2016 reallocation method") %>% 
  unique()
results4 <- as.numeric(results4)
results4 <- floor(results4)

results5 <- data_table1 %>% 
  filter(measure_name=="DALYs (Disability-Adjusted Life Years)") %>% 
  filter(location_name =="High income") %>% 
  select("val_number_Composite method") %>% 
  unique()
results5 <- as.numeric(results5)
results5 <- round(results5, 0)

results6 <- data_table1 %>% 
  filter(measure_name=="DALYs (Disability-Adjusted Life Years)") %>% 
  filter(location_name =="Low income") %>% 
  select("val_number_Composite method") %>% 
  unique()
results6 <- as.numeric(results6)
results6 <- round(results6, 0)

results_sentence1 <- paste0(
"Under GBD 2019, over ",
results1,
" million DALYs were attributed to mental disorders, or roughly ",
results2,
"% of the global burden. After including alcohol and drug use, neurological disorders, chronic pain, suicide, and self-harm, the share of morbidity and mortality due to mental disorders rose to ",
results3,
"% of global DALYs (approximately ",
results4,
" million DALYs). Under the composite approach to further capture premature mortality, an additional ",
abstract1-results4,
" million DALYs were attributed to mental disorders, encompassing, in total, over ",
abstract2,
"% of global DALYs. Under all three methods, the burden of mental disorders (in DALYs) exhibited a country-income gradient, with mental disorders comprising ",
ifelse(results5/results6 > 2.4, "over twice", ifelse(results5/results6> 2, "roughly twice", "approximately twice")),
" the burden of disease in high-income countries compared to low-income countries.")

results7 <- table2 %>% 
  filter(estimate=="Composite method") %>% 
  filter(numeric_name =="val") %>% 
  select("cost_who1") %>% 
  unique()
results7 <- as.numeric(results7)

results8 <- table2 %>% 
  filter(estimate=="2016 reallocation method") %>% 
  filter(numeric_name =="val") %>% 
  select("cost_who1") %>% 
  unique()
results8 <- as.numeric(results8)

results9 <- table2 %>% 
  filter(estimate=="GBD 2019") %>% 
  filter(numeric_name =="val") %>% 
  select("cost_who1") %>% 
  unique()
results9 <- as.numeric(results9)

results_sentence2 <- paste0(
"Using GDP per capita as a proxy for the value per DALY, economic losses due to mental disorders were estimated at ",
abstract4,
" trillion USD using our composite approach. This estimate is ",
floor((results7 - results8)*10)/10,
" trillion USD larger than that reached using the 2016 reallocation approach and over ",
floor((results7 - results9)*10)/10,
" trillion USD larger than that reached from the unadjusted GBD 2019 estimates.")

results_sentence3 <- paste0(
"Under the relative, GDP-per-capita values, the economic burden due to mental disorders account for between ",
abstract8,
       "% of gross domestic product in ",
       abstract7,
       " and ",
       abstract6,
       "% in ",
       abstract5,
       " under our composite estimation approach.")

# Discussion

discussion_sentence1 <- paste0(
"Ultimately, this approach suggests that the global DALYs attributable to mental disorders exceed ",
abstract1,
" million per year, or ",
abstract2,
"% of the total burden. If this estimate is taken to approximate the true burden of mental disorders, the current GBD classification would underestimate the burden by as many as ",
abstract1-results1,
" million DALYs, or ",
round((abstract1-results1)/abstract1*100),
"% of the current estimate.")


discussion1 <- table2 %>% 
  filter(estimate=="Composite method") %>% 
  filter(numeric_name =="lower") %>% 
  select("cost_who1") %>% 
  unique()
discussion1 <- as.numeric(discussion1)


discussion2 <- table2 %>% 
  filter(estimate=="Composite method") %>% 
  filter(numeric_name =="upper") %>% 
  select("cost_who1") %>% 
  unique()
discussion2 <- as.numeric(discussion2)

discussion_sentence2 <- paste0(
  "When applied against an economic value per DALY of one times GDP per capita, this approach further suggests that the per year losses associated with this burden exceeded ",
abstract4,
" trillion USD in 2019. This is over ",
floor((abstract4 - 2.9)*10)/10,
" trillion USD greater than Bloom et al.’s global welfare loss projection for 2030 (2.9 trillion USD, adjusted to 2019), using the same value per DALY approach. When adjusting for the wide uncertainty in estimates of the attributable burden of disease, the losses could range from ",
floor((discussion1)*10)/10,
" trillion to more than ",
floor((discussion2)*10)/10,
" trillion USD.")


abstract_sentence1
abstract_sentence2
abstract_sentence3
results_sentence1
results_sentence2
results_sentence3
discussion_sentence1
discussion_sentence2


#########################################
##        SENSITIVTY ANALYSIS          ##
#########################################

set.seed(82420212)
prev.sample <- rnorm(n = 1000, mean = .1304, sd = ((.1402 - .1212) / 3.92))
rr.sample <- rnorm (n = 1000,  mean = 2.22, sd = ((2.33 - 2.12) / 3.92))
deaths.sample <- rnorm (n = 1000,  mean = 56526959.51, sd = ((59205883 - 53742682.45) / 3.92))
psa_data <- cbind.data.frame(rr.sample, prev.sample, deaths.sample)
psa_data$Cases <- ((psa_data$prev.sample  * ((psa_data$rr.sample - 1)))) /  ((psa_data$rr.sample))
psa_data$TotalPop <- ((psa_data$prev.sample  * ((psa_data$rr.sample - 1)))) /  (((psa_data$prev.sample  * ((psa_data$rr.sample - 1))))+1)

psa_data <- psa_data %>% pivot_longer(cols = c(TotalPop, Cases), values_to = "paf")
psa_data$deaths_mh <- psa_data$deaths.sample * psa_data$paf

psa_data %>% ggplot(aes(x = prev.sample*100, y = deaths_mh/1000000, alpha = rr.sample, color = name)) +
  geom_point() + 
  labs(x = "Prevalence of mental disorders, %",  
       y = "Global # of deaths attributable to mental disorders, millions",
       color = "Prevalence assumed to be for",
       caption = "Cases: p(RR -1)/ RR. TotalPop: [p(RR - 1)] / [(p(RR -1) + 1].",
       alpha = "Relative risk") + 
  theme_pubr()

psa_data %>% ggplot(aes(x = rr.sample, y = deaths_mh/1000000, alpha = prev.sample, color = name)) +
  geom_point() + 
  labs(alpha = "Prevalence of mental disorders, %",  
       y = "Global # of deaths attributable to mental disorders, millions",
       color = "Prevalence assumed to be for",
       caption = "Cases: p(RR -1)/ RR. TotalPop: [p(RR - 1)] / [(p(RR -1) + 1].",
       x = "Relative risk") + 
  theme_pubr()


set.seed(82420212)
prev.sample <- runif(n = 1000, min = 0.01, max = 0.2)
a <- c(.5, 1, 1.5, 2, 2.5, 3)
rr.sample <- sample(a,  size=1000, replace=T)
>>>>>>> 53996c3571acf22ca7c251859011dc4222c2d3fa
deaths.sample <- rnorm (n = 1000,  mean = 56526959.51, sd = ((59205883 - 53742682.45) / 3.92))
psa_data <- cbind.data.frame(rr.sample, prev.sample, deaths.sample)
psa_data$Cases <- ((psa_data$prev.sample  * ((psa_data$rr.sample - 1)))) /  ((psa_data$rr.sample))
psa_data$TotalPop <- ((psa_data$prev.sample  * ((psa_data$rr.sample - 1)))) /  (((psa_data$prev.sample  * ((psa_data$rr.sample - 1))))+1)

psa_data <- psa_data %>% pivot_longer(cols = c(TotalPop, Cases), values_to = "paf")
psa_data$deaths_mh <- psa_data$deaths.sample * psa_data$paf

<<<<<<< HEAD
psa_data %>% ggplot(aes(x = prev.sample*100, y = deaths_mh/1000000, alpha = rr.sample, color = name)) +
  geom_point() + 
  labs(x = "Prevalence of mental disorders, %",  
       y = "Global # of deaths attributable to mental disorders, millions",
       color = "Prevalence assumed to be for",
       caption = "Cases: p(RR -1)/ RR. TotalPop: [p(RR - 1)] / [(p(RR -1) + 1].",
       alpha = "Relative risk") + 
  theme_pubr()

psa_data %>% ggplot(aes(x = rr.sample, y = deaths_mh/1000000, alpha = prev.sample, color = name)) +
  geom_point() + 
  labs(alpha = "Prevalence of mental disorders, %",  
       y = "Global # of deaths attributable to mental disorders, millions",
       color = "Prevalence assumed to be for",
       caption = "Cases: p(RR -1)/ RR. TotalPop: [p(RR - 1)] / [(p(RR -1) + 1].",
       x = "Relative risk") + 
  theme_pubr()


set.seed(82420212)
prev.sample <- runif(n = 1000, min = 0.01, max = 0.2)
a <- c(.5, 1, 1.5, 2, 2.5, 3)
rr.sample <- sample(a,  size=1000, replace=T)
deaths.sample <- rnorm (n = 1000,  mean = 56526959.51, sd = ((59205883 - 53742682.45) / 3.92))
psa_data <- cbind.data.frame(rr.sample, prev.sample, deaths.sample)
psa_data$Cases <- ((psa_data$prev.sample  * ((psa_data$rr.sample - 1)))) /  ((psa_data$rr.sample))
psa_data$TotalPop <- ((psa_data$prev.sample  * ((psa_data$rr.sample - 1)))) /  (((psa_data$prev.sample  * ((psa_data$rr.sample - 1))))+1)

psa_data <- psa_data %>% pivot_longer(cols = c(TotalPop, Cases), values_to = "paf")
psa_data$deaths_mh <- psa_data$deaths.sample * psa_data$paf

psa_data %>% ggplot(aes(x = prev.sample*100, y = deaths_mh/1000000, color = name)) +
  geom_point() + 
  labs(x = "Prevalence of mental disorders, %",  
       y = "Global # of deaths attributable to mental disorders, millions",
       color = "Prevalence of risk factor w.r.t",
       caption = "Cases: p(RR -1)/ RR. TotalPop: [p(RR - 1)] / [(p(RR -1) + 1].")+ 
  theme_pubr() +
  facet_wrap(~rr.sample)

### Walker et al sensitivity analysis
set.seed(82420212)
prev.sample <- rnorm(n = 1000, mean = .261, sd = .05)
rr.sample <- runif(n = 1000, min = 2.22-.5, max = 2.72)
deaths.sample <- rnorm (n = 1000,  mean = 56526959.51, sd = ((59205883 - 53742682.45) / 3.92))
psa_data <- cbind.data.frame(rr.sample, prev.sample, deaths.sample)
psa_data$Cases <- ((psa_data$prev.sample  * ((psa_data$rr.sample - 1)))) /  ((psa_data$rr.sample))
psa_data$TotalPop <- ((psa_data$prev.sample  * ((psa_data$rr.sample - 1)))) /  (((psa_data$prev.sample  * ((psa_data$rr.sample - 1))))+1)

=======
psa_data %>% ggplot(aes(x = prev.sample*100, y = deaths_mh/1000000, color = name)) +
  geom_point() + 
  labs(x = "Prevalence of mental disorders, %",  
       y = "Global # of deaths attributable to mental disorders, millions",
       color = "Prevalence of risk factor w.r.t",
       caption = "Cases: p(RR -1)/ RR. TotalPop: [p(RR - 1)] / [(p(RR -1) + 1].")+ 
  theme_pubr() +
  facet_wrap(~rr.sample)

### Walker et al sensitivity analysis
set.seed(82420212)
prev.sample <- rnorm(n = 1000, mean = .261, sd = .05)
rr.sample <- runif(n = 1000, min = 2.22-.5, max = 2.72)
deaths.sample <- rnorm (n = 1000,  mean = 56526959.51, sd = ((59205883 - 53742682.45) / 3.92))
psa_data <- cbind.data.frame(rr.sample, prev.sample, deaths.sample)
psa_data$Cases <- ((psa_data$prev.sample  * ((psa_data$rr.sample - 1)))) /  ((psa_data$rr.sample))
psa_data$TotalPop <- ((psa_data$prev.sample  * ((psa_data$rr.sample - 1)))) /  (((psa_data$prev.sample  * ((psa_data$rr.sample - 1))))+1)

>>>>>>> 53996c3571acf22ca7c251859011dc4222c2d3fa
psa_data <- psa_data %>% pivot_longer(cols = c(TotalPop, Cases), values_to = "paf")
psa_data$deaths_mh <- psa_data$deaths.sample * psa_data$paf

psa_data %>% ggplot(aes(x = rr.sample, y = deaths_mh/1000000)) +
  geom_rect(aes(xmin=2.12, xmax=2.33, ymin = -Inf, ymax= Inf), fill = "grey90") +
  geom_point(aes(alpha = prev.sample*100, color = name)) + 
  geom_vline(xintercept = 2.22) +
  labs(alpha = "Prevalence of mental disorders, %",  
       y = "Global # of deaths attributable to mental disorders, millions",
       color = "Prevalence of risk factor w.r.t",
       x = "Relative risk",
       caption = "Cases: p(RR -1)/ RR. TotalPop: [p(RR - 1)] / [(p(RR -1) + 1]. Vertical line = pooled RR from Walker et. al (2.22), grey shaded region represents 95% CI.")+ 
  theme_pubr() +
  lims(y = c(0,max(psa_data$deaths_mh/1000000)))