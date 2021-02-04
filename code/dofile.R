##################################################################
#
# Re-evaluating the Global Economic Burden of Mental Illness
# Daniel Arias, PhD Student, Population Health Sciences
# November 23, 2020
#
##################################################################

######################
##   HOUSEKEEPING   ##
######################

# setting directories - change to your local directory
path <- "~/3. PhD/WQE/WQE II/gmh_cost"
datapath <- paste(path,'data', sep = "/")

# Loading libraries
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(maps)
library(scales)
library(viridis)


######################
##       DATA       ##
######################


# Loading IHME Global Burden of Disease Data
data <- read.csv(file = file.path(datapath,"IHME-GBD_2019_DATA-2019-1.csv"))

# Adding alpha-3 codes from ISO 3166-1 (i.e., World Bank Codes)
ihme_crosswalk <- read_excel(path = file.path(datapath, "ihme_crosswalk.xlsx")) %>% 
  select(-c("location_name"))

data <- left_join(data, ihme_crosswalk, by = "location_id")

# clean up
rm(ihme_crosswalk) 

# Adding World Bank income classifications
income_level <- read_excel(path = file.path(datapath, "OGHIST.xls"), sheet = 2) %>% 
  select(-(c(2:34))) %>% 
  rename("income_level" = "2019")

data <- left_join(data, income_level, by = "iso_code")

# clean up
rm(income_level) 

# Adding population
data <- data %>% unique() %>% gather("numeric_name", "est", "val":"lower") 
data_number <- data %>% filter(metric_id == 1) %>% rename("number" = "est")
data_rate <- data %>% filter(metric_id == 3) 
data_rate <- data %>% select("measure_id","location_id", "sex_id", "age_id", "cause_id", "year", "numeric_name", "est") %>% rename("rate" = "est")
data <- left_join(data_number, data_rate, by = c("measure_id","location_id", "sex_id", "age_id", "cause_id", "year", "numeric_name"))
rm(data_number, data_rate)

data <- data %>% unique() %>% filter(numeric_name == "val") %>% spread(numeric_name, number)
data$population <- data$val/data$rate
data <- data %>% filter(population != 1) %>% unique()

############################
## vIGO ET AL REPLICATION ##
############################

# VIGO METHOD: ALLOCATION OF THE FOLLOWING TO MENTAL HEALTH (PERCENTS DENOTE AMOUNT REALLOCATED):

# Dementia - 100%
# Epilepsy - 100%
# Migraine - 100%
# Tension-type headache - 100%
# Self-harm - 100%
# Chronic pain syndrom currently attributed to musculoskeletal disorders - 33%

mental_disorder_cause_id <- 558
dementia_cause_id <- c(543, 544)
epilepsy_cause_id <- 545
migraine_cause_id <- 547
tension_type_headache_cause_id <- 548
self_harm <- 718
musculoskeletal_cause_id <- 626
all_cause_id <- 294

revision_full <- c(mental_disorder_cause_id,
              dementia_cause_id,
              epilepsy_cause_id,
              migraine_cause_id,
              tension_type_headache_cause_id,
              self_harm)

revision_third <- musculoskeletal_cause_id
inclusion <- c(mental_disorder_cause_id,
               dementia_cause_id,
               epilepsy_cause_id,
               migraine_cause_id,
               tension_type_headache_cause_id,
               self_harm,
               musculoskeletal_cause_id,
               all_cause_id)

data_vigo <- data %>% filter(cause_id %in% inclusion)

data_vigo$mh_gbd <- ifelse(data_vigo$cause_id == mental_disorder_cause_id, 1, 0)
data_vigo$mh_vigo <- ifelse(data_vigo$cause_id %in% revision_full, 1, 0)
data_vigo$mh_vigo[data_vigo$cause_id %in% revision_third] <- 1/3
data_vigo$all <- ifelse(data_vigo$cause_id == all_cause_id, 1, 0)
data_vigo$value <- data_vigo$val * data_vigo$mh_gbd
data_vigo$value_rev <- data_vigo$val * data_vigo$mh_vigo
data_vigo <- data_vigo %>% group_by(location_id, measure_id, sex_id, age_id, metric_id) %>% mutate(mh_value_agg_rev = sum(value_rev), mh_value_agg = sum(value))

rm(all_cause_id, dementia_cause_id, epilepsy_cause_id, mental_disorder_cause_id, migraine_cause_id, musculoskeletal_cause_id, 
   revision_full, revision_third, self_harm, tension_type_headache_cause_id)

data_vigo_global <- data_vigo %>% filter(location_id == 1) %>% filter(population != 1) %>% filter(metric_id == 1) %>% filter(cause_id == 294) %>% unique()

global_2019_dalys <- data_vigo_global$val[data_vigo_global$measure_id == 2]
global_2019_ylds <- data_vigo_global$val[data_vigo_global$measure_id == 3]

global_2019_dalys_mh_orig <- data_vigo_global$mh_value_agg[data_vigo_global$measure_id == 2]
global_2019_ylds_mh_orig <- data_vigo_global$mh_value_agg[data_vigo_global$measure_id == 3]

# Testing: all.equal tests should yield roughly equivalent results
# Source: http://ghdx.healthdata_vigo.org/gbd-results-tool

all.equal(125311322, global_2019_dalys_mh_orig)
all.equal(125293960, global_2019_ylds_mh_orig)
all.equal(0.0455 , global_2019_dalys_mh_orig/global_2019_dalys)
all.equal(0.1494 , global_2019_ylds_mh_orig/global_2019_ylds)

global_2019_dalys_mh_vigo <- data_vigo_global$mh_value_agg_rev[data_vigo_global$measure_id == 2]
global_2019_ylds_mh_vigo <- data_vigo_global$mh_value_agg_rev[data_vigo_global$measure_id == 3]

global_2019_dalys_mh_vigo
global_2019_ylds_mh_vigo

global_2019_dalys_mh_vigo/global_2019_dalys
global_2019_ylds_mh_vigo/global_2019_ylds

# Subset of data
data_rev <- data_vigo %>%
  filter(cause_id == "294") %>% #Filters to all causes; data_gh$val == total burden by location id
  filter(metric_id == "1") %>%  #Filters to numeric metric
  rename("Revised - Vigo et al. method" = mh_value_agg_rev, "Original" = mh_value_agg, "measure_total" = val) %>%
  filter(measure_id %in% 1:4) %>%
  ungroup %>%
  select(c(1:4, 13:15, 17:18,24:25))

############################
##  WALKER ET AL METHOD  ##
############################


# Loading IHME Global Burden of Disease Data
data_prev <- read.csv(file = file.path(datapath,"IHME-GBD_2019_DATA-2019-prev.csv"))

# source: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4461039/
# Due to the distributive property of the Population Attributable Fraction, a category-specific attribution fraction is estimated
# using the formula from Rockhill et al, 1998:https://pubmed.ncbi.nlm.nih.gov/9584027/  

mental_disorder_cause_id <- 558
schizophrenia_cause_id <- 559
depressive_cause_id <- 567
bipolar_cause_id <- 570
anxiety_cause_id <- 571 

relative_risk <- data.frame(
  c(mental_disorder_cause_id,
    schizophrenia_cause_id,
    depressive_cause_id,
    bipolar_cause_id,
    anxiety_cause_id),
  c(2.22,
    2.54,
    1.71,
    2,
    1.43)
)
colnames(relative_risk) <- c("cause_id", "rel_risk")

data_prev <- inner_join(data_prev, relative_risk, by = "cause_id")
data_prev$par <-     data_prev$val  * ((data_prev$rel_risk - 1) /    data_prev$rel_risk)

data_prev_allmentaldisorders <- data_prev %>% filter(cause_id == mental_disorder_cause_id) %>% select(c(location_id,par))

data_rev <- full_join(data_rev, data_prev_allmentaldisorders, by = "location_id")

rm(relative_risk, data_prev, data_prev_allmentaldisorders)

data_rev <- data_rev %>% relocate(par, .after = population)
data_rev$"Revised - Walker et al. method" = data_rev$par * data_rev$measure_total
data_rev <- data_rev %>% rename("Original GBD method" = Original)
data_rev$cause_name <- "Mental disorders"
data_rev <- data_rev %>% gather("estimate", "number", "Revised - Walker et al. method":"Revised - Vigo et al. method")

data_rev$rate_per_100k <- data_rev$number/data_rev$population
data_rev$percent <- data_rev$number/data_rev$measure_total*100

rm(data_vigo, data_vigo_global, anxiety_cause_id, bipolar_cause_id, depressive_cause_id, inclusion, mental_disorder_cause_id, schizophrenia_cause_id)

############################
##           VSL         ##
############################

# Import GDP
gdp <- read.csv(file = file.path(datapath,"wdi_gdp.csv")) %>%
  select(-(c(3:62))) %>% 
  rename("gdp" = "X2019")
gdp <- gdp %>% select(c(iso_code, gdp))

data_rev <- left_join(data_rev, gdp, by = "iso_code")
data_rev <- data_rev %>% relocate(gdp, .after = iso_code)
data_rev$population_100k <- data_rev$population
data_rev$population <- data_rev$population_100k  * 100000
data_rev$gdp_per_capita <- data_rev$gdp/data_rev$population
data_rev <- data_rev %>% relocate (gdp_per_capita, .after = gdp)
data_rev <- data_rev %>% relocate (population_100k, .after = population)





vsl_2006 <- 7400000 # Source:https://www.epa.gov/sites/production/files/2017-08/documents/ee-0568-50.pdf
gdp_deflator_2019 <- 107.494 # Source: https://data.worldbank.org/indicator/NY.GDP.DEFL.ZS?locations=US
gdp_deflator_2006 <- 86.01
vsl_2019 <- vsl_2006 * gdp_deflator_2019 / gdp_deflator_2006

vsl <- vsl_2019
rm(vsl_2019, vsl_2006, gdp_deflator_2019, gdp_deflator_2006)




world_map$deaths_vsl <- world_map$deaths * vsl
world_map$deaths_vsl_per_gdp <- world_map$deaths_vsl / world_map$gdp
world_map$deaths_vsl_per_gdp_per_cap <- world_map$deaths_vsl/(world_map$gdp/(world_map$population*100000))





############################
##           MAPS         ##
############################

world_map = map_data("world")
world_map_crosswalk <-  read.csv(file = file.path(datapath,"worldmap_crossover.csv"))
world_map <- inner_join(world_map, world_map_crosswalk, by = "region")
world_map <- world_map %>% filter(iso_code != "..") %>% select(!(c(6)))
data_rev_map <- full_join(data_rev, world_map, by = "iso_code")
rm(world_map_crosswalk, world_map)

# Rates

caption <- "Source: Global Burden of Disease Study, Vigo et al. 2016, Walker et al. 2015"

data_rev_map %>% filter(measure_id== "1") %>% 
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = rate_per_100k), color = "black", size = 0.01) + 
  theme(panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(title="Deaths due to mental disorders per 100,000",
       subtitle="2019",
       caption=caption,
       fill="Deaths") +
  scale_fill_distiller(palette = "RdYlBu") +
  facet_grid(~estimate)

# Percent of deaths

data_rev_map %>% filter(measure_id== "1") %>% 
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = percent), color = "black", size = 0.01) + 
  theme(panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(title="Deaths due to mental disorders, % of deaths",
       subtitle="2019",
       caption=caption,
       fill="% of deaths ") +
  scale_fill_viridis(option = "plasma", direction = 1,
                     limits=c(0, 15), 
                     oob=squish) +
    facet_grid(~estimate)



# 
# Check changes in rankings
# Look up if there have been changes in disability weights
# Critiques of disability weights 
# 



