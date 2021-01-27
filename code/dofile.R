
######################
##   HOUSEKEEPING   ##
######################

# setting directories
path <- "~/3. PhD/WQE/WQE II/gbd_mh"
datapath <- paste(path,'data', sep = "/")

# Loading libraries
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(maps)


######################
##       DATA       ##
######################


# Loading IHME Global Burden of Disease Data
data <- read.csv(file = file.path(datapath,"IHME-GBD_2019_DATA-4aa992a4-1.csv"))

# Adding alpha-3 codes from ISO 3166-1 (i.e., World Bank Codes)
ihme_crosswalk <- read_excel(path = file.path(datapath, "ihme_crosswalk.xlsx")) %>% 
  select(-c("location_name"))

data <- full_join(data, ihme_crosswalk, by = "location_id")

# clean up
rm(ihme_crosswalk) 

# Adding World Bank income classifications
income_level <- read_excel(path = file.path(datapath, "OGHIST.xls"), sheet = 2) %>% 
  select(-(c(2:34))) %>% 
  rename("income_level" = "2019")

data <- full_join(data, income_level, by = "iso_code")

# clean up
rm(income_level) 

# Adding population
population <- read.csv(file = file.path(datapath, "population.csv")) %>%  
  select((c(2, 3, 23))) %>% 
  filter(indicator_name == "Population, total")%>% 
  select(-(c(2)))  %>% 
  rename("population" = "X2019")
population$population <- as.character(population$population)
population$population <- as.numeric(population$population)
population$population[population$iso_code == "ERI"] <- "6711213" 

#http://ghdx.healthdata.org/geography/eritrea?page=1



data <- full_join(data, population, by = "iso_code")
data$population <- as.numeric(data$population)

# clean up
rm(population) 

# Adding rates
data <- data %>%  mutate(rate = val/population)

######################
## DATA EXPLORATION ##
######################

# Pull out world map data frame
world_map = map_data("world")
world_map_crosswalk <-  read.csv(file = file.path(datapath,"worldmap_crossover.csv"))
world_map <- inner_join(world_map, world_map_crosswalk, by = "region")
world_map <- world_map %>% filter(iso_code != "..") %>% select(!(c(6)))

rm(world_map_crosswalk)

# Subset of data
data_selfharm_dalys <- data %>% filter(cause_name == "Self-harm") %>%
  filter(metric_id == "1") %>%
  filter(measure_id== "2") %>% select(c(14,17, 20))

world_map <- inner_join(world_map, data_selfharm_dalys, by = "iso_code")
rm(data_selfharm_dalys)

# DALYs due to self-harm, totals

world_map %>% 
  ggplot(aes(x = long, y = lat, group = group, fill = val/1000000)) +
  geom_polygon(color = "black", size = 0.01) + 
  theme(panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(title="Disability-adjusted life years due to self-harm, millions",
       subtitle="2019",
       caption="Source: Global Burden of Disease Study",
       fill="DALYs, total") +
  scale_fill_distiller(palette = "RdYlBu")

# DALYs due to self-harm, rate

world_map %>% 
  ggplot(aes(x = long, y = lat, group = group, fill = rate*100000)) +
  geom_polygon(color = "black", size = 0.01) + 
  theme(panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(title="Disability-adjusted life years due to self-harm per 100,000",
       subtitle="2019, all sexes and ages",
       caption="Source: Global Burden of Disease Study",
       fill="DALYs, total") +
  scale_fill_distiller(palette = "RdYlBu")

############################
## vIGO ET AL REPLICATION ##
############################

####### Load 2013 data

# Loading IHME Global Burden of Disease Data
data_2019 <- read.csv(file = file.path(datapath,"IHME-GBD_2019_DATA-2013.csv"))

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

data_2019$mh_gbd <- ifelse(data_2019$cause == mental_disorder_cause_id, 1, 0)
data_2019$mh_vigo <- ifelse(data_2019$cause %in% revision_full, 1, 0)
data_2019$mh_vigo[data_2019$cause %in% revision_third] <- 1/3
data_2019$all <- ifelse(data_2019$cause == all_cause_id, 1, 0)
data_2019$value <- data_2019$val * data_2019$mh_gbd
data_2019$value_rev <- data_2019$val * data_2019$mh_vigo
data_2019 <- data_2019 %>% group_by(location, measure, sex, age, metric) %>% mutate(mh_value_agg_rev = sum(value_rev), mh_value_agg = sum(value))

data_2019_global <- data_2019 %>% filter(location == 1) %>% unique()

global_2019_dalys <- data_2019_global$val[data_2019_global$cause == 294 & data_2019_global$measure == 2]
global_2019_ylls <- data_2019_global$val[data_2019_global$cause == 294 & data_2019_global$measure == 3]

global_2019_dalys_mh_orig <- data_2019_global$mh_value_agg[data_2019_global$cause == 294 & data_2019_global$measure == 2]
global_2019_ylls_mh_orig <- data_2019_global$mh_value_agg[data_2019_global$cause == 294 & data_2019_global$measure == 3]

# Testing: all.equal tests should yield roughly equivalent results
# Source: http://ghdx.healthdata.org/gbd-results-tool

all.equal(115674919, global_2019_dalys_mh_orig)
all.equal(115658862, global_2019_ylls_mh_orig)
all.equal(0.0455 , global_2019_dalys_mh_orig/global_2019_dalys)
all.equal(0.1494 , global_2019_ylls_mh_orig/global_2019_ylls)

# global_2019_dalys_mh_vigo <- data_2019_global$mh_value_agg_rev[data_2019_global$cause == 294 & data_2019_global$measure == 2]
# global_2019_ylls_mh_vigo <- data_2019_global$mh_value_agg_rev[data_2019_global$cause == 294 & data_2019_global$measure == 3]

# global_2019_dalys_mh_vigo
# global_2019_ylls_mh_vigo

# global_2019_dalys_mh_vigo/global_2019_dalys
# global_2019_ylls_mh_vigo/global_2019_ylls


# 

data$mh_gbd <- ifelse(data$cause_id == mental_disorder_cause_id, 1, 0)
data$mh_vigo <- ifelse(data$cause_id %in% revision_full, 1, 0)
data$mh_vigo[data$cause_id %in% revision_third] <- 1/3
data$all <- ifelse(data$cause_id == all_cause_id, 1, 0)
data$value <- data$val * data$mh_gbd
data$value_rev <- data$val * data$mh_vigo
data <- data %>% group_by(location_id, measure_id, sex_id, age_id, metric_id) %>% mutate(mh_value_agg_rev = sum(value_rev), mh_value_agg = sum(value))

rm(all_cause_id, dementia_cause_id, epilepsy_cause_id, mental_disorder_cause_id, migraine_cause_id, musculoskeletal_cause_id, 
   revision_full, revision_third, self_harm, tension_type_headache_cause_id)

data_global <- data %>% filter(location_id == 1) %>% unique()

global_2019_dalys <- data_global$val[data_global$cause_id == 294 & data_global$measure_id == 2]
global_2019_ylls <- data_global$val[data_global$cause_id == 294 & data_global$measure_id == 3]

global_2019_dalys_mh_orig <- data_global$mh_value_agg[data_global$cause_id == 294 & data_global$measure_id == 2]
global_2019_ylls_mh_orig <- data_global$mh_value_agg[data_global$cause_id == 294 & data_global$measure_id == 3]

# Testing: all.equal tests should yield roughly equivalent results
# Source: http://ghdx.healthdata.org/gbd-results-tool

all.equal(115674919, global_2019_dalys_mh_orig)
all.equal(115658862, global_2019_ylls_mh_orig)
all.equal(0.0455 , global_2019_dalys_mh_orig/global_2019_dalys)
all.equal(0.1494 , global_2019_ylls_mh_orig/global_2019_ylls)

global_2019_dalys_mh_vigo <- data_2019_global$mh_value_agg_rev[data_2019_global$cause_id == 294 & data_2019_global$measure_id == 2]
global_2019_ylls_mh_vigo <- data_2019_global$mh_value_agg_rev[data_2019_global$cause_id == 294 & data_2019_global$measure_id == 3]

global_2019_dalys_mh_vigo
global_2019_ylls_mh_vigo

global_2019_dalys_mh_vigo/global_2019_dalys
global_2019_ylls_mh_vigo/global_2019_ylls


######################
##   EXPLORATION    ##
######################


# Subset of data
data_gh <- data %>%
  filter(cause_id == "294") %>%
  filter(metric_id == "1") %>%
  select(c(1, 14,17, 19, 26, 27)) 

data_gh <- data_gh %>% rename("Revised" = mh_value_agg_rev, "Original" = mh_value_agg)
data_gh <- data_gh %>% ungroup() %>% select(c(5:10))
data_gh_long <- data_gh %>% gather("estimate", "number", Revised:Original)
data_gh_long$rate <- data_gh_long$number/data_gh_long$population*100000
data_gh_long$percent <- data_gh_long$number/data_gh_long$val*100

world_map = map_data("world")
world_map_crosswalk <-  read.csv(file = file.path(datapath,"worldmap_crossover.csv"))
world_map <- inner_join(world_map, world_map_crosswalk, by = "region")
world_map <- world_map %>% filter(iso_code != "..") %>% select(!(c(6)))

rm(world_map_crosswalk)

world_map <- inner_join(world_map, data_gh_long, by = "iso_code") 
rm(data_gh, data_gh_long)

world_map %>% filter(measure_id== "2") %>% 
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = rate), color = "black", size = 0.01) + 
  theme(panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(title="Disability-adjusted life years due to mental disorders per 100,000",
       subtitle="2019",
       caption="Source: Global Burden of Disease Study",
       fill="DALYs") +
  scale_fill_distiller(palette = "RdYlBu", limits = c(0,max(world_map$rate))) +
  facet_grid(~estimate)

world_map %>% filter(measure_id== "3") %>% 
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = rate), color = "black", size = 0.01) + 
  theme(panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(title="Years lived with disability due to mental disorders per 100,000",
       subtitle="2019",
       caption="Source: Global Burden of Disease Study",
       fill="YLDs ") +
  scale_fill_distiller(palette = "RdYlBu", limits = c(0,max(world_map$rate))) +
  facet_grid(~estimate)



world_map %>% filter(measure_id== "2") %>% 
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = percent), color = "black", size = 0.01) + 
  theme(panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(title="Disability-adjusted life years due to mental disorders",
       subtitle="2019",
       caption="Source: Global Burden of Disease Study",
       fill="% of DALYs, total") +
  scale_fill_distiller(palette = "RdYlBu", limits = c(0,max(world_map$percent))) +
  facet_grid(~estimate)

world_map %>% filter(measure_id== "3") %>% 
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = percent), color = "black", size = 0.01) + 
  theme(panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(title="Years lived with disability due to mental disorders",
       subtitle="2019",
       caption="Source: Global Burden of Disease Study",
       fill="% of YLDs, total ") +
  scale_fill_distiller(palette = "RdYlBu", limits = c(0,max(world_map$percent))) +
  facet_grid(~estimate)


## To Do
# Redownload data to include 
# Fix scales
# Add all causes and all locations line for totals
# Do not include percents or rates
# Look up if there have been changes in disability weights
# Critiques of disability weights 
# Consider looking at deaths - especially suicide





