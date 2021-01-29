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
  filter(measure_id== "2") %>% select(c(14,16, 17))

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
  ggplot(aes(x = long, y = lat, group = group, fill = rate)) +
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

data <- data %>% filter(cause_id %in% inclusion)

data$mh_gbd <- ifelse(data$cause_id == mental_disorder_cause_id, 1, 0)
data$mh_vigo <- ifelse(data$cause_id %in% revision_full, 1, 0)
data$mh_vigo[data$cause_id %in% revision_third] <- 1/3
data$all <- ifelse(data$cause_id == all_cause_id, 1, 0)
data$value <- data$val * data$mh_gbd
data$value_rev <- data$val * data$mh_vigo
data <- data %>% group_by(location_id, measure_id, sex_id, age_id, metric_id) %>% mutate(mh_value_agg_rev = sum(value_rev), mh_value_agg = sum(value))

rm(all_cause_id, dementia_cause_id, epilepsy_cause_id, mental_disorder_cause_id, migraine_cause_id, musculoskeletal_cause_id, 
   revision_full, revision_third, self_harm, tension_type_headache_cause_id)

data_global <- data %>% filter(location_id == 1) %>% filter(population != 1) %>% filter(metric_id == 1) %>% filter(cause_id == 294) %>% unique()

global_2019_dalys <- data_global$val[data_global$measure_id == 2]
global_2019_ylds <- data_global$val[data_global$measure_id == 3]

global_2019_dalys_mh_orig <- data_global$mh_value_agg[data_global$measure_id == 2]
global_2019_ylds_mh_orig <- data_global$mh_value_agg[data_global$measure_id == 3]

# Testing: all.equal tests should yield roughly equivalent results
# Source: http://ghdx.healthdata.org/gbd-results-tool

all.equal(125311322, global_2019_dalys_mh_orig)
all.equal(125293960, global_2019_ylds_mh_orig)
all.equal(0.0455 , global_2019_dalys_mh_orig/global_2019_dalys)
all.equal(0.1494 , global_2019_ylds_mh_orig/global_2019_ylds)

global_2019_dalys_mh_vigo <- data_global$mh_value_agg_rev[data_global$measure_id == 2]
global_2019_ylds_mh_vigo <- data_global$mh_value_agg_rev[data_global$measure_id == 3]

global_2019_dalys_mh_vigo
global_2019_ylds_mh_vigo

global_2019_dalys_mh_vigo/global_2019_dalys
global_2019_ylds_mh_vigo/global_2019_ylds


######################
##   EXPLORATION    ##
######################


# Subset of data
data_gh <- data %>%
  filter(cause_id == "294") %>% #Filters to all causes; data_gh$val == total burden by location id
  filter(metric_id == "1") %>%  #Filters to numeric metric
  select(c(1, 14:18, 22:25)) 

data_gh <- data_gh %>% rename("Revised - Vigo et al. method" = mh_value_agg_rev, "Original" = mh_value_agg)

data_gh <- data_gh %>% ungroup() %>% select(c(5:14))
data_gh_long <- data_gh %>% gather("estimate", "number", "Revised - Vigo et al. method":"Original")
data_gh_long$rate_100k <- data_gh_long$number/data_gh_long$population
data_gh_long$percent <- data_gh_long$number/data_gh_long$val*100

world_map = map_data("world")
world_map_crosswalk <-  read.csv(file = file.path(datapath,"worldmap_crossover.csv"))
world_map <- inner_join(world_map, world_map_crosswalk, by = "region")
world_map <- world_map %>% filter(iso_code != "..") %>% select(!(c(6)))

rm(world_map_crosswalk)
data_gh_long <- data_gh_long %>% filter(population != 1) %>% select(iso_code, measure_id, number, percent, rate_100k, estimate)
world_map <- inner_join(world_map, data_gh_long, by = "iso_code") 

rm(data_gh, data_gh_long)

# Rates

caption <- "Source: Global Burden of Disease Study, Vigo et al. 2016"

world_map %>% filter(measure_id== "1") %>% 
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = rate_100k), color = "black", size = 0.01) + 
  theme(panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(title="Deaths due to mental disorders per 100,000",
       subtitle="2019",
       caption=caption,
       fill="Deaths") +
  scale_fill_distiller(palette = "RdYlBu", limits = c(0,max(world_map$rate[world_map$measure_id== "1"]))) +
  facet_grid(~estimate)

world_map %>% filter(measure_id== "2") %>% 
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = rate_100k), color = "black", size = 0.01) + 
  theme(panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(title="Disability-adjusted life years due to mental disorders per 100,000",
       subtitle="2019",
       caption=caption,
       fill="DALYs") +
  scale_fill_distiller(palette = "RdYlBu", limits = c(0,max(world_map$rate[world_map$measure_id== "2"]))) +
  facet_grid(~estimate)

world_map %>% filter(measure_id== "3") %>% 
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = rate_100k), color = "black", size = 0.01) + 
  theme(panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(title="Years lived with disability due to mental disorders per 100,000",
       subtitle="2019",
       caption=caption,
       fill="YLDs ") +
  scale_fill_distiller(palette = "RdYlBu", limits = c(0,max(world_map$rate[world_map$measure_id== "3"]))) +
  facet_grid(~estimate)

world_map %>% filter(measure_id== "4") %>% 
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = rate_100k), color = "black", size = 0.01) + 
  theme(panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(title="Years of life lost due to mental disorders per 100,000",
       subtitle="2019",
       caption=caption,
       fill="YLLs ") +
  scale_fill_distiller(palette = "RdYlBu", limits = c(0,max(world_map$rate[world_map$measure_id== "4"]))) +
  facet_grid(~estimate)

# Percents of burden

world_map %>% filter(measure_id== "1") %>% 
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = percent), color = "black", size = 0.01) + 
  theme(panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(title="Deaths due to mental disorders",
       subtitle="2019",
       caption=caption,
       fill="% of deaths, total") +
  scale_fill_distiller(palette = "RdYlBu", limits = c(0,max(world_map$percent[world_map$measure_id== "1"]))) +
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
       caption=caption,
       fill="% of DALYs, total") +
  scale_fill_distiller(palette = "RdYlBu", limits = c(0,max(world_map$percent[world_map$measure_id== "2"]))) +
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
       caption=caption,
       fill="% of YLDs, total ") +
  scale_fill_distiller(palette = "RdYlBu", limits = c(0,max(world_map$percent[world_map$measure_id== "3"]))) +
  facet_grid(~estimate)

world_map %>% filter(measure_id== "4") %>% 
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = percent), color = "black", size = 0.01) + 
  theme(panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(title="Years of life lost due to mental disorders",
       subtitle="2019",
       caption=caption,
       fill="% of YLLs, total ") +
  scale_fill_distiller(palette = "RdYlBu", limits = c(0,max(world_map$percent[world_map$measure_id== "4"]))) +
  facet_grid(~estimate)


## To Do
# Walker alternative -- use percent attributable risk to calculate 
# Check changes in rankings
# Look up if there have been changes in disability weights
# Critiques of disability weights 





