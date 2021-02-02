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

############################
##  WALKER ET AL METHOD  ##
############################



# Loading IHME Global Burden of Disease Data
data_prev <- read.csv(file = file.path(datapath,"IHME-GBD_2019_DATA-2019-prev.csv"))

# Adding alpha-3 codes from ISO 3166-1 (i.e., World Bank Codes)
ihme_crosswalk <- read_excel(path = file.path(datapath, "ihme_crosswalk.xlsx")) %>% 
  select(-c("location_name"))

data_prev <- left_join(data_prev, ihme_crosswalk, by = "location_id")

# clean up
rm(ihme_crosswalk) 

# Adding World Bank income classifications
income_level <- read_excel(path = file.path(datapath, "OGHIST.xls"), sheet = 2) %>% 
  select(-(c(2:34))) %>% 
  rename("income_level" = "2019")

data_prev <- left_join(data_prev, income_level, by = "iso_code")

# clean up
rm(income_level) 

# Adding population
data_pop <- data %>% ungroup %>% filter(cause_id==294 & measure_id == 1 & metric_id == 1) %>% select(population, iso_code) %>%  filter(population != 1)  %>% unique()
data_prev <- left_join(data_prev, data_pop, by = "iso_code") %>%  filter(population != 1)  %>% unique()
rm(data_pop) 

# Adding total all cause mortality

data_deaths <- read.csv(file = file.path(datapath,"IHME-GBD_2019_DATA-2019-deaths.csv"))
data_deaths <- data_deaths %>% rename("deaths" = val) %>% select (location_id, deaths)
data_prev <- left_join(data_prev, data_deaths, by = "location_id")
rm(data_deaths)

# source: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4461039/
# Due to the distributive property of the Population Attributable Fraction, a category-specific attribution fraction is estimated
# using the formula from Rockhill et al, 1998:https://pubmed.ncbi.nlm.nih.gov/9584027/  
PAR <- function(RR, prev){
  attrib_risk <-   
    prev *
    ((RR - 1) /    RR)
  return(attrib_risk)
}

PAR(2.22, 0.194)
PAR(1.86,.106)
PAR(1.43,.143)
PAR(2.54,.0104)

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

data_prev$attrib_deaths <- data_prev$par * data_prev$deaths

world_map = map_data("world")
world_map_crosswalk <-  read.csv(file = file.path(datapath,"worldmap_crossover.csv"))
world_map <- inner_join(world_map, world_map_crosswalk, by = "region")
world_map <- world_map %>% filter(iso_code != "..") %>% select(!(c(6)))

rm(world_map_crosswalk)
data_prev <- data_prev  %>% select(iso_code, cause_id, measure_id, attrib_deaths, par)
world_map <- inner_join(world_map, data_prev, by = "iso_code") 

world_map %>% filter(cause_id == mental_disorder_cause_id) %>%
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = par*100), color = "black", size = 0.01) + 
  theme(panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(title="Percent of deaths attributable to mental disorders",
       subtitle="2019",
       caption=caption,
       fill="% of deaths, total ") +
  scale_fill_distiller(palette = "RdYlBu", limits = c(0,max(100*world_map$par[world_map$cause_id== mental_disorder_cause_id]))) 

# Pulling together deaths


# Subset of data
data_gh <- data %>%
  filter(cause_id == "294") %>% #Filters to all causes; data_gh$val == total burden by location id
  filter(metric_id == "1") %>%  #Filters to numeric metric
  filter(measure_id == "1") %>%  #Filters to deaths
  select(c(1, 14:18, 22:25)) 

data_prev_mh <- data_prev %>% filter(cause_id == mental_disorder_cause_id) %>% unique()
data_gh <- full_join(data_prev_mh, data_gh, by = "iso_code")%>% 
  select(iso_code, population, val, attrib_deaths, par, location_id, mh_value_agg_rev, mh_value_agg)  %>% filter(population != 1) %>%
  unique() 
data_gh$mh_val_walker <- data_gh$attrib_deaths

data_gh <- data_gh %>% rename("Revised - Vigo et al. method" = mh_value_agg_rev, "Original GBD method" = mh_value_agg, "Revised - Walker et al. method" = mh_val_walker) %>% select(!c(attrib_deaths))
data_gh_long <- data_gh %>% gather("estimate", "number", "Revised - Walker et al. method":"Revised - Vigo et al. method") %>% select(!c(par, location_id))

data_gh_long$rate_100k <- data_gh_long$number/data_gh_long$population
data_gh_long$percent <- data_gh_long$number/data_gh_long$val*100

world_map = map_data("world")
world_map_crosswalk <-  read.csv(file = file.path(datapath,"worldmap_crossover.csv"))
world_map <- inner_join(world_map, world_map_crosswalk, by = "region")
world_map <- world_map %>% filter(iso_code != "..") %>% select(!(c(6)))

rm(world_map_crosswalk)
world_map <- inner_join(data_gh_long, world_map, by = "iso_code") %>% rename("deaths" = number, "deaths_total" = val)
rm(data_gh, data_gh_long)

# Rates

caption <- "Source: Global Burden of Disease Study, Vigo et al. 2016, Walker et al. 2015"

world_map %>% 
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
  scale_fill_distiller(palette = "RdYlBu", limits = c(0,max(world_map$rate))) +
  facet_grid(~estimate)

world_map %>% 
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
       fill="% of deaths") +
  scale_fill_viridis(option = "plasma", 
                     limits=c(0, 20), 
                     oob=squish) +
    facet_grid(~estimate)

## To Do
# Walker alternative -- use percent attributable risk to calculate 
# Check changes in rankings
# Look up if there have been changes in disability weights
# Critiques of disability weights 





