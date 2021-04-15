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
path <- "~/3. PhD/WQE/WQE II/gmh_econ"
datapath <- paste(path,'data', sep = "/")
resultspath <- paste(path,'results', sep = "/")

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

# Adding population (backed out using IHME rate data for consistency)
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

# VIGO METHOD: ALLOCATION OF THE FOLLOWING TO MENTAL HEALTH 
# (PERCENTS DENOTE AMOUNT REALLOCATED TO MENTAL HEALTH)

# Dementia - 100%
# Epilepsy - 100%
# Migraine - 100%
# Tension-type headache - 100%
# Self-harm - 100%
# Chronic pain syndrom currently attributed to musculoskeletal disorders - 33%

############################

# Relevant IHME cause IDs
mental_disorder_cause_id <- 558
dementia_cause_id <- c(543, 544)
epilepsy_cause_id <- 545
migraine_cause_id <- 547
tension_type_headache_cause_id <- 548
self_harm <- 718
musculoskeletal_cause_id <- 626
all_cause_id <- 294

# Causes to fully include under mental disorders
revision_full <- c(mental_disorder_cause_id,
                   dementia_cause_id,
                   epilepsy_cause_id,
                   migraine_cause_id,
                   tension_type_headache_cause_id,
                   self_harm)

# Causes to include under mental disorders, 1/3rd
revision_third <- musculoskeletal_cause_id

# Relevant causes to retain
inclusion <- c(mental_disorder_cause_id,
               dementia_cause_id,
               epilepsy_cause_id,
               migraine_cause_id,
               tension_type_headache_cause_id,
               self_harm,
               musculoskeletal_cause_id,
               all_cause_id)

data_vigo <- data %>% filter(cause_id %in% inclusion)

# Applying Vigo et al. re-allocations

data_vigo$mh_gbd <- ifelse(data_vigo$cause_id == mental_disorder_cause_id, 1, 0)  # Fully retain existing mental disorders
data_vigo$mh_vigo <- ifelse(data_vigo$cause_id %in% revision_full, 1, 0)          # Allocate 100% of these conditions
data_vigo$mh_vigo[data_vigo$cause_id %in% revision_third] <- 1/3                  # Allocate 33% of these conditions
data_vigo$all <- ifelse(data_vigo$cause_id == all_cause_id, 1, 0)                 # Binary in case of all_cause 
data_vigo$value <- data_vigo$val * data_vigo$mh_gbd                               # Retain original IHME values
data_vigo$value_rev <- data_vigo$val * data_vigo$mh_vigo                          # Apply allocation percents against original IHME values

# Combine original and re-allocated values by location, measure, sex (both), age (all), and metric (number)
data_vigo <- data_vigo %>% 
  group_by(location_id, measure_id, sex_id, age_id, metric_id) %>% 
  mutate(mh_value_agg_rev = sum(value_rev), mh_value_agg = sum(value))

# Clean up
rm(all_cause_id, dementia_cause_id, epilepsy_cause_id, mental_disorder_cause_id, migraine_cause_id, musculoskeletal_cause_id, 
   revision_full, revision_third, self_harm, tension_type_headache_cause_id)

# Pull out global data to aid in calculations 
data_vigo_global <- data_vigo %>% filter(location_id == 1) %>% filter(population != 1) %>% filter(metric_id == 1) %>% filter(cause_id == 294) %>% unique()

global_2019_dalys <- data_vigo_global$val[data_vigo_global$measure_id == 2]
global_2019_ylds <- data_vigo_global$val[data_vigo_global$measure_id == 3]

global_2019_dalys_mh_orig <- data_vigo_global$mh_value_agg[data_vigo_global$measure_id == 2]
global_2019_ylds_mh_orig <- data_vigo_global$mh_value_agg[data_vigo_global$measure_id == 3]

# Testing: all.equal tests should yield roughly equivalent results
# Source: http://ghdx.healthdata_vigo.org/gbd-results-tool
# This confirms that the sums of original IHME DALYS and YLDS were correctly replicated

all.equal(125311322, global_2019_dalys_mh_orig)
all.equal(125293960, global_2019_ylds_mh_orig)
all.equal(0.0455 , global_2019_dalys_mh_orig/global_2019_dalys)
all.equal(0.1494 , global_2019_ylds_mh_orig/global_2019_ylds)

# Subset of data
data_rev <- data_vigo %>%
  filter(cause_id == "294") %>% #Filters to all causes; data_gh$val == total burden by location id
  filter(metric_id == "1") %>%  #Filters to numeric metric
  rename("Revised - Vigo et al. method" = mh_value_agg_rev, "Original" = mh_value_agg, "measure_total" = val) %>%
  filter(measure_id %in% 1:4) %>% #Retains only deaths, DALYS, YLDs, and YLLs
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

# Relevant IHME cause IDs
mental_disorder_cause_id <- 558
schizophrenia_cause_id <- 559
depressive_cause_id <- 567
bipolar_cause_id <- 570
anxiety_cause_id <- 571 

# Relative risks from Walker et al.
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
    1.43))
colnames(relative_risk) <- c("cause_id", "rel_risk")

# Matching relative risks to IHME cause IDs
data_prev <- inner_join(data_prev, relative_risk, by = "cause_id")

# Calculating PAR using formula from Walker et al.
data_prev$par <-     data_prev$val  * ((data_prev$rel_risk - 1) /    data_prev$rel_risk)

# Filtering for mental disorders combined, only
data_prev_allmentaldisorders <- data_prev %>% filter(cause_id == mental_disorder_cause_id) %>% select(c(location_id,par))

# Joining PAR for mental disorders to dataframe
data_rev <- full_join(data_rev, data_prev_allmentaldisorders, by = "location_id")

# Using PAR to calculate attributable burden due to mental disorders
data_rev$"Revised - Walker et al. method" = data_rev$par * data_rev$measure_total

# Clean up, re-ordering, and renaming columns
rm(relative_risk, data_prev_allmentaldisorders)
data_rev <- data_rev %>% relocate(par, .after = population)
data_rev <- data_rev %>% rename("Original GBD method" = Original)

############################
##  COMPOSITE METHOD  ##
############################

# This method pulls YLLs from Walker et al. and YLDs from Vigo et al. together to re-estimate 
# DALYs attributable to mental disorders. This captures attributable mortality from mental disorders
# (not captured by Vigo et al.) without double counting DALYs due to suicide.

# Draw in YLDs from Vigo et al.
data_rev$composite_ylds <-ifelse(data_rev$measure_id == 3, data_rev$"Revised - Vigo et al. method", 0)

# Draw in YLLs from Walker et al.
data_rev$composite_ylls <-ifelse(data_rev$measure_id == 4, data_rev$"Revised - Walker et al. method", 0)

# Combine to calculate DALYs
data_rev_composite <- data_rev %>% select(location_id, measure_id, composite_ylds, composite_ylls) %>%
  group_by(location_id) %>%
  mutate(composite_dalys = sum(composite_ylds, composite_ylls))

data_rev_composite <- data_rev_composite %>% filter(measure_id == 2) %>% ungroup() %>% unique() %>% select(c(1,5))
data_rev <- inner_join(data_rev, data_rev_composite, by = "location_id")
data_rev$composite_dalys[data_rev$measure_id != 2] <- 0
data_rev$composite_ylds[data_rev$measure_id != 3] <- 0
data_rev$composite_ylls[data_rev$measure_id != 4] <- 0

# Draw in deaths from Walker et al.
data_rev$composite_deaths <-ifelse(data_rev$measure_id == 1, data_rev$"Revised - Walker et al. method", 0)
data_rev$composite_deaths[data_rev$measure_id != 1] <- 0


# Combined columns, clean up, re-organizing, tidying, and labeling
data_rev$composite <- data_rev$composite_ylds + data_rev$composite_ylls + data_rev$composite_dalys + data_rev$composite_deaths
data_rev <- data_rev %>% select(!c(14:17))
data_rev <- data_rev %>% rename("Revised - Composite method" = composite)
data_rev <- data_rev %>% relocate(12, .after = par)
data_rev$cause_name <- "Mental disorders"
data_rev <- data_rev %>% gather("estimate", "number", 11:14)
rm(data_vigo, data_vigo_global, anxiety_cause_id, bipolar_cause_id, depressive_cause_id, inclusion, mental_disorder_cause_id, schizophrenia_cause_id, data_rev_composite)
data_rev$estimate_id <- ifelse(data_rev$estimate == "Revised - Composite method", 4, 
                               ifelse(data_rev$estimate == "Revised - Walker et al. method", 3,
                                      ifelse(data_rev$estimate == "Revised - Vigo et al. method", 2, 1)))
data_rev <- data_rev %>% relocate(estimate_id, .after = estimate)

# Calculate rates and percents of burden
data_rev$population_100k <- data_rev$population
data_rev$population <- data_rev$population_100k  * 100000
data_rev <- data_rev %>% relocate (population_100k, .after = population)
data_rev$rate_per_100k <- data_rev$number/data_rev$population_100k
data_rev$percent <- data_rev$number/data_rev$measure_total*100

############################
##        COSTING         ##
############################

# Import GDP
gdp <- read.csv(file = file.path(datapath,"wdi_gdp.csv")) %>%
  select(-(c(3:62))) %>% 
  rename("gdp" = "X2019")
gdp <- gdp %>% select(c(iso_code, gdp))
data_rev <- left_join(data_rev, gdp, by = "iso_code")
data_rev <- data_rev %>% relocate(gdp, .after = iso_code)
data_rev$gdp[data_rev$location_id == 1] <- 87798525859220.9          # Source: World Bank, WDI

# Calculate GDP per capita, with reference to IHME population values, for consistency
data_rev$gdp_per_capita <- data_rev$gdp/data_rev$population
data_rev <- data_rev %>% relocate (gdp_per_capita, .after = gdp)

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

# Pulling out global estimates
data_global <- data_rev %>% filter(location_id == 1) %>% filter(measure_id == 2)

############################
##         MAPS           ##
############################

# Import map data
world <- ne_countries(scale = "medium", returnclass = "sf")
world$iso_code <- world$gu_a3
world$iso_code[world$adm0_a3 == "SDS"] <- "SSD"


# Merging map and dataframe
data_rev_map <- full_join(world, data_rev, by = "iso_code")

# Clean up
rm(world)

# Re-order estimates: original, Vigo, Walker, composite
data_rev_map$estimate <- factor(data_rev_map$estimate,      # Reordering group factor levels
                                levels = c("Original GBD method",
                                           "Revised - Vigo et al. method",
                                           "Revised - Walker et al. method",
                                           "Revised - Composite method"))

# Titles, subtitles, and captions

subtitle_1 <- "Value per DALY: $1,000"
subtitle_2 <- "Value per DALY: $5,000"
subtitle_3 <- "Value per DALY: GDP/capita"
subtitle_4 <- "Value per DALY: 3 X GDP/capita"
caption <- "Source: Global Burden of Disease Study, Vigo et al. 2016"

########################
# Graphs


map_deaths_per_cap <-
  data_rev_map %>% 
  filter(measure_id== "1") %>% 
  filter(estimate_id %in% c(1,2,4)) %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = rate_per_100k), color = "black", size = 0.01) +
  theme(panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(title="Deaths due to mental disorders per 100,000",
       subtitle="2019",
       caption=caption,
       fill="Deaths") +
  scale_fill_distiller(palette = "RdYlBu",
                       limits = c(0,100),
                       oob = squish) +
  facet_grid(~estimate)

ggsave(filename = "fig1.png", plot = last_plot(), 
       path = resultspath,
       width = 10,
       height = 4)

# DALYS per capita

map_dalys_per_cap <-
  data_rev_map %>% filter(measure_id== "2") %>% 
  filter(estimate_id %in% c(1,2,4)) %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = rate_per_100k), color = "black", size = 0.01) +
  theme(panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(title="DALYs due to mental disorders per 100,000",
       subtitle="2019",
       caption=caption,
       fill="DALYs") +
  scale_fill_distiller(palette = "RdYlBu",
                       limits = c(0,6000),
                       oob = squish) +
  facet_grid(~estimate)

ggsave(filename = "fig2.png", plot = last_plot(), 
       path = resultspath,
       width = 10,
       height = 4)

# Percent of deaths

map_deaths_percent <-
  data_rev_map %>% filter(measure_id== "1") %>% 
  filter(estimate_id %in% c(1,2,4)) %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = percent), color = "black", size = 0.01) +
  theme(panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(title="Deaths due to mental disorders, % of deaths",
       subtitle="2019",
       caption=caption,
       fill="Percent") +
  scale_fill_distiller(palette = "YlGn",
                       direction = 1,
                       limits = c(0,15),
                       oob = squish) +
  facet_grid(~estimate)

ggsave(filename = "fig3.png", plot = last_plot(), 
       path = resultspath,
       width = 10,
       height = 4)

# Percent of DALYs

map_dalys_percent <-
  data_rev_map %>% filter(measure_id== "2") %>% 
  filter(estimate_id %in% c(1,2,4)) %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = percent), color = "black", size = 0.01) +
  theme(panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(title="DALYs due to mental disorders, % of DALYs",
       subtitle="2019",
       caption=caption,
       fill="Percent") +
  scale_fill_distiller(palette = "YlGn",
                       direction = 1,
                       limits = c(0,25),
                       oob = squish) +
  facet_grid(~estimate)

ggsave(filename = "fig4.png", plot = last_plot(), 
       path = resultspath,
       width = 10,
       height = 4)

# Value (CC1), % of GDP

map_value_cc1 <- 
  data_rev_map %>% filter(measure_id== "2") %>% 
  filter(estimate_id %in% c(1,2,4)) %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = cost_cc1/gdp*100), color = "black", size = 0.01) +
    theme(panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(title="Value of DALYs due to mental disorders in current USD, % of GDP",
       subtitle=subtitle_1,
       caption=caption,
       fill="% of GDP") +
  scale_fill_distiller(palette = "OrRd", direction = 1,
                       limit = c(0, 25),
                       oob = squish) +
  facet_grid(~estimate)

ggsave(filename = "fig5.png", plot = last_plot(), 
       path = resultspath,
       width = 10,
       height = 3)

# Value (CC2), % of GDP

map_value_cc2 <- 
  data_rev_map %>% filter(measure_id== "2") %>% 
  filter(estimate_id %in% c(1,2,4)) %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = cost_cc2/gdp*100), color = "black", size = 0.01) +
  theme(panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(title="Value of DALYs due to mental disorders in current USD, % of GDP",
       subtitle=subtitle_2,
       caption=caption,
       fill="% of GDP") +
  scale_fill_distiller(palette = "OrRd", direction = 1,
                       limit = c(0, 25),
                       oob = squish) +
  facet_grid(~estimate)

ggsave(filename = "fig6.png", plot = last_plot(), 
       path = resultspath,
       width = 10,
       height = 3)

# Value (WHO1), % of GDP

map_value_who1 <- 
  data_rev_map %>% filter(measure_id== "2") %>% 
  filter(estimate_id %in% c(1,2,4)) %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = cost_who1/gdp*100), color = "black", size = 0.01) +
  theme(panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(title="Value of DALYs due to mental disorders in current USD, % of GDP",
       subtitle=subtitle_3,
       caption=caption,
       fill="% of GDP") +
  scale_fill_distiller(palette = "OrRd", direction = 1,
                       limits = c(0,25),
                       oob = squish) +
  facet_grid(~estimate)

ggsave(filename = "fig7.png", plot = last_plot(), 
       path = resultspath,
       width = 10,
       height = 3)

# Value (WHO2), % of GDP

map_value_who2 <- 
  data_rev_map %>% filter(measure_id== "2") %>% 
  filter(estimate_id %in% c(1,2,4)) %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = cost_who2/gdp*100), color = "black", size = 0.01) +
  theme(panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(title="Value of DALYs due to mental disorders in current USD, % of GDP",
       subtitle=subtitle_4,
       caption=caption,
       fill="% of GDP") +
  scale_fill_distiller(palette = "OrRd", direction = 1,
                       limits = c(0,25),
                       oob = squish) +
  facet_grid(~estimate)

ggsave(filename = "fig8.png", plot = last_plot(), 
       path = resultspath,
       width = 10,
       height = 3)

# Combined value maps


map_value_cc1_notitle <- 
  data_rev_map %>% filter(measure_id== "2") %>% 
  filter(estimate_id %in% c(1,2,4)) %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = cost_cc1/gdp*100), color = "black", size = 0.01) +
  theme(panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(title=subtitle_1,
       fill="% of GDP") +
  scale_fill_distiller(palette = "OrRd", direction = 1,
                       limit = c(0, 25),
                       oob = squish) +
  facet_grid(~estimate)

map_value_cc2_notitle <- 
  data_rev_map %>% filter(measure_id== "2") %>% 
  filter(estimate_id %in% c(1,2,4)) %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = cost_cc2/gdp*100), color = "black", size = 0.01) +
  theme(panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(title=subtitle_2,
       fill="% of GDP") +
  scale_fill_distiller(palette = "OrRd", direction = 1,
                       limit = c(0, 25),
                       oob = squish) +
  facet_grid(~estimate)


map_value_who1_notitle <- 
  data_rev_map %>% filter(measure_id== "2") %>% 
  filter(estimate_id %in% c(1,2,4)) %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = cost_who1/gdp*100), color = "black", size = 0.01) +
  theme(panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(title=subtitle_3,
       fill="% of GDP") +
  scale_fill_distiller(palette = "OrRd", direction = 1,
                       limits = c(0,25),
                       oob = squish) +
  facet_grid(~estimate)


map_value_who2_notitle <- 
  data_rev_map %>% filter(measure_id== "2") %>% 
  filter(estimate_id %in% c(1,2,4)) %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = cost_who2/gdp*100), color = "black", size = 0.01) +
  theme(panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(title=subtitle_4,
       fill="% of GDP") +
  scale_fill_distiller(palette = "OrRd", direction = 1,
                       limits = c(0,25),
                       oob = squish) +
  facet_grid(~estimate)


map_value_combined <- ggarrange(map_value_cc1_notitle, map_value_cc2_notitle, map_value_who1_notitle, map_value_who2_notitle,
                        ncol = 1, nrow = 4)


ggsave(filename = "fig9.png", plot = last_plot(), 
       path = resultspath,
       width = 10,
       height = 8)



### Region maps

# Load in region data
region <- read_excel(path = file.path(datapath, "regions.xlsx"))

# Merge
region <- region %>% select (iso_code, who_region, ihme_region)

data_rev_map_region <- full_join(data_rev_map, region, by = "iso_code")

# WHO regions

data_rev_who_region <- data_rev_map_region %>% group_by(estimate, measure_id, who_region) %>% 
  mutate(region_pop100k = sum(population_100k),
         region_number = sum(number),
         region_measure_total = sum(measure_total),
         region_gdp = sum(gdp, na.rm=TRUE)
  ) %>%
  mutate(region_percent = region_number/region_measure_total*100,
         region_population = region_pop100k * 100000)

data_rev_who_region$region_gdp_per_capita <- data_rev_who_region$region_gdp/data_rev_who_region$region_population

data_rev_who_region$region_cost_cc1 <- ifelse(data_rev_who_region$measure_id == 2,
                                              data_rev_who_region$region_number * 1000, 0)

data_rev_who_region$region_cost_cc2 <- data_rev_who_region$region_cost_cc1 * 5

data_rev_who_region$region_cost_who1 <-ifelse(data_rev_who_region$measure_id == 2,
                                              data_rev_who_region$region_number * data_rev_who_region$region_gdp_per_capita, 0)

data_rev_who_region$region_cost_who2 <- data_rev_who_region$region_cost_who1 * 3



# Deaths per capita

map_deaths_per_cap_who_region <- 
  data_rev_who_region %>% 
  filter(measure_id== "1") %>% 
  filter(estimate_id %in% c(1,2,4)) %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = region_number/region_pop100k), color = "white", size = 0.01) +
  geom_sf(data = . %>%   group_by(who_region) %>% st_set_precision(1e4) %>%
            summarize(geometry = st_union(geometry)), fill = "transparent", color = 'black', size = 0.01) +
  theme(panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(title="Deaths due to mental disorders per 100,000",
       subtitle="2019",
       caption=caption,
       fill="Deaths") +
  scale_fill_distiller(palette = "RdYlBu",
                       limits = c(0,100),
                       oob = squish) +
  coord_sf(ndiscr = F) + 
  facet_grid(~estimate) 

ggsave(filename = "fig1_who_region.png", plot = last_plot(), 
       path = resultspath,
       width = 10,
       height = 4)


# DALYS per capita

map_dalys_per_cap_who_region <-
  data_rev_who_region %>% filter(measure_id== "2") %>% 
  filter(estimate_id %in% c(1,2,4)) %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = region_number/region_pop100k), color = "white", size = 0.01) +
  geom_sf(data = . %>%   group_by(who_region) %>% st_set_precision(1e4) %>%
            summarize(geometry = st_union(geometry)), fill = "transparent", color = 'black', size = 0.01) +
  theme(panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(title="DALYs due to mental disorders per 100,000",
       subtitle="2019",
       caption=caption,
       fill="DALYs") +
  scale_fill_distiller(palette = "RdYlBu",
                       limits = c(0,6000),
                       oob = squish) +
  facet_grid(~estimate)

ggsave(filename = "fig2_who_region.png", plot = last_plot(), 
       path = resultspath,
       width = 10,
       height = 4)

# Percent of deaths

map_deaths_percent_who_region <-
  data_rev_who_region %>% filter(measure_id== "1") %>% 
  filter(estimate_id %in% c(1,2,4)) %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = region_percent), color = "white", size = 0.01) +
  geom_sf(data = . %>%   group_by(who_region) %>% st_set_precision(1e4) %>%
            summarize(geometry = st_union(geometry)), fill = "transparent", color = 'black', size = 0.01) +
  theme(panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(title="Deaths due to mental disorders, % of deaths",
       subtitle="2019",
       caption=caption,
       fill="Percent") +
  scale_fill_distiller(palette = "YlGn",
                       direction = 1,
                       limits = c(0,15),
                       oob = squish) +
  facet_grid(~estimate)

ggsave(filename = "fig3_who_region.png", plot = last_plot(), 
       path = resultspath,
       width = 10,
       height = 4)

# Percent of DALYs

map_dalys_percent_who_region <-
  data_rev_who_region %>% filter(measure_id== "2") %>% 
  filter(estimate_id %in% c(1,2,4)) %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = region_percent), color = "white", size = 0.01) +
  geom_sf(data = . %>%   group_by(who_region) %>% st_set_precision(1e4) %>%
            summarize(geometry = st_union(geometry)), fill = "transparent", color = 'black', size = 0.01) +
  theme(panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(title="DALYs due to mental disorders, % of DALYs",
       subtitle="2019",
       caption=caption,
       fill="Percent") +
  scale_fill_distiller(palette = "YlGn",
                       direction = 1,
                       limits = c(0,25),
                       oob = squish) +
  facet_grid(~estimate)

ggsave(filename = "fig4_who_region.png", plot = last_plot(), 
       path = resultspath,
       width = 10,
       height = 4)


# Value (CC1), % of GDP

map_value_cc1_who_region <- 
  data_rev_who_region %>% filter(measure_id== "2") %>% 
  filter(estimate_id %in% c(1,2,4)) %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = region_cost_cc1/region_gdp * 100), color = "white", size = 0.01) +
  geom_sf(data = . %>%   group_by(who_region) %>% st_set_precision(1e4) %>%
            summarize(geometry = st_union(geometry)), fill = "transparent", color = 'black', size = 0.01) +
  theme(panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(title="Value of DALYs due to mental disorders in current USD, % of GDP",
       subtitle=subtitle_1,
       caption=caption,
       fill="% of GDP") +
  scale_fill_distiller(palette = "OrRd", direction = 1,
                       limit = c(0, 25),
                       oob = squish) +
  facet_grid(~estimate)

ggsave(filename = "fig5_who_region.png", plot = last_plot(), 
       path = resultspath,
       width = 10,
       height = 3)

# Value (CC2), % of GDP


map_value_cc2_who_region <- 
  data_rev_who_region %>% filter(measure_id== "2") %>% 
  filter(estimate_id %in% c(1,2,4)) %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = region_cost_cc1/region_gdp * 100 * 5), color = "white", size = 0.01) +
  geom_sf(data = . %>%   group_by(who_region) %>% st_set_precision(1e4) %>%
            summarize(geometry = st_union(geometry)), fill = "transparent", color = 'black', size = 0.01) +
  theme(panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(title="Value of DALYs due to mental disorders in current USD, % of GDP",
       subtitle=subtitle_2,
       caption=caption,
       fill="% of GDP") +
  scale_fill_distiller(palette = "OrRd", direction = 1,
                       limit = c(0, 25),
                       oob = squish) +
  facet_grid(~estimate)

ggsave(filename = "fig6_who_region.png", plot = last_plot(), 
       path = resultspath,
       width = 10,
       height = 3)

# Value (WHO1), % of GDP

map_value_who1_who_region <- 
  data_rev_who_region %>% filter(measure_id== "2") %>% 
  filter(estimate_id %in% c(1,2,4)) %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = region_cost_who1/region_gdp * 100), color = "white", size = 0.01) +
  geom_sf(data = . %>%   group_by(who_region) %>% st_set_precision(1e4) %>%
            summarize(geometry = st_union(geometry)), fill = "transparent", color = 'black', size = 0.01) +
  theme(panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(title="Value of DALYs due to mental disorders in current USD, % of GDP",
       subtitle=subtitle_3,
       caption=caption,
       fill="% of GDP") +
  scale_fill_distiller(palette = "OrRd", direction = 1,
                       limits = c(0,25),
                       oob = squish) +
  facet_grid(~estimate)

ggsave(filename = "fig7_who_region.png", plot = last_plot(), 
       path = resultspath,
       width = 10,
       height = 3)

# Value (WHO2), % of GDP

map_value_who2_who_region <- 
  data_rev_who_region %>% filter(measure_id== "2") %>% 
  filter(estimate_id %in% c(1,2,4)) %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = region_cost_who2/region_gdp * 100), color = "white", size = 0.01) +
  geom_sf(data = . %>%   group_by(who_region) %>% st_set_precision(1e4) %>%
            summarize(geometry = st_union(geometry)), fill = "transparent", color = 'black', size = 0.01) +
  theme(panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(title="Value of DALYs due to mental disorders in current USD, % of GDP",
       subtitle=subtitle_4,
       caption=caption,
       fill="% of GDP") +
  scale_fill_distiller(palette = "OrRd", direction = 1,
                       limits = c(0,25),
                       oob = squish) +
  facet_grid(~estimate)
ggsave(filename = "fig8_who_region.png", plot = last_plot(), 
       path = resultspath,
       width = 10,
       height = 3)

# Combined value maps


map_value_cc1_notitle_who_region <- 
  data_rev_who_region %>% filter(measure_id== "2") %>% 
  filter(estimate_id %in% c(1,2,4)) %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = region_cost_cc1/region_gdp * 100), color = "white", size = 0.01) +
  geom_sf(data = . %>%   group_by(who_region) %>% st_set_precision(1e4) %>%
            summarize(geometry = st_union(geometry)), fill = "transparent", color = 'black', size = 0.01) +
  theme(panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(title=subtitle_1,
       fill="% of GDP") +
  scale_fill_distiller(palette = "OrRd", direction = 1,
                       limit = c(0, 25),
                       oob = squish) +
  facet_grid(~estimate)



map_value_cc2_notitle_who_region <- 
  data_rev_who_region %>% filter(measure_id== "2") %>% 
  filter(estimate_id %in% c(1,2,4)) %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = region_cost_cc1/region_gdp * 100 * 5), color = "white", size = 0.01) +
  geom_sf(data = . %>%   group_by(who_region) %>% st_set_precision(1e4) %>%
            summarize(geometry = st_union(geometry)), fill = "transparent", color = 'black', size = 0.01) +
  theme(panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(title=subtitle_2,
       
       fill="% of GDP") +
  scale_fill_distiller(palette = "OrRd", direction = 1,
                       limit = c(0, 25),
                       oob = squish) +
  facet_grid(~estimate)



map_value_who1_notitle_who_region <- 
  data_rev_who_region %>% filter(measure_id== "2") %>% 
  filter(estimate_id %in% c(1,2,4)) %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = region_cost_who1/region_gdp * 100), color = "white", size = 0.01) +
  geom_sf(data = . %>%   group_by(who_region) %>% st_set_precision(1e4) %>%
            summarize(geometry = st_union(geometry)), fill = "transparent", color = 'black', size = 0.01) +
  theme(panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(title=subtitle_3,
       
       fill="% of GDP") +
  scale_fill_distiller(palette = "OrRd", direction = 1,
                       limits = c(0,25),
                       oob = squish) +
  facet_grid(~estimate)


map_value_who2_notitle_who_region <- 
  data_rev_who_region %>% filter(measure_id== "2") %>% 
  filter(estimate_id %in% c(1,2,4)) %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = region_cost_who1/region_gdp * 100 * 3), color = "white", size = 0.01) +
  geom_sf(data = . %>%   group_by(who_region) %>% st_set_precision(1e4) %>%
            summarize(geometry = st_union(geometry)), fill = "transparent", color = 'black', size = 0.01) +
  theme(panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(title=subtitle_4,
       fill="% of GDP") +
  scale_fill_distiller(palette = "OrRd", direction = 1,
                       limits = c(0,25),
                       oob = squish) +
  facet_grid(~estimate)


map_value_combined <- ggarrange(map_value_cc1_notitle_who_region, map_value_cc2_notitle_who_region, map_value_who1_notitle_who_region, map_value_who2_notitle_who_region,
                                ncol = 1, nrow = 4)


ggsave(filename = "fig9_who_region.png", plot = last_plot(), 
       path = resultspath,
       width = 10,
       height = 8)





# IHME regions



data_rev_ihme_region <- data_rev_map_region %>% group_by(estimate, measure_id, ihme_region) %>% 
  mutate(region_pop100k = sum(population_100k, na.rm=TRUE),
         region_number = sum(number),
         region_measure_total = sum(measure_total),
         region_gdp = sum(gdp, na.rm=TRUE)
  ) %>%
  mutate(region_percent = region_number/region_measure_total*100,
         region_population = region_pop100k * 100000)

data_rev_ihme_region$region_gdp_per_capita <- data_rev_ihme_region$region_gdp/data_rev_ihme_region$region_population

data_rev_ihme_region$region_cost_cc1 <- ifelse(data_rev_ihme_region$measure_id == 2,
                                              data_rev_ihme_region$region_number * 1000, 0)

data_rev_ihme_region$region_cost_cc2 <- data_rev_ihme_region$region_cost_cc1 * 5

data_rev_ihme_region$region_cost_who1 <-ifelse(data_rev_ihme_region$measure_id == 2,
                                              data_rev_ihme_region$region_number * data_rev_ihme_region$region_gdp_per_capita, 0)

data_rev_ihme_region$region_cost_who2 <- data_rev_ihme_region$region_cost_who1 * 3



# Deaths per capita

map_deaths_per_cap_ihme_region <- 
  data_rev_ihme_region %>% 
  filter(measure_id== "1") %>% 
  filter(estimate_id %in% c(1,2,4)) %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = region_number/region_pop100k), color = "white", size = 0.01) +
  geom_sf(data = . %>%   group_by(ihme_region) %>% st_set_precision(1e4) %>%
            summarize(geometry = st_union(geometry)), fill = "transparent", color = 'black', size = 0.01) +
  theme(panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(title="Deaths due to mental disorders per 100,000",
       subtitle="2019",
       caption=caption,
       fill="Deaths") +
  scale_fill_distiller(palette = "RdYlBu",
                       limits = c(0,100),
                       oob = squish) +
  coord_sf(ndiscr = F) + 
  facet_grid(~estimate) 

ggsave(filename = "fig1_ihme_region.png", plot = last_plot(), 
       path = resultspath,
       width = 10,
       height = 4)


# DALYS per capita

map_dalys_per_cap_ihme_region <-
  data_rev_ihme_region %>% filter(measure_id== "2") %>% 
  filter(estimate_id %in% c(1,2,4)) %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = region_number/region_pop100k), color = "white", size = 0.01) +
  geom_sf(data = . %>%   group_by(ihme_region) %>% st_set_precision(1e4) %>%
            summarize(geometry = st_union(geometry)), fill = "transparent", color = 'black', size = 0.01) +
  theme(panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(title="DALYs due to mental disorders per 100,000",
       subtitle="2019",
       caption=caption,
       fill="DALYs") +
  scale_fill_distiller(palette = "RdYlBu",
                       limits = c(0,6000),
                       oob = squish) +
  facet_grid(~estimate)

ggsave(filename = "fig2_ihme_region.png", plot = last_plot(), 
       path = resultspath,
       width = 10,
       height = 4)

# Percent of deaths

map_deaths_percent_ihme_region <-
  data_rev_ihme_region %>% filter(measure_id== "1") %>% 
  filter(estimate_id %in% c(1,2,4)) %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = region_percent), color = "white", size = 0.01) +
  geom_sf(data = . %>%   group_by(ihme_region) %>% st_set_precision(1e4) %>%
            summarize(geometry = st_union(geometry)), fill = "transparent", color = 'black', size = 0.01) +
  theme(panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(title="Deaths due to mental disorders, % of deaths",
       subtitle="2019",
       caption=caption,
       fill="Percent") +
  scale_fill_distiller(palette = "YlGn",
                       direction = 1,
                       limits = c(0,15),
                       oob = squish) +
  facet_grid(~estimate)

ggsave(filename = "fig3_ihme_region.png", plot = last_plot(), 
       path = resultspath,
       width = 10,
       height = 4)

# Percent of DALYs

map_dalys_percent_ihme_region <-
  data_rev_ihme_region %>% filter(measure_id== "2") %>% 
  filter(estimate_id %in% c(1,2,4)) %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = region_percent), color = "white", size = 0.01) +
  geom_sf(data = . %>%   group_by(ihme_region) %>% st_set_precision(1e4) %>%
            summarize(geometry = st_union(geometry)), fill = "transparent", color = 'black', size = 0.01) +
  theme(panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(title="DALYs due to mental disorders, % of DALYs",
       subtitle="2019",
       caption=caption,
       fill="Percent") +
  scale_fill_distiller(palette = "YlGn",
                       direction = 1,
                       limits = c(0,25),
                       oob = squish) +
  facet_grid(~estimate)

ggsave(filename = "fig4_ihme_region.png", plot = last_plot(), 
       path = resultspath,
       width = 10,
       height = 4)


# Value (CC1), % of GDP

map_value_cc1_ihme_region <- 
  data_rev_ihme_region %>% filter(measure_id== "2") %>% 
  filter(estimate_id %in% c(1,2,4)) %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = region_cost_cc1/region_gdp * 100), color = "white", size = 0.01) +
  geom_sf(data = . %>%   group_by(ihme_region) %>% st_set_precision(1e4) %>%
            summarize(geometry = st_union(geometry)), fill = "transparent", color = 'black', size = 0.01) +
  theme(panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(title="Value of DALYs due to mental disorders in current USD, % of GDP",
       subtitle=subtitle_1,
       caption=caption,
       fill="% of GDP") +
  scale_fill_distiller(palette = "OrRd", direction = 1,
                       limit = c(0, 25),
                       oob = squish) +
  facet_grid(~estimate)

ggsave(filename = "fig5_ihme_region.png", plot = last_plot(), 
       path = resultspath,
       width = 10,
       height = 3)

# Value (CC2), % of GDP


map_value_cc2_ihme_region <- 
  data_rev_ihme_region %>% filter(measure_id== "2") %>% 
  filter(estimate_id %in% c(1,2,4)) %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = region_cost_cc1/region_gdp * 100 * 5), color = "white", size = 0.01) +
  geom_sf(data = . %>%   group_by(ihme_region) %>% st_set_precision(1e4) %>%
            summarize(geometry = st_union(geometry)), fill = "transparent", color = 'black', size = 0.01) +
  theme(panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(title="Value of DALYs due to mental disorders in current USD, % of GDP",
       subtitle=subtitle_2,
       caption=caption,
       fill="% of GDP") +
  scale_fill_distiller(palette = "OrRd", direction = 1,
                       limit = c(0, 25),
                       oob = squish) +
  facet_grid(~estimate)

ggsave(filename = "fig6_ihme_region.png", plot = last_plot(), 
       path = resultspath,
       width = 10,
       height = 3)

# Value (WHO1), % of GDP

map_value_who1_ihme_region <- 
  data_rev_ihme_region %>% filter(measure_id== "2") %>% 
  filter(estimate_id %in% c(1,2,4)) %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = region_cost_who1/region_gdp * 100), color = "white", size = 0.01) +
  geom_sf(data = . %>%   group_by(ihme_region) %>% st_set_precision(1e4) %>%
            summarize(geometry = st_union(geometry)), fill = "transparent", color = 'black', size = 0.01) +
  theme(panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(title="Value of DALYs due to mental disorders in current USD, % of GDP",
       subtitle=subtitle_3,
       caption=caption,
       fill="% of GDP") +
  scale_fill_distiller(palette = "OrRd", direction = 1,
                       limits = c(0,25),
                       oob = squish) +
  facet_grid(~estimate)

ggsave(filename = "fig7_ihme_region.png", plot = last_plot(), 
       path = resultspath,
       width = 10,
       height = 3)

# Value (WHO2), % of GDP

map_value_who2_ihme_region <- 
  data_rev_ihme_region %>% filter(measure_id== "2") %>% 
  filter(estimate_id %in% c(1,2,4)) %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = region_cost_who1/region_gdp * 100 * 3), color = "white", size = 0.01) +
  geom_sf(data = . %>%   group_by(ihme_region) %>% st_set_precision(1e4) %>%
            summarize(geometry = st_union(geometry)), fill = "transparent", color = 'black', size = 0.01) +
  theme(panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(title="Value of DALYs due to mental disorders in current USD, % of GDP",
       subtitle=subtitle_4,
       caption=caption,
       fill="% of GDP") +
  scale_fill_distiller(palette = "OrRd", direction = 1,
                       limits = c(0,25),
                       oob = squish) +
  facet_grid(~estimate)

ggsave(filename = "fig8_ihme_region.png", plot = last_plot(), 
       path = resultspath,
       width = 10,
       height = 3)

# Combined value maps


map_value_cc1_notitle_ihme_region <- 
  data_rev_ihme_region %>% filter(measure_id== "2") %>% 
  filter(estimate_id %in% c(1,2,4)) %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = region_cost_cc1/region_gdp * 100), color = "white", size = 0.01) +
  geom_sf(data = . %>%   group_by(ihme_region) %>% st_set_precision(1e4) %>%
            summarize(geometry = st_union(geometry)), fill = "transparent", color = 'black', size = 0.01) +
  theme(panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(title=subtitle_1,
       fill="% of GDP") +
  scale_fill_distiller(palette = "OrRd", direction = 1,
                       limit = c(0, 25),
                       oob = squish) +
  facet_grid(~estimate)



map_value_cc2_notitle_ihme_region <- 
  data_rev_ihme_region %>% filter(measure_id== "2") %>% 
  filter(estimate_id %in% c(1,2,4)) %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = region_cost_cc1/region_gdp * 100 * 5), color = "white", size = 0.01) +
  geom_sf(data = . %>%   group_by(ihme_region) %>% st_set_precision(1e4) %>%
            summarize(geometry = st_union(geometry)), fill = "transparent", color = 'black', size = 0.01) +
  theme(panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(title=subtitle_2,

       fill="% of GDP") +
  scale_fill_distiller(palette = "OrRd", direction = 1,
                       limit = c(0, 25),
                       oob = squish) +
  facet_grid(~estimate)



map_value_who1_notitle_ihme_region <- 
  data_rev_ihme_region %>% filter(measure_id== "2") %>% 
  filter(estimate_id %in% c(1,2,4)) %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = region_cost_who1/region_gdp * 100), color = "white", size = 0.01) +
  geom_sf(data = . %>%   group_by(ihme_region) %>% st_set_precision(1e4) %>%
            summarize(geometry = st_union(geometry)), fill = "transparent", color = 'black', size = 0.01) +
  theme(panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(title=subtitle_3,
       
       fill="% of GDP") +
  scale_fill_distiller(palette = "OrRd", direction = 1,
                       limits = c(0,25),
                       oob = squish) +
  facet_grid(~estimate)


map_value_who2_notitle_ihme_region <- 
  data_rev_ihme_region %>% filter(measure_id== "2") %>% 
  filter(estimate_id %in% c(1,2,4)) %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = region_cost_who1/region_gdp * 100 * 3), color = "white", size = 0.01) +
  geom_sf(data = . %>%   group_by(ihme_region) %>% st_set_precision(1e4) %>%
            summarize(geometry = st_union(geometry)), fill = "transparent", color = 'black', size = 0.01) +
  theme(panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(title=subtitle_4,
       fill="% of GDP") +
  scale_fill_distiller(palette = "OrRd", direction = 1,
                       limits = c(0,25),
                       oob = squish) +
  facet_grid(~estimate)


map_value_combined <- ggarrange(map_value_cc1_notitle_ihme_region, map_value_cc2_notitle_ihme_region, map_value_who1_notitle_ihme_region, map_value_who2_notitle_ihme_region,
                                ncol = 1, nrow = 4)


ggsave(filename = "fig9_ihme_region.png", plot = last_plot(), 
       path = resultspath,
       width = 10,
       height = 8)



############################
##        TABLES          ##
############################

# Table 1: Burden of disease due to mental disorders

# Global
data_global <- data_rev %>% filter(location_id == 1)
table1 <- data_global  %>% select(measure_id, estimate, number, percent)
table1$percent <- round(table1$percent, 2)
table1$number <- round(table1$number/1000000,3)

write.csv(table1, file = "results/table1_global_val.csv")

# Regional
table1 <- data_rev %>% filter(estimate_id != 3) %>% 
  filter(location_id != 1)%>% 
  select(measure_id, iso_code, measure_total, estimate, number)


# Adding regions
region <- read_excel(path = file.path(datapath, "regions.xlsx"))
region <- region %>% select (iso_code, continent, who_region, ihme_region)
table1 <- left_join(table1, region, by = "iso_code")

table1 <- table1 %>% group_by(who_region, estimate, measure_id) %>% mutate(region_dalytotal = sum(measure_total),
                                                                 region_dalymh = sum(number))

table1$percent <- round(table1$region_dalymh/table1$region_dalytotal*100, 2)
table1 <- table1 %>% ungroup() %>% select(measure_id, who_region, estimate, region_dalymh, percent) %>% unique()
table1$region_dalymh <- round(table1$region_dalymh/1000000,3)

write.csv(table1, file = "results/table1_region_val.csv")

# Income level

table1 <- data_rev %>% filter(estimate_id != 3) %>% 
  filter(location_id != 1)%>% 
  select(income_level, measure_id, measure_total, estimate, number)

table1 <- table1 %>% group_by(measure_id, income_level, estimate) %>% mutate(income_level_dalytotal = sum(measure_total),
                                                       income_level_dalymh = sum(number))

table1$percent <- round(table1$income_level_dalymh/table1$income_level_dalytotal*100, 2)
table1 <- table1 %>% ungroup() %>% select(income_level, measure_id, estimate, income_level_dalymh, percent) %>% unique() 

table1$income_level_dalymh <- round(table1$income_level_dalymh/1000000,3)

write.csv(table1, file = "results/table1_income_val.csv")

rm(table1, region)

# Table 2: Value of economic welfare estimates 

table2 <- data_global %>% filter(estimate_id != 3) %>% select(estimate, cost_cc1, cost_cc2, cost_who1, cost_who2)
table2$cost_cc1 <- round(table2$cost_cc1/1000000000000,2)
table2$cost_cc2 <- round(table2$cost_cc2/1000000000000,2)
table2$cost_who1 <- round(table2$cost_who1/1000000000000,2)
table2$cost_who2 <- round(table2$cost_who2/1000000000000,2)
table2_t <- transpose(table2)

colnames(table2_t) <- rownames(table2)
rownames(table2_t) <- colnames(table2)

write.csv(table2_t, file = "results/table2_val.csv")
rm(table2, table2_t)



############################
##         CHARTS           ##
############################


data_rev_who_region_1 <- data_rev_who_region %>% select(c(estimate, estimate_id, measure_id, who_region, region_pop100k:region_cost_who2)) %>% st_drop_geometry()
data_rev_who_region_1 <- data_rev_who_region_1  %>% filter(estimate_id %in% c(1,2,4)) %>% unique()


chart_1_who_region <- 
  
  data_rev_who_region_1 %>% 
  filter(measure_id== "2") %>% 
  filter(!is.na(who_region)) %>%
  ggplot(aes(x = who_region, fill=who_region, y=region_percent)) +
  geom_bar(stat="identity")+
  labs(title="DALYs due to mental disorders, % of DALYs",
       subtitle="2019",
       caption=caption,
       x = "",
       y = "Percent of DALYs",
       fill="Region") +
  facet_grid(~estimate) + 
  theme(panel.grid.major.y =  element_blank(), 
        panel.background = element_blank(),
        axis.title = element_blank(), 
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) 


ggsave(filename = "fig10_who_region.png", plot = last_plot(), 
       path = resultspath,
       width = 10,
       height = 8)


chart_2_who_region <- 

  data_rev_who_region_1 %>% 
  filter(measure_id== "1") %>% 
  filter(!is.na(who_region)) %>%
  ggplot(aes(x = who_region, fill=who_region, y=region_percent)) +
  geom_bar(stat="identity")+
  labs(title="Deaths due to mental disorders, % of deaths",
       subtitle="2019",
       caption=caption,
       x = "",
       y = "Percent of deaths",
       fill="Region") +
  facet_grid(~estimate) + 
  theme(panel.grid.major.y =  element_blank(), 
        panel.background = element_blank(),
        axis.title = element_blank(), 
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) 


ggsave(filename = "fig11_who_region.png", plot = last_plot(), 
       path = resultspath,
       width = 10,
       height = 8)


chart_3_who_region <- 
  
  data_rev_who_region_1 %>% 
  filter(measure_id== "2") %>% 
  filter(!is.na(who_region)) %>%
  ggplot(aes(x = who_region, fill=who_region, y=region_cost_who1/region_gdp*100)) +
  geom_bar(stat="identity")+
  labs(title="Value of DALYs due to mental disorders in current USD, % of GDP",
       subtitle="Value per DALY: GDP/capita",
       caption=caption,
       x = "",
       y = "Percent of GDP",
       fill="Region") +
  facet_grid(~estimate) + 
  theme(panel.grid.major.y =  element_blank(), 
        panel.background = element_blank(),
        axis.title = element_blank(), 
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  geom_text(aes(label=round(region_cost_who1/region_gdp*100,1)), vjust=1.6, color="black", size=3.5)


ggsave(filename = "fig12_who_region.png", plot = last_plot(), 
       path = resultspath,
       width = 10,
       height = 8)


data_rev_ihme_region_1 <- data_rev_ihme_region %>% select(c(estimate, estimate_id, measure_id, ihme_region, region_pop100k:region_cost_who2)) %>% st_drop_geometry()
data_rev_ihme_region_1 <- data_rev_ihme_region_1  %>% filter(estimate_id %in% c(1,2,4)) %>% unique()

ihme_positions <- c("East Asia", "Southeast Asia", "Oceania", "Central Asia", "Eastern Europe", "Central Europe",
                    "Caribbean", "Central Latin America", "Tropical Latin America", "Andean Latin America", "North Africa and Middle East",
                    "Southern Sub-Saharan Africa", "Western Sub-Saharan Africa", "Central Sub-Saharan Africa", "Eastern Sub-Saharan Africa",
                    "South Asia", "Southern Latin America", "Western Europe", "High-income North America",
                    "Australasia", "High-income Asia Pacific", "#NA")

ihme_pal <- c("#8C95F1", "#D7ECFC" , "#B6CDD4", "#EAA144", "#F3DA89", "#F9EFC2", "#B1F585", "#B6F2D5",
              "#E3FD8C", "#E2FDD7", "#99ACA4", "#9D5F56", "#E989A9", "#F5DBD8", "#F8EEEF",
              "#F4FF7C", "#C42AF1", "#E986F8", "#D7C2FC", "#DD9BE9", "#FAE5FE", "#7F7F7F")

chart_1_ihme_region <- 
  
  data_rev_ihme_region_1 %>% 
  filter(measure_id== "2") %>% 
  filter(!is.na(ihme_region)) %>%
  mutate(ihme_region = factor(ihme_region, levels = ihme_positions)) %>%
  ggplot(aes(x = ihme_region, fill=ihme_region, y=region_percent)) +
  geom_bar(stat="identity")+
  labs(title="DALYs due to mental disorders, % of DALYs",
       subtitle="2019",
       caption=caption,
       x = "",
       y = "Percent of DALYs",
       fill="Region") +
  facet_grid(~estimate) + 
  theme(panel.grid.major.y =  element_blank(), 
        panel.background = element_blank(),
        axis.title = element_blank(), 
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())  + 
  scale_fill_manual(name = "Region", values = ihme_pal)


ggsave(filename = "fig10_ihme_region.png", plot = last_plot(), 
       path = resultspath,
       width = 10,
       height = 8)


chart_2_ihme_region <- 
  
  data_rev_ihme_region_1 %>% 
  filter(measure_id== "1") %>% 
  filter(!is.na(ihme_region)) %>%
  mutate(ihme_region = factor(ihme_region, levels = ihme_positions)) %>%
  ggplot(aes(x = ihme_region, fill=ihme_region, y=region_percent)) +
  geom_bar(stat="identity")+
  labs(title="Deaths due to mental disorders, % of deaths",
       subtitle="2019",
       caption=caption,
       x = "",
       y = "Percent of deaths",
       fill="Region") +
  facet_grid(~estimate) + 
  theme(panel.grid.major.y =  element_blank(), 
        panel.background = element_blank(),
        axis.title = element_blank(), 
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())  + 
  scale_fill_manual(name = "Region", values = ihme_pal)


ggsave(filename = "fig11_ihme_region.png", plot = last_plot(), 
       path = resultspath,
       width = 10,
       height = 8)



chart_3_ihme_region <- 
  
  data_rev_ihme_region_1 %>% 
  filter(measure_id== "2") %>% 
  filter(!is.na(ihme_region)) %>%
  mutate(ihme_region = factor(ihme_region, levels = ihme_positions)) %>%
  ggplot(aes(x = ihme_region, fill=ihme_region, y=region_cost_who1/region_gdp*100)) +
  geom_bar(stat="identity")+
  labs(title="Value of DALYs due to mental disorders in current USD, % of GDP",
       subtitle="Value per DALY: GDP/capita",
       caption=caption,
       x = "",
       y = "Percent of GDP") +
  facet_grid(~estimate) + 
  theme(panel.grid.major.y =  element_blank(), 
        panel.background = element_blank(),
        axis.title = element_blank(), 
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  geom_text(aes(label=round(region_cost_who1/region_gdp*100,1)), vjust=1.6, color="black", size=2) + 
  scale_fill_manual(name = "Region", values = ihme_pal)
                      


ggsave(filename = "fig12_ihme_region.png", plot = last_plot(), 
       path = resultspath,
       width = 13,
       height = 4)

chart_3_cc1_notitle_ihme_region <- 
  
  data_rev_ihme_region_1 %>% 
  filter(measure_id== "2") %>% 
  filter(!is.na(ihme_region)) %>%
  mutate(ihme_region = factor(ihme_region, levels = ihme_positions)) %>%
  ggplot(aes(x = ihme_region, fill=ihme_region, y=region_cost_cc1/region_gdp*100)) +
  geom_bar(stat="identity")+
  labs(title="Value per DALY: $1,000",
       x = "",
       y = "Percent of GDP") +
  facet_grid(~estimate) + 
  theme(panel.grid.major.y =  element_blank(), 
        panel.background = element_blank(),
        axis.title = element_blank(), 
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  geom_text(aes(label=round(region_cost_cc1/region_gdp*100,1)), vjust=1.6, color="black", size=2) + 
  coord_cartesian(ylim = c(0, 25)) + 
  scale_fill_manual(name = "Region", values = ihme_pal)

chart_3_cc2_notitle_ihme_region <- 
  
  data_rev_ihme_region_1 %>% 
  filter(measure_id== "2") %>% 
  filter(!is.na(ihme_region)) %>%
  mutate(ihme_region = factor(ihme_region, levels = ihme_positions)) %>%
  ggplot(aes(x = ihme_region, fill=ihme_region, y=region_cost_cc2/region_gdp*100)) +
  geom_bar(stat="identity")+
  labs(title="Value per DALY: $5,000",
       x = "",
       y = "Percent of GDP") +
  facet_grid(~estimate) + 
  theme(panel.grid.major.y =  element_blank(), 
        panel.background = element_blank(),
        axis.title = element_blank(), 
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  geom_text(aes(label=round(region_cost_cc2/region_gdp*100,1)), vjust=1.6, color="black", size=2) + 
  coord_cartesian(ylim = c(0, 25)) + 
  scale_fill_manual(name = "Region", values = ihme_pal)



chart_3_who1_notitle_ihme_region <- 
  
  data_rev_ihme_region_1 %>% 
  filter(measure_id== "2") %>% 
  filter(!is.na(ihme_region)) %>%
  mutate(ihme_region = factor(ihme_region, levels = ihme_positions)) %>%
  ggplot(aes(x = ihme_region, fill=ihme_region, y=region_cost_who1/region_gdp*100)) +
  geom_bar(stat="identity")+
  labs(title="Value per DALY: GDP/capita",
       x = "",
       y = "Percent of GDP") +
  facet_grid(~estimate) + 
  theme(panel.grid.major.y =  element_blank(), 
        panel.background = element_blank(),
        axis.title = element_blank(), 
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  geom_text(aes(label=round(region_cost_who1/region_gdp*100,1)), vjust=1.6, color="black", size=2) + 
  coord_cartesian(ylim = c(0, 25)) + 
  scale_fill_manual(name = "Region", values = ihme_pal)

chart_3_who2_notitle_ihme_region <- 
  
  data_rev_ihme_region_1 %>% 
  filter(measure_id== "2") %>% 
  filter(!is.na(ihme_region)) %>%
  mutate(ihme_region = factor(ihme_region, levels = ihme_positions)) %>%
  ggplot(aes(x = ihme_region, fill=ihme_region, y=region_cost_who2/region_gdp*100)) +
  geom_bar(stat="identity")+
  labs(title="Value per DALY: 3 X GDP/capita",
       x = "",
       y = "Percent of GDP") +
  facet_grid(~estimate) + 
  theme(panel.grid.major.y =  element_blank(), 
        panel.background = element_blank(),
        axis.title = element_blank(), 
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  geom_text(aes(label=round(region_cost_who2/region_gdp*100,1)), vjust=1.6, color="black", size=2) + 
  coord_cartesian(ylim = c(0, 25)) + 
  scale_fill_manual(name = "Region", values = ihme_pal)



combo_chart_3_notitle_ihme_region <- ggarrange(chart_3_cc1_notitle_ihme_region,
                                chart_3_cc2_notitle_ihme_region,
                                chart_3_who1_notitle_ihme_region,
                                chart_3_who2_notitle_ihme_region,
                                common.legend = TRUE,
                                legend = "bottom",
                                ncol = 1, nrow = 4)


ggsave(filename = "fig13_ihme_region.png", plot = last_plot(), 
       path = resultspath,
       width = 13,
       height = 16)

data_rev_ihme_inputs <- read.csv(file = file.path(datapath,"IHME-GBD_2019_DATA-2019-inputs.csv"))
data_rev_ihme_inputs <- data_rev_ihme_inputs %>% gather(inputs_mental:inputs_all,key = "input", value = "count")

chart_4_gbdinputs_ihme_region_percent <- 
  data_rev_ihme_inputs %>% 
  filter(!ihme_region %in% c("Global", "Other/Duplicates")) %>%
  filter(input != "inputs_all") %>%
  mutate(ihme_region = factor(ihme_region, levels = ihme_positions)) %>%
  ggplot(aes(x = input, fill=ihme_region, y=count)) +
  geom_bar(position = "fill", stat="identity")+
  theme(panel.grid.major.y =  element_blank(), 
        panel.background = element_blank(),
        axis.title = element_blank(), 
        axis.ticks.x = element_blank()) +
  ggtitle("Share of cause-specific inputs") +
  scale_fill_manual(name = "Region", values = ihme_pal) + 
  scale_x_discrete(limit = c("inputs_mental", "inputs_maternal"),
                     labels = c(
                       
                       expression(paste("Mental disorders \n         (n=2870)" )),
                       expression(paste("Maternal and neonatal disorders \n                    (n=6149)" ))))

chart_4_gbdinputs_ihme_region_stacked <- 
  data_rev_ihme_inputs %>% 
  filter(!ihme_region %in% c("Global", "Other/Duplicates")) %>%
  filter(input != "inputs_all") %>%
  mutate(ihme_region = factor(ihme_region, levels = ihme_positions)) %>%
  ggplot(aes(x = input, fill=ihme_region, y=count)) +
  geom_bar(position = "stack", stat="identity")+
  theme(panel.grid.major.y =  element_blank(), 
        panel.background = element_blank(),
        axis.title = element_blank(), 
        axis.ticks.x = element_blank()) +
  ggtitle("Number of cause-specific inputs") +
  scale_fill_manual(name = "Region", values = ihme_pal) + 
  scale_x_discrete(limit = c("inputs_mental", "inputs_maternal"),
                   labels = c(
                     
                     expression(paste("Mental disorders \n         (n=2870)" )),
                     expression(paste("Maternal and neonatal disorders \n                    (n=6149)" ))))

combo_chart_4_gbdinputs_ihme_region <- ggarrange(chart_4_gbdinputs_ihme_region_stacked,
                                                 chart_4_gbdinputs_ihme_region_percent,
                                               common.legend = TRUE,
                                               legend = "bottom",
                                               ncol = 2, nrow = 1)


 ggsave(filename = "fig14_ihme_region.png", plot = last_plot(), 
        path = resultspath,
        width = 13,
        height = 8)
############################################
# TO DO

# Check changes in rankings
# Look up if there have been changes in disability weights
# Critiques of disability weights 
# Method 5: Bloom et al.
# Method 6: Lancet 
# Consider filtering out islands to reduce visual clutter
# Figure out adjustment for scale to account for out of bounds in labeling
# Proofread code
# Clean up sensitivty analysis
# Make Table 1 easier to replicate
# Incorporate sensitivity analysis within this
# Gather a few studies that focus on NCDs (cancers, CVD, etc.) and risk of death due to mental disorders
# 

# Old code for VSL US

#vsl_2006 <- 7400000 # Source:https://www.epa.gov/sites/production/files/2017-08/documents/ee-0568-50.pdf
#gdp_deflator_2019 <- 107.494 # Source: https://data.worldbank.org/indicator/NY.GDP.DEFL.ZS?locations=US
#gdp_deflator_2006 <- 86.01
#vsl_2019 <- vsl_2006 * gdp_deflator_2019 / gdp_deflator_2006
#vsl <- vsl_2019
#rm(vsl_2019, vsl_2006, gdp_deflator_2019, gdp_deflator_2006)
#rm(list = ls())


#################
# Lower bounds


# Loading IHME Global Burden of Disease Data
data <- read.csv(file = file.path(datapath,"IHME-GBD_2019_DATA-2019-1.csv"))
ihme_crosswalk <- read_excel(path = file.path(datapath, "ihme_crosswalk.xlsx")) %>% 
  select(-c("location_name"))
data <- left_join(data, ihme_crosswalk, by = "location_id")
rm(ihme_crosswalk) 
income_level <- read_excel(path = file.path(datapath, "OGHIST.xls"), sheet = 2) %>% 
  select(-(c(2:34))) %>% 
  rename("income_level" = "2019")
data <- left_join(data, income_level, by = "iso_code")
rm(income_level) 
data <- data %>% unique() %>% gather("numeric_name", "est", "val":"lower") 
data_number <- data %>% filter(metric_id == 1) %>% rename("number" = "est")
data_rate <- data %>% filter(metric_id == 3) 
data_rate <- data %>% select("measure_id","location_id", "sex_id", "age_id", "cause_id", "year", "numeric_name", "est") %>% rename("rate" = "est")
data <- left_join(data_number, data_rate, by = c("measure_id","location_id", "sex_id", "age_id", "cause_id", "year", "numeric_name"))
rm(data_number, data_rate)

# Select lower estimates
data <- data %>% unique() %>% filter(numeric_name == "lower") %>% spread(numeric_name, number)
data <- data %>% rename("val" = "lower")
data$population <- data$val/data$rate
data <- data %>% filter(population != 1) %>% unique()

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

no_revision <- musculoskeletal_cause_id

inclusion <- c(mental_disorder_cause_id,
               dementia_cause_id,
               epilepsy_cause_id,
               migraine_cause_id,
               tension_type_headache_cause_id,
               self_harm,
               musculoskeletal_cause_id,
               all_cause_id)

data_vigo <- data %>% filter(cause_id %in% inclusion)

data_vigo$mh_gbd <- ifelse(data_vigo$cause_id == mental_disorder_cause_id, 1, 0)  # Fully retain existing mental disorders
data_vigo$mh_vigo <- ifelse(data_vigo$cause_id %in% revision_full, 1, 0)          # Allocate 100% of these conditions
data_vigo$mh_vigo[data_vigo$cause_id %in% no_revision] <- 0                  # Allocate 0% of these conditions
data_vigo$all <- ifelse(data_vigo$cause_id == all_cause_id, 1, 0)                 # Binary in case of all_cause 
data_vigo$value <- data_vigo$val * data_vigo$mh_gbd                               # Retain original IHME values
data_vigo$value_rev <- data_vigo$val * data_vigo$mh_vigo                          # Apply allocation percents against original IHME values

data_vigo <- data_vigo %>% 
  group_by(location_id, measure_id, sex_id, age_id, metric_id) %>% 
  mutate(mh_value_agg_rev = sum(value_rev), mh_value_agg = sum(value))

data_rev <- data_vigo %>%
  filter(cause_id == "294") %>% #Filters to all causes; data_gh$val == total burden by location id
  filter(metric_id == "1") %>%  #Filters to numeric metric
  rename("Revised - Vigo et al. method" = mh_value_agg_rev, "Original" = mh_value_agg, "measure_total" = val) %>%
  filter(measure_id %in% 1:4) %>% #Retains only deaths, DALYS, YLDs, and YLLs
  ungroup %>%
  select(c(1:4, 13:15, 17:18,24:25))

data_prev <- read.csv(file = file.path(datapath,"IHME-GBD_2019_DATA-2019-prev.csv"))

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
  c(2.12,# Lower estimates of PAR
    2.54,
    1.71,
    2,
    1.43))
colnames(relative_risk) <- c("cause_id", "rel_risk")

data_prev <- inner_join(data_prev, relative_risk, by = "cause_id")
data_prev$par <-     data_prev$lower  * ((data_prev$rel_risk - 1) /    data_prev$rel_risk)
data_prev_allmentaldisorders <- data_prev %>% filter(cause_id == mental_disorder_cause_id) %>% select(c(location_id,par))

data_rev <- full_join(data_rev, data_prev_allmentaldisorders, by = "location_id")

data_rev$"Revised - Walker et al. method" = data_rev$par * data_rev$measure_total

rm(relative_risk, data_prev_allmentaldisorders)
data_rev <- data_rev %>% relocate(par, .after = population)
data_rev <- data_rev %>% rename("Original GBD method" = Original)

data_rev$composite_ylds <-ifelse(data_rev$measure_id == 3, data_rev$"Revised - Vigo et al. method", 0)
data_rev$composite_ylls <-ifelse(data_rev$measure_id == 4, data_rev$"Revised - Walker et al. method", 0)

data_rev_composite <- data_rev %>% select(location_id, measure_id, composite_ylds, composite_ylls) %>%
  group_by(location_id) %>%
  mutate(composite_dalys = sum(composite_ylds, composite_ylls))

data_rev_composite <- data_rev_composite %>% filter(measure_id == 2) %>% ungroup() %>% unique() %>% select(c(1,5))
data_rev <- inner_join(data_rev, data_rev_composite, by = "location_id")
data_rev$composite_dalys[data_rev$measure_id != 2] <- 0

data_rev$composite_deaths <-ifelse(data_rev$measure_id == 1, data_rev$"Revised - Walker et al. method", 0)

data_rev$composite <- data_rev$composite_ylds + data_rev$composite_ylls + data_rev$composite_dalys + data_rev$composite_deaths
data_rev <- data_rev %>% select(!c(14:17))
data_rev <- data_rev %>% rename("Revised - Composite method" = composite)
data_rev <- data_rev %>% relocate(12, .after = par)
data_rev$cause_name <- "Mental disorders"
data_rev <- data_rev %>% gather("estimate", "number", 11:14)

data_rev$estimate_id <- ifelse(data_rev$estimate == "Revised - Composite method", 4, 
                               ifelse(data_rev$estimate == "Revised - Walker et al. method", 3,
                                      ifelse(data_rev$estimate == "Revised - Vigo et al. method", 2, 1)))
data_rev <- data_rev %>% relocate(estimate_id, .after = estimate)

data_rev$population_100k <- data_rev$population
data_rev$population <- data_rev$population_100k  * 100000
data_rev <- data_rev %>% relocate (population_100k, .after = population)
data_rev$rate_per_100k <- data_rev$number/data_rev$population_100k
data_rev$percent <- data_rev$number/data_rev$measure_total*100

gdp <- read.csv(file = file.path(datapath,"wdi_gdp.csv")) %>%
  select(-(c(3:62))) %>% 
  rename("gdp" = "X2019")
gdp <- gdp %>% select(c(iso_code, gdp))
data_rev <- left_join(data_rev, gdp, by = "iso_code")
data_rev <- data_rev %>% relocate(gdp, .after = iso_code)
data_rev$gdp[data_rev$location_id == 1] <- 87798525859220.9          # Source: World Bank, WDI

data_rev$gdp_per_capita <- data_rev$gdp/data_rev$population
data_rev <- data_rev %>% relocate (gdp_per_capita, .after = gdp)
rm(gdp)

############################
# Cost per DALY

data_rev$cost_cc1 <- ifelse(data_rev$measure_id == 2,
                            data_rev$number * 1000, 0)
data_rev$cost_cc2 <- data_rev$cost_cc1 * 5
data_rev$cost_who1 <-ifelse(data_rev$measure_id == 2,
                            data_rev$number * data_rev$gdp_per_capita, 0)
data_rev$cost_who2 <- data_rev$cost_who1 * 3

data_global <- data_rev %>% filter(location_id == 1) %>% filter(measure_id == 2)


# Table 1: Burden of disease due to mental disorders

# Global
data_global <- data_rev %>% filter(location_id == 1)
table1 <- data_global  %>% select(measure_id, estimate, number, percent)
table1$percent <- round(table1$percent, 2)
table1$number <- round(table1$number/1000000,3)

write.csv(table1, file = "results/table1_global_lower.csv")

# Regional
table1 <- data_rev %>% filter(estimate_id != 3) %>% 
  filter(location_id != 1)%>% 
  select(measure_id, iso_code, measure_total, estimate, number)


# Adding regions
region <- read_excel(path = file.path(datapath, "regions.xlsx"))
table1 <- left_join(table1, region, by = "iso_code")

table1 <- table1 %>% group_by(who_region, estimate, measure_id) %>% mutate(region_dalytotal = sum(measure_total),
                                                                       region_dalymh = sum(number))

table1$percent <- round(table1$region_dalymh/table1$region_dalytotal*100, 2)
table1 <- table1 %>% ungroup() %>% select(measure_id, who_region, estimate, region_dalymh, percent) %>% unique()
table1$region_dalymh <- round(table1$region_dalymh/1000000,3)

write.csv(table1, file = "results/table1_region_lower.csv")

# Income level

table1 <- data_rev %>% filter(estimate_id != 3) %>% 
  filter(location_id != 1)%>% 
  select(income_level, measure_id, measure_total, estimate, number)

table1 <- table1 %>% group_by(measure_id, income_level, estimate) %>% mutate(income_level_dalytotal = sum(measure_total),
                                                                             income_level_dalymh = sum(number))

table1$percent <- round(table1$income_level_dalymh/table1$income_level_dalytotal*100, 2)
table1 <- table1 %>% ungroup() %>% select(income_level, measure_id, estimate, income_level_dalymh, percent) %>% unique() 

table1$income_level_dalymh <- round(table1$income_level_dalymh/1000000,3)

write.csv(table1, file = "results/table1_income_lower.csv")

rm(table1, region)

# Table 2: Value of economic welfare estimates 

table2 <- data_global %>% filter(estimate_id != 3) %>% select(estimate, cost_cc1, cost_cc2, cost_who1, cost_who2)
table2$cost_cc1 <- round(table2$cost_cc1/1000000000000,2)
table2$cost_cc2 <- round(table2$cost_cc2/1000000000000,2)
table2$cost_who1 <- round(table2$cost_who1/1000000000000,2)
table2$cost_who2 <- round(table2$cost_who2/1000000000000,2)
table2_t <- transpose(table2)

colnames(table2_t) <- rownames(table2)
rownames(table2_t) <- colnames(table2)

write.csv(table2_t, file = "results/table2_lower.csv")
rm(table2, table2_t)

#################
# Upper bounds


data <- read.csv(file = file.path(datapath,"IHME-GBD_2019_DATA-2019-1.csv"))
ihme_crosswalk <- read_excel(path = file.path(datapath, "ihme_crosswalk.xlsx")) %>% 
  select(-c("location_name"))

data <- left_join(data, ihme_crosswalk, by = "location_id")
rm(ihme_crosswalk) 

income_level <- read_excel(path = file.path(datapath, "OGHIST.xls"), sheet = 2) %>% 
  select(-(c(2:34))) %>% 
  rename("income_level" = "2019")
data <- left_join(data, income_level, by = "iso_code")

rm(income_level) 
data <- data %>% unique() %>% gather("numeric_name", "est", "val":"lower") 
data_number <- data %>% filter(metric_id == 1) %>% rename("number" = "est")
data_rate <- data %>% filter(metric_id == 3) 
data_rate <- data %>% select("measure_id","location_id", "sex_id", "age_id", "cause_id", "year", "numeric_name", "est") %>% rename("rate" = "est")
data <- left_join(data_number, data_rate, by = c("measure_id","location_id", "sex_id", "age_id", "cause_id", "year", "numeric_name"))
rm(data_number, data_rate)

data <- data %>% unique() %>% filter(numeric_name == "upper") %>% spread(numeric_name, number)
data <- data %>% rename("val" = "upper")
data$population <- data$val/data$rate
data <- data %>% filter(population != 1) %>% unique()

mental_disorder_cause_id <- 558
dementia_cause_id <- c(543, 544)
epilepsy_cause_id <- 545
migraine_cause_id <- 547
tension_type_headache_cause_id <- 548
self_harm <- 718
musculoskeletal_cause_id <- 626
all_cause_id <- 294

# Causes to fully include under mental disorders
revision_full <- c(mental_disorder_cause_id,
                   dementia_cause_id,
                   epilepsy_cause_id,
                   migraine_cause_id,
                   tension_type_headache_cause_id,
                   self_harm)

# Causes to include under mental disorders, 2/5th
ms_id <- musculoskeletal_cause_id

# Relevant causes to retain
inclusion <- c(mental_disorder_cause_id,
               dementia_cause_id,
               epilepsy_cause_id,
               migraine_cause_id,
               tension_type_headache_cause_id,
               self_harm,
               musculoskeletal_cause_id,
               all_cause_id)

data_vigo <- data %>% filter(cause_id %in% inclusion)

# Applying Vigo et al. re-allocations

data_vigo$mh_gbd <- ifelse(data_vigo$cause_id == mental_disorder_cause_id, 1, 0)  # Fully retain existing mental disorders
data_vigo$mh_vigo <- ifelse(data_vigo$cause_id %in% revision_full, 1, 0)          # Allocate 100% of these conditions
data_vigo$mh_vigo[data_vigo$cause_id %in% ms_id] <- 2/5                  # Allocate 0% of these conditions
data_vigo$all <- ifelse(data_vigo$cause_id == all_cause_id, 1, 0)                 # Binary in case of all_cause 
data_vigo$value <- data_vigo$val * data_vigo$mh_gbd                               # Retain original IHME values
data_vigo$value_rev <- data_vigo$val * data_vigo$mh_vigo                          # Apply allocation percents against original IHME values

data_vigo <- data_vigo %>% 
  group_by(location_id, measure_id, sex_id, age_id, metric_id) %>% 
  mutate(mh_value_agg_rev = sum(value_rev), mh_value_agg = sum(value))

data_rev <- data_vigo %>%
  filter(cause_id == "294") %>% #Filters to all causes; data_gh$val == total burden by location id
  filter(metric_id == "1") %>%  #Filters to numeric metric
  rename("Revised - Vigo et al. method" = mh_value_agg_rev, "Original" = mh_value_agg, "measure_total" = val) %>%
  filter(measure_id %in% 1:4) %>% #Retains only deaths, DALYS, YLDs, and YLLs
  ungroup %>%
  select(c(1:4, 13:15, 17:18,24:25))

data_prev <- read.csv(file = file.path(datapath,"IHME-GBD_2019_DATA-2019-prev.csv"))

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
  c(2.33, #Upper bounds of pooled RRs
    2.54,
    1.71,
    2,
    1.43))
colnames(relative_risk) <- c("cause_id", "rel_risk")

data_prev <- inner_join(data_prev, relative_risk, by = "cause_id")

data_prev$par <-     data_prev$upper  * ((data_prev$rel_risk - 1) /    data_prev$rel_risk)

data_prev_allmentaldisorders <- data_prev %>% filter(cause_id == mental_disorder_cause_id) %>% select(c(location_id,par))

data_rev <- full_join(data_rev, data_prev_allmentaldisorders, by = "location_id")

data_rev$"Revised - Walker et al. method" = data_rev$par * data_rev$measure_total

rm(relative_risk, data_prev_allmentaldisorders)
data_rev <- data_rev %>% relocate(par, .after = population)
data_rev <- data_rev %>% rename("Original GBD method" = Original)


data_rev$composite_ylds <-ifelse(data_rev$measure_id == 3, data_rev$"Revised - Vigo et al. method", 0)
data_rev$composite_ylls <-ifelse(data_rev$measure_id == 4, data_rev$"Revised - Walker et al. method", 0)
data_rev_composite <- data_rev %>% select(location_id, measure_id, composite_ylds, composite_ylls) %>%
  group_by(location_id) %>%
  mutate(composite_dalys = sum(composite_ylds, composite_ylls))

data_rev_composite <- data_rev_composite %>% filter(measure_id == 2) %>% ungroup() %>% unique() %>% select(c(1,5))
data_rev <- inner_join(data_rev, data_rev_composite, by = "location_id")
data_rev$composite_dalys[data_rev$measure_id != 2] <- 0
data_rev$composite_deaths <-ifelse(data_rev$measure_id == 1, data_rev$"Revised - Walker et al. method", 0)
data_rev$composite <- data_rev$composite_ylds + data_rev$composite_ylls + data_rev$composite_dalys + data_rev$composite_deaths
data_rev <- data_rev %>% select(!c(14:17))
data_rev <- data_rev %>% rename("Revised - Composite method" = composite)
data_rev <- data_rev %>% relocate(12, .after = par)
data_rev$cause_name <- "Mental disorders"
data_rev <- data_rev %>% gather("estimate", "number", 11:14)
rm(data_vigo, data_vigo_global, anxiety_cause_id, bipolar_cause_id, depressive_cause_id, inclusion, mental_disorder_cause_id, schizophrenia_cause_id, data_rev_composite)
data_rev$estimate_id <- ifelse(data_rev$estimate == "Revised - Composite method", 4, 
                               ifelse(data_rev$estimate == "Revised - Walker et al. method", 3,
                                      ifelse(data_rev$estimate == "Revised - Vigo et al. method", 2, 1)))
data_rev <- data_rev %>% relocate(estimate_id, .after = estimate)

data_rev$population_100k <- data_rev$population
data_rev$population <- data_rev$population_100k  * 100000
data_rev <- data_rev %>% relocate (population_100k, .after = population)
data_rev$rate_per_100k <- data_rev$number/data_rev$population_100k
data_rev$percent <- data_rev$number/data_rev$measure_total*100

gdp <- read.csv(file = file.path(datapath,"wdi_gdp.csv")) %>%
  select(-(c(3:62))) %>% 
  rename("gdp" = "X2019")
gdp <- gdp %>% select(c(iso_code, gdp))
data_rev <- left_join(data_rev, gdp, by = "iso_code")
data_rev <- data_rev %>% relocate(gdp, .after = iso_code)
data_rev$gdp[data_rev$location_id == 1] <- 87798525859220.9        

data_rev$gdp_per_capita <- data_rev$gdp/data_rev$population
data_rev <- data_rev %>% relocate (gdp_per_capita, .after = gdp)

rm(gdp)

data_rev$cost_cc1 <- ifelse(data_rev$measure_id == 2,
                            data_rev$number * 1000, 0)
data_rev$cost_cc2 <- data_rev$cost_cc1 * 5
data_rev$cost_who1 <-ifelse(data_rev$measure_id == 2,
                            data_rev$number * data_rev$gdp_per_capita, 0)
data_rev$cost_who2 <- data_rev$cost_who1 * 3
data_global <- data_rev %>% filter(location_id == 1) %>% filter(measure_id == 2)


# Table 1: Burden of disease due to mental disorders

# Global
data_global <- data_rev %>% filter(location_id == 1)
table1 <- data_global  %>% select(measure_id, estimate, number, percent)
table1$percent <- round(table1$percent, 2)
table1$number <- round(table1$number/1000000,3)

write.csv(table1, file = "results/table1_global_upper.csv")

# Regional
table1 <- data_rev %>% filter(estimate_id != 3) %>% 
  filter(location_id != 1)%>% 
  select(measure_id, iso_code, measure_total, estimate, number)


# Adding regions
region <- read_excel(path = file.path(datapath, "regions.xlsx"))
table1 <- left_join(table1, region, by = "iso_code")

table1 <- table1 %>% group_by(who_region, estimate, measure_id) %>% mutate(region_dalytotal = sum(measure_total),
                                                                       region_dalymh = sum(number))

table1$percent <- round(table1$region_dalymh/table1$region_dalytotal*100, 2)
table1 <- table1 %>% ungroup() %>% select(measure_id, who_region, estimate, region_dalymh, percent) %>% unique()
table1$region_dalymh <- round(table1$region_dalymh/1000000,3)

write.csv(table1, file = "results/table1_region_upper.csv")

# Income level

table1 <- data_rev %>% filter(estimate_id != 3) %>% 
  filter(location_id != 1)%>% 
  select(income_level, measure_id, measure_total, estimate, number)

table1 <- table1 %>% group_by(measure_id, income_level, estimate) %>% mutate(income_level_dalytotal = sum(measure_total),
                                                                             income_level_dalymh = sum(number))

table1$percent <- round(table1$income_level_dalymh/table1$income_level_dalytotal*100, 2)
table1 <- table1 %>% ungroup() %>% select(income_level, measure_id, estimate, income_level_dalymh, percent) %>% unique() 

table1$income_level_dalymh <- round(table1$income_level_dalymh/1000000,3)

write.csv(table1, file = "results/table1_income_upper.csv")

rm(table1, region)