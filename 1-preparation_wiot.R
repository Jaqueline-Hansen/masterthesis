# Preparation WIOT ####################################################################################
# This code prepares the WIOD for the rest of the analysis:
# 1. Reshape wiot into longtable
# 2. Correct negative VA for Luxemboug and Malta
# 3. Remove inventory changes and recalculate wiot
# 4. Calculate "wiot_nozero" with corrected zero-domestic-sourcing industries
#
#######################################################################################################
# Libraries ============================================================================================
library(tidyverse)
library(here)

#Data ==================================================================================================
load(here("raw_data/WIOT2014_October16_ROW.RData"))

# clean data
wiot <- wiot %>%
  filter(!(IndustryCode %in% c("II_fob", "TXSP", "EXP_adj", "PURR", "PURNR", "VA", "IntTTM", "GO"))) %>% 
  filter(Country != "TOT") 

# save a list of industry numbers and names
industries <- select(wiot, sector_name = IndustryDescription, sector = RNr) %>%
  distinct()

wiot <- wiot %>% 
  select(-TOT, -IndustryCode, -IndustryDescription, -Year, sector = RNr, 
         producer = Country) %>%
  gather("importer_use", "flow", -producer, -sector) %>%
  separate(importer_use, into = c("importer", "use"), 3) %>%
  mutate(use = as.numeric(use))


# Correct negative VA observed in Luxembourg and Malta
lux_fix <- wiot %>% 
  filter(producer == "LUX" & sector == 32 & use == 57) %>% 
  mutate(flow = flow * (sum(flow) + 55) / sum(flow))  

mlt_fix <- wiot %>% 
  filter(producer == "MLT" & sector == 4 & use == 57) %>% 
  mutate(flow = flow * (sum(flow) + 3) / sum(flow))

wiot <- wiot %>% 
  filter(!(producer == "LUX" & sector == 32 & use == 57)) %>% 
  filter(!(producer == "MLT" & sector == 4 & use == 57)) %>% 
  bind_rows(lux_fix) %>% 
  bind_rows(mlt_fix)



# Remove inventory changes and recalculate wiot ========================================================
# demand with adapted inventory ------------------------------------------------------------------------
new_demand <- wiot %>% 
  filter(use > 56) %>% 
  mutate(flow = ifelse(use %in% c(60,61) & flow < 0, 0, flow)) %>%  
  select(-use) %>% 
  group_by(producer, sector, importer) %>% 
  summarise(demand = sum(flow)) %>% 
  ungroup

# Coefficient Matrix ----------------------------------------------------------------------------------
x <-  wiot %>% 
  filter(use < 57) %>% 
  arrange(importer, use, producer, sector) %>%
  .$flow %>% 
  matrix(nrow = 44*56)

# get a vector of total output (revenue) 
output <- wiot %>%
  group_by(producer, sector) %>% 
  summarise(flow = sum(flow)) %>% 
  ungroup %>% 
  arrange(producer, sector) %>% 
  .$flow

# coefficient matrix - divide each flow by the total output
A <- x / (replace(output, output == 0, 0.000001) %>% rep(each = nrow(x)))


# Update I-O table to new demand ----------------------------------------------------------------------
# use leontief-inverse to calculate new x given the constructed new demand
leontief <- diag(nrow(A)) - A

agg_new_demand <- new_demand %>% 
  group_by(producer, sector) %>%
  summarise(demand = sum(demand)) %>% 
  ungroup() %>% 
  arrange(producer, sector) %>% 
  .$demand

new_output <- solve(leontief, agg_new_demand)
new_x <-  A %*% diag(new_output)

# reorder
new_wiot <- wiot %>%  
  filter(use < 57) %>% 
  arrange(importer, use, producer, sector) %>%
  mutate(flow = as.vector(new_x))
new_wiot <- new_demand %>% 
  mutate(use = 57) %>% 
  rename(flow = demand) %>% 
  bind_rows(new_wiot)

# Remove 0-own flows ==================================================================================
# if for any importer-use-sector own flows are 0 but flows from another producer
# are larger 0, flows are set to the smallest flow from another producer or to 
# 1$, whichever is smaller
wiot_nozero <- new_wiot %>% 
  group_by(importer, use) %>% 
  mutate(flow = ifelse(flow[producer == importer] == 0 & any(flow > 0) &
                       producer == importer, 
                       min(1e-6, min(flow[flow != 0])), 
                       flow)) %>% 
  ungroup()



saveRDS(wiot, here("data/wiot.rds"))
saveRDS(new_wiot, here("data/new_wiot.rds"))
saveRDS(wiot_nozero, here("data/wiot_nozero.rds"))