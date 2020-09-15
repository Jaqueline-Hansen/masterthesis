# Visualization #######################################################################################
# 
# Maps found in the thesis
# 1. Distribution of household income
# 2. Welfare effects from switching off GVC
# 3. Main results: four panel map TFP/GDP elasticities benchmark/noGVC
# 4. Benchmark Welfare elasticities 
#
#######################################################################################################
# Libraries ===========================================================================================
library(tidyverse)
library(here)
library(ggplot2)
library(gridExtra)
library(tmap)
library(sf)
library(mapview)

# =====================================================================================================
countries <- readRDS(here("data/countries.rds"))

elasticities <- readRDS(here("results/agg_elasticities"))

bench_tfp <- as.data.frame(cbind(elasticities$TFP_elas_bench, countries)) %>%
  mutate(elas = as.numeric(as.character(V1))) %>%
  select(-V1)


nogvc_tfp <- as.data.frame(cbind(elasticities$TFP_elas_gvc, countries)) %>%
  mutate(elas = as.numeric(as.character(V1))) %>%
  select(-V1)

bench_gdp <- as.data.frame(cbind(elasticities$GDP_elas_bench, countries)) %>%
  mutate(elas = as.numeric(as.character(V1))) %>%
  select(-V1)


nogvc_gdp <- as.data.frame(cbind(elasticities$GDP_elas_gvc, countries)) %>%
  mutate(elas = as.numeric(as.character(V1))) %>%
  select(-V1)

variables<- readRDS(here("data/variables")) 
income <- variables$I                                 
income <- as.data.frame(cbind(income, countries)) %>%
  mutate(income = as.numeric(as.character(income))) %>% 
  mutate (income = 1000000*income)            #WIOD is measured in 1,000,000 U.S.$  


U_hat_gvc <- readRDS(here("results/U_hat_gvc"))
U_hat_gvc <- as.data.frame(cbind(U_hat_gvc, countries)) %>%
  mutate(U_hat = as.numeric(as.character(U_hat_gvc))) %>%
  mutate(welfare = (U_hat-1)*100 ) %>%
  select(-U_hat_gvc)

U_elas_benchmark <- readRDS("results/benchmark_welfare_elasticity")
U_elas_benchmark <- as.data.frame(cbind(U_elas_benchmark, countries)) %>%
  mutate(U_hat = as.numeric(as.character(U_elas_benchmark)))

# Map set-up ===========================================================================================
world <- st_read(here("raw_data/99bfd9e7-bb42-4728-87b5-07f8c8ac631c2020328-1-1vef4ev.lu5nk.shp")) 

world <-world %>%
  mutate(country = as.character(CNTRY_NAME))%>%  
  mutate(country = if_else(country=="Czech Republic", "Czechia", country),
         country = if_else(country=="South Korea", "Korea", country),
         country = if_else(country=="Luxembourg", "Luxemberg", country),
         country = if_else(country=="United States", "USA", country))

# National Results =====================================================================================
# Benchmark TFP elasticities
shape1 <- world %>%
  left_join(bench_tfp, by = c("country" = "countries"))

map_bench_tfp <- tm_shape(shape1)+
  tm_polygons("elas", 
              style="quantile", 
              palette="Blues",
              colorNA="white", 
              textNA="",
              title = "") +
  tm_layout(frame = FALSE, 
            title="a)")


shape2 <- world %>%
  left_join(nogvc_tfp, by = c("country" = "countries"))

map_gvc_tfp <- tm_shape(shape2)+
  tm_polygons("elas", 
              style="quantile",
              palette="Blues",
              colorNA="white",
              textNA = "",
              title = "") +
  tm_layout(frame = FALSE, 
            title="b)") 


shape3 <- world %>%
  left_join(bench_gdp, by = c("country" = "countries"))

map_bench_gdp <- tm_shape(shape3)+
  tm_polygons("elas", 
              style="quantile",
              palette="Blues",
              colorNA="white",
              textNA = "",
              title = "") +
  tm_layout(frame = FALSE, 
            title="c)") 



shape4 <- world %>%
  left_join(nogvc_gdp, by = c("country" = "countries"))


map_gvc_gdp <- tm_shape(shape4)+
  tm_polygons("elas", 
              style="quantile",
              palette="Blues",
              colorNA="white",
              textNA = "",
              title = "") +
  tm_layout(frame = FALSE, 
            title="d)") 

total_map <- tmap_arrange(map_bench_tfp, map_gvc_tfp, map_bench_gdp, map_gvc_gdp)
tmap_save(total_map, here("graphs/total_map.png"), width=8, height=10)


# Income =============================================================================================
householdIncome <- world %>%
  left_join(income, by = c("country" = "countries"))

inc <- tm_shape(householdIncome)+
  tm_polygons("income", 
              style="quantile",
              palette="Blues",
              colorNA="white",
              textNA="",
              title = "Household income in U.S.$") +
  tm_layout(frame = FALSE,
            legend.position =  c(0, 0.1)) 

tmap_save(inc, here("graphs/tmap_inc.png"))

# Welfare =============================================================================================
# Switching GVC off -----------------------------------------------------------------------------------
welfare<- world %>%
  left_join(U_hat_gvc, by = c("country" = "countries"))

welfare_effects <- tm_shape(welfare)+
  tm_polygons("welfare", 
              style="jenks",
              palette="Blues",
              colorNA="white",
              textNA="",
              title = "Welfare losses in %") +
  tm_layout(frame = FALSE,
            legend.position =  c(0, 0.1)) 

tmap_save(welfare_effects, here("graphs/tmap_welfare.png"))

# Benchmark elasticities ===============================================================================
welfare_elas <- world %>%
  left_join(U_elas_benchmark, by = c("country" = "countries"))

welfare_elas_map <- tm_shape(welfare_elas)+
  tm_polygons("U_hat", 
              style="quantile",
              palette="Blues",
              colorNA="white",
              textNA="",
              title = "") +
  tm_layout(frame = FALSE,
            legend.position = c(0, 0.1)) 

tmap_save(welfare_elas_map, here("graphs/tmap_welfare_elas.png"))
