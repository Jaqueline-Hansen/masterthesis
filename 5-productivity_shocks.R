# Simulation #########################################################################################
#
# This script simulates a positive 10% productivity shock in the benchmark and the noGVC scenario 
# It also computes elasticities of TFP and GDP for both scenario and the elasticity of welfare 
# in the benchmark scenario
# 
#######################################################################################################

# Libraries ===========================================================================================
library(tidyverse)
library(here)
library(rlist)
library(abind)
library(matrixStats) 

# Data ================================================================================================
variables_baseline <- readRDS(here("results/variables_baseline"))
variables_nogvc <- readRDS(here("results/variables_nogvc"))

variables <- readRDS(here("data/variables"))
parameters <- readRDS(here("data/parameters")) 

parameters_nozero <- readRDS(here("data/parameters_nozero")) 

N <- dim(variables$pi)[1]
J <- dim(variables$pi)[3]

# Functions ===========================================================================================
source("code/3-counterfac_fct.R")

#======================================================================================================
# Simulate 10% Productivity shock in every country in both scenarios
#======================================================================================================
# Baseline scenario
europe_id <- c(2, 3, 4, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 23,
               24, 27, 28, 29, 31, 32, 34, 35, 36, 39, 40, 41 )


# Shock
T_hat <- matrix(rep(1, J*N), nrow=N)                  # unchanged
kappa_hat <- array(rep(1, J*N*J*N), dim=c(N,N,J,J))   # unchanged
kappa_hat_F <- array(rep(1, J*N*N), dim=c(N,N,J))     # unchanged
S_diff_prime <- variables$S_diff
S_diff_prime[europe_id] <- 0

shock <- (list("T_hat" = T_hat,
               "kappa_hat" = kappa_hat,
               "kappa_hat_F" = kappa_hat_F,
               "S_diff_prime" = S_diff_prime))


prod_shock <- shock

# Benchmark scenario ==================================================================================
TFP_elas_bench <- array(0, N)
GDP_elas_bench <- array(0, N)
U_elas_bench <- array(0, 44)
U_hat_bench <- array(0,44)

for (i in 1:44 ) {
  prod_shock <- shock
  
  prod_shock$T_hat[i,] <- rep(1.1, J)
  
  prod_result <- Calc_cf(variables_baseline, parameters, prod_shock)
  
  A_change <- calc_A(variables_baseline, parameters, prod_result)
  
  GDP_change <- calc_GDP(variables_baseline, parameters, prod_result)
  
  U_hat <- calc_U(variables_baseline, parameters, prod_result) 
  
  saveRDS(prod_result, file = here(paste0("results/","benchmark", i )))
  saveRDS(A_change, file = here(paste0("results/","benchmark_tfp", i )))
  saveRDS(GDP_change,file = here(paste0("results/","benchmark_gdp", i )))
  saveRDS(U_hat, file = here(paste0("results/","benchmark_u", i )))
  
  TFP_elas_bench[i] <- A_change$agg_elas_TFP_n[i]
  GDP_elas_bench[i] <- GDP_change$agg_elas_GDP_n[i]
  U_hat_bench[i] <- U_hat$U_hat[i]
  U_elas_bench[i] <- U_hat$U_elas[i]      
  
}

saveRDS(TFP_elas_bench, here(("results/benchmark_tfp_elasticity" )))
saveRDS(GDP_elas_bench, here(("results/benchmark_gdp_elasticity" )))
saveRDS(U_elas_bench, here(("results/benchmark_welfare_elasticity" )))

# No GVC ==============================================================================================
TFP_elas_gvc <- array(0, N)
GDP_elas_gvc <- array(0, N)


for (i in 1:44 ) {
  prod_shock <- shock
  
  prod_shock$T_hat[i,] <- rep(1.1, J)
  
  prod_result <- Calc_cf(variables_nogvc, parameters_nozero, prod_shock)
  
  A_change <- calc_A(variables_nogvc, parameters_nozero, prod_result)
  
  GDP_change <- calc_GDP(variables_nogvc, parameters_nozero, prod_result)
  
  saveRDS(prod_result, file = here(paste0("results/","gvc", i )))
  saveRDS(A_change, file = here(paste0("results/","gvc_tfp", i )))
  saveRDS(GDP_change, file = here(paste0("results/","gvc_gdp", i )))

  TFP_elas_gvc[i] <- A_change$agg_elas_TFP_n[i]
  GDP_elas_gvc[i] <- GDP_change$agg_elas_GDP_n[i]
}

saveRDS(TFP_elas_gvc, here(("results/gvc_tfp_elasticity")))
saveRDS(GDP_elas_gvc, here(("results/gvc_gdp_elasticity" )))


# Summarize Results ===================================================================================
countries <- readRDS(here("data/countries.rds"))

agg_elasticities <- as.data.frame(cbind(countries, TFP_elas_bench, TFP_elas_gvc, GDP_elas_bench, 
                                        GDP_elas_gvc, U_elas_bench)) %>% 
  mutate(TFP_elas_bench = as.numeric(as.character(TFP_elas_bench)),
         TFP_elas_gvc = as.numeric(as.character(TFP_elas_gvc)),
         GDP_elas_bench = as.numeric(as.character(GDP_elas_bench)),
         GDP_elas_gvc = as.numeric(as.character(GDP_elas_gvc)),
         U_elas_bench = as.numeric(as.character(U_elas_bench)))

saveRDS(agg_elasticities, here("results/agg_elasticities"))

