# This script computes the counterfactual baseline scenarios #########################################
#
# Following Caliendo et el. (2018), 1) and 2.1) create baseline economies where the exogenous part of 
# the trade surplus S_diff is equal to zero by simulating a shock that sets S_diff_prime = 0. These
# baselines are used to compute new sets of variables (variables_baseline & variables_baseline_nozero).
#
# variables_baseline_nozero is then used to simulate the shock that switches GVC off by setting the 
# trade barriers for intermediates to infinity. This shock is used to derive the baseline economy
# without GVC (variables_nogvc).
#
# Ultimately, variables_nogvc is used to compute the welfare losses from switching off GVC 
######################################################################################################
# Libraries ===========================================================================================
library(tidyverse)
library(here)
library(matrixStats) 
library(readxl)
library(matlib)
library(rlist)
library(abind)


# Data ================================================================================================
variables <- readRDS(here("data/variables"))
parameters <- readRDS(here("data/parameters")) 

variables_nozero <- readRDS(here("data/variables_nozero"))
parameters_nozero <- readRDS(here("data/parameters_nozero")) 

J <- dim(variables$pi)[3]
N <- dim(variables$pi)[1]

europe_id <- c(2, 3, 4, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 23,
               24, 27, 28, 29, 31, 32, 34, 35, 36, 39, 40, 41 )


# Shock
T_hat <- matrix(rep(1, J*N), nrow=N)                # unchanged
kappa_hat <- array(rep(1, J*N*J*N), dim=c(N,N,J,J)) # unchanged
kappa_hat_F <- array(rep(1, J*N*N), dim=c(N,N,J))   # unchanged
S_diff_prime <- variables$S_diff

shock <- (list("T_hat" = T_hat,
               "kappa_hat" = kappa_hat,
               "kappa_hat_F" = kappa_hat_F,
               "S_diff_prime" = S_diff_prime))

# Functions ===========================================================================================
source("code/3-counterfac_fct.R")

# 1. Baseline ========================================================================================
shock_baseline <- shock
shock_baseline$S_diff_prime <- variables$S_diff
shock_baseline$S_diff_prime[europe_id] <- 0

baseline_cf <- Calc_cf(variables, parameters, shock_baseline)

# get baseline variables -----------------------------------------------------------------------------
variables_baseline <- list()
variables_baseline$pi <- variables$pi * baseline_cf$pi_hat
variables_baseline$pi_f <- variables$pi_f * baseline_cf$pi_hat_f
variables_baseline$eu_surplus <- variables$eu_surplus
variables_baseline$S_diff <- shock_baseline$S_diff_prime 
variables_baseline$I <- variables$I * baseline_cf$U_hat * 
  rowProds(baseline_cf$P_hat_f^parameters$alpha)
variables_baseline$L <- variables$L * baseline_cf$L_hat
variables_baseline$phi <- variables_baseline$L * variables_baseline$I /
  (variables_baseline$L * variables_baseline$I + 
     baseline_cf$Upsilon_prime + variables_baseline$S_diff)
variables_baseline$VA <- rowSums(parameters$gamma_nj*variables$O) * baseline_cf$omega_hat * 
  baseline_cf$L_hat^(1 - parameters$beta)
variables_baseline$O <- baseline_cf$Y_prime

saveRDS(baseline_cf, here("results/baseline_cf"))
saveRDS(variables_baseline, here("results/variables_baseline"))



# 2.1 No-zero Baseline ================================================================================
baseline_cf_nozero <- Calc_cf(variables_nozero, parameters_nozero, shock_baseline)

# get baseline variables -----------------------------------------------------------------------------
variables_baseline_nozero <- list()
variables_baseline_nozero$pi <- variables_nozero$pi * baseline_cf_nozero$pi_hat
variables_baseline_nozero$pi_f <- variables_nozero$pi_f * baseline_cf_nozero$pi_hat_f
variables_baseline_nozero$eu_surplus <- variables_nozero$eu_surplus
variables_baseline_nozero$S_diff <- shock_baseline$S_diff_prime 
variables_baseline_nozero$I <- variables_nozero$I * baseline_cf_nozero$U_hat * 
  rowProds(baseline_cf_nozero$P_hat_f^parameters_nozero$alpha)
variables_baseline_nozero$L <- variables_nozero$L * baseline_cf_nozero$L_hat
variables_baseline_nozero$phi <- variables_baseline_nozero$L * variables_baseline_nozero$I /
  (variables_baseline_nozero$L * variables_baseline_nozero$I + 
     baseline_cf_nozero$Upsilon_prime + variables_baseline_nozero$S_diff)
variables_baseline_nozero$VA <- rowSums(parameters_nozero$gamma_nj*variables_nozero$O) * 
  baseline_cf_nozero$omega_hat * baseline_cf_nozero$L_hat^(1 - parameters_nozero$beta)
variables_baseline_nozero$O <- baseline_cf_nozero$Y_prime 

saveRDS(baseline_cf_nozero, here("results/baseline_cf_nozero"))
saveRDS(variables_baseline_nozero, here("results/variables_baseline_nozero"))


# 2.2 No GVC Baseline =================================================================================
shock_nogvc <- shock
  # define matrix for kappy that with infinite trade barriers for intermediates
  mat <- matrix(rep(Inf, N*N), nrow=N)
  diag(mat) <- rep(1, N)
  shock_nogvc$kappa_hat  <- array(rep(mat, J*J), dim=c(N,N,J,J))
# S_diff has been set to zero for EU countries
shock_nogvc$S_diff_prime <- variables_baseline_nozero$S_diff

baseline_nogvc <- Calc_cf(variables_baseline_nozero, parameters_nozero, shock_nogvc)

# get baseline variables --------------------------------------------------------------------
variables_nogvc <- list()
variables_nogvc$pi <- variables_baseline_nozero$pi * baseline_nogvc$pi_hat
variables_nogvc$pi_f <- variables_baseline_nozero$pi_f * baseline_nogvc$pi_hat_f
variables_nogvc$eu_surplus <- variables_baseline_nozero$eu_surplus
variables_nogvc$S_diff <- shock_nogvc$S_diff_prime 
variables_nogvc$I <- variables_baseline_nozero$I * baseline_nogvc$U_hat * 
  rowProds(baseline_nogvc$P_hat_f^parameters_nozero$alpha)
variables_nogvc$L <- variables_baseline_nozero$L * baseline_nogvc$L_hat
variables_nogvc$phi <- variables_baseline_nozero$L * variables_baseline_nozero$I /
  (variables_baseline_nozero$L * variables_baseline_nozero$I + 
     baseline_nogvc$Upsilon_prime + variables_baseline_nozero$S_diff)
variables_nogvc$VA <- variables_baseline_nozero$VA * baseline_nogvc$omega_hat * 
  baseline_nogvc$L_hat^(1 - parameters_nozero$beta)
variables_nogvc$O <- baseline_nogvc$Y_prime

saveRDS(baseline_nogvc, here("results/baseline_nogvc"))
saveRDS(variables_nogvc, here("results/variables_nogvc"))


# Utility change from switching off GVC -----------------------------------------------
U_hat <- calc_U(variables_baseline_nozero, parameters_nozero, baseline_nogvc)
saveRDS(U_hat$U_hat, here("results/U_hat_gvc"))
