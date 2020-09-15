# Functions ###########################################################################################
#
# This script defines functions that calculate counterfactual scenarios (Calc_cf) and changes in TFP 
#(calc_TFP), GDP (calc_GDP) and Welfare (calc_U)
#
#######################################################################################################
# Libraries ============================================================================================

library(tidyverse)
library(here)
library(matrixStats) 
library(readxl)
library(matlib)
library(rlist)
library(lpSolve)
library(lpSolveAPI)


#Equilibrium Conditions=================================================================================
Calc_cf <- function(variables,
                    parameters,
                    shock)  
{
  
  # Initialize data -----------------------------------------------------------------------------------
  pi <- variables$pi                    # intermediate trade shares
  pi_f <- variables$pi_f                # final trade shares
  eu_surplus <- variables$eu_surplus    # aggregate eu surplus 
  S_diff <- variables$S_diff            # difference between matched and observed surplus
  L <- variables$L                      # labor force
  I <- variables$I                      # income
  O <- variables$O                      # revenue
  phi <- variables$phi                  # phi = (1/(1+(Upsilon+S_diff/(L*I))))
                                        #     = (1/(1+(S/(L*I))))
  
  alpha <- parameters$alpha             # consumption share
  beta <- parameters$beta               # share of payments to land in VA
  gamma_njk <- parameters$gamma_njk     # sectoral use shares
  iota <- parameters$iota               # portfolio contribution (EU)
  gamma_nj <- parameters$gamma_nj       # share of VA in gross output
  sigma_j <- parameters$sigma_j         # trade elasticity / dispersion of productivities 
  
  VA <- rowSums(gamma_nj*O)             # compute VA
  
  N <- dim(pi)[1]
  J <- dim(pi)[3]
  
  # Initialize shock ----------------------------------------------------------------------------------
  T_hat <- shock$T_hat                  # productivity 
  kappa_hat <- shock$kappa_hat          # intermediate trade barriers (NxNxJxJ)
  kappa_hat_F <- shock$kappa_hat_F      # final trade barriers (NxNxJ)
  S_diff_prime <- shock$S_diff_prime    # exogenous part of surplus (set to zero in baselines)
  
  # Set up initial 'guesses' for variables-------------------------------------------------------------
  omega_hat <- rep(1,N)
  x_hat <- matrix(rep(1, J*N), nrow=N)
  P_hat <- array(1, dim = c(N,J,J) )
  P_hat_F <-matrix(rep(1, J*N), nrow=N)
  L_hat <- rep(1,N)
  pi_hat <- array(1, dim = c(N,N,J,J) )
  U_hat <- 1
  Y_prime <- matrix(rep(1, J*N), nrow=N)
  b_hat <- rep(1,N)
  L_prime <- L
  
    # Define member states of the EU (including UK, data from 2014)
  europe_id <- c(2, 3, 4, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 23,
                 24, 27, 28, 29, 31, 32, 34, 35, 36, 39, 40, 41 )
  
  # modeled trade surpluses ---------------------------------------------------------------------------
  # endogenous part of the trade surplus adjusts within the loop to account for foreign factor 
  # ownership
  
  Upsilon <-  rep(0,44)  
  
  Upsilon[europe_id] <- iota * beta[europe_id] * VA[europe_id] - 
    L[europe_id] / sum(L[europe_id]) * 
    (sum(iota * beta[europe_id] * VA[europe_id]) - eu_surplus) 
  
  Upsilon_prime <- Upsilon
  
  # ====================================================================================================  
  # Starting the loop 
  kTol <- 1e-6       
  omega_diff <- kTol + 1
  iter <- 1 
  incr_dampen <- 0 
  kDampening <- 0.3
  
  
  # Iterate Steps 1-5
  while((omega_diff > kTol) & iter < 1000) {
    
    # Iteration ======================================================================================
    # Cost change                                       #1. Input cost changes consistent with omega&P
    x_hat <- omega_hat^gamma_nj *
      sapply(1:J, function(j) {
        sapply(1:N, function(n) {
          product(sapply(1:J, function(k) { 
            (P_hat[n,j,k]^gamma_njk[n,j,k])
          }))
        })
      })
    
    # Multilateral resistance                                       #2. Price changes and trade shares 
    M <- sapply(1:J, function (k) {
      sapply(1:J, function (j) {
        t((x_hat[,k] * t(kappa_hat[,,j,k]))^(-sigma_j[k])* 
            T_hat[,k]^(sigma_j[k] * gamma_nj[,k]))
      }, simplify = "array")
    }, simplify = "array")
    
    M_f <- sapply(1:J, function(k) {
      t((x_hat[,k] * t(kappa_hat_F[,,k]))^(-sigma_j[k]) *
          T_hat[,k]^(sigma_j[k] * gamma_nj[,k]))
    }, simplify = "array")
    
    # Price indices
    P_hat <- sapply(1:J, function(k) {
      sapply(1:J, function(j) {
        (rowSums(M[,,j,k] * pi[,,j,k]))^(-1/sigma_j[k])
      }, simplify = "array")
    }, simplify = "array")
    P_hat[is.infinite(P_hat)] <- 1
    
    
    P_hat_f <- sapply(1:J, function(k) {
      (rowSums(M_f[,,k] * pi_f[,,k]))^(-1/sigma_j[k])
    }, simplify = "array")
    P_hat_f[is.infinite(P_hat_f)] <- 1
    
    
    # Trade shares    
    pi_hat <- sapply(1:J, function(k) {
      sapply(1:J, function(j) {
        M[,,j,k]/rowSums(M[,,j,k] * pi[,,j,k])
      }, simplify = "array")
    }, simplify = "array")
    pi_hat[pi == 0] <- 1
    pi_prime <- pi_hat * pi
    
    # final trade shares
    pi_hat_f <- sapply(1:J, function(j) {
      M_f[,,j]/rowSums(M_f[,,j] * pi_f[,,j])
    }, simplify = "array")
    pi_hat_f[pi_f == 0] <- 1
    pi_prime_f <- pi_hat_f * pi_f
    
    
    
    # Labor Mobility & Utility                              #3. Step: solve for change in labor force
    P_hat_n <- rowProds(P_hat_f^alpha)
    
    L_prime = L_hat * L
   
    VAR_prime = beta * VA * omega_hat * L_hat^(1 - beta)
   
    Upsilon_prime[europe_id] <- iota * VAR_prime[europe_id] - 
      L_prime[europe_id] / sum(L_prime[europe_id]) * 
      (sum(iota * VAR_prime[europe_id]) - eu_surplus) 
   
    b_hat <- (Upsilon_prime / L_prime + S_diff_prime / L_prime) / 
      (Upsilon / L + S_diff / L)
    
    U_hat <- 1 / sum(L[europe_id]) * 
      (sum(L[europe_id] * 1 / phi[europe_id] * 
             omega_hat[europe_id] / P_hat_n[europe_id] * 
             L_hat[europe_id]^(1 - beta[europe_id])) -
         sum(L[europe_id] * ((1 - phi[europe_id]) / phi[europe_id]) * 
               (L_hat[europe_id] * b_hat[europe_id] / P_hat_n[europe_id])))
    
    help <- (omega_hat / (phi * P_hat_n * U_hat + (1 - phi) * b_hat))^(1 / beta)
    
    L_hat[europe_id] <- help[europe_id] / sum(L[europe_id] * help[europe_id]) * 
      sum(L[europe_id])
    
    # updating
    L_prime <- L * L_hat 
    
    
    
    
    # Counterfactual output                                                  #4. Goods market clearing 
    final_exp_long <- sapply(1:J, function(j)  {
      sapply(1:N, function(n) {sum(
        sapply(1:N, function(i) { 
          pi_prime_f[i,n,j] * alpha[i,j] * 
            (omega_hat[i] * L_hat[i]^(1-beta[i]) * (I[i]* L[i] + Upsilon[i] + S_diff[i]) -  Upsilon_prime[i] - S_diff_prime[i])
        }))})}) %>% matrix(nrow=N*J)
    
    
    exp_shares_help <- sapply(1:J, function(k) {
      sapply(1:N, function(i) {
        sapply(1:J, function(j) {
          pi_prime[i,,k,j]*gamma_njk[i,k,j]
        })})}) %>% matrix(nrow=N*J)
    
    
    exp_shares <- diag(J*N) - exp_shares_help
    
    Y_prime <- solve(exp_shares, final_exp_long) %>%
      matrix(nrow=N)
    
   
    # New equilibrium wages                                # 5. Trade balance and new equilibrium wages 
    denominator <- L_hat^(1 - beta) * (L * I + Upsilon + S_diff)
    
    exp <-  colSums(sapply(1:N, function(n) {
      sapply(1:J, function(j) {
        (pi_prime[,n,,j]*gamma_njk[,,j]*Y_prime)      
      })}))  
    
    final_exp <- sapply(1:N, function(n) { sum(
      sapply(1:N, function(i) { 
        sapply(1:J, function(j) {
          pi_prime_f[i,n,j]*alpha[i,j]*((omega_hat[i] * L_hat[i]^(1-beta[i])*(L[i]*I[i]+Upsilon[i]+S_diff[i]))-Upsilon_prime[i]-S_diff_prime[i])
        })}))})
    
    imp <- colSums(sapply(1:N, function(n) { 
      sapply(1:J, function(k) {
        sapply(1:N, function(i) {
          (pi_prime[n,i,k,]*gamma_njk[n,k,]*Y_prime[n,k])     
        }) }) }) )
    
    
    
    omega_hat_new <- (exp + final_exp - imp)/denominator
    
    
    # -------------------------------------------------------------------------------------------------   
    
    # get the maximum change (to compare to the tolerance)
    omega_diff_new <- max(abs((omega_hat_new - omega_hat)))
    
    #Dampening
    omega_hat_new <- omega_hat + kDampening * (omega_hat_new - omega_hat)
  
    
    #Iterate
    omega_diff <- omega_diff_new
    omega_hat <- omega_hat_new
    
    
    # print current status 
    print(paste0("iter: ", iter, " omega_diff: ", omega_diff))
    iter <- iter + 1
    
  }
  
  return(list("x_hat" = x_hat, 
              "P_hat" = P_hat,
              "P_hat_f" = P_hat_f,
              "L_hat" = L_hat,
              "U_hat" = U_hat,
              "pi_hat" = pi_hat,
              "pi_hat_f" = pi_hat_f, 
              "Y_prime" = Y_prime,
              "Upsilon_prime" = Upsilon_prime,
              "omega_hat" = omega_hat_new))
}

#=======================================================================================================
# Changes in TFP ---------------------------------------------------------------------------------------
calc_A <- function(variables, 
                   parameters,
                   result) 
{
  # Initialize Variables
  Y <- variables$O
  P_hat_f <- result$P_hat_f
  x_hat <- result$x_hat
  alpha <- parameters$alpha
  
  P_hat_n <- rowProds(P_hat_f^alpha)
  
  ln_A_hat <- log(x_hat / P_hat_n)
  
  A_hat_nj <- exp(ln_A_hat)
  
  
  A_hat_n <- rowSums(sapply(1:J, function(j){                           #weighted by output shares
    sapply(1:N, function(n){
      Y[n,j]/(sum(Y[n,]))* A_hat_nj[n,j] })}))
  
  A_hat_j <- colSums(sapply(1:J, function(j){                           #weighted by output shares
    sapply(1:N, function(n){
      Y[n,j]/(sum(Y[,j]))* A_hat_nj[n,j] })}))
  
  
  A_hat <- sum(sapply(1:J, function(j){                                 #weighted by output shares
    sapply(1:N, function(n){
      Y[n,j]/(sum(Y))* A_hat_nj[n,j] })}))
  
  
  #Compute aggregate Elasitcities
  agg_elas_TFP_n <-  sapply(1:N, function(n){
    (A_hat - 1)/((sum(Y[n,])/sum(Y))*0.1)
  }) 
  
  agg_elas_TFP_j <-  sapply(1:J, function(j){
    (A_hat - 1)/((sum(Y[,j])/sum(Y))*0.1)
  }) 
  
  # Compute regional/sectoral elasticities -> not used later
  elas_TFP_n <-  (A_hat_n -1) / 0.1
  
  elas_TFP_j <- (A_hat_j -1) / 0.1
  
  
  
  
  return( list ("A_hat_nj" = A_hat_nj,
                "A_hat_n" = A_hat_n,
                "A_hat" = A_hat,
                "A_hat_j" = A_hat_j,
                "agg_elas_TFP_n" = agg_elas_TFP_n,
                "agg_elas_TFP_j" = agg_elas_TFP_j,
                "elas_TFP_n" = elas_TFP_n, 
                "elas_TFP_j" = elas_TFP_j))
}

# Changes in GDP ---------------------------------------------------------------------------------------
calc_GDP <- function(variables, 
                     parameters,
                     results) 
{
  #Initialize Variables
  L_hat <-  results$L_hat
  omega_hat <-  results$omega_hat
  x_hat <-  results$x_hat
  Y_prime <- results$Y_prime
  
  Y <- variables$O
  beta <-  parameters$beta
  gamma_nj <- parameters$gamma_nj
  VA <- gamma_nj*Y
  
  Y_hat <- Y_prime/Y
  Y_hat[is.na(Y_hat)]=1
  
  
  P_hat_f <- results$P_hat_f
  alpha <- parameters$alpha
  
  P_hat_n <- rowProds(P_hat_f^alpha)
  
  
  # Compute w_hat from w = (omega*(H/L)^beta) * (1-beta)
  w_hat <- omega_hat/(L_hat^beta)
  
  # Compute L_hat_nj
  L_hat_nj <- Y_hat/w_hat
  
  #weight VA = gamma*Y
  VA = gamma_nj*Y
  
  
  ln_GDP_hat <- log(w_hat)+log(L_hat_nj)-log(P_hat_n)
  
  GDP_hat_nj <- exp(ln_GDP_hat)
  
  GDP_hat_n <- rowSums(sapply(1:J, function(j){                           #weighted by VA shares             
    sapply(1:N, function(n){
      (VA[n,j]/(sum(VA[n,])))* GDP_hat_nj[n,j]
    })}))
  
  GDP_hat_j <- colSums(sapply(1:J, function(j){                           #weighted by VA shares
    sapply(1:N, function(n){
      ( VA[,j]/(sum(VA[,j])))* GDP_hat_nj[n,j] })}))
  
  GDP_hat <- sum(sapply(1:J, function(j){                                 #weighted by VA shares
    sapply(1:N, function(n){
      VA[n,j]/sum(VA)*GDP_hat_nj[n,j] 
    })}))
  
  
  #aggregate elasticities
  agg_elas_GDP_n <-  sapply(1:N, function(n){
    (GDP_hat - 1) / ((sum(VA[n,])/sum(VA))*0.1)
  })   
  
  agg_elas_GDP_j <-  sapply(1:J, function(j){
    (GDP_hat-1) / ((sum(VA[,j])/sum(VA))*0.1)
  }) 
  
  #regional and sectoral elasticities
  elas_GDP_n <-  (GDP_hat_n -1) / 0.1
  
  elas_GDP_j <- (GDP_hat_j -1) / 0.1
  
  
  return( list ("GDP_hat_nj" = GDP_hat_nj,
                "GDP_hat_n" = GDP_hat_n,
                "GDP_hat" = GDP_hat,
                "GDP_hat_j" = GDP_hat_j,
                "agg_elas_GDP_n" = agg_elas_GDP_n,
                "agg_elas_GDP_j" = agg_elas_GDP_j,
                "elas_GDP_n" = elas_GDP_n, 
                "elas_GDP_j" = elas_GDP_j))
  
}

# Changes in Welfare -----------------------------------------------------------------------------------
calc_U <- function(variables, 
                   parameters, 
                   result)
  
{
  # Initialize Variables and Parameters
  phi <- variables$phi
  Y <- variables$O
  L <- variables$L
  eu_surplus <- variables$eu_surplus
  
  S_diff <- variables$S_diff
  S_diff_prime <- variables$S_diff 
  
  L_hat <- result$L_hat
  omega_hat <- result$omega_hat
  P_hat_f <- result$P_hat_f
  
  alpha <- parameters$alpha
  beta <- parameters$beta
  iota <- parameters$iota
  gamma_nj <- parameters$gamma_nj
  
  VA <- variables$VA
  
  # Compute P_hat_n, Upsilon (prime) and b_hat
  P_hat_n <- rowProds(P_hat_f^alpha)
  
  Upsilon <-  rep(0,44)
  Upsilon_prime <- rep(0,44)
  
  Upsilon[europe_id] <- iota * beta[europe_id] * VA[europe_id] - 
    L[europe_id] / sum(L[europe_id]) * 
    (sum(iota * beta[europe_id] * VA[europe_id]) - eu_surplus) 
  
  VAR_prime <- beta * VA * omega_hat * L_hat^(1 - beta)
  
  L_prime <- L*L_hat
  
  Upsilon_prime[europe_id] <- iota * VAR_prime[europe_id] - 
    L_prime[europe_id] / sum(L_prime[europe_id]) * 
    (sum(iota * VAR_prime[europe_id]) - eu_surplus) 
  
  b_hat <- (Upsilon_prime / L_prime + S_diff_prime / L_prime) / 
    (Upsilon / L + S_diff / L)
  
  
  # Compute welfare level for non-European countries
  U_hat <- 1/phi * (omega_hat/P_hat_n) *  L_hat^(-beta)  -
    ((1 - phi)/phi) * (b_hat / P_hat_n)
  
  # Then correct for European countries using labor market clearing condition U*L = sum(U*L_hat*L)
  U_hat[europe_id] <- 1 / sum(L[europe_id]) * sum((U_hat * L_hat * L)[europe_id])
  
  
  # Elasticity of Welfare 
  U_elas <- (U_hat-1)/0.1
    # Weight by share of labor force for European countries
  U_elas[europe_id] <-  (U_hat[europe_id]-1)/(L[europe_id]/sum(L[europe_id])*0.1)
  
  
  return(list ("U_hat" = U_hat,
               "U_elas" = U_elas))
}

