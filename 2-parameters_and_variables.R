# Simulation Input ####################################################################################
# In this script:
#   1. Variables and parameters are derived from the WIOD (pi, pi_f, VA, gamma,
#      alpha, trade imbalances, income)
#   2. Trade balances and contributions to the global portfolio are calibrated
#   3. Labor force data from the ILO are used
#   4. The SEO is used to calculate the respective share of payment to labor 
#      and structures in value added (beta) and adjusted for the equipment as
#      suggested by Caliendo et al.
#######################################################################################################

# Libraries ============================================================================================
library(tidyverse)
library(here)
library(readxl)
library(matlib)
library(matrixStats) 
library(reshape2)

# Data =================================================================================================
wiot <- readRDS(here("data/new_wiot.rds"))

  # wiot contains iso codes of countries 
locations <- sort(unique(wiot$producer))

countries <- c("Australia", "Austria", "Belgium", "Bulgaria", "Brazil", "Canada", "Switzerland", "China",
               "Cyprus", "Czechia", "Germany", "Denmark", "Spain", "Estonia", "Finland", "France", 
               "United Kingdom", "Greece",  "Croatia", "Hungary", "Indonesia", "India", "Ireland", 
               "Italy", "Japan", "Korea", "Lithuania", "Luxemberg", "Latvia", "Mexico", "Malta", 
               "Netherlands", "Norway", "Poland", "Portugal", "Romania", "Rest of World", 
               "Russia", "Slovakia", "Slovenia", "Sweden", "Turkey", "Taiwan", "USA")

  # define EU-28 (data from 2014, so UK is included)
europe_id <- c(2, 3, 4, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 23,
               24, 27, 28, 29, 31, 32, 34, 35, 36, 39, 40, 41 )


countries <- cbind(countries, locations)

  # save a list of countries and ido codes
saveRDS(countries, here("data/countries.rds"))
  
  # determine number of sectors
sectors <- sort(unique(wiot$sector))



N <- length(locations)
J <- length(sectors)

# Parameters ============================================================================================
# Bilateral sector import shares pi
interm_trade <- wiot %>% 
  filter(use<57) %>%
  group_by(sector, importer, use) %>% 
  mutate(pi = flow / ifelse(sum(flow) == 0, 1, sum(flow))) %>% 
  ungroup %>%
  group_by(importer, use) %>%
  mutate(test = sum(pi))

pi <- interm_trade %>%                            # variables are stored as arrays
  arrange(sector, use, producer,importer) %>% 
  .$pi %>% array(dim=c(N,N,J,J))  

# trade shares in final goods pi_f
final_trade <- wiot %>%
  filter(use == 57) %>%
  group_by(importer, sector) %>%
  mutate(total_final_cons = sum(flow)) %>%
  mutate(pi_f = ifelse(total_final_cons == 0, 0, flow/total_final_cons))

pi_f <- final_trade %>%
  arrange(sector, producer, importer) %>%
  .$pi_f %>% array(dim=c(N,N,J))

# sectoral intermediate shares -------------------------------------------------------------------------
# country-sector revenues 
prod_data <- wiot %>% 
  group_by(producer, sector) %>% 
  summarise(tot_revenue = sum(flow)) %>% 
  ungroup

prod_data <- wiot %>% 
  group_by(importer, use) %>% 
  summarise(tot_intermediate = sum(flow)) %>%
  filter(use != 57) %>% 
  rename(producer = importer, sector = use) %>% 
  right_join(prod_data, by = c("producer", "sector"))

O <- prod_data %>%
  arrange(sector, producer) %>%
  .$tot_revenue %>% array(dim=c(N,J))

# sectoral use shares (gamma_njk) -----------------------------------------------------------------------
prod_data <- prod_data %>% 
  mutate(value_added = tot_revenue - tot_intermediate) %>% 
  mutate(gamma_nj = ifelse(tot_revenue == 0, 0, value_added / tot_revenue))

VA <- prod_data %>%
  distinct(producer, sector, value_added) %>%
  group_by(producer) %>%
  summarise(VA = sum(value_added)) %>%
  arrange(producer) %>%
  .$VA %>% as.vector()

prod_data <- wiot %>% 
  group_by(importer, use, sector) %>% 
  summarise(intermediate = sum(flow)) %>% 
  ungroup %>% 
  rename(producer = importer, sector = use, intermediate_sec = sector) %>% 
  right_join(prod_data, by = c("producer", "sector")) %>%
  mutate(gamma_njk = ifelse(tot_revenue == 0, 0, intermediate / tot_revenue))

gamma_njk <- prod_data %>%
  arrange(intermediate_sec, sector, producer) %>% 
  .$gamma_njk %>% array(dim=c(N,J,J))

gamma_help <- apply(gamma_njk, c(1,2), function(x) sum(x))
gamma_nj <- matrix(rep(1, N*J), nrow=N)-gamma_help

# consumption shares alpha_nj --------------------------------------------------------------------------
cnsmptn_data <- wiot %>% 
  filter(use == 57) %>%
  group_by(importer, sector) %>% 
  summarise(sec_cnsmptn = sum(flow)) %>%
  group_by(importer) %>%
  mutate(alpha = sec_cnsmptn/sum(sec_cnsmptn))

alpha <- cnsmptn_data %>%
  arrange( sector, importer) %>%
  .$alpha %>% matrix(nrow = N) 


# labor force per country -------------------------------------------------------------------------------
# data from International Organisation of Labor (ILO) (Total labor force per country, 2014)
labor <-read.csv(here("raw_data/ilostat-2020-06-05.csv")) %>% 
  select(ï..ref_area.label, source.label, obs_value) %>%
  rename(country = ï..ref_area.label) %>%
  mutate(worker = obs_value*1000)%>%                          # workers are counted in 1,000
  select(-obs_value) %>%
  separate(source.label, into=("producer"), sep=3) 

total_labor_force <- labor$worker[labor$producer=="X01"]      # save value global labor force

labor <- labor %>%                                            # replace global labor force by value 
  filter(producer !="X01")                                    # for ROW

labor<- labor %>%
  add_row(producer = "ROW", worker = total_labor_force - sum(labor$worker))

L <- labor %>%
  arrange(producer) %>%
  .$worker %>%
  as.vector()  


# Socio-Economic Account (SEA)--------------------------------------------------------------------------
# Use SEA to compute share of payments to labor/land in VA (SEA contains data on compensation)

seo <- read_excel(here("raw_data/WIOD_SEA_Nov16.xlsx"), sheet = "DATA")  %>%
  select(-("2000":"2013")) %>%
  select(-description) %>%
  rename( baseyear = "2014")

#share of payment to labor / land&structures in value added----------------------------------------------
beta_nj <- seo %>%
  filter(variable == "COMP" | variable =="VA") %>%
  group_by(variable) %>%
  group_split()

comp<- beta_nj[[1]]
VAx <- beta_nj[[2]]

beta_n <- inner_join(comp, VAx, by = c("country", "code")) %>%
  rename(VA = baseyear.y, comp=baseyear.x) %>%
  select( -c(variable.x, variable.y)) %>%
  group_by(country)%>%
  summarise(VA=sum(VA), comp=sum(comp)) %>%                           # beta is the share of payment
  mutate(beta_n = ifelse(1 - (comp/VA) < 0, 0, 1 - (comp/VA) )) %>%   # to land & structures in VA 
  add_row(country="ROW") %>%                                          # add ROW as average of 
  mutate(beta_n = if_else(country=="ROW", mean(beta_n, na.rm = TRUE) , beta_n) ) # all other countries


# as suggested in the appendix, beta_n is adjusted for the share of equipment in land&structures, 0.17
beta_n <- mutate(beta_n, beta_n_adj = (beta_n-0.17)/0.83)

beta <- beta_n %>%
  arrange(country) %>%
  .$beta_n_adj %>% as.vector()


# Observed Trade surpluses===============================================================================
surplus <- wiot %>%
  group_by(importer) %>%
  mutate(agg_use = sum(flow)) %>%
  group_by(producer) %>%
  mutate(agg_revenue = sum(flow),
         agg_use = unique(agg_use[producer == importer]),
         surplus = agg_revenue - agg_use ) %>%
  ungroup()


surplus <- surplus %>%
  distinct(producer, surplus) %>%
  left_join(labor, by="producer") %>%
  mutate(sur_pc = surplus/worker)

S <- surplus %>%
  arrange(producer) %>%
  .$surplus %>% as.vector()


# Deficit Calibration =================================================================================
# EU trade balance for trade portfolio
europe <- c("AUT", "BEL", "BGR", "CYP", "CZE", "DEU", "DNK", "ESP",
               "EST", "FIN", "FRA", "GBR", "GRC", "HRV", "HUN", "IRL", "ITA",
               "LTU", "LUX", "LVA", "MLT", "NLD", "POL", "PRT", "ROU",
               "SVK", "SVN", "SWE")

# 1. Compute aggregate EU imports and exports to obtain the EU's external trade balance
eu_exports <- wiot %>%
  filter(producer %in% europe) %>%
  filter(!(importer %in% europe)) %>%
  mutate(exports = sum(flow)) %>%
  distinct(exports) %>%
  pull()


eu_imports <- wiot %>%
  filter(!(producer %in% europe)) %>%
  filter(importer %in% europe) %>%
  mutate(imports = sum(flow)) %>%
  distinct(imports) %>%
  pull()

eu_surplus <- eu_exports - eu_imports


# Follow Caliendo et al. (2018) and compute rH as beta*VA
rH <- beta[europe_id]*VA[europe_id]

# Optimizing function ----------------------------------------------------------------------------------
fct <- function(x) {
  sum((S[europe_id] - (x*rH - L[europe_id]*((sum(x*rH)- eu_surplus)/sum(L[europe_id]))))^2)
}

iota_opt <- optim(par= rep(0.5, 28), fn=fct, method = "L-BFGS-B",
                  lower = 0, upper = 1,  hessian = FALSE)

iota <- iota_opt$par


# Testing ----------------------------------------------------------------------------------------------
# estimates deficit
surplus_est <- sapply(1:28, function(n) {
  iota[n]*rH[n]- L[europe_id][n]/sum(L[europe_id])*((sum(iota*rH)-eu_surplus))
})

# dataframe containing country, iota, actual and estimated deficit
new_sur <- data.frame(europe, S[europe_id], surplus_est)%>%
  rename("Observed Surplus" = S.europe_id.) %>%
  rename("Portfolio balance" = surplus_est)

new_sur_long <- melt(new_sur, id="europe")  # convert to long format

# plot difference
surplus_plot <- ggplot(data = new_sur_long, aes(x=europe_id, y= value, fill = variable)) +
  geom_bar(stat="identity", position=position_dodge()) +
  scale_fill_brewer(palette="Paired")+
  scale_y_continuous(labels = scales::comma) +
  guides(fill = guide_legend(title=NULL)) +
  theme_minimal()+ 
  theme(legend.position = c(0.8, 0.8),
        legend.text=element_text(size=12)) +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())

# plot iota
iota_data <- data.frame(iota, europe)

iota_plot <-  ggplot(data = iota_data, aes(europe_id, iota)) +
  geom_bar(stat = "identity",  width = 0.7, fill="steelblue") +
  scale_fill_brewer(palette="Paired")+
  scale_y_continuous( labels = scales::number_format(accuracy = 0.1)) +
  guides(fill = guide_legend(title=NULL)) +
  theme_minimal()+ 
  theme(legend.position = c(0.8, 0.8),
        legend.text=element_text(size=12)) +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())

# Corrected/Modeled Trade imbalances -------------------------------------------------------------------
surplus_matched <- surplus %>%
  left_join(iota_data, by=c("producer" = "europe"))%>%
  replace_na(list(iota=0)) %>%
  left_join(new_sur, by= c("producer" = "europe")) %>%
  select( -"Observed Surplus")%>%
  rename(balance= "Portfolio balance") %>%
  replace_na(list(balance = 0)) %>%
  mutate(S_diff = if_else(producer%in%europe, surplus - balance, surplus ))


S_diff <- surplus_matched %>%              #surplus that is not explained by the model
  arrange(producer) %>%
  .$S_diff %>% as.vector()


#Income ------------------------------------------------------------------------------------------------
income <- prod_data %>%
  distinct(producer, sector, value_added) %>%
  group_by(producer) %>%
  summarise (VA = sum(value_added)) %>%              
  left_join(surplus_matched, by="producer") %>%
  mutate(inc = (VA/worker) - (surplus/worker)) %>%
  mutate(phi = (1/(1+(surplus/(worker*inc)))))

I <- income %>%
  arrange(producer) %>%
  .$inc %>% as.vector()

phi <- income %>%
  arrange(producer) %>%
  .$phi %>% as.vector()


#--------------------------------------------------------------------------------------------------------
# values for sigma_j from Felbermayr et al 2017/IfW
sigma_j <- c(1.956, 1.869, 3.584, 3.584, 1.634, 3.584, 3.584, 1.037, 2.042,
             6.039, 3.776, 7.630, 2.815, 1.417, 4.715, 1.841, 5.731, 6.424,
             7.509, 4.390, 5.173, 3.416, 7.509, rep(5.959, 33))

#Store Results===========================================================================================
variables <-   (list("pi" = pi,
                     "pi_f" = pi_f, 
                     "eu_surplus" = eu_surplus,
                     "S" = S,
                     "S_diff" = S_diff,
                     "I" = I, 
                     "L" = L,                     
                     "phi" = phi,
                     "O" = O))

parameters <- (list("alpha" = alpha,
                    "beta" = beta,
                    "iota" = iota,
                    "gamma_njk" = gamma_njk,
                    "gamma_nj" = gamma_nj, 
                    "sigma_j" = sigma_j))


saveRDS(variables, here("data/variables"))
saveRDS(parameters,here("data/parameters"))

