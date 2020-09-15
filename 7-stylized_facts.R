# Stylized facts on GVC #################################################################################
#
# This script uses the EORA database to compute the GVC participation index and two graphs for
# visualization
#
#########################################################################################################
# Libraries ==============================================================================================
library(tidyverse)
library(here)
library(gridExtra)

# ========================================================================================================
# Load Eora Database & compute GVC participation

eora <- read.csv(here("raw_data/Database_GVC_2018update_rev0323.csv")) %>%
  gather(key="year", value="DVX","DVX1990":"DVA2018" ) %>%
  select( -X.1) %>%
  rename(Country = X) %>% 
  separate(year, c("data","year" ), sep = -4) %>% 
  spread(data, DVX ) %>%
  mutate(gross_exp = FVA + DVA) %>%
  mutate(participation = GVC/gross_exp) %>%   #participation index
  mutate("FVA/E" = FVA/gross_exp) %>%         #backwards linkage
  mutate("IV/E" = DVX/gross_exp)              #forward linkage

countries_eora <- as.vector(unique(eora$Country))


# Graph for development of GVC participation -------------------------------------------------------------
subsample_timline <- eora %>%
  filter(Country %in% c("China", "Germany", "Luxembourg", "Russia", "USA")) 

graph1 <-  ggplot(data = subsample_timline, aes(x=year, y=participation, group=Country, color= Country))+
  geom_line(size=1)+
  scale_colour_brewer(palette="Paired")+
  theme_minimal()+
  scale_x_discrete(breaks=seq(1990, 2020, 5))+
  theme(legend.title = element_blank())+
  scale_y_continuous(breaks=seq(0, 1, 0.2), limits = c(0.25,0.95))+
  theme(axis.title.y=element_blank(), 
        axis.title.x=element_blank())

# Graph that compares composition of GVC participation 1993/2003 ----------------------------------------
subsample_compare <- subsample_timline %>% 
  filter(year==1993 | year==2013) %>%
  select(Country, year, "FVA/E", "IV/E") %>%
  gather(key="GVC", value="share", "FVA/E":"IV/E") %>%
  mutate(label= 100*(round(share, digits = 4))) %>%
  group_by(Country) %>% 
  mutate(y_lable_pos = cumsum(share))

graph2 <-  ggplot(data = subsample_compare, aes(x=year, y=share, fill=GVC, label=label) )+
  geom_bar(stat="identity", position="stack")+
  facet_grid(~Country)+
  scale_fill_brewer(palette="Paired")+
  geom_text(size = 3, position = position_stack(vjust=0.6), color="white")+
  scale_y_continuous(breaks=seq(0, 1, 0.2), limits = c(0,1))+
  theme_minimal()+
  theme(legend.title = element_blank()) +
  theme(axis.title.y=element_blank(), 
        axis.title.x=element_blank())

# Arrange together ======================================================================================
grid.arrange(graph1, graph2, ncol=2)
