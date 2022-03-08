# SPR approach
# this code calculates Spawners per recruit for each species based on:
# max age, age of recruits, weight at age, maturity ogives, and M

# SSBR = sum(w*p*exp(M))

library(tidyverse)
library(data.table)

setwd('C:/Users/Alberto Rovellini/Documents/GOA/Parametrization/stock_recruit/data/')

# read groups
atlantis_fg <- read.csv('GOA_Groups.csv')
atlantis_fg <- atlantis_fg %>% select(Code,Name,GroupType) %>% filter(GroupType %in% c('FISH','SHARK')) %>% select(-GroupType)

# read ogives
ogive_sa <- read.csv('ogives_from_assessment.csv')
ogive_default <- read.csv('ogives_generic.csv')

ogive_default <- ogive_default %>% 
  select(atlantis_fg,value1:value45) %>%
  left_join(atlantis_fg, by = c('atlantis_fg'='Code')) %>%
  select(Name,value1:value45) %>%
  pivot_longer(!Name, names_to='age', values_to='prop') %>%
  drop_na() %>%
  rename(fg=Name)

ogive_default$age <- gsub('value','',ogive_default$age)
ogive_default$age <- as.numeric(ogive_default$age)

# read biol prm
params <- read.csv('biol_params.csv')

all_fish <- unique(ogive_default$fg)
sa_fish <- unique(ogive_sa$fg)

#function to calculate Spawning Stock Biomass per Recruit (SSBR)
# This is calculated in g wet weight of spawning biomass per recruit
# multiply this by R0 to obtain SSBR for the unfished state (the formula I use here does not contain F)
# That will be the S0 corresponding to R0. Take care to change that to mg N for Atlantis (*1000 /20 /5.7)

get_ssbr <- function(this_fg){
  
  print(paste('doing',this_fg,sep=' '))
  
  this_Linf <- params %>% filter(Name==this_fg) %>% pull(Linf_FUNC)
  this_k <- params %>% filter(Name==this_fg) %>% pull(k_FUNC)
  this_M <- params %>% filter(Name==this_fg) %>% pull(M_FUNC)
  this_a <- params %>% filter(Name==this_fg) %>% pull(a_FUNC)
  this_b <- params %>% filter(Name==this_fg) %>% pull(b_FUNC)
  this_maxage <- params %>% filter(Name==this_fg) %>% pull(tmax_Max_age)
  these_age_classes <- params %>% filter(Name==this_fg) %>% pull(num_age_class)
  
  if(this_fg %in% sa_fish){
    this_ogive <- ogive_sa %>% filter(fg==this_fg) %>% pull(prop)
    # if ogive from sa, make sure the age classes are as they should be in Atlantis:
    # pad with 1 if ogive is shorter than maxage
    # truncate ogive if longer than max age
    if(length(this_ogive!=this_maxage)){
      if(length(this_ogive)<this_maxage){
        this_ogive <- c(this_ogive,rep(1,(this_maxage-length(this_ogive))))
      } else {
        this_ogive <- this_ogive[1:this_maxage]
      }
    }
  } else {
    this_ogive <- ogive_default %>% filter(fg==this_fg) %>% pull(prop)
  } 
  
  # if ogive is a default from a group with multi-year age classes, do an interpolation
  if(this_fg %in% setdiff(all_fish,sa_fish)){
    if(this_maxage>these_age_classes){
      ac_size <- this_maxage/these_age_classes
      
      # construct a vector of the real age of each atlantis ageclass
      real_ages <- ac_size*(1:these_age_classes)
      
      # do a linear interpolation
      this_ogive <- approx(real_ages,this_ogive,seq(1:this_maxage),yleft = 0,yright = 1)[[2]]
    }
  }
  
  dat <- data.frame('age'=1:this_maxage) %>%
    mutate(length = this_Linf * (1 - exp(- this_k * (age))),
           weight = this_a * length ^ this_b,
           mature = this_ogive,
           M = this_M,
           SSBR = weight * mature * exp(-M * age))
  
  ssbr <- data.frame('fg'=this_fg,'ssbr_g_wet'=sum(dat$SSBR))
  
  return(ssbr)

}

ssbr_all <- rbindlist(lapply(all_fish,get_ssbr))

# convert to mg N

ssbr_all <- ssbr_all %>%
  mutate(ssbr_mgN = ssbr_g_wet * 1000 / 20 / 5.7)

# add code

ssbr_all <- ssbr_all %>%
  left_join(atlantis_fg, by = c('fg'='Name')) %>%
  select(fg,Code,ssbr_g_wet,ssbr_mgN)

# write this out
write.csv(ssbr_all,'ssbr.csv',row.names = F)
