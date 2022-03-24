# turn ogives from years to age classes, weighting by numbers at age 
# weight that by exp(-M)

# read parameters

library(tidyverse)
library(data.table)

setwd('C:/Users/Alberto Rovellini/Documents/GOA/Parametrization/stock_recruit/data/')

# read groups
atlantis_fg <- read.csv('GOA_Groups.csv')
atlantis_fg <- atlantis_fg %>% select(Code,Name,GroupType) %>% filter(GroupType %in% c('FISH','SHARK')) %>% select(-GroupType)

# read ogives
ogive_sa <- read.csv('ogives_from_assessment.csv')
# ogive_default <- read.csv('ogives_generic.csv')
# 
# ogive_default <- ogive_default %>% 
#   select(atlantis_fg,value1:value45) %>%
#   left_join(atlantis_fg, by = c('atlantis_fg'='Code')) %>%
#   select(Name,value1:value45) %>%
#   pivot_longer(!Name, names_to='age', values_to='prop') %>%
#   drop_na() %>%
#   rename(fg=Name)

ogive_default$age <- gsub('value','',ogive_default$age)
ogive_default$age <- as.numeric(ogive_default$age)

# read biol prm
params <- read.csv('biol_params.csv')

all_fish <- unique(ogive_default$fg)
sa_fish <- unique(ogive_sa$fg)
# for each species with an ogive:

# pull M, max age, no of age classes, age class size

# calc proportion from M

# assign age class to each year

# for each age class, average prop but weight by prop?

# compare results with what we have in spreadsheet

# update spreadsheet