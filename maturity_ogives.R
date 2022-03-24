# Alberto Rovellini 23/3/2022
# Read maturity ogives from stock assessments for those species that we have them for
# Also constructs generic maturity ogives for other vertebrate groups based on age at maturity

# OVERARCHING ASSUMPTION:
# We assume that the proportion mature in the first age class is always 0, no matter the size of the age class
# This is to avoid a case in Atlantis where we artificially avoid overfishing because the smallest age class keeps
# producing recruits. For consistency we use this logic also when getting SPR

library(tidyverse)
library(data.table)

# Read data ---------------------------------------------------------------

setwd('C:/Users/Alberto Rovellini/Documents/GOA/Parametrization/stock_recruit/data/Tier 3 assessments/dat_files/')

list_dat <- list.files(full.names = F)

# make a key with atlantis fg names
fg <- c('Arrowtooth_flounder','Cod','Flatfish_deep','Rockfish_pelagic_shelf','Flathead_sole','Halibut','Rockfish_slope',
        'Flatfish_shallow','Flatfish_shallow','Pollock','Pacific_ocean_perch','Rockfish_slope','Rex_sole','Rex_sole',
        'Sablefish','Flatfish_shallow','Flatfish_shallow')

# Also read in Atlantis groups
atlantis_fg <- read.csv('../../../data/GOA_Groups.csv')

# read in biological parameters
biol_params <- read.csv('../../../data/biol_params.csv')

# Ogives from assessment --------------------------------------------------

# These have values per year, not per age class

key <- data.frame('species'=substr(list_dat,1,(nchar(list_dat)-4)), 'fg'=fg)

matlist <- list()

for(i in 1:length(list_dat)){
  
  this_file <- list_dat[i]
  
  this_species <- substr(this_file,1,(nchar(this_file)-4))
  
  matvec <- read.table(this_file,sep=' ')
  
  matvec <- data.frame('species'=this_species, 'age'=1:ncol(matvec), 'prop'=t(matvec))
  
  matlist[[i]] <- matvec
  
}

mat_at_age <- rbindlist(matlist)

mat_at_age <- mat_at_age %>%
  left_join(key, by = 'species') %>%
  group_by(fg,age) %>%
  summarise(prop=mean(prop,na.rm=T))

mat_at_age %>%
  ggplot(aes(x=age,y=prop))+
           geom_point()+
           geom_line()+
           theme_bw()+
           facet_wrap(~fg, scales='free')

mat_at_age %>% write.csv('../../ogives_from_assessment.csv', row.names = F)

# turn prop is smallest age class to 0
mat_at_age <- mat_at_age %>% rowwise() %>%
  mutate(prop = ifelse(age==1, 0, prop))

####### pulling for spreadsheet (do not run)
# this <- mat_at_age %>% filter(fg=='Rockfish_pelagic_shelf') %>% pull(prop) 
# 
# this <- c(this, rep(1,40-length(this)))
# 
# this <- this[(1:10)*4]

# Other vertebrates -------------------------------------------------------

# These have a value per age class
# These follow the assumption 0, 0.1, 0.5, 0.9, 1
# where 0.5 is the age class at 50% maturity

missing <- setdiff((atlantis_fg %>% filter(GroupType %in% c('FISH','BIRD','SHARK','MAMMAL')) %>% pull(Name)),
                   (mat_at_age %>% pull(fg) %>% unique()))

make_default_ogive <- function(this_fg){
  
  print(paste('Doing',this_fg,sep=' '))
  
  this_num_age_class <- biol_params %>% filter(Name==this_fg) %>% pull(num_age_class)
  this_mat_FUNC <- biol_params %>% filter(Name==this_fg) %>% pull(mat_FUNC)
  this_ypa_FUNC <- biol_params %>% filter(Name==this_fg) %>% pull(ypa_FUNC)
  
  this_mat_age_class <- ceiling(this_mat_FUNC/this_ypa_FUNC)
  
  dat <- data.frame(fg=this_fg,age=1:this_num_age_class,prop=0)
  idx <- which(dat$age==this_mat_age_class)
  
  # construct the ogive around 0.5
  dat[idx,]$prop <- 0.5
  if(idx>1){
    dat[idx-1,]$prop <- 0.1
  }
  dat[idx+1,]$prop <- 0.9
  if(idx>2){
    dat[1:(idx-2),]$prop <- 0
  }
  dat[(idx+2):nrow(dat),]$prop <- 1
  
  # drop any extra rows that should not be there
  dat <- dat %>% drop_na()
  
  # if fish or shark make sure that age 1 has 0% inds reproducing
  if(this_fg %in% (atlantis_fg %>% filter(GroupType %in% c('FISH','SHARK')) %>% pull(Name))){
    dat[dat$age==1,]$prop <- 0
  }
  
  dat

}


mat_at_age_other <- rbindlist(lapply(missing, make_default_ogive))

mat_at_age_other %>% write.csv('../../ogives_default.csv', row.names = F)
