# Read maturity ogives for stocks that we have them for

library(tidyverse)
library(data.table)

setwd('data/Tier 3 assessments/dat_files/')

list_dat <- list.files(full.names = F)

# make a key with atlantis fg names
fg <- c('Arrowtooth_flounder','Cod','Flatfish_deep','Rockfish_pelagic_shelf','Flathead_sole','Halibut','Rockfish_slope',
        'Flatfish_shallow','Flatfish_shallow','Pollock','Pacific_ocean_perch','Rockfish_slope','Flatfish_shallow','Flatfish_shallow',
        'Sablefish','Flatfish_shallow','Flatfish_shallow')

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

