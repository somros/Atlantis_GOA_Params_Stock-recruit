# turn ogives from years to age classes, weighting by numbers at age 
# weight that by exp(-M)

# read parameters

library(tidyverse)
library(data.table)

setwd('C:/Users/Alberto Rovellini/Documents/GOA/Parametrization/stock_recruit/data/')

# read groups
atlantis_groups <- read.csv('GOA_Groups.csv')
atlantis_fg <- atlantis_groups %>% 
  select(Code,Name,GroupType) %>% 
  filter(GroupType %in% c('FISH','SHARK','BIRD','MAMMAL')) %>% 
  select(-GroupType) %>%
  pull(Name)

# read ogives
ogive_sa <- read.csv('ogives_from_assessment.csv')
ogive_default <- read.csv('ogives_default.csv')
ogives <- rbind(ogive_sa,ogive_default)

# read biol prm
biol_params <- read.csv('biol_params.csv')

# for each species with an ogive:

make_FSPB <- function(this_fg){
  
  # get params
  this_M <- params %>% filter(Name==this_fg) %>% pull(M_FUNC)
  this_num_age_class <- biol_params %>% filter(Name==this_fg) %>% pull(num_age_class)
  this_mat_FUNC <- biol_params %>% filter(Name==this_fg) %>% pull(mat_FUNC)
  this_ypa_FUNC <- biol_params %>% filter(Name==this_fg) %>% pull(ypa_FUNC)
  this_maxage <- params %>% filter(Name==this_fg) %>% pull(tmax_Max_age)
  
  # so here write the product of age classes and age class size, and compare with max age from params
  this_maxage_actual <- this_num_age_class*this_ypa_FUNC
  
  print(paste('The difference between real max age and max age from age classes for',
              this_fg,'is',(this_maxage-this_maxage_actual),'years',sep=' '))
  
  # get ogive
  this_ogive <- ogives %>% filter(fg==this_fg) %>% pull(prop)
  
  # trim ogive to the actual max age
  
  # the below is not working for groups that have annual ogives from stock assessments and then multi-yr age bins
  # for example flatfish shallow has 
  
  if(this_fg %in% (ogive_sa %>% pull(fg) %>% unique())){
    
    if(length(this_ogive)>this_maxage_actual){ 
      
      this_ogive <- this_ogive[1:this_maxage_actual]
      
    } else if(length(this_ogive)<this_maxage_actual){
      
      this_ogive <- c(this_ogive,rep(1,(this_maxage_actual-length(this_ogive))))
      
    }
  } else {
    
    if(length(this_ogive)>(this_maxage_actual/this_ypa_FUNC)){ 
      
      this_ogive <- this_ogive[1:(this_maxage_actual/this_ypa_FUNC)]
      
    } else if(length(this_ogive)<(this_maxage_actual/this_ypa_FUNC)){
      
      this_ogive <- c(this_ogive,rep(1,((this_maxage_actual/this_ypa_FUNC)-length(this_ogive))))
    }
  }
  
  dat <- data.frame(fg=this_fg,age=1:this_maxage_actual) %>%
    mutate(age_class=rep(1:this_num_age_class,each=this_ypa_FUNC))
  
  if(length(this_ogive)==this_maxage_actual){
    dat <- dat %>% mutate(prop = this_ogive)
  } else {
    dat <- dat %>% mutate(prop = rep(this_ogive,each=this_ypa_FUNC))
  }
    
  dat <- dat %>%
    mutate(Exponential_decay = exp(-this_M*(age-1)), # calc proportion from M
           Prop_ind = Exponential_decay/sum(Exponential_decay)) %>%
    group_by(fg,age_class) %>%
    summarise(FSPB = weighted.mean(prop,Prop_ind)) # for each age class, average prop but weight by no at age
  
}

fspb <- rbindlist(lapply(atlantis_fg, make_FSPB))

# view
fspb %>% ggplot(aes(x=age_class,y=FSPB))+
  geom_point()+
  geom_line()+
  theme_bw()+
  facet_wrap(~fg, scales = 'free')

# add NA's
fspb_all <- expand.grid(unique(fspb$fg), unique(fspb$age_class)) %>% set_names(c('fg','age_class'))
fspb_all <- fspb_all[order(factor(fspb_all$fg, levels=atlantis_fg)),]
fspb_all <- fspb_all %>% 
  left_join(fspb, by = c('fg','age_class')) 

fspb_all <- fspb_all %>%
  pivot_wider(names_from = age_class, values_from = FSPB)

write.csv(fspb_all, '../output/FSPB_XXX.csv', row.names = F)  

# compare to old ogives before Martin's suggestion of weighting prop mature by the numbers at age

old_fspb <- read.csv('../data/ogives_old.csv', header = F) %>%
  left_join((atlantis_groups %>% select(Code, Name)), by = c('V1'='Code')) %>%
  select(Name, V2:V46) %>%
  set_names(colnames(fspb_all))

new_long <- fspb_all %>% pivot_longer(cols = -fg, names_to = 'age_class', values_to = 'FSPB') %>% mutate(Vers='New')
old_long <- old_fspb %>% pivot_longer(cols = -fg, names_to = 'age_class', values_to = 'FSPB') %>% mutate(Vers='Old')
comb <- rbind(new_long, old_long)
comb$age_class <- as.numeric(comb$age_class)

comb %>%
  ggplot(aes(x=age_class,y=FSPB,color=Vers))+
  geom_point()+
  geom_line()+
  theme_bw()+
  facet_wrap(~fg, scales = 'free')

# basically completely unchanged, but here, now we are weighting by numbers at age...
