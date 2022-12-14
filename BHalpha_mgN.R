# Alberto Rovellini
# 12/13/2022
# Code to recalculate BHalpha as mgN instead of number of individuals, as suggested in the manual

library(tidyverse)

nums_at_age <- read.csv('data/nums_age_functional_groups.csv')
bhalpha <- read.csv('data/BHalpha.csv')

# clean
bhalpha <- bhalpha %>%
  drop_na() %>%
  mutate(BHalpha_XXX = as.numeric(BHalpha_XXX))

grp <- bhalpha %>% pull(atlantis_fg) %>% unique()

age1_mgN <- nums_at_age %>%
  filter(Code %in% grp, age_class == 1) %>%
  mutate(mgN = struct_N_mg + resN_mg) %>%
  select(Code, mgN)

new_bhalpha <-  bhalpha %>%
  left_join(age1_mgN, by = c('atlantis_fg'='Code')) %>%
  mutate(newalpha = BHalpha_XXX * mgN) %>%
  select(atlantis_fg, newalpha) %>%
  mutate(atlantis_fg = paste0('BHalpha_', atlantis_fg))

write.table(new_bhalpha, 'BHalpha_mgN.txt', sep = ' ', row.names = F, quote = F, col.names = F)

