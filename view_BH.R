library(tidyverse)
library(data.table)

bh <- read.csv("bh_parameters_for_atlantis.csv")
fg <- read.csv("GOA_Groups.csv")

# BHalpha is additive, BHbeta probably not so be careful with FFS
# for plotting here, use form R = (a*S)/(S+b)

bh <- bh %>% group_by(Code) %>% summarize_all(sum) %>% mutate(BHbeta=BHbeta*144/1000000000) %>%
  left_join((fg %>% select(Code,Name)),by='Code')

max(bh$S0)

allcodes <- bh$Code

alldf <- list()

for (i in 1:length(allcodes)){
  tb <- bh %>% filter(Code==allcodes[i])
  biovec <- seq(0,tb$S0,length.out=50)
  rec <- (tb$BHalpha*biovec)/(tb$BHbeta+biovec)
  df <- data.frame("Name"=tb$Name,"SSB"=biovec,"R"=rec,"S0"=tb$S0,"BHalpha"=tb$BHalpha,"BHbeta"=tb$BHbeta)
  alldf[[i]] <- df
}

allbh <- rbindlist(alldf)

r50 <- allbh %>% group_by(Name,BHalpha,BHbeta,S0) %>%
  summarise(r50=max(R)/2)

s50 <- r50 %>% mutate(s50 = (r50*BHbeta)/(BHalpha-r50))

allbh %>% ggplot(aes(x=SSB/S0,y=R))+
  geom_line()+
  geom_hline(data=r50, aes(yintercept=r50), linetype = 'dashed')+
  geom_vline(data = s50, aes(xintercept=s50/S0), color = 'red', linetype = 'dashed')+
  theme_minimal()+
  labs(y='Recruits (number)')+
  facet_wrap(~Name, scales = 'free')
  
