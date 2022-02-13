library(tidyverse)
library(data.table)

bh <- read.csv("bh_parameters_for_atlantis.csv")
fg <- read.csv("GOA_Groups.csv")

# BHalpha is additive, BHbeta probably not so be careful with FFS
# for plotting here, use form R = (a*S)/(S+b)

hmean <- bh %>% select(Code, h) %>% group_by(Code) %>% summarize(h=mean(h))

bh <- bh %>% select(-h) %>% group_by(Code) %>% summarize_all(sum) %>% 
  ungroup() %>%
  left_join(hmean,by='Code') %>%
  mutate(BHbeta=BHbeta*144/1000000000) %>%
  left_join((fg %>% select(Code,Name)),by='Code')

allcodes <- bh$Code

alldf <- list()

for (i in 1:length(allcodes)){
  tb <- bh %>% filter(Code==allcodes[i])
  biovec <- seq(0,tb$S0,length.out=50)
  rec <- (tb$BHalpha*biovec)/(tb$BHbeta+biovec)
  df <- data.frame("Name"=tb$Name,"SSB"=biovec,"R"=rec,"S0"=tb$S0,"BHalpha"=tb$BHalpha,"BHbeta"=tb$BHbeta,"h"=tb$h)
  alldf[[i]] <- df
}

allbh <- rbindlist(alldf)

# make as set for where 50% of the recruits are produced
r50 <- allbh %>% group_by(Name,BHalpha,BHbeta,S0,h) %>%
  summarise(r50=max(R)/2) %>%
  ungroup()

# make a set of where r50 meets SSB
s50 <- r50 %>% mutate(s50 = (r50*BHbeta)/(BHalpha-r50))

# write h as annotation
annotations <- r50 %>% select(Name,h) %>%
  mutate(xpos=Inf,ypos=0,hjustvar=1,vjustvar=0,h=round(h,digits=3))

allbh %>% ggplot(aes(x=SSB/S0,y=R/1000))+
  geom_line(size=1.5, color='dodgerblue4')+
  geom_hline(data=r50, aes(yintercept=r50/1000), linetype = 'dashed')+
  geom_vline(data = s50, aes(xintercept=s50/S0), color = 'red', linetype = 'dashed')+
  geom_text(data = annotations, aes(x=xpos,y=ypos,hjust=hjustvar,
                                    vjust=vjustvar,label=paste('h',h,sep='='),group=Name))+
  theme_bw()+
  labs(y="Recruits (1000's)")+
  facet_wrap(~Name, scales = 'free',ncol=4)
  
