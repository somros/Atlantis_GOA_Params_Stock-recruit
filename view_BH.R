library(tidyverse)
library(data.table)

bh <- read.csv("bh_parameters_for_atlantis.csv")
fg <- read.csv("GOA_Groups.csv")

# BHalpha is additive, BHbeta probably not so be careful with FFS
# for plotting here, use form R = (a*S)/(S+b)

hmean <- bh %>% select(Code, h) %>% group_by(Code) %>% summarize(h=mean(h))

Methodmean <- bh %>% select(Code, Method) %>% group_by(Code) %>% summarize(Method=max(Method)) # default to the 'worst method' for RFS

bh <- bh %>% select(-h,-Method) %>% group_by(Code) %>% summarize_all(sum) %>% 
  ungroup() %>%
  left_join(hmean,by='Code') %>%
  left_join(Methodmean, by='Code')%>%
  mutate(BHbeta=BHbeta*144/1000000000) %>%
  left_join((fg %>% select(Code,Name)),by='Code')

allcodes <- bh$Code

alldf <- list()

for (i in 1:length(allcodes)){
  tb <- bh %>% filter(Code==allcodes[i])
  biovec <- seq(0,tb$S0,length.out=50)
  rec <- (tb$BHalpha*biovec)/(tb$BHbeta+biovec)
  df <- data.frame("Name"=tb$Name,"SSB"=biovec,"R"=rec,"S0"=tb$S0,"BHalpha"=tb$BHalpha,"BHbeta"=tb$BHbeta,"h"=tb$h,"Method"=tb$Method)
  alldf[[i]] <- df
}

allbh <- rbindlist(alldf)

# make as set for where 50% of the recruits are produced
r50 <- allbh %>% group_by(Name,BHalpha,BHbeta,S0,h,Method) %>%
  summarise(r50=max(R)/2) %>%
  ungroup()

# make a set of where r50 meets SSB
s50 <- r50 %>% mutate(s50 = (r50*BHbeta)/(BHalpha-r50))

# write h as annotation
method_key <- data.frame('Method'=1:3,'Assessment'=c('AK and BC','AK','No assessment'))

annotations <- r50 %>% select(Name,h,Method) %>%
  mutate(xpos=Inf,ypos=0,hjustvar=1,vjustvar=0,h=round(h,digits=3)) %>%
  left_join(method_key,by='Method')

annotations$Assessment<-factor(annotations$Assessment,levels=c('AK and BC','AK','No assessment'))

p <- allbh %>% ggplot(aes(x=SSB/S0,y=R/1000))+
  geom_line(size=1.5, color='dodgerblue4')+
  geom_hline(data=r50, aes(yintercept=r50/1000), linetype = 'dashed')+
  geom_vline(data = s50, aes(xintercept=s50/S0), color = 'red', linetype = 'dashed')+
  geom_text(data = annotations, aes(x=xpos,y=ypos,hjust=hjustvar,
                                    vjust=vjustvar,label=paste('h',h,sep='='),colour=Assessment),
            size=4, fontface='bold')+
  scale_color_manual(values = c('green','orange','red'))+
  theme_bw()+
  labs(y="Recruits (1000's)",x=bquote(SSB/S[0]))+
  facet_wrap(~Name, scales = 'free',ncol=3)
p

ggsave('bh_recruitment.png',p,width = 7.5, height = 12)
  
