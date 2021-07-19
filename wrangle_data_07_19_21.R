## load libraries
# install.packages("tidyverse") ## only run this if tidyverse is not already installed
# install.packages("glue") ## same
library(tidyverse)
library(glue)

# set file paths
dataDir="/Users/heatherwelch/Dropbox/Devynn_offshore_wind/data/"
outDir="/Users/heatherwelch/Dropbox/Devynn_offshore_wind/plots/"

## load data ## change file path to the location of this file on your computer
dat=read.csv(glue("{dataDir}MonthlyClimatology_AllSpeciesPredictions.csv"))
glimpse(dat)

## filter data to Humboldt and Socal domains
Humboldt=dat %>% filter(lon>-125.5) %>% filter(lat>40 & lat<41.5) %>% 
  mutate(Domain="Humboldt")
Socal=dat %>% filter(lon>-122.1) %>% filter(lat>34.5 & lat<36) %>% 
  mutate(Domain="Socal")

## combine Humboldt and Socal domains
master=rbind(Humboldt,Socal)
glimpse(master)

## find average habitat suitibility for each species, each month, each domain
master_average=master %>% group_by(month,Domain) %>% 
  summarise(Swordfish=mean(predSwordfish,na.rm=T),
            BlueShark=mean(predBlueShark,na.rm=T),
            ThresherShark=mean(predCommonThresherShark,na.rm = T),
            ShortfinMakoShark=mean(predShortfinMakoShark,na.rm = T))
glimpse(master_average)

## rearrange the dataset and clean it up to make it easier to plot
master_rearrange=master_average %>% gather(Species,value,-c(month,Domain))
glimpse(master_rearrange) ## note all of the species are in one column now (called Species), and all of the predictions are in one column (called value)

master_clean=master_rearrange %>% 
  mutate(month=case_when(month==1~"January",
                         month==9~"September",
                         month==10~"October",
                         month==11~"November",
                         month==12~"December")) %>% 
  mutate(Species=case_when(Species=="Swordfish"~"Swordfish",
                           Species=="BlueShark"~"Blue shark",
                           Species=="ThresherShark"~"Common thresher shark",
                           Species=="ShortfinMakoShark"~"Shortfin mako shark")) %>% 
  mutate(group=glue("{Species}_{Domain}")) %>% 
  mutate(month=factor(month,levels = c("September","October","November","December","January")))

## make some plots!
## the data is now in a format that you can make a ton of different plots: line plots, bar plots, pie charts....

line_plot=ggplot(master_clean,aes(x=month,y=value,color=Species,group=group,linetype=Domain))+
  geom_line()+
  theme_classic()+
  xlab(NULL)+
  ylab("Habitat suitability")+
  ggtitle("Climatological monthly species habitat suitability in two Wind Energy Areas")
  
png(glue("{outDir}lineplot.png"),width=20,height=10,units='cm',res=400)
par(ps=10)
par(cex=1)
line_plot
dev.off()  
  
  
  
  
