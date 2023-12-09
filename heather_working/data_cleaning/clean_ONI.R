### clean oni

library(tidyverse)
library(glue)
dat=read.csv("/Users/heatherwelch/Dropbox/EFH/ONI/raw_oni.csv")
positions=grep("Year",dat$Year)

empty=list()

for (i in 0:length(positions)){
  start=positions[i]
  end=positions[i+1]-1
  
  if (i == 0){
    start=1
    # end=positions[i]
  }
  
  if (i == 7){
    # start=1
    end=nrow(dat)
  }
  
  print(glue("start={start}"))
  print(glue("end={end}"))
  
  if (i == 0){
    dat2=dat[(start):end,]
  }else {
  dat2=dat[(start+1):end,]
  }

  
  dat3=dat2 %>% gather(season, anomaly,-Year) %>% 
    mutate(season=factor(season,levels=c("DJF","JFM","FMA","MAM","AMJ","MJJ","JJA","JAS","ASO","SON","OND","NDJ"))) %>% 
    arrange(Year,season) 
  
  empty[[length(empty)+1]]=dat3
}

master=do.call("rbind",empty)
write.csv(master,"/Users/heatherwelch/Dropbox/EFH/ONI/cleaned_oni.csv")
