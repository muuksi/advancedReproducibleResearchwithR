################
#plotting
###############
#create a plot with percentage dead patients per gender and per treatment (HEP/ASP yes,no)
#############
####
#create a data frame for plotting
library(tidyverse)
my_data$DDEAD
####
plot_data <- 
  #based on my_data
  my_data %>% 
  #create a new variable (T/F) whether a patient is dead from DDEAD variable
  #reason for this is: we cannot calculate means across "Y" and "N" values
  mutate(., isdead = if_else(DDEAD=="Y", 1, 0)) %>%

  #group the analysis by the variables of interest (SEX, Heparin or Aspirin Treatment)
  group_by(., SEX, HEP, ASP) %>%
  #Now summarise the percentage and SEM, calculate for the plot also mean plus/minus sem
  summarise(., N = n(),
            percentdead = mean(isdead, na.rm=T),
            sddead = mean(isdead, na.rm=T),
            sdplus = percentdead + sddead / sqrt(N),
            sdminus = percentdead - sddead / sqrt(N)) 
head(plot_data)
#now create a barplot with error bars
#how can you create a combined factor from ASP HEP?
#also create two separate facets for male and female
ggplot(data=plot_data, aes(y=percentdead, x = factor(paste(ASP,HEP)))) + 
  geom_bar(stat = "identity") + 
  geom_errorbar( ymin = plot_data$sdminus, ymax = plot_data$sdplus, na.rm = T) + 
  facet_wrap(~SEX)


#######
#often we need to visualize binary variables
#here we use an example
#death of patient after # days after randomisation in dependence of AGE  
#FDEADD vs AGE
#######
my_data$isdead<-ifelse(my_data$DEAD!=9,1,0)
ggplot(aes(y=isdead,x=AGE),data=my_data)+geom_point()+geom_smooth()
#this is not a nice representation!
#we need something better!
#see for example here: 
#https://doi.org/10.1890/0012-9623(2004)85[100:ANMOPT]2.0.CO;2
#also as pdf on osf
#first summarise the data in a histogram format
#Summarise data to create histogram counts
#what is the min and max age?
min(my_data$AGE)
max(my_data$AGE)

#looking at this I decide to start at 10 years until 100 years in steps of 1
hist_data = my_data %>% 
  #first add new variable that codes breaks
  mutate(breaks = findInterval(AGE, seq(10,100,1))) %>%
  #then group by dead/alive and the breaks
  group_by(isdead, breaks) %>%
  #count
  summarise(n = n()) %>%
  #if patients are dead, we want them to show on top with histogram on top so you need to 
  #calculate in this case the percentage as 1-percentage
  mutate(pct = ifelse(isdead==0, n/sum(n), 1 - n/sum(n)),breaks=seq(10,100,1)[breaks]) 

####
#now plot this
####
ggplot() + #this just sets an empty frame to build upon
  #first add a histopgram with geom_segment use the help of geom_segment
  geom_segment(data=hist_data, size=2, show.legend=FALSE,
               aes(x=breaks, xend=breaks, y=isdead, yend=pct, colour=factor(isdead)))+
  #then predict a logistic regression via stat_smooth and the glm method (we will cover the details in the next session)
  stat_smooth(data=my_data,aes(y=isdead,x=AGE),method="glm", method.args = list(family = "binomial"))+
  #some cosmetics 
  scale_y_continuous(limits=c(-0.02,1.02)) +
  scale_x_continuous(limits=c(15,101)) +
  theme_bw(base_size=12)+
  ylab("Patient Alive=0/Dead=1")+xlab("Age")

#####
#find here more interesting visualisations
#http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html
#####

