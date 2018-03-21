## Calculate risk scores
## 3/19/18

library(tidyverse)
library(scales)

user <- 'lennon'

if(user == 'lennon') { boxdir <- '/Users/lennonthomas/Box Sync/Waitt Institute/Blue Halo 2016/Carib_aqua_16/'}
if(user == 'tyler')  { boxdir <- '../../../Box Sync/Carib_aqua_16/'}



#read in data, rescale parameters from 1 to 5, and take average to calculate risk score

data<-read.csv(paste0(boxdir,"data/risk_parms.csv")) 

data_rescale<-data %>%
  mutate(reg_qual_rescale = rescale(reg_quality, to = c(5,1),na.rm = TRUE),
         pol_stab_rescale = rescale(pol_stab, to = c(5,1), na.rm = TRUE),
         con_cor_rescale = rescale(con_cor, to = c(5,1), na.rm =TRUE),
         gdp_rescale = rescale(gdp_capita, to = c(5,1), na.rm = TRUE),
         cpi_growth_rescale = rescale(cpi_growth,to = c(5,1),na.rm = TRUE)) %>%
  dplyr::select(Country,reg_qual_rescale,pol_stab_rescale,con_cor_rescale,gdp_rescale,cpi_growth_rescale) %>%
  gather(parameter,value,-Country) %>%
  mutate(risk_type = ifelse(parameter %in% c("reg_qual_rescale","con_cor_rescale","pol_stab_rescale"),"political","economic")) %>%
  spread(risk_type,value) %>% 
  group_by(Country) %>%
  summarise(political_score = mean(political,na.rm = TRUE),
            economic_score = mean(economic,na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(Country) %>%
  mutate(risk_score = mean (c(political_score, economic_score), na.rm = TRUE)) %>%
  arrange(risk_score)

data_rescale<-merge(data_rescale,data, by = "Country") %>%
  dplyr::select(c(1,2,3,4,6))

regression_data<-data_rescale[!is.na(data_rescale$political_score),]

est_missing_risk<-lm(regression_data$risk_score~regression_data$gdp)

intercept<-as.numeric(est_missing_risk$coefficients[1])

slope<-as.numeric(est_missing_risk$coefficients[2])

data_rescale<-data_rescale %>%
  mutate(predicted_risk = intercept + slope*gdp_capita,
         new_risk = round(ifelse(is.na(political_score),predicted_risk,risk_score),2),
        new_risk_rescale = rescale(new_risk, to = c(1,5))) 

risk_score<-data_rescale %>%
  dplyr::select(c(Country,new_risk_rescale)) %>%
  arrange(new_risk_rescale) 

colnames(risk_score)<-c("Territory1","risk_score")

write_csv(risk_score,paste0(boxdir,"final_risk_score2.csv"))  

  
