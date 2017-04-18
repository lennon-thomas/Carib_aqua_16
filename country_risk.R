library(dplyr)
library(tidyr)
library(Hmisc)
library(scales)

## Read in risk metrics
data<-read.csv("economic/risk/economic_parameters.csv")

## Create correlation matrix

parms<-data[,3:20]
matrix<-rcorr(as.matrix(parms), type="pearson") 
write.csv(matrix$r,"economic/risk/r_values.csv")
write.csv(matrix$P,"economic/risk/p_values.csv")


# Rescale values to 1 -5 range

data<-data %>%
         mutate(GDP_rescale = rescale(data$GDP.per.capita,to = c(1,5)),
           reg_q_rescale = rescale(data$Regulatory.Quality,to = c(1,5)),
           pol_sta_rescale = rescale(data$Political.Stability,to = c(1,5)),
           con_cor_rescale = rescale(data$Control.of.Corruption,to = c(1,5)),
           fdi_rescale = rescale(data$FDI,to = c(1,5)),
           cpi_rescale = rescale(data$CPI.Growth,to = c(1,5))) 

write.csv( data,"economic/risk/economic_parameters_rescale.csv")

# create new dataframe with rescaled data

re_data<-data[,c(1,21:26)]

re_data<-re_data %>%
  #gather(key=index,value=value,2:7) %>%
  arrange(Island.Country.EEZ) %>%
  group_by(Island.Country.EEZ) %>%
  summarize(political_score=((0.35*reg_q_rescale)+(0.35*pol_sta_rescale)+(0.3*con_cor_rescale)),
                             economic_score=((0.3*GDP_rescale)+(0.35*fdi_rescale)+(0.35*cpi_rescale))) %>%
  mutate(risk_score=(0.4*political_score)+(0.6*economic_score))



# save calculated risk scores

write.csv(re_data,'economic/risk/risk_scores.csv')

# check for risk score correlations

all<-cbind(re_data[,2:4],parms)%>%
  mutate(fgdp=FDI/GDP)
risk_corr<-rcorr(as.matrix(all), type="pearson") 
View(risk_corr$r)

fit<-lm(risk_score~GDP.per.capita+Population,data=all,na.action = na.omit)
summary(fit)




reg<-cbind(re_data$risk_score,all$GDP.per.capita,all$Population)
#reg<-reg[!is.na(reg[,1]),]
reg<-as.data.frame(reg)
colnames(reg)<-c("risk_score","gdp.capita","population")
reg$t_gdp.capita<-log(reg$gdp.capita)

options(na.action=na.exclude)
fit<-lm(risk_score~gdp.capita,data=reg,na.action = na.omit)
summary(fit)
fitted(fit)

attach(reg)

pred.frame<-data.frame(gdp.capita)
pred.values<-as.data.frame(predict(fit,int="p",newdata=pred.frame))
write.csv(pred.values,"economic/risk/predict_risk_scores.csv")

p<-ggplot(reg,aes(x=gdp.capita,y=c(risk_score)))+
  geom_point(shape=1)+
  xlab("GDP per capita (current US$)")+
  ylab("Risk score")+
  geom_smooth(method=lm)+
  geom_line(aes(x=pred.frame$gdp.capita,y=pred.values$fit),col="black")+
  geom_line(aes(x=pred.frame$gdp.capita,y=pred.values$lwr),lty="dotted")+
  geom_line(aes(x=pred.frame$gdp.capita,y=pred.values$upr),lty="dotted")+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 
plot(p)
