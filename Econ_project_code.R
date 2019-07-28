#Package and library setup
install.packages("sandwich")
install.packages("lmtest")
install.packages("psych")
install.packages("plm")
library(sandwich)
library(lmtest)
library(psych)
library(plm)

#Adding Logged versions of independent variables to dataframe.
complete_data <- read_csv("complete_data.csv")
d<-complete_data
d$LOG_TOT_25<- log(d$TOT_POP_25)
d$LOG_TOT_POP<- log(d$TOT_POP)
d$LOG_WHT<- log(d$WHT)
d$LOG_BLK<- log(d$BLK)
d$LOG_AI_ALSK<- log(d$AI_ALSK)
d$LOG_ASN<- log(d$ASN)
d$LOG_NH_PI<- log(d$NH_PI)
d$LOG_POV_AA<- log(d$POV_AA)
d$LOG_UN_EMP<- log(d$UN_EMP)
d$NINE_TO_12_NO_D<-d$`9_12_NO_D`

#Creating dummy variables for each state and year for the LSDV
d$DummyYear<- factor(d$Year)
d$DummyState<-factor(d$State)

#Auxiliary for multicollinearity
aux<-lm(d$LOG_TOT_POP~d$LOG_WHT+d$LOG_BLK+d$LOG_AI_ALSK+d$LOG_NH_PI+d$LOG_ASN+d$LOG_TOT_25+d$LOG_UN_EMP+d$LESS_9+d$NINE_TO_12_NO_D+d$HSC_EQU+d$SC_NO_D+d$ASSO+d$BACH+d$GRAD+d$DummyState+d$DummyYear)
summary(aux)

#LSDV panels that were run (Remove # to run summary)
#--------------------------------------------
PANEL_NO_UNEMP_NO_STATE<-lm(d$LOG_POV_AA~d$LOG_WHT+d$LOG_BLK+d$LOG_AI_ALSK+d$LOG_NH_PI+d$LOG_ASN+d$LOG_TOT_25+d$LESS_9+d$NINE_TO_12_NO_D+d$HSC_EQU+d$SC_NO_D+d$ASSO+d$BACH+d$GRAD+d$DummyYear)
#summary(PANEL_NO_UNEMP_NO_STATE)
PANEL_NO_UNEMP_NO_STATE_NO_YEAR<-lm(d$LOG_POV_AA~d$LOG_WHT+d$LOG_BLK+d$LOG_AI_ALSK+d$LOG_NH_PI+d$LOG_ASN+d$LOG_TOT_25+d$LESS_9+d$NINE_TO_12_NO_D+d$HSC_EQU+d$SC_NO_D+d$ASSO+d$BACH+d$GRAD)
#summary(PANEL_NO_UNEMP_NO_STATE_NO_YEAR)
PANEL_NO_UNEMP<-lm(d$LOG_POV_AA~d$LOG_WHT+d$LOG_BLK+d$LOG_AI_ALSK+d$LOG_NH_PI+d$LOG_ASN+d$LOG_TOT_25+d$LESS_9+d$NINE_TO_12_NO_D+d$HSC_EQU+d$SC_NO_D+d$ASSO+d$BACH+d$GRAD+d$DummyState+d$DummyYear)
#summary(PANEL_NO_UNEMP)
PANEL_UNREST<- lm(d$LOG_POV_AA~d$LOG_WHT+d$LOG_BLK+d$LOG_AI_ALSK+d$LOG_NH_PI+d$LOG_ASN+d$LOG_TOT_25+d$LOG_UN_EMP+d$LESS_9+d$NINE_TO_12_NO_D+d$HSC_EQU+d$SC_NO_D+d$ASSO+d$BACH+d$GRAD+d$DummyState+d$DummyYear)
#summary(PANEL_UNREST)
PANELOLS_NO_TOT_25<- lm(d$LOG_POV_AA~d$LOG_WHT+d$LOG_BLK+d$LOG_AI_ALSK+d$LOG_NH_PI+d$LOG_ASN+d$LOG_UN_EMP+d$LESS_9+d$NINE_TO_12_NO_D+d$HSC_EQU+d$SC_NO_D+d$ASSO+d$BACH+d$GRAD+d$DummyState+d$DummyYear)
#summary(PANELOLS_NO_TOT_25)
PANELOLS_NO_WHT<- lm(d$LOG_POV_AA~d$LOG_BLK+d$LOG_AI_ALSK+d$LOG_NH_PI+d$LOG_ASN+d$LOG_TOT_25+d$LOG_UN_EMP+d$LESS_9+d$NINE_TO_12_NO_D+d$HSC_EQU+d$SC_NO_D+d$ASSO+d$BACH+d$GRAD+d$DummyState+d$DummyYear)
#summary(PANELOLS_NO_WHT)
PANELOLS_Final<- lm(d$LOG_POV_AA~d$LOG_WHT+d$LOG_BLK+d$LOG_AI_ALSK+d$LOG_NH_PI+d$LOG_ASN+d$LOG_TOT_25+d$LOG_UN_EMP+d$LESS_9+d$NINE_TO_12_NO_D+d$HSC_EQU+d$SC_NO_D+d$ASSO+d$BACH+d$GRAD+d$DummyState+d$DummyYear)
#summary(PANELOLS_Final)
PANELOLS_Final_UN<-lm(d$LOG_POV_AA~d$LOG_UN_EMP)
#summary(PANELOLS_Final_UN)
PANELOLS_NO_WHT_YR<- lm(d$LOG_POV_AA~d$LOG_UN_EMP+d$LOG_BLK+d$LOG_AI_ALSK+d$LOG_NH_PI+d$LOG_ASN+d$LOG_TOT_25+d$LESS_9+d$NINE_TO_12_NO_D+d$HSC_EQU+d$SC_NO_D+d$ASSO+d$BACH+d$GRAD+d$DummyState)
#summary(PANELOLS_NO_WHT_YR)
PANELOLS_NO_WHT_YR_STATE<- lm(d$LOG_POV_AA~d$LOG_UN_EMP+d$LOG_BLK+d$LOG_AI_ALSK+d$LOG_NH_PI+d$LOG_ASN+d$LOG_TOT_25+d$LESS_9+d$NINE_TO_12_NO_D+d$HSC_EQU+d$SC_NO_D+d$ASSO+d$BACH+d$GRAD)
#summary(PANELOLS_NO_WHT_YR_STATE)
PANELOLS_NO_WHT_NO_TOT_25<- lm(d$LOG_POV_AA~d$LOG_BLK+d$LOG_AI_ALSK+d$LOG_NH_PI+d$LOG_ASN+d$LOG_UN_EMP+d$LESS_9+d$NINE_TO_12_NO_D+d$HSC_EQU+d$SC_NO_D+d$ASSO+d$BACH+d$GRAD+d$DummyState+d$DummyYear)
#summary(PANELOLS_NO_WHT_NO_TOT_25)
#-------------------------------------------------------

#summary of unrestriced LSDV model
summary(PANEL_UNREST)

#LSDV panel that was ultimately used
PANELOLS_Final_UN_PANEL<-lm(d$LOG_POV_AA~d$LOG_UN_EMP+d$DummyState+d$DummyYear)
summary(PANELOLS_Final_UN_PANEL)

#----------
#PLM models that were used to conduct F-test
#Restricted
PLM_Restricted<-plm(LOG_POV_AA~LOG_UN_EMP, data = d, index=c('State','Year'), model='within', effect = 'twoways')
summary(PLM_Restricted)

#corrected for multicollinearity and heteroskedasicity
PLM_Restricted_Corrected<-coeftest(PLM_Restricted, vcov=vcovHC(PLM_Restricted, type="HC1"))

#view restricted corrected model
PLM_Restricted_Corrected

#Unrestricted
PLM_Unrestricted<-plm(LOG_POV_AA~LOG_WHT+LOG_BLK+LOG_AI_ALSK+LOG_NH_PI+LOG_ASN+LOG_TOT_25+LOG_UN_EMP+LESS_9+NINE_TO_12_NO_D+HSC_EQU+SC_NO_D+ASSO+BACH+GRAD, data = d, index=c('State','Year'), model='within', effect = 'twoways')
summary(PLM_Unrestricted)

#corrected for multicollinearity and heteroskedasicity
PLM_Unrestricted_Corrected<-coeftest(PLM_Unrestricted, vcov=vcovHC(PLM_Unrestricted, type="HC1"))

#view unrestricted corrected model
PLM_Unrestricted_Corrected

#-----------
#F-Test using Adjusted R squared from PLM_Restricted and PLM_Unrestricted
((.11461 - .077906)/13)/((1-.11461)/336)
# 1.071457
qf(.95, df1=13, df2=336)
# 1.74936

#endogeneity testing
Endo_Test1<-lm(PANELOLS_Final_UN_PANEL$residuals~d$LOG_POV_AA)
Endo_Test2<-lm(PANELOLS_Final_UN_PANEL$residuals~PANELOLS_Final_UN_PANEL$fitted.values)
summary(Endo_Test1)#no correlation
summary(Endo_Test2)#no correlation

#plotting to test for endogeneity
plot(PANELOLS_Final_UN_PANEL$residuals~PANELOLS_Final_UN_PANEL$fitted.values, xlab='LSDV Panel Fitted Values', ylab='LSDV Panel Residuals', main='Endogeneity Test 1')#no pattern
plot(PANELOLS_Final_UN_PANEL$residuals~d$LOG_POV_AA, xlab='Logged Poverty', ylab='LSDV Panel Residuals', main='Endogeneity Test 2') #no pattern

#creating column with fitted values of the uncorrected OLS since fitted values is not a column that is available with the results of coeftest
d$final_fitted<-PANELOLS_Final_UN_PANEL$fitted.values

#plotting relationship between fitted log of poverty and the log of unemployment which was the only statistically significant variable
#---------------------------------------------------------
plot(d$LOG_UN_EMP[d$State == 'Texas'],d$final_fitted[d$State == 'Texas'], xlab='Logged Total Population Unemployed',main='Texas' ,ylab='Fitted Values of Poverty All Ages')
text( d$LOG_UN_EMP[d$State == 'Texas'],d$final_fitted[d$State == 'Texas'], labels=d$Year[d$State == 'Texas'])

plot(d$LOG_UN_EMP[d$State == 'Georgia'],d$final_fitted[d$State == 'Georgia'], xlab='Logged Total Population Unemployed',main='Georgia' ,ylab='Fitted Values of Poverty All Ages')
text( d$LOG_UN_EMP[d$State == 'Georgia'],d$final_fitted[d$State == 'Georgia'], labels=d$Year[d$State == 'Georgia'])

plot(d$LOG_UN_EMP[d$State == 'Florida'],d$final_fitted[d$State == 'Florida'], xlab='Logged Total Population Unemployed',main='Florida' ,ylab='Fitted Values of Poverty All Ages')
text( d$LOG_UN_EMP[d$State == 'Florida'],d$final_fitted[d$State == 'Florida'], labels=d$Year[d$State == 'Florida'])

plot(d$LOG_UN_EMP[d$State == 'Ohio'],d$final_fitted[d$State == 'Ohio'], xlab='Logged Total Population Unemployed',main='Ohio' ,ylab='Fitted Values of Poverty All Ages')
text( d$LOG_UN_EMP[d$State == 'Ohio'],d$final_fitted[d$State == 'Ohio'], labels=d$Year[d$State == 'Ohio'])

plot(d$LOG_UN_EMP[d$State == 'California'],d$final_fitted[d$State == 'California'], xlab='Logged Total Population Unemployed California',main='California' ,ylab='Fitted Values of Poverty All Ages')
text( d$LOG_UN_EMP[d$State == 'California'],d$final_fitted[d$State == 'California'], labels=d$Year[d$State == 'California'])

plot(d$LOG_UN_EMP[d$State == 'Maine'], d$final_fitted[d$State == 'Maine'], xlab='Logged Total Population Unemployed',main='Maine' ,ylab='Fitted Values of Poverty All Ages')
text(d$LOG_UN_EMP[d$State == 'Maine'], d$final_fitted[d$State == 'Maine'], labels=d$Year[d$State == 'Maine'])

plot(d$LOG_UN_EMP[d$State == 'District of Columbia'], d$final_fitted[d$State == 'District of Columbia'], xlab='Logged Total Population Unemployed',main='District of Columbia' ,ylab='Fitted Values of Poverty All Ages')
text(d$LOG_UN_EMP[d$State == 'District of Columbia'], d$final_fitted[d$State == 'District of Columbia'], labels=d$Year[d$State == 'District of Columbia'])
#---------------------------------------------------

#creating final models
Final_Model_Corrected_PLM<-coeftest(PLM_Restricted, vcov=vcovHC(PLM_Restricted, type="HC1"))
Final_Model_Corrected_LSDV<-coeftest(PANELOLS_Final_UN_PANEL, vcov=vcovHC(PANELOLS_Final_UN_PANEL, type="HC1"))

#displaying results
Final_Model_Corrected_LSDV
Final_Model_Corrected_PLM
