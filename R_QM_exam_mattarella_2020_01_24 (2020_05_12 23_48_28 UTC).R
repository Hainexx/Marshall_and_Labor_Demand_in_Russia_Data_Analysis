########################################################################################################################
# Project: "R_QM_Exam"                                                                                    #
# Description: In this script we are going to to estimate the Russian labour demand function ln = f(lw, ly) for the years 1996 and 1997 and assess which model fits our data best. 
#            The first part is dedicated to the preliminary analysis of each variable. Here we will check for the presence of outliers, analyse the distributions of the variables
#            and run the normality test. We will then move on, taking into consideration just the data available for the year 1997 
#            looking at the OLS regression of the labour demand function in that period.
#            In order to better understand the them we will run on the regression model the tests for heteroskedasticity, normality, and the RESET test.
#            At the end of the first part, some comparison will be made between different models in order to find the best one for our data.
#            We will start by looking at the results obtained from the OLS regression and the robust regression. Then, we will look at the differences 
#            between the robust OLS regression and the IV regression. Lastly, moving from a cross-sectional approach to a panel data setting, we will look at
#            the differences between the polled OLS regression and the fixed effect regression.
#            The main object of this script is to reproduce an analysis of the Austrian country
#            using the data from the WEO database on National outcomes figures. 
#            We're going to analyze mainly the data on NGDP_R, the misures of Inflation, PCPI
#            and NGDP_D, and finally the data on unemployment. We're going to initially visualize the time series 
#            of both log levels and growth percentage. Furthermore we're going to check for autocorrelation and for 
#            Stationarity through the Augmented Dickey-Fuller test and finally try to estimate a "Phillip Curve".
#
#
# File_name: "R_QM_exam_mattarella_2020_01_24.R"                                                                              #
# Filed by: Gaspare Mattarella                                                                                           #
# First filed on: 2019/12/23                                                                                             #
#                                                                                                                        #
# input: "dati_esame.dta"                                                                                                #
# temp files: none                                                                                                       #
# output: none                                                                                         #
#########################################################################################################################

# clear memory
rm(list=ls())

# setwd
setwd('C:\\Users\\Gaspa\\OneDrive\\Desktop\\R_exam_GM_2019_10_20\\mattarella')

# Library
library(AER) 
library(systemfit)
library(moments) # kurtosis and skewness calculations
library(stargazer) # table format for regression results
library(MASS)
library(lmtest) # required for coefci and coeftest
library(sandwich) # required for heteroskedastic robust standard errors calculation
library(plm)
library(gridExtra) # arrange graphs
library(olsrr) # Breusch-Pagan Test
library(fBasics) # dagoTest (approximates sktest)
library(ivpack)
library(ggplot2)
library(haven) 
library(dplyr)
library(ggpubr)
library(car)
library(olsrr)
library(hrbrthemes)
library(viridis)
library(dplyr)

# read data
data <- read_dta('in_data/dati_esame.dta')

col.names <- c("id", "year")

data[,col.names] <- lapply(data[,col.names], as.factor)

# drop observations as requested 
# id >= 11	& id < 20
data_exam_panel <- data %>%
  dplyr::filter(
    !dplyr::between(id, 155, 163)
  )


# make sure is intended as a Panel Data 
data_exam_panel <- pdata.frame(data_exam_panel, 
                               index = c("id", "year"))

# relation between: ln = f(lw, ly)  [labor demand function]
# a brief summary
summary(data_exam_panel, type = "text")

#calculate SD
sd(data_exam_panel$ln)
sd(data_exam_panel$lw)
sd(data_exam_panel$ly)

# calculate trimmed means
mean(data_exam_panel$ln, trim = 0.10)
mean(data_exam_panel$lw, trim = 0.10)
mean(data_exam_panel$ly, trim = 0.10)

# build my own function to count mild outliers
find_mild_OL <- function(x) {
  lowerq = quantile(x)[2]
  upperq = quantile(x)[4]
  iqr = IQR(x) 
  mild.threshold.upper = (iqr * 1.5) + upperq
  mild.threshold.lower = lowerq - (iqr * 1.5)
  print(results_mild <- length(which(x > mild.threshold.upper | x < mild.threshold.lower)))
}

# run the function to identify n outliers
find_mild_OL(data_exam_panel$ln)
find_mild_OL(data_exam_panel$lw)
find_mild_OL(data_exam_panel$ly)

# build a function to count severe outliers
find_severe_OL <- function(x) {
  lowerq = quantile(x)[2]
  upperq = quantile(x)[4]
  iqr = IQR(x) 
  extreme.threshold.upper = (iqr * 3) + upperq
  extreme.threshold.lower = lowerq - (iqr * 3)
  print(results_severe <- length(which(x > extreme.threshold.upper | x < extreme.threshold.lower)))
}

# use the function to identify outliers
find_severe_OL(data_exam_panel$ln)
find_severe_OL(data_exam_panel$lw)
find_severe_OL(data_exam_panel$ly)
              

#calculate normality prob
jarqueberaTest(data_exam_panel$ln)
jarqueberaTest(data_exam_panel$lw)
jarqueberaTest(data_exam_panel$ly)


# draw a qq map
ggqqplot(data_exam_panel$ln,title = "Normality qq for ln")
ggqqplot(data_exam_panel$lw, title = "Normality qq for lw")
ggqqplot(data_exam_panel$ly,title = "Normality qq for ly")

ggplot(data = data_exam_panel, aes(x = ln)) +
  geom_histogram(aes(y =..density..), fill="#69b3a2", color="#e9ecef", alpha=0.7, binwidth = 0.7) +
  stat_function(fun = dnorm, args = list(mean = mean(data_exam_panel$ln), sd = sd(data_exam_panel$ln)))+
  labs(x = "ln: Logarithm of the number of employees by firm", y ='Density') +
  theme_minimal()

ggplot(data = data_exam_panel, aes(x = lw)) +
  geom_histogram(aes(y =..density..),color="#e9ecef", fill = '#404080', alpha=0.7, binwidth = 0.3) +
  stat_function(fun = dnorm, color = 'black', args = list(mean = mean(data_exam_panel$lw), sd = sd(data_exam_panel$lw)))+
  labs(x = "lw: Logarithm of wage bill by firm / number of employees by firm", y ='Density') +
  theme_minimal()

ggplot(data = data_exam_panel, aes(x = ly)) +
  geom_histogram(aes(y =..density..),color="white", fill = 'thistle2', alpha=0.8, binwidth = 0.9) +
  stat_function(fun = dnorm, color = 'black', args = list(mean = mean(data_exam_panel$ly), sd = sd(data_exam_panel$ly)))+
  labs(x = "ly:  Logarithm of output by firm", y ='Density') +
  theme_minimal()

#  linear model 
MarshLab1997<- subset(data_exam_panel, year == "1997")

# linear regression via lm ()
lb_lm_1 <- lm(ln ~ ly + lw, data = MarshLab1997)
stargazer(lb_lm_1, type = 'text')

# Save residuals
residuals_lm_1 <- lb_lm_1$residuals
MarshLab1997$residuals <- residuals(lb_lm_1)
MarshLab1997$predicted <- predict(lb_lm_1)

# Breusch-Pagan Test for heteroskedasticity - H0 for constant variance is rejected!!
ols_test_breusch_pagan(lb_lm_1)

# Ramsey reset test - H0 no omitted variables rejected!!!
resettest(lb_lm_1)

# Normality test - H0 for normality rejected!!!
shapiroTest(residuals_lm_1)

#plot the residuals
ggqqplot(residuals_lm_1) +
  labs(title = "Distribution Plot of the Residuals", x = "Distribution")

#
ggplot(data = lb_lm_1) +
  geom_point(mapping = aes(x = lb_lm_1$fitted.values, y = residuals_lm_1)) +
  geom_abline(mapping = aes(slope = 0, intercept = 0), color= 'darkred') +
  theme(panel.background = element_rect(fill = NA, colour = NA)) +
  labs(title = "Residuals Plot", x = "Fitted Values", y = "Residuals")


# Calculate HC Robust Linear Regression
OLS_HC <- lb_lm_1
coeftest(OLS_HC, vcov = vcovHC(OLS_HC, type = "HC1" ))

# robust reg model (rlm)
rlm_mod <- rlm(ln ~ ly + lw, data = MarshLab1997)  
summary(rlm_mod)



# INSTRUMENTAL VARIABLES (IV) REGRESSION

MarshLab1996<- subset(data_exam_panel, year == "1996") # divide the subsets

# perform TSLS using 'ivreg()'

Mrsh_ivreg_1 <- ivreg(MarshLab1997$ln ~ MarshLab1997$ly + MarshLab1997$lw | 
                     MarshLab1996$lw + MarshLab1996$ly)

Mrsh_ivreg_2 <- ivreg(MarshLab1997$ln ~ MarshLab1997$ly + MarshLab1997$lw | 
                        MarshLab1996$lw + MarshLab1996$ly + MarshLab1996$ln)


# robust coefficient summary
summary(Mrsh_ivreg_1, vcov = sandwich, diagnostics = TRUE)
summary(Mrsh_ivreg_2, vcov = sandwich, diagnostics = TRUE) # Mrsh_ivreg_2 fails to pass the Sargan test for overidentifying restrictions. 


# For Mrsh_ivreg_1, we can reject the H0 of the Wu-Hausman test for endogeneity so The IV is better suited than OLS.
# Mrsh_ivreg_2, instead, fails to pass the Sargan test for overidentifying restrictions, 
# that means that we can not add ln1996 as an instrument in Mrsh_ivreg_2 and

# F  stat to see if the IV are valid

# perform the first stage regression to obtain Mrsh_ivreg_1
ivmod_1sls_1 <- lm(MarshLab1997$ly ~ MarshLab1996$ly + MarshLab1996$lw)
ivmod_1sls_2 <- lm(MarshLab1997$lw ~ MarshLab1996$ly + MarshLab1996$lw)

# check instrument relevance for model (1)
linearHypothesis(ivmod_1sls_1, 
                 c("MarshLab1996$lw = 0",  "MarshLab1996$ly = 0"),
                 vcov = vcovHC, type = "HC1")                     # the instruments are valid


# check instrument relevance for model (1)
linearHypothesis(ivmod_1sls_2, 
                 c("MarshLab1996$lw = 0",  "MarshLab1996$ly = 0"), # The instruments are valid
                 vcov = vcovHC, type = "HC1")

# Obtain the same IV regression in "two stages"

# once obtaiend the first stage, store the predicted values and residuals 
iv_pred <- ivmod_1sls_1$fitted.values
iv_pred_d2 <- ivmod_1sls_2$fitted.values

iv_res <- ivmod_1sls_1$residuals
iv_res_d2 <- ivmod_1sls_2$residuals


# run the Second Stage Regression
iv_mod_2sls <- lm(MarshLab1997$ln ~ iv_pred + iv_pred_d2)


coeftest(iv_mod_2sls, vcov. = vcovHC, type = 'HC1')


# gather robust standard errors in a list
rob_se_iv <- list(sqrt(diag(vcovHC(Mrsh_ivreg_1, type = "HC1")))
               )

# generate table 
stargazer(Mrsh_ivreg_1,lb_lm_1,
          header = FALSE, 
          type = "text",
          omit.table.layout = "n",
          digits = 3, 
          column.labels = c("IV:ivreg", 'OLS'),
          dep.var.labels.include = FALSE,
          se = rob_se_iv)

# Manual Hausmann test
# we reuse the first stage model estimated previously mod_2 and estimate the 
# second stage model including the residuals
Hausman_reg <- lm(MarshLab1997$ln ~ MarshLab1997$lw + MarshLab1997$ly + iv_res + iv_res_d2)
print(summary(Hausman_reg))

# The F Test gives the same result as the Wu-Hausmann from ivreg diagnostics:
# H0 rejects exogeneity of lw and ly, so IV is suited better than OLS
waldtest(Hausman_reg,.~.-iv_res - iv_res_d2)

# The Ho that the isntruments are irrelevant is rejected!
waldtest(ivmod_1sls_1,.~.-MarshLab1996$lw - MarshLab1996$ly)
waldtest(ivmod_1sls_2,.~.-MarshLab1996$lw - MarshLab1996$ly)







# PANEL APPROACH
#
#

# Panel Regression with Fixed Effects 
marsh_id_lm_mod <- lm(ln ~ ly + lw + id -1, data = data_exam_panel)
marsh_id_lm_mod


# estimate the same fixed effects regression but with plm()
plm_id_fix <- plm(ln ~ ly + lw, 
                   data = data_exam_panel,
                   index = c("id", "year"),
                   model = 'within')

# print summary using robust standard errors
coeftest(marsh_id_lm_mod, vcov. = vcovHC, type = "HC1")
coeftest(plm_id_fix, vcov. = vcovHC, type = "HC1")
#summary(marsh_id_lm_mod, diagnostic = TRUE)   #diagnostic to obtain data on R2, F-Stat, etc..



# Estimate a combined time and entity fixed effects regression model
# via lm()
marsh_tefe_lm_mod <- lm(ln ~ ly + lw + id + year - 1, data = data_exam_panel)
marsh_tefe_lm_mod

# same via plm()
plm_fixed <- plm(ln ~ ly + lw, 
                      data = data_exam_panel,
                      index = c("id", "year"), 
                      model = "within",
                      effect = 'twoways')


# PLM_fixed_2 is another way to obtain the same coefficients of the Fixed Effect but it shows every id coef.
# plm_fixed_2 <- plm(ln ~ ly + lw + id + year - 1, 
#                   data = data_exam_panel,
#                   index = c("id", "year"),
#                   model = 'pooling')       ## it may take few seconds to elaborate
#
# summary(marsh_tefe_lm_mod, diagnostic = TRUE)  ##diagnostic to obtain data on R2, F-Stat, etc. 
# Run HAC Robust standard errors
coef_test(plm_pool, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
coef_test(plm_id_fix, vcov = "CR2", cluster = "individual", test = "Satterthwaite")
coef_test(plm_fixed,vcov = "CR1", cluster = "individual", test = "naive-t")

# coeftest(plm_fixed_2, vcov = vcovHC, type = "HC1")  #it may take few seconds to elaborate


# POOLING EFFECTS
#
# estimate the pooled effects regression with plm()
plm_pool <- plm(ln ~ ly + lw, 
                   data = data_exam_panel,
                   index = c("id", "year"),
                   model = 'pooling')
                   
summary(plm_pool, diagnostics = TRUE)

# robust standard errors
rob_se_pan <- list(sqrt(diag(vcovHC(plm_fixed, type = "HC1"))),
                   sqrt(diag(vcovHC(plm_pool, type = "HC1"))),
                   sqrt(diag(vcovHC(plm_id_fix, type = "HC1")))
                   )


# for the correct Diagnostics, see for "PLm_fixed_2" and "marsh_id_lm_mod": 
# they are not here used for graphical reasons, because they show every single id coeffcient.
# Stargazer the table.
stargazer(plm_fixed,plm_pool,plm_id_fix,      
          header = FALSE, 
          type = "text",
          omit.table.layout = "n",
          digits = 3, 
          column.labels = c("Fixed", 'id Fixed', 'Pooled'),
          dep.var.labels.include = FALSE,
          se = rob_se_pan)


# Hausman test for consistency:
phtest(plm_fixed, plm_pool)      #results: plm_pool is inconsistent 
phtest(plm_fixed_2, plm_pool)    #results: plm_pool is inconsistent 
phtest(plm_id_fix, plm_pool)     #results: plm_pool is inconsistent 


# F- test for individual effects
pFtest(plm_fixed, plm_pool) # twoways effective
pFtest(plm_id_fix, plm_pool) # inidvidual effective
pFtest(plm_fixed_2, plm_pool) #individual effective



#### PART 2 - TIME SERIES
########################################################################################################################

# clear memory
rm(list=ls())

# mount the Library
library(tidyverse)
library(WDI)
library(stargazer)
library(readr)
library(AER)
library(dynlm)
library(forecast)
library(readxl)
library(stargazer)
library(scales)
library(quantmod)
library(urca)
library(reshape)
library(reshape2)
library(tseries)

# mount the data
WEOApr2019all <- read_csv("in_data/WEOApr2019all.csv")

# set missing values as "NA", as required by R
WEOApr2019all[WEOApr2019all == ""] <- NA
WEOApr2019all[WEOApr2019all == "n/a"] <- NA 

# filter for "Austria"
WEOApr2019all <- filter(
  WEOApr2019all, 
  Country == 'Austria')

# filter for the needed variables
WEOApr2019all <- filter(WEOApr2019all, WEOApr2019all$`WEO Subject Code` == "NGDP_R" |
                          WEOApr2019all$`WEO Subject Code` == "NGDP_D"|
                          WEOApr2019all$`WEO Subject Code` == "LUR"|
                          WEOApr2019all$`WEO Subject Code` == "PCPI")

# To simplify merging dataframes, rename the ISO 3 letter country code "ccISO3".
WEOApr2019all <- plyr::rename(WEOApr2019all, c(ISO = "ccISO3", `WEO Subject Code` = "var", `Subject Descriptor` = "label"))

# We need to reshape dataset to give it a "panel" structure
# Reshape using "melt"
WEOApr2019all <- melt(WEOApr2019all, id=c("Country","ccISO3","var","WEO Country Code","Subject Notes","label","Scale","Units","Country/Series-specific Notes","Estimates Start After"))

# Call year the time value 
WEOApr2019all <- rename(WEOApr2019all, c(variable = "year"))

# Reshape again, using "cast"
WEO <- cast(WEOApr2019all, Country+year~var)

# a "technical" issue. We want the time variable to be an integer. Also, we take out the "x" at the beginning of string
# The following does the trick
WEO$year <- as.ts(substr(WEO$year, start = 1, stop = 4))
WEO$year <- as.xts(substr(WEO$year, start = 1, stop = 4))

# A "technical" issue. We want numbers to be numbers (not "factors"). The following does the trick
WEO$LUR <- as.numeric(as.character(WEO$LUR))
WEO$NGDP_D <- as.numeric(as.character(WEO$NGDP_D))
WEO$NGDP_R <- as.numeric(as.character(WEO$NGDP_R))
WEO$PCPI <- as.numeric(as.character(WEO$PCPI))

# format date column
WEO$year <- as.yearmon(WEO$year)

# GDP series as xts object
Gdp_lev <- xts(WEO$NGDP_R, WEO$year)

# GDP growth series as xts object
Growth_Gdp <- (100 * log(Gdp_lev/lag(Gdp_lev)))

# NGDP_D series as xts object
Deflat_lev <- xts(WEO$NGDP_D, WEO$year)

# NGDP_D growth series as xts object
deflat_Grow <- xts(100 * log(Deflat_lev/lag(Deflat_lev)))

# PCPI series as xts object
infl_lev <- xts(WEO$PCPI, WEO$year)

# PCPI growth series as xts object
infl_Growth <- xts(100 * log(infl_lev/lag(infl_lev)))

# LUR series as xts object
unemp_lev <- xts(WEO$LUR, WEO$year)

# LUR growth series as xts object
unemp_Growth <- xts(100 * log(unemp_lev/lag(unemp_lev)))

# Plot log level of Austrian GDP
plot(log(as.zoo(Gdp_lev)),
     col = "steelblue",
     lwd = 2,
     ylab = "Log",
     xlab = "Date",
     main = "Austrian Yearly Real GDP log")

# plot Growth rate of Austrian GDP
plot(as.zoo(Growth_Gdp),
     col = "steelblue",
     lwd = 2,
     ylab = "%",
     xlab = "Date",
     main = "Austria Real GDP Growth Rates")

# GDP Deflator log
plot(log(as.zoo(Deflat_lev)),
     col = "steelblue",
     lwd = 2,
     ylab = "Log",
     xlab = "Date",
     main = "Austrian Yearly GDP Deflator log")

# GDP Deflator Rates
plot(as.zoo(deflat_Grow),
     col = "steelblue",
     lwd = 2,
     ylab = "%",
     xlab = "Date",
     main = "Austrian GDP Deflator Growth Rates")

# PCPI log levels
plot(log(as.zoo(infl_lev)),
     col = "steelblue",
     lwd = 2,
     ylab = "Log",
     xlab = "Date",
     main = "Austrian PCPI Log")

#PCPI Growth rate
plot(as.zoo(infl_Growth),
     col = "steelblue",
     lwd = 2,
     ylab = "%",
     xlab = "Date",
     main = "Austrian PCPI Growth rate")

# LUR log Levels
plot(as.zoo(unemp_lev),
     col = "steelblue",
     lwd = 2,
     ylab = "Rate",
     xlab = "Date",
     main = "Austrian Unemployment Rate")

# LUR Growth Rate
plot(as.zoo(unemp_Growth),
     col = "steelblue",
     lwd = 2,
     ylab = "%",
     xlab = "Date",
     main = "Austrian Unemployment Growth rate")


# Compare the 2 misures of Inflation
plot(merge(as.zoo(infl_lev), as.zoo(Deflat_lev)), 
     plot.type = "single", 
     col = c("darkred", "darkgreen"),
     lwd = 2,
     xlab = "Date",
     ylab = "Log",
     main = "Consume Price Index and GDP Deflator Log Comparison",
     legend = c("PCPI", "Deflat"))


legend("bottomright",
       legend = c("PCPI", "NGDP_D"), 
       col = c("darkred","darkgreen"), 
       pch = 16)

# Compare the 2 misures of Inflation growth rates
plot(merge(as.zoo(infl_Growth), as.zoo(deflat_Grow)), 
     plot.type = "single", 
     col = c("steelblue", "orange"),
     lwd = 2,
     xlab = "Date",
     ylab = "%",
     main = "Log Price Grotw Rate")

legend("topright",
       legend = c("PCPI", "NGDP_D"), 
       col = c("steelblue","orange"), 
       pch = 16)

# compute sample autocorrelation 
acf(na.omit(log(Gdp_lev)), plot = F)

# plot sample autocorrelation 
acf(na.omit(log(Gdp_lev)), main = "Autocorrelation for GDP")

# compute sample autocorrelation 
acf(na.omit(Growth_Gdp), plot = F)

# plot sample autocorrelation
acf(na.omit(Growth_Gdp), main = "Autocorrelation for GDP Growth rate")

# divide plotting area into 2x2 matrix
par(mfrow = c(2, 2))

# compute sample autocorrelation 
acf(na.omit(log(infl_lev)), plot = F)

# plot sample autocorrelation 
acf(na.omit(log(infl_lev)), main = "Autocorrelation for PCPI")

# compute sample autocorrelation
acf(na.omit(infl_Growth), plot = F)

# plot sample autocorrelation 
acf(na.omit(infl_Growth), main = "Autocorrelation for PCPI growth rate")

# compute sample autocorrelation 
acf(na.omit(log(Deflat_lev)), plot = F)

# plot sample autocorrelation 
acf(na.omit(log(Deflat_lev)), main = "Autocorrelation for GDP Deflator")

# compute sample autocorrelation
acf(na.omit(deflat_Grow), plot = F)

# plot sample autocorrelation
acf(na.omit(deflat_Grow), main = "Autocorrelation for Deflator Growth rate")

# return to 1
par(mfrow = c(1, 1))

# compute sample autocorrelation
acf(na.omit(unemp_lev), plot = F)

# plot sample autocorrelation
acf(na.omit(unemp_lev), main = "Autocorrelation for Unemployment")

# compute sample autocorrelation 
acf(na.omit(unemp_Growth), plot = F)

# plot sample autocorrelation
acf(na.omit(unemp_Growth), main = "Autocorrelation for Unemployment Growth rate")



#log lev
log_lev_gdp <- log(Gdp_lev)
# First Differences of log NGDP_R
lndf_Gdp <- diff(log_lev_gdp)
#logarthim differences of NGDP_R
lndf_Gdp <- log(Gdp_lev/lag(Gdp_lev))


# subset data
GDPGRSub <- lndf_Gdp

# estimate the model of The first differences of the log for NGDP_R
mod_1 <- ar.ols(na.omit(GDPGRSub), 
                order.max = 1, 
                demean = F, 
                intercept = T)

mod_1

# length of data set
N <-length(GDPGRSub)

GDPGR_level <- as.numeric(GDPGRSub[-1])
GDPGR_lags <- as.numeric(GDPGRSub[-N])

# estimate the model and for the same results of mod_1
GDPGR_AR1 <- lm(GDPGR_level ~ GDPGR_lags)

# robust summary
coeftest(GDPGR_AR1, vcov. = vcovHC, type = "HC1")

# estimate an AR(2) and AR(3) model
GDPGR_AR2 <- dynlm(ts(GDPGR_level) ~ Lag(ts(GDPGR_level)) + Lag(ts(GDPGR_level), 2))
GDPGR_AR3 <- dynlm(ts(GDPGR_level) ~ Lag(ts(GDPGR_level)) + Lag(ts(GDPGR_level), 2) + Lag(ts(GDPGR_level), 3))

coeftest(GDPGR_AR2, vcov. = sandwich)
coeftest(GDPGR_AR3, vcov. = sandwich)


# build a function that gives us the BIC/AIC values and also some more information
BICme <- function(model) {
  
  ssr <- sum(model$residuals^2)
  t <- length(model$residuals)
  npar <- length(model$coef)
  
  return(
    round(c("p" = npar - 1,
            "BIC" = BIC(model),
            "R2" = summary(model)$r.squared), 4)
  )
}
# ""
AICme <- function(model) {
  
  ssr <- sum(model$residuals^2)
  t <- length(model$residuals)
  npar <- length(model$coef)
  
  return(
    round(c("p" = npar - 1,
            "AIC" = AIC(model),
            "R2" = summary(model)$r.squared), 4)
  )
}

# loop BIC over models of different orders
order <- 1:21

# try for GDP GROWTH
BICs <- sapply(order, function(x) 
  "AR" = BICme(dynlm(ts(GDPGRSub) ~ L(ts(GDPGRSub), 1:x))))

AICs <- sapply(order, function(x) 
  "AR" = AICme(dynlm(ts(GDPGRSub) ~ L(ts(GDPGRSub), 1:x))))

# select the AR model with the smallest BIC
BICs[, which.min(BICs[2, ])] # 1
AICs[, which.min(BICs[2, ])] # 1


# for GDP LOG LEVELS
BICs <- sapply(order, function(x) 
  "AR" = BICme(dynlm(ts(log(Gdp_lev)) ~ L(ts(log(Gdp_lev)), 1:x))))

AICs <- sapply(order, function(x) 
  "AR" = AICme(dynlm(ts(log(Gdp_lev)) ~ L(ts(log(Gdp_lev)), 1:x))))

# select the AR model with the smallest BIC
BICs[, which.min(BICs[2, ])] # 1
AICs[, which.min(BICs[2, ])] # 1


# for PCPI LOG LEVELS # 2LaGS
BICs <- sapply(order, function(x) 
  "AR" = BICme(dynlm(ts(log(infl_lev)) ~ L(ts(log(infl_lev)), 1:x))))

AICs <- sapply(order, function(x) 
  "AR" = AICme(dynlm(ts(log(infl_lev)) ~ L(ts(log(infl_lev)), 1:x))))

# select the AR model with the smallest BIC
BICs[, which.min(BICs[2, ])] # 2
AICs[, which.min(BICs[2, ])] # 2

#First differences of PCPI
diff_Infl <- (infl_lev/lag(infl_lev))

# for PCPI fisrt diff Log LEVELS
BICs <- sapply(order, function(x) 
  "AR" = BICme(dynlm(ts(log(diff_Infl)) ~ L(ts(log(diff_Infl)), 1:x))))

AICs <- sapply(order, function(x) 
  "AR" = AICme(dynlm(ts(log(diff_Infl)) ~ L(ts(log(diff_Infl)), 1:x))))

# select the AR model with the smallest BIC
BICs[, which.min(BICs[2, ])] # 1
AICs[, which.min(BICs[2, ])]


# for NGDP_D LOG LEVELS
BICs <- sapply(order, function(x) 
  "AR" = BICme(dynlm(ts(log(Deflat_lev)) ~ L(ts(log(Deflat_lev)), 1:x))))

AICs <- sapply(order, function(x) 
  "AR" = AICme(dynlm(ts(log(Deflat_lev)) ~ L(ts(log(Deflat_lev)), 1:x))))

# select the AR model with the smallest BIC
BICs[, which.min(BICs[2, ])] # 2
AICs[, which.min(BICs[2, ])]


#First differences of NGDP_D
diff_defl <- (Deflat_lev/lag(Deflat_lev))

# for NGDP_D fisrt diff Log LEVELS
BICs <- sapply(order, function(x) 
  "AR" = BICme(dynlm(ts(log(diff_defl)) ~ L(ts(log(diff_defl)), 1:x))))

AICs <- sapply(order, function(x) 
  "AR" = AICme(dynlm(ts(log(diff_defl)) ~ L(ts(log(diff_defl)), 1:x))))

# select the AR model with the smallest BIC
BICs[, which.min(BICs[2, ])] #1
AICs[, which.min(BICs[2, ])]


# for LUR LOG LEVELS
BICs <- sapply(order, function(x) 
  "AR" = BICme(dynlm(ts(log(unemp_lev)) ~ L(ts(log(unemp_lev)), 1:x))))

AICs <- sapply(order, function(x) 
  "AR" = AICme(dynlm(ts(log(unemp_lev)) ~ L(ts(log(unemp_lev)), 1:x))))

# select the AR model with the smallest BIC
BICs[, which.min(BICs[2, ])] # 1
AICs[, which.min(BICs[2, ])]


#First differences of LUR
diff_unemp <- (unemp_lev/lag(unemp_lev))
# for LUR fisrt diff Log LEVELS
BICs <- sapply(order, function(x) 
  "AR" = BICme(dynlm(ts(log(diff_unemp)) ~ L(ts(log(diff_unemp)), 1:x))))

AICs <- sapply(order, function(x) 
  "AR" = AICme(dynlm(ts(log(diff_unemp)) ~ L(ts(log(diff_unemp)), 1:x))))

# select the AR model with the smallest BIC
BICs[, which.min(BICs[2, ])] # 2
AICs[, which.min(BICs[2, ])]



# AUGMENTED DICKEY-FULLER TEST

adf.test(na.omit(lndf_Gdp), k = 1) #NULL IS REJECTED, THE VARIABLE IS STATIONARY

adf.test(log(Gdp_lev), k=1) #NULL IS NOT REJECTED, V IS STOCASTIC

adf.test((log(infl_lev)), k = 2) # Null not rejected, V STOCASTIC

adf.test(na.omit(log(diff_Infl)), k=1) # Null is rejected, Stationary

adf.test(log(Deflat_lev), k = 2) # Null not rejected, Stocastic

adf.test(na.omit(log(diff_defl)), k = 1) # null not rejected, Stocastic

adf.test(na.omit(log(diff_unemp)), k=2) #NULL IS REJECTED, THE V IS STATIONARY

adf.test((log(unemp_lev)), k=1) # NULL IS REJECTED, V IS stationary


# create the object for the Philips Curve
# I'm going to subset the data so can test the model later with a little forecas

P <- ts(log(diff_Infl),
        start = c(1980), 
        end = c(2022), 
        frequency = 1)

U <- ts(unemp_lev,
        start = c(1980), 
        end = c(2022), 
        frequency = 1)

# melt the variable in a single dataset
PHC <- ts.union(P,U)

# loop 'BIC()' over multiple ADL models 
order <- 1:10

BICs <- sapply(order, function(x) 
  BICme(dynlm(P ~ L(P, 1:x) + L(U, 1:x))))

AICs <- sapply(order, function(x) 
  AICme(dynlm(P ~ L(P, 1:x) + L(U, 1:x))))

BICs
AICs

# select the AR model with the smallest BIC
BICs[, which.min(BICs[2, ])] # 2p which means 1 lag for each variable
AICs[, which.min(BICs[2, ])]


# estimate the ADL(1,1) model
ADL11 <- dynlm(P ~ L(P) + L(U))


# compute robust standard errors
rob_se <- list(sqrt(diag(sandwich(ADL11)))
)

# create a table with coef and se
stargazer(ADL11,
          type = "text",
          title = "",
          header = FALSE, 
          model.numbers = F,
          omit.table.layout = "n",
          digits = 3, 
          column.labels = "ADL(1,1)",
          dep.var.labels.include = FALSE,
          se = rob_se
)


# Now try to forecast the infl FD of ln for the 2023
# 
subset <- window(PHC,2022)

# Forecast
ADL11_forecast <- coef(ADL11) %*% c(1,subset[1, 1], subset[1, 2])

# Show the actual value we've got in the data for the year 2023
CPI_lev <- log(diff_Infl) 
CPI_lev[44]
# Now show the forecast result
ADL11_forecast 

# The model seems to forecast the first differences of log inflation pretty well



