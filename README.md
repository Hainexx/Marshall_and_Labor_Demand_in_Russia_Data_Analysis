# Marshall and Labor Demand in Russia Data Analysis 
Inference of the Russian labour demand function for the years 1996 and 1997 estimating and testing hypothesis on OLS, In this script we are going to to estimate the Russian labour demand function ln = f(lw, ly) for the years 1996 and 1997 and assess which model fits our data best. 
The first part is dedicated to the preliminary analysis of each variable. Here we will check for the presence of outliers, analyse the distributions of the variables
and run the normality test. We will then move on, taking into consideration just the data available for the year 1997 
looking at the OLS regression of the labour demand function in that period.
In order to better understand the them we will run on the regression model the tests for heteroskedasticity, normality, and the RESET test.
At the end of the first part, some comparison will be made between different models in order to find the best one for our data.
We will start by looking at the results obtained from the OLS regression and the robust regression. Then, we will look at the differences 
between the robust OLS regression and the IV regression. Lastly, moving from a cross-sectional approach to a panel data setting, we will look at
the differences between the polled OLS regression and the fixed effect regression.
The main object of this script is to reproduce an analysis of the Austrian country
using the data from the WEO database on National outcomes figures. 
We're going to analyze mainly the data on NGDP_R, the misures of Inflation, PCPI
and NGDP_D, and finally the data on unemployment. We're going to initially visualize the time series 
of both log levels and growth percentage. Furthermore we're going to check for autocorrelation and for 
Stationarity through the Augmented Dickey-Fuller test and finally try to estimate a "Phillip Curve".