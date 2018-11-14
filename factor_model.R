######################
#### FF factor model 
#### 2013310443 √÷¿Á«  
######################

Sys.setlocale("LC_ALL", "korean")
rm(list = ls())

library(dplyr)
library(xts)
library(quantmod)
library(httr) # web scraping
library(rvest)
library(PerformanceAnalytics)

###############
### import data
###############

## Fama-French 3 Factor
ff3 <- read.csv("F-F_Research_Data_Factors_daily.csv", skip=4) # => from 1926.07.01 ~ 2018.09.28
colnames(ff3)[1] <- 'date'
colnames(ff3)[2] <- 'Rm_Rf'
ff3 <- ff3[-nrow(ff3),]
dim(ff3)[1] # => 24328
ff3$date <- as.Date.character(ff3$date, "%Y%m%d")
ff3xts <- xts(ff3[,-1],ff3[,1])
View(ff3xts)

## Scrape S&P500 list from Wikipedia
url <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"
res <- GET(url)
wiki <- read_html(res)
wiki_table <- wiki %>% html_nodes(".wikitable") %>% html_table(fill=TRUE)
fcodes <- unlist(c(wiki_table[[1]][1]), use.names=FALSE)
fcodes
length(fcodes) # More than 500 in S&P500 because of different share classes. => 505 

## Get stock data of S&P500 from Yahoo Finance.(It takes some time)
# However, there may be 'hindsight(survivorship) bias' in this selection. (Current S&P 500 firms are survivors)
SP500 <- NULL
start_date <- "2015-01-01"
for (f in fcodes) {
  tryCatch({
    SP500 <- cbind(SP500, getSymbols(f, from=start_date, auto.assign=FALSE)[,6]) 
  }, error=function(e){cat("ERROR!:", conditionMessage(e), "\n")})
} # => BHF, DXC, JEF, LIN, UA has missing values.
ncol(SP500) # => 505
SP500_noNA <-SP500[, !sapply(SP500, function(x) any(is.na(x)))] # drop columns with missing values. 
ncol(SP500_noNA) # => 492
View(SP500_noNA)

##################################
### Setting up investment universe 
##################################

## Select Top 100 companies as of 2015-12-31 (by annual return)
df <- SP500_noNA
yearlyReturn(df[,1])
yearly_df <- NULL
for (i in (1:ncol(df))) {
  temp_df <- yearlyReturn(df[,i])
  colnames(temp_df) <- paste0(colnames(df[,i]))
  yearly_df <- cbind(yearly_df, temp_df)
}
yearly_df <- as.data.frame(t(yearly_df)) # transpose
top100 <- yearly_df[order(-yearly_df[,1]),] # order by 2015's return. 
top100 <- head(top100, n=100)
universe <- attr(top100, 'row.names')  
df <- df[,universe] 
View(df) # Time series data of top 100 stocks. => (2015-1-01-01 ~ 2018-10-10)

###################################
### Merge portfolio return with FF3
###################################

## Daily portfolio return 
portfolio_daily <- dailyReturn(df)
View(portfolio_daily)
table.Stats(portfolio_daily)
table.AnnualizedReturns(portfolio_daily, scale=252)

## Merge with FF3 (no lag given yet.)
merged_data <- merge(portfolio_daily, ff3xts, join='left')
merged_data <- na.omit(merged_data)
merged_data$Rf_daily <- (merged_data$RF/252) # Convert Rf(yearly return) to daily return because Rp is daily. (We want proper Rp-Rf)
merged_data$Rp_Rf_daily <- (merged_data$daily.returns - merged_data$Rf_daily)
merged_data$Rm_Rf_daily <- (merged_data$Rm_Rf/252)
View(merged_data)

#########################
### Run regression on FF3
#########################
## * Googling 'optimal lag selection' gives some results of advanced statistical techniques for time series analysis. 
## * AR/ MA/ ARMA/ ARIMA may be needed to find the right answer.

## No lag
ff.reg <- lm(Rp_Rf_daily ~ Rm_Rf_daily + SMB + HML, data=merged_data)
summary(ff.reg)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.0009933  0.0005062   1.962    0.050 .  
# Rm_Rf_daily  3.0406385  0.1579523  19.250   <2e-16 ***
#   SMB         -0.0010558  0.0010347  -1.020    0.308    
# HML         -0.0094495  0.0009781  -9.661   <2e-16 ***
#   ---
#   Signif. codes:  0 °Æ***°Ø 0.001 °Æ**°Ø 0.01 °Æ*°Ø 0.05 °Æ.°Ø 0.1 °Æ °Ø 1
# 
# Residual standard error: 0.01551 on 939 degrees of freedom
# Multiple R-squared:  0.3297,	Adjusted R-squared:  0.3276 
# F-statistic: 153.9 on 3 and 939 DF,  p-value: < 2.2e-16

## Give 1 day lag
nolagx <- merged_data[,c("Rm_Rf_daily", "SMB", "HML")]
lagy <- lag(merged_data$Rp_Rf_daily, 1, na.pad=TRUE)
lag1 <- na.omit(cbind(nolagx, lagy))
ff.lag1.reg <- lm(Rp_Rf_daily ~ Rm_Rf_daily + SMB + HML, data=lag1)
summary(ff.reg) 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.0009933  0.0005062   1.962    0.050 .  
# Rm_Rf_daily  3.0406385  0.1579523  19.250   <2e-16 ***
#   SMB         -0.0010558  0.0010347  -1.020    0.308    
# HML         -0.0094495  0.0009781  -9.661   <2e-16 ***
#   ---
#   Signif. codes:  0 °Æ***°Ø 0.001 °Æ**°Ø 0.01 °Æ*°Ø 0.05 °Æ.°Ø 0.1 °Æ °Ø 1
# 
# Residual standard error: 0.01551 on 939 degrees of freedom
# Multiple R-squared:  0.3297,	Adjusted R-squared:  0.3276 
# F-statistic: 153.9 on 3 and 939 DF,  p-value: < 2.2e-16

## Give 2 day lag => coefficient's P value start to increase significantly as I give more lag, making it irrelevant. 
nolagx <- merged_data[,c("Rm_Rf_daily", "SMB", "HML")]
lagy <- lag(merged_data$Rp_Rf_daily, 2, na.pad=TRUE)
lag2 <- na.omit(cbind(nolagx, lagy))
ff.lag2.reg <- lm(Rp_Rf_daily ~ Rm_Rf_daily + SMB + HML, data=lag2)
summary(ff.lag2.reg) 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)   
# (Intercept)  0.0017012  0.0006189   2.749   0.0061 **
#   Rm_Rf_daily -0.1505631  0.1935079  -0.778   0.4367   
# SMB          0.0009393  0.0012651   0.742   0.4580   
# HML         -0.0007224  0.0011954  -0.604   0.5458   
# ---
#   Signif. codes:  0 °Æ***°Ø 0.001 °Æ**°Ø 0.01 °Æ*°Ø 0.05 °Æ.°Ø 0.1 °Æ °Ø 1
# 
# Residual standard error: 0.01894 on 937 degrees of freedom
# Multiple R-squared:  0.001609,	Adjusted R-squared:  -0.001587 
# F-statistic: 0.5034 on 3 and 937 DF,  p-value: 0.68



