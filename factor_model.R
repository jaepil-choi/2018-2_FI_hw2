######################
#### FF factor model 
#### 2013310443 √÷¿Á«  
######################

Sys.setlocale("LC_ALL", "korean")

library(dplyr)
library(xts)
library(quantmod)
library(httr) # web scraping
library(rvest)

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

## S&P500 list from Wikipedia
url <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"
res <- GET(url)
wiki <- read_html(res)
wiki_table <- wiki %>% html_nodes(".wikitable") %>% html_table(fill=TRUE)
fcodes <- unlist(c(wiki_table[[1]][1]), use.names=FALSE)
fcodes
length(fcodes) # More than 500 in S&P500 because of different share classes. => 505 

## Stock data of S&P500
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
View(df) # Time series data of top 100 stocks. (2015-1-01-01 ~ 2018-10-10)

###################################
### Merge portfolio return with FF3
###################################

portfolio_daily <- dailyReturn(df)


#########################
### Run regression on FF3
#########################

