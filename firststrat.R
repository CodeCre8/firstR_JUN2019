# Quantstrat Financial Trading Strategy - created by James Craig
# https://rpubs.com/jwcb1025/quantstrat_trading_strategy
# Strategy Overview:
# - Buy when 50day SMA > 200day SMA & DVO < 20
# - Sell when 50day SMA < 200day SMA & DVO > 80
### Learn by doing - Helen Shiu, Jun 16, 2019

require(devtools)
library(quantstrat)
library(TTR)
library(IKTrading)

# The closing price of SPY over the past 4.5 years.
getSymbols("SPY",
           from = "2014-01-01",
           to = "2019-06-14",
           src = "yahoo",
           adjust = TRUE)

# Plot the closing price of SPY.
plot(Cl(SPY))

# To observe the assets trend:
# Create a 200 day simple moving avergage (SMA) of SPY.
### Whenever an aesset’s price is above the 200-day moving average, 
### a whole assortment of good things usually happen, such as the 
### asset appreciating in price, low volatility, and so on.
lines(SMA(Cl(SPY), n = 200), col = "blue")


# Setting up Trading Strategy

# 1. Define Initialization Settings
# ===============================================================
# Date and Time Paremeters - creat initdate, from, and to strings
initdate <- "2010-01-01"
from <- "2014-01-01"
to <- "2019-06-014"
# ===============================================================
# Set the timezone to EST
Sys.setenv(TZ = "EST")
# ===============================================================
# Set currency to USD
currency("USD")





