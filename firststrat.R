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
library(blotter)


# ===============================================================
# Testing RStudio
# ===============================================================
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


# ===============================================================
# 1. Initialization Sittings
# ===============================================================
# Date and Time Paremeters - creat initdate, from, and to strings
initdate <- "2010-01-01"
from <- "2014-01-01"
to <- "2019-06-14"
# ===============================================================
# Set the timezone to EST
Sys.setenv(TZ = "EST")
# ===============================================================
# Set currency to USD
currency("USD")

# 1.1 Obtain Adjusted Data between from:to dates
getSymbols("SPY",
           from = from,
           to = to,
           src = "yahoo",
           adjust = TRUE)
# 1.2 Create a stock instructment based on USD
stock("SPY", currency = "USD")

# 1.3 Define Trade Size and Initial Equity
tradesize <- 100000
initeq <- 100000

# 1.4 Define three important objects in any Quantstrat trading strategy.
# ===============================================================
# 1) strategy
strategy.st <- "firststrat"
# ===============================================================
# 2) portfolio 
portfolio.st <- "firststrat"
# ===============================================================
# 3) account
account.st <- "firststrat"

# Remove the existing strategy if it exists
rm.strat("strategy.st")


# ===============================================================
# 2. Initialization - portfolio, account, order and strategy
# ===============================================================
# Initialize the portfolio
initPortf(portfolio.st, symbols = "SPY", initDate = initdate, currency = "USD")
# Initialize the account
initAcct(account.st, portfolios = portfolio.st, symbols = "SPY", initDate = initdate, currency = "USD", initEq = initeq)
# Initialize the order
initOrders(portfolio.st, initDate = initdate)

# 2.1 Store the strategy
strategy(strategy.st, store = TRUE)


# ===============================================================
# 3. Indicators - add.indicator()
# ===============================================================
# This strategy will incorporate a combination of 
# basic moving average crossover and RSI as an oscillation indicator.
#
# SMA - Simple Moving Average
# ===============================================================
# A fast moving average with a slower moving average is a simple 
# and standard way to predict when prices in an asset are expected 
# to rise in the future. While a single indicator can provide 
# a lot of information, a truly robust trading system requires 
# multiple indicators to operate effectively.
# ===============================================================
# 3.1 Add a 200-day SMA indicator to strategy.st
add.indicator(strategy = strategy.st,
              # Add the SMA function
              name = "SMA",
              # Create a lookback period
              arguments = list(x = quote(Cl(mktdata)),
                               n = 200),
              label = "SMA200"
)
# 3.2 Add a more robust indicator 50-day SMA to strategy.st
add.indicator(strategy = strategy.st,
              name = "SMA",
              arguments = list(x = quote(Cl(mktdata)),
                               n = 50),
              label = "SMA50")

# RSI - Relative Strength Indicator
# ===============================================================
# The RSI can predict when a price has sufficiently pulled back, 
# especially with a short period such as 2 or 3 trading days.
# Indicator that rises with positive price changes
# and falls with negative price changes. 
# It is equal to 100 - 100 / (1 + RS), where RS is the 
# average gain over average loss over the lookback period. 
# At various lengths, this indicator can range from a 
# reversion indicator, to a trend filter, or anywhere in between. 
# There are various ways of computing the RSI.
# ===============================================================
# 3.3 Add a RSI indicator with a 3-day lookback period
add.indicator(strategy = strategy.st,
              name = "RSI",
              arguments = list(price = quote(Cl(mktdata)),
                               n = 3),
              label = "RSI_3")
# 3.4 Use a function to add a 3.5 day RSI by averaging 3 and 4 day RSI
calc_RSI_avg <- function(price, n1, n2) {
  # RSI 1 takes an input of the price and n1
  RSI_1 <- RSI(price = price, n = n1)
  # RSI 2 takes an input of the price and n2
  RSI_2 <- RSI(price = price, n = n2)
  # RSI_avg is the avg of RSI_1 and RSI_2
  RSI_avg <- (RSI_1 + RSI_2) / 2
  # The output of RSI_avg needs a column
  colnames(price) <- "RSI_avg"
  # Return price
  return(price)
}
# 3.4.1 Add this function calc_RSI_avg to strategy.st
# as RSI_3_4 with n1 = 3 and n2 = 4
add.indicator(strategy = strategy.st,
              name = calc_RSI_avg,
              arguments = list(price = quote(Cl(mktdata)),
                               n1 = 3, n2 = 4), 
              label = "RSI_3_4")

# David Varadi Oscillator - DVO
# ===============================================================
# The purpose of the DVO is similar to something like the RSI 
# in that it attempts to find opportunities to buy a temporary dip 
# and sell in a temporary uptrend. In addition to obligatory market data, 
# an oscillator function takes in two lookback periods.
#
# First, the function computes a ratio between the closing price 
# and average of high and low prices. Next, it applies an SMA 
# to that quantity to smooth out noise, usually on a very small time frame, 
# such as two days. Finally, it uses the runPercentRank() function 
# to take a running percentage rank of this average ratio, and 
# multiplies it by 100 to convert it to a 0-100 quantity.
#
# **Note: High Low Close (HLC) is a built in function 
# used to obtain the High, Low, and Closing Price of an asset 
# on a given trading day.
# ===============================================================
# 3.5 # Declare the DVO function 
# with a 2-day time frame and a lookback period of 126
DVO <- function(HLC, navg = 2, percentlookback = 126) {
  # Compute the ratio between closing prices to the average of high and low
  ratio <- Cl(HLC) / ((Hi(HLC) + Lo(HLC)) / 2 )
  # Use SMA to smooth out the ratio output - avgratio
  avgratio <- SMA(ratio, n = navg)
  # Convert ratio into a 0-100 value using runPercentRank() - out
  out <- runPercentRank(avgratio, n = percentlookback, exact.multiplier = 1) * 100
  # The output of out needs a column
  colnames(out) <- "DVO"
  # Return out
  return(out)
}
# 3.5.1 Add indicator DVO to strategy.st
add.indicator(strategy = strategy.st,
              name = "DVO",
              arguments = list(HLC = quote(HLC(mktdata)), navg = 2, percentlookback = 126),
              label = "DVO_2_126")


# ===============================================================
# 4. Test - applyIndicators()
# ===============================================================
# Use applyIndicators to test the indicators in the strategy
test <- applyIndicators(strategy = strategy.st, mktdata = OHLC(SPY))
# Remove test and create again
rm(mktdata,test)
test <- applyIndicators(strategy = strategy.st, mktdata = OHLC(SPY))
# Subset the data between November 5th and 9th of 2013
test_subset <- test["2013-11-05/2013-11-09"]
# Remove subset because the dates yielded nothing. Use new dates
rm(test_subset)
test_subset <- test["2016-11-05/2016-11-09"]


# ===============================================================
# 5. Signals - add.signal()
# ===============================================================
# In this strategy, 4 types of signal will be used
# ===============================================================
# 5.1 sigComparison
# Signify whether SMA50 > SMA200
add.signal(strategy = strategy.st, name = "sigComparison",
           # Look at the relationship between the SMA50 & SMA200
           arguments = list(columns = c("SMA50", "SMA200"),
                            # Discover whether the SMA50 is greater than SMA200
                            relationship = "gt"),
           # Label this signal "longfilter"
           label = "longfilter"
           )
# 5.2 sigCrossover
# Signify whether SMA50 < SMA200
add.signal(strategy = strategy.st, name = "sigCrossover",
           arguments = list(columns = c("SMA50", "SMA200"),
                            # Discover when the SMA50 crosses under the SMA200
                            relationship = "lt"),
           label = "filterexit"
           )
# 5.3 sigThreshold
# Specify that DVO_2_126 < 20
add.signal(strategy = strategy.st, name = "sigThreshold",
           # Use the column DVO_2_126
           arguments = list(column = "DVO_2_126",
                            # Set threshold to 20
                            threshold = 20,
                            # Set oscillator to be under the value
                            relationship = "lt",
                            # Interest in oscillator < 20
                            cross = FALSE),
           label = "longthreshold"
           )
# 5.3.1 sigThreshold2
# Signal when DVO_2_126 crosses above 80
add.signal(strategy = strategy.st, name = "sigThreshold",
           # Use the column DVO_2_126
           arguments = list(column = "DVO_2_126",
                            # Set threshold to 80
                            threshold = 80,
                            # Set oscillator to greater than 80
                            relationship = "gt",
                            # Interest in the cross
                            cross = TRUE),
           label = "thresholdexit"
           )
# 5.4 sigFormula - add.signal() & applyIndicators()
# Combine various indicators and signals already added to the strategy 
# in order to create composite signals. A function uses string evaluation 
# to offer immense flexibility
test_init <- applyIndicators(strategy = strategy.st, mktdata = OHLC(SPY))
test <- applySignals(strategy = strategy.st, mktdata = test_init)
add.signal(strategy = strategy.st, name = "sigFormula",
           # Specify that both longfilter and longthreshold must be TRUE
           arguments = list(formula = "longfilter & longthreshold",
                            # Set cross to be TRUE
                            cross = TRUE),
           label = "longentry"
           )


# ===============================================================
# 6. Rules - add.rule()
# ===============================================================
# Add trading rules to enter/exit an asset based on the trading 
# signals. Like signals and indicators, all rules reference a 
# column already present in the strategy.
# Rules relies on signals and must reference the signal columns
# in the strategy to work right.
# ===============================================================
# 6.1 Exit Rule
add.rule(strategy = strategy.st, name = "ruleSignal",
         arguments = list(
           # Use the signal "filterexit" set in the strategy:
           # the SMA50 has crossed under SMA200
           # Rule for this signal indicating the condition to exit 
           # as the market is no longer conducive to the position.
           sigcol = "filterexit",
           # Specifying sigval to indicate whether the rule is triggered 
           # when the signal value is TRUE or FALSE.
           # To proceed with this exit rule, specify that a transaction
           # should occcur when filterexit is equal to TRUE.
           sigval = TRUE,
           # orderqty: Set to "all" for liquidation of asset or
           # set to an integer value of a number of shares
           orderqty = "all",
           # ordertype: 
           # "market" for buy or sell the asset at the prevailing price.
           # "limit" for certain price conditions are met (namely, if the price 
           # falls below a certain further threshold on the day of the order)
           # "stoplimit" for when price above a certain price
           ordertype = "market",
           # orderside:
           # "long" is to classify a set of rules that profits by buying an asset 
           # in the hopes that the asset's price will rise. 
           # "short" is one that shorts a stock by selling an asset before owning it, 
           # hoping to buy it back later at a lower price.
           orderside = "long",
           # replace when set to TRUE will cancel other signals in the strategy.
           # set to FALSE is the good rule of thumb to keep other signals.
           # Specifies whether or not to ignore all other signals on the same date 
           # when the strategy acts upon one signal.
           replace = FALSE,
           # prefer indicates when it is favourable to enter a position. 
           # Default is set to buy at the close of next day (bar).
           # In Quantstrat, orders have a "next-bar" mechanism. There is a day lag
           # bewteen observing signal and buying next day.
           # This can be solved by placing orders to execute on the next
           # possible bar: Open, High, Low, or Close price
           prefer = "Open"),
         type = "exit")
# 6.2 Enter Rule
# Create an entry rule of 1 share when all conditions line up
add.rule(strategy = strategy.st, name = "ruleSignal",
         arguments = list(
           # Use the signal longentry set in the strategy:
           # Specify that both longfilter and longthreshold must be TRUE
           sigcol = "longentry",
           # Set sigval to TRUE
           sigval = TRUE,
           # Set orderqty to 1
           orderqty = 1,
           ordertype = "market",
           orderside = "long",
           replace = FALSE,
           prefer = "Open"),
         type = "enter")
# 6.3 Enter Rule with Order Sizing Function
add.rule(strategy = strategy.st, name = "ruleSignal",
         arguments = list(
           sigcol = "longentry",
           sigval = TRUE,
           ordertype = "market",
           orderside = "long",
           replace = FALSE,
           prefer = "Open",
           # Use the osMaxDollar order size function
           # osFUN - the constructs that allow quantstrat to vary the amount 
           # of shares bought or sold are called order sizing functions. 
           # The first thing to know is that when using an order sizing function, 
           # the orderqty argument is no longer relevant, as the order quantity 
           # is determined by the order sizing function, osFUN. The additional 
           # arguments to this function are tradeSize and maxSize, both of which 
           # should take tradesize, which we defined early on in our code.
           orderqty = osMaxDollar(
             portfolio = portfolio.st,
             tradeSize = tradesize,
             maxSize = tradesize)),
         type = "enter"
         )


# ===============================================================
# 7. Analyse - applyStrategy()
# ===============================================================
# Analysing the strategy is the last step in a quantstrat strategy.
# Indicators:
# This strategy requires both the threshold of the DVO_2_126 indicator 
# to be under 20 and the SMA50 to be greater than the SMA200. 
# This strategy sells when the DVO_2_126 crosses above 80, or 
# the SMA50 crosses under the SMA200.
# Signals:
# Set up five separate signals for this strategy to work properly:
# sigComparison for SMA50 being greater than SMA200;
# sigThreshold with cross set to FALSE for DVO_2_126 less than 20;
# sigFormula to tie them together and set cross to TRUE;
# sigCrossover with SMA50 less than SMA200; and
# sigThreshold with cross set to TRUE for DVO_2_126 greater than 80.
# ===============================================================
# The strategy invests $100,000 (the initeq) into each trade, and 
# may have some small dollar cost averaging if the DVO_2_126 oscillates 
# around 20 (though the effect is mostly negligible compared to the 
# initial allocation)(?-I don't quite understand).
# ===============================================================
# Use applyStrategy() to apply your strategy. Save this to out
out <- applyStrategy(strategy = strategy.st, portfolios = portfolio.st)
# Use updateProf() to update portfolio.st
updatePortf(portfolio.st)
# Set the date range
daterange <- time(getPortfolio(portfolio.st)$summary)[-1]
# Use updateAcct() to update account.st with daterange
updateAcct(account.st, daterange)
updateEndEq(account.st)


















