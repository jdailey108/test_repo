
#Joseph Dailey
#NYSE Volatility


library(tidyverse);
library(tidyquant);

#Preparing NYSE tickers to be called in tq_get
tickers = read.csv("NYSE tickers.txt", sep = "\t");
st = tickers$Symbol;

#Getting month's worth of NYSE prices
Ra = st %>%
  tq_get(get  = "stock.prices",
         from = "2021-02-01",
         to   = "2021-03-01");

#Clearing NA's
Ra = na.omit(Ra);
str(Ra);

#Converting prices to daily returns
stock_returns_daily = Ra %>%
  group_by(symbol) %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "daily", 
               col_rename = "Ra");
  
#Calculating each stock's volatility 
volatility = stock_returns_daily %>%
  tq_performance(
    Ra = Ra,
    Rb = NULL,
    performance_fun = sd
  );

#quality of life changes
volatility = data.frame(volatility);
volatility = na.omit(volatility);



#Faceting for comparison graphing
summary(volatility$sd.1);
volatility$bucket = "very low";
volatility$bucket[(volatility$sd.1>=0.012)&(volatility$sd.1<0.019)] = "low";
volatility$bucket[(volatility$sd.1>=0.019)&(volatility$sd.1<0.03)] = "normal";
volatility$bucket[volatility$sd.1>=0.03] = "high";

library(ggplot2);

#Comparing each bin... could use work
ggplot(volatility) +
  geom_boxplot(aes(x = bucket, y = sd.1)) +
  scale_y_continuous(limits = c(0.0, 0.05)) +
  labs(x = "", y = "", title = "NYSE Returns Volatility") +
  facet_wrap(~bucket, scale="free");

#Isolating the most volatile
tail(sort(volatility$sd.1));
mostVolatile = volatility[volatility$sd.1>0.12,];
str(mostVolatile);

#Comparing the most volatile
ggplot(mostVolatile, aes(x = reorder(symbol, sd.1), y = sd.1)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(x = "", y = "", title = "Greatest Volatility");
  
