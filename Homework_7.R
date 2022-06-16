# https://dplyr.tidyverse.org/articles/dplyr.html - the basic
# https://dplyr.tidyverse.org/articles/grouping.html - group by in details
# https://dplyr.tidyverse.org/articles/base.html - comparing dplyr functions
# to the base R function.
#####Problem 1##### 
library(tidyquant)
library(tidyverse) 
library(lubridate)
# 1.Download the stock prices for AMZN, FB, NFLX, stocks from 2019-01-01 
# to 2021-04-01. Keep only the symbol/date/adjusted columns. 
stock_prices <- tidyquant::tq_get(c("AMZN","FB","NFLX"),
                                  from = "2019-01-01",
                                  to = "2021-04-01") %>% 
  dplyr::select(symbol,date,adjusted) 

# 2.Add all the missing dates(such as 2019-01-01), so that we have 
# observations for every single date. Fill in the missing values for adjusted 
# with the last non-missing observation. 

missing_dates <- data.frame(Dates = rep(seq.Date(from = ymd("2019-01-01"),
                                                 to = ymd("2021-04-01"),
                                                 by = "day"),3), 
                            Symbol = c(rep("AMZN", as.numeric(ymd("2021-04-01")-ymd("2019-01-01")) + 1),
                                       rep("NFLX", as.numeric(ymd("2021-04-01")-ymd("2019-01-01")) + 1),
                                       rep("FB", as.numeric(ymd("2021-04-01")-ymd("2019-01-01")) + 1))) 

Final <- missing_dates %>% 
  dplyr::left_join(stock_prices, by = c("Dates"="date", "Symbol"="symbol")) %>% 
  dplyr::group_by(Symbol) %>% 
  tidyr::fill(adjusted, .direction = "downup")


# 3.Create a new data frame, which consist only of stocks from AMZN or FB and 
# has observations from 2019-01-01 to 2019-07-01 or 2020-04-01 to 2020-07-01. 
# Arrange the data frame first by the symbol name and by the date in 
# descending order. 
new_data <- Final %>% 
  dplyr::filter(Symbol %in% c("AMZN", "FB"),
                between(Dates, ymd("2019-01-01"), ymd("2019-07-01")) |
                  between(Dates, ymd("2020-04-01"), ymd("2020-07-01"))) %>% 
  dplyr::arrange(Symbol, desc(Dates))




# 4.Select the first and last observation of the aforementioned dataframe
# for each of the two stocks - AMZN and FB. 
class(new_data$Dates)

first_last_obs <- new_data %>% 
  group_by(Symbol)%>% 
  summarise(first = min(Dates),
            last = max(Dates))

first_last_obs <- new_data %>% 
  group_by(Symbol)%>% 
  slice(1,n())




# 5.Select the last observation for each stock, for each month. 
last_obs <- new_data %>% 
  mutate(year_month = floor_date(Dates, unit = "month")) %>% 
  group_by(Symbol, year_month) %>% 
  slice(n())

# In order to do this, first create a new column, which will show you the 
# year and the month. You can do this using the functions substr() or floor_date.
#####Problem 1#####

#####Problem 2#####
#Use the dataframe from problem 1.2.
# Use the SMA function from the tidyquant package to calculate the 10day SMA 
# and the 26 day SMA for each of the 3 stocks. 
# How many times did the 10 day SMA line cross 26 day SMA line from below? 
# How many times did the 10 day SMA line cross 26 day SMA line from above? 
SMA_data <- Final %>% 
  mutate(SMA10 = SMA(adjusted, n=10),
         SMA26 = SMA(adjusted, n=26),
         lagSMA10 = lag(SMA10),
         lagSMA26 = lag(SMA26)) %>% 
  ungroup() %>% 
  filter(!is.na(SMA26))%>% 
  mutate(crossed = case_when(lagSMA10 > lagSMA26 & SMA10<SMA26 ~ "crossed from above", 
                             lagSMA10 < lagSMA26 & SMA10 > SMA26 ~ "crossed from bellow"
                             TRUE ~ "did not cross"))


# You can take a look at this article: https://www.investopedia.com/trading/macd/
# Essentially by cross from above/below I want you to find the buy/sell signals.