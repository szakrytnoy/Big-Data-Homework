library(tidyverse)

flights <- nycflights13::flights(stringsAsFactors = FALSE)

# Counting number of flights in February
n_february = flights %>% 
  filter(month == 2) %>% 
  nrow()

# Counting flights in February that were on time, exclude nans, filter dep_delay
n_februay_on_time = flights %>%
  filter(month == 2 & dep_delay <= 0) %>% 
  nrow()

# Counting unique tailnums from the respective column
n_unique_tailnums = flights[['tailnum']] %>% 
  unique() %>% 
  length()

# Calculating mean dep_delay from LGA to MIA (exclude omitted dep_delays)
temp = flights %>% 
  filter(origin == 'LGA' & dest == 'MIA') %>% 
  select(dep_delay)
temp = temp[[1]]
average_delay_LGA_MIA = mean.default(temp, na.rm = TRUE)

#Calculating correlation between arr_delay and dep_delay for late flights from JFK
arr_dep_correlation = flights %>% 
  filter(origin == 'JFK' & dep_delay > 0) %>%
  select(dep_delay, arr_delay) %>% 
  na.omit() %>%
  cor() 

#Defining the function for counting percentage of delayed flights
monthly_delayed_pct <- function(flights, month_no){
  delayed = flights %>% 
    filter(month == month_no & dep_delay > 0) %>% 
    nrow()
  
  flights_count = flights %>%
    filter(month == month_no) %>% 
    nrow()
  
  delayed_pct = delayed/flights_count*100
  return(delayed_pct)
}

#Finding the month with most delays %
delayed_pct_stat = list()
for (month_no in c(1:12)){
  delayed_pct = monthly_delayed_pct(flights, month_no)
  delayed_pct_stat[[(length(delayed_pct_stat) +1)]] <- delayed_pct
}
  
month_with_most_delays = which.max(delayed_pct_stat)

# Plot barplot of flights per month
ggplot(flights, aes(x = month)) +
  geom_bar()
  
