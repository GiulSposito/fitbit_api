# config & setup
library(httr)
library(glue)
library(jsonlite)
library(tidyverse)
library(lubridate)
source("./R/common/config.R")

# authentication and authorization infra
fb_app   <- oauth_app(.config$app_name, .config$client_id, .config$client_secret)
fb_oauth <- oauth_endpoint(authorize = .config$auth_uri, access = .config$refresh_token_uri)
token    <- oauth2.0_token(fb_oauth, fb_app, scope = c("activity","heartrate","sleep"), cache = F, use_basic_auth = T)

# request HR data
# Resource URL - There are two acceptable formats for retrieving time series data:
#
# GET https://api.fitbit.com/1/user/[user-id]/activities/heart/date/[date]/[period].json
# GET https://api.fitbit.com/1/user/[user-id]/activities/heart/date/[base-date]/[end-date].json
#
# user-id:   The encoded ID of the user. Use "-" (dash) for current logged-in user.
# base-date: The range start date, in the format yyyy-MM-dd or today.
# end-date:	 The end date of the range.
# date:	     The end date of the period specified in the format yyyy-MM-dd or today.
# period:	   The range for which data will be returned. Options are 1d, 7d, 30d, 1w, 1m.

# shortcut to definy a url to get heart rate
get_hr_url <- function(.user_id="-",.date="today",.period="1d"){
  glue("https://api.fitbit.com/1/user/{.user_id}/activities/heart/date/{.date}/{.period}.json")
}

# make a HTTP GET 
resp <- GET(get_hr_url(.date="2019-02-24"), conf=config(token=token))

# check if the result is 200 (OK)
resp$status_code

# process content
data <- fromJSON(content(resp, "text"))
str(data)

# plot hr in the period
hrdt <- data$`activities-heart-intraday`$dataset
str(hrdt)

# convert "time" in date-time
hrdt %>% 
  as.tibble() %>% 
  mutate( datetime = paste0("2019-02-24 ", time) ) %>% 
  mutate( datetime = ymd_hms(datetime) ) %>% 
  rename(fitbit_hr = value) %>% 
  select(datetime, fitbit_hr) -> fitbit_hr

glimpse(fitbit_hr)

fitbit_hr %>% 
  ggplot() +
  geom_line(aes(x=datetime, y=fitbit_hr, color=fitbit_hr)) +
  scale_color_gradient(name="heart rate (bpm)",low="green", high="red") + #,breaks=seq(hr_lower_limit,hr_upper_limit,20), limits=c(hr_lower_limit,hr_upper_limit)) +
  theme_minimal()
