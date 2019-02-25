library(tidyverse)
library(lubridate)

# datasets (loaded from imports)
fitbit_hr <- hrtb %>% 
  rename(hr_fitbit=value) %>% 
  select(timestamp, hr_fitbit)

polar_hr
fitbit_hr

polar_hr %>% 
  mutate(timestamp = timestamp - hours(2)) %>% 
  rename(hr_polar=hr) %>% 
  inner_join(fitbit_hr, by = "timestamp") %>% 
  mutate(
    residual = hr_fitbit-hr_polar,
    error    = residual/hr_polar
  ) -> hrdata

hrdata %>% 
  ggplot(aes(x=timestamp)) +
  geom_line(aes(y=hr_fitbit), color="blue") +
  geom_line(aes(y=hr_polar), color="red") +
  theme_minimal()

hrdata %>% 
  ggplot() +
  geom_point(aes(x=hr_polar, y=hr_fitbit, color=error)) +
  scale_color_gradient(name="error %",low="green", high="red") + 
  theme_minimal()

hrdata %>% 
  ggplot(aes(x=timestamp)) +
  geom_line(aes(y=error, color=error)) +
  scale_color_gradient(name="heart rate (bpm)",low="green", high="red") + 
  theme_minimal()

summary(hrdata)
hist(hrdata$error, breaks=25, col="red")
