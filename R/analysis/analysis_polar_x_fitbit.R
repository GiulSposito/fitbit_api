library(tidyverse)
library(lubridate)

source("./R/import/import_Fitbit_HR.R")
source("./R/import/import_gpx_HR.R")

glimpse(polar_hr)
glimpse(fitbit_hr)

# ajusting the "timezone" and merging both devices
polar_hr %>% 
  mutate(datetime = datetime - hours(2)) %>% 
  inner_join(fitbit_hr, by = "datetime") -> hr_data

glimpse(hr_data)

# lets plot the dataset
hr_data %>% 
  gather(device, hr, -datetime) %>% 
  ggplot(aes(x=datetime, y=hr, group=device)) +
  geom_line(aes(color=device)) +
  theme_minimal()

# lets see the correlation
hr_data %>% 
  ggplot(aes(x=polar_hr, y=fitbit_hr)) +
  geom_point() +
  stat_smooth(method = "lm", se=T, level=.95) +
  theme_minimal()

# correlation test
cor.test(x=hr_data$polar_hr, y=hr_data$fitbit_hr, alternative = "two.sided")

# check the quality of a linear correlation
model <- lm(fitbit_hr~polar_hr, hr_data)
summary(model)

par(mfrow = c(2, 2))
plot(model)


## teste Bland Altman

# Deve ser avaliado se as diferenças entre as variáveis dependem ou não do tamanho da
# medida. Isto pode ser feito através de uma correlação entre as diferenças e as médias, que
# deve ser nula. A hipótese do viés ser ou não igual a zero pode ser testada por um teste t para
# amostras emparelhadas. A partir do cálculo do viés ( d ) e do seu desvio-padrão (sd) é possível
# chegar aos limites de concordância: d ± 1,96sd, que devem ser calculados e incluídos no gráfico.
# Se o viés apresenta distribuição normal, estes limites representam a região em
# que se encontram 95% das diferenças nos casos estudados.
# Nas situações em que o viés não apresenta
# distribuição normal, é recomendada uma abordagem não-paramétrica.

# math
hr_data %>% 
  mutate(
    mean    = (polar_hr + fitbit_hr)/2,
    diff    = polar_hr - fitbit_hr,
    diff.mn = mean(diff),
    diff.sd = sqrt(var(diff)),
    upper.lim = diff.mn + (2*diff.sd), 
    lower.lim = diff.mn - (2*diff.sd),
  ) -> hr_data_ba

# overview
summary(hr_data_ba)

# plotting the differences
hr_data_ba %>% 
  ggplot() +
  geom_segment(aes(x=datetime, xend = datetime, y=0, yend = diff), color="red", size=1) +
  geom_point(aes(x=datetime, y=diff), color='black') +
  theme_minimal()

# distribuição das diferenças
hr_data_ba %>% 
  ggplot() +
  geom_density(aes(x=diff), color="red", fill="red" ) +
  theme_minimal()

# Deve ser avaliado se as diferenças entre as variáveis dependem ou não do tamanho da
# medida. Isto pode ser feito através de uma correlação entre as diferenças e as médias, que
# deve ser nula.  
cor.test(x=hr_data_ba$diff, y=hr_data_ba$mean)

# A hipótese do viés ser ou não igual a zero pode ser testada por um teste t para
# amostras emparelhadas.
t.test(x=hr_data_ba$diff, y=hr_data_ba$mean, paired = T)

# plot
hr_data_ba %>% 
  ggplot(aes(x=mean, y=diff)) + 
  geom_point() +
  geom_hline(yintercept=0) +
  geom_hline(yintercept=hr_data_ba$diff.mn[1], linetype=2) +
  geom_hline(yintercept=hr_data_ba$upper.lim[1], linetype=2) +
  geom_hline(yintercept=hr_data_ba$lower.lim[1], linetype=2) +
  theme_minimal()

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
