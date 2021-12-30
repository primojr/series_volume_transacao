# EDA e Previsão da Serie

# Pacotes 
library(tidyverse)
library(prophet)

# Carregar dados 
df <- read_csv("data/serie.csv") %>% 
  rename(ds = data, y = valor)
glimpse(df)


## Grafico sazonalidade
df %>% 
  mutate( mes = lubridate::month(ds,label = TRUE)
         ,ano = lubridate::year(ds) %>% as.factor()) %>% 
  group_by(mes, ano) %>% 
  summarise(y = mean(y)) %>%
  ggplot(aes(x = mes, y = y, group = ano, col=ano)) +
  geom_line()



# Datas Sazonais
library(dplyr)

# BF
bf <- c(str_c("2020-11-",c(23:30)), str_c("2021-11-",c(22:29)))
holiday <- data_frame(
   holiday = 'bf'
  ,ds = as.Date(bf)
  ,lower_window = 0
  ,upper_window = 1
)



model <- prophet(df, holidays = holiday)

future <- make_future_dataframe(model, periods = 20)
forecast <- predict(model, future)


# polt
dyplot.prophet(model, forecast)
prophet_plot_components(model, forecast)

