# EDA e Previsão da Serie

# Pacotes 
library(tidyverse)
library(prophet)

# Carregar dados 
df <- read_csv("data/serie.csv") %>% 
  rename(ds = data, y = valor) %>% 
  arrange(ds)
glimpse(df)


## Grafico sazonalidade
df %>% 
  mutate( mes = lubridate::month(ds,label = TRUE)
         ,ano = lubridate::year(ds) %>% as.factor()) %>% 
  group_by(mes, ano) %>% 
  summarise(y = mean(y)) %>%
  ggplot(aes(x = mes, y = y, group = ano, col=ano)) +
  geom_line() + 
  geom_point() 


# Datas Sazonais
library(dplyr)

# BF
holiday <- data_frame(
   holiday = 'bf'
  ,ds = as.Date(c('2021-11-27','2020-11-26'))
  ,lower_window = -5
  ,upper_window = 2
)


# Predicao
model <- prophet(df, holidays = holiday)
future <- make_future_dataframe(model, periods = 20)
forecast <- predict(model, future)


# polt
dyplot.prophet(model, forecast)
prophet_plot_components(model, forecast)


