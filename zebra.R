library(dplyr)
library(readr)
library(lubridate)
library(tidyr)

dataset_futebol_internacional <- read_csv("dataset_copa2002-2022.csv")

dataset_zebra <- dataset_futebol_internacional %>%
  filter(jogo_terminado == 1) %>%
  select(id_jogo, ano_copa, time, mandante, oponente, zebra) %>%
  drop_na()  

dataset_teste <- dataset_futebol_internacional %>%
  filter(jogo_terminado == 1) %>%
  select(ano_copa, time, mandante, oponente)

modelo_zebra <- glm(zebra ~ time + mandante + oponente, data = dataset_zebra, 
                    family = "binomial")

resultado_zebra <- cbind(dataset_teste, 
                         zebra = round(predict(modelo_zebra, newdata = dataset_teste, type = "response"), digits = 2))

                   