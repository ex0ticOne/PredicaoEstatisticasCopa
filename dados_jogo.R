library(dplyr)
library(readr)

#Dataset com estatísticas
dataset_estatisticas_copa <- read_csv("dataset_copa2002-2022.csv")

#Vetor para filtro dos países da Copa de 2022
paises_2022 <- unique(dataset_estatisticas_copa$time[dataset_estatisticas_copa$ano_copa == 2022])

#Dataset com os 32 países participantes de 2022
dataset_treino <- dataset_estatisticas_copa %>%
  filter(time %in% paises_2022)

#Criação dos datasets para as predições com as médias das estatísticas de jogo por time
dataset_teste_amarelos <- dataset_treino %>%
  group_by(time) %>%
  summarise(faltas_cometidas = mean(faltas_cometidas, na.rm = TRUE)) %>%
  arrange(desc(faltas_cometidas))

dataset_teste_escanteios <- dataset_treino %>%
  group_by(time) %>%
  summarise(posse_bola = mean(posse_bola, na.rm = TRUE),
            chutes_gol = mean(chutes_gol, na.rm = TRUE),
            chutes = mean(chutes, na.rm = TRUE))

dataset_teste_media <- dataset_treino %>%
  group_by(time) %>%
  summarise(posse_bola = mean(posse_bola, na.rm = TRUE),
            chutes_gol = mean(chutes_gol, na.rm = TRUE),
            chutes = mean(chutes, na.rm = TRUE),
            escanteios_afavor = mean(escanteios_afavor, na.rm = TRUE))

#Modelos
modelo_amarelos <- glm(cartoes_amarelos ~ time + faltas_cometidas, 
                       data = dataset_treino, family = "gaussian")

modelo_escanteios <- glm(escanteios_afavor ~ time + posse_bola + chutes_gol + chutes, 
                         data = dataset_treino, family =  "gaussian")

modelo_primeiro_gol <- glm(abriu_placar ~ time + posse_bola + chutes_gol + chutes + escanteios_afavor, 
                           data = dataset_treino, family = "binomial")

#Resultados
resultados_amarelos <- cbind(dataset_teste_amarelos, 
                            amarelos_previstos = round(predict(modelo_amarelos, newdata = dataset_teste_amarelos, type = "response"), digits = 4))

resultados_escanteios <- cbind(dataset_teste_escanteios, 
                               escanteios_previstos = round(predict(modelo_escanteios, newdata = dataset_teste_escanteios, type = "response"), digits = 4))

resultados_primeiro_gol <- cbind(dataset_teste_media,
                                 chance_primeiro_gol_prevista = round(predict(modelo_primeiro_gol, newdata = dataset_teste_media, type = "response"), digits = 4))
