library(dplyr)
library(readr)

dataset_estatisticas_copa <- read_csv("dataset_copa2010-2022.csv", 
                                      show_col_types = FALSE)

paises_2022 <- unique(dataset_estatisticas_copa$time[dataset_estatisticas_copa$ano_copa == 2022])

dataset_treino <- dataset_estatisticas_copa %>%
  filter(time %in% paises_2022)

matriz_estatisticas <- dataset_estatisticas_copa %>%
  select(12, c(15:23)) %>%
  as.matrix()

correlacao_matriz <- cor(matriz_estatisticas, use = "complete.obs", method = "pearson")

corrplot(correlacao_matriz, 
         diag = FALSE, method = 'circle')

#Criação dos datasets para as predições com as médias das estatísticas de jogo por time
dataset_teste_amarelos <- dataset_treino %>%
  group_by(time) %>%
  summarise(faltas_cometidas = mean(faltas_cometidas, na.rm = TRUE))

dataset_teste_escanteios <- dataset_treino %>%
  group_by(time) %>%
  summarise(posse_bola = mean(posse_bola, na.rm = TRUE),
            chutes_gol = mean(chutes_gol, na.rm = TRUE),
            chutes = mean(chutes, na.rm = TRUE))

dataset_teste_primeiro_gol <- dataset_treino %>%
  group_by(time) %>%
  summarise(chutes_gol = mean(chutes_gol, na.rm = TRUE), 
            precisao_chute = mean(precisao_chute, na.rm = TRUE))

#Modelos
modelo_amarelos <- glm(cartoes_amarelos ~ time + faltas_cometidas, 
                       data = dataset_treino, family = "poisson")

modelo_escanteios <- glm(escanteios_afavor ~ time + posse_bola + chutes_gol + chutes, 
                         data = dataset_treino, family =  "poisson")

modelo_primeiro_gol <- glm(abriu_placar ~ time + chutes_gol + precisao_chute, 
                           data = dataset_treino, family = "binomial")

#Resultados
resultados_amarelos <- cbind(dataset_teste_amarelos, 
                             amarelos_previstos = round(predict(modelo_amarelos, newdata = dataset_teste_amarelos, type = "response"), digits = 4))

resultados_amarelos <- resultados_amarelos %>% 
  select(time, amarelos_previstos) %>%
  arrange(desc(amarelos_previstos)) %>%
  as_tibble()

resultados_escanteios <- cbind(dataset_teste_escanteios, 
                               escanteios_previstos = round(predict(modelo_escanteios, newdata = dataset_teste_escanteios, type = "response"), digits = 4))

resultados_escanteios <- resultados_escanteios %>%
  select(time, escanteios_previstos) %>%
  arrange(desc(escanteios_previstos)) %>% 
  as_tibble()

resultados_primeiro_gol <- cbind(dataset_teste_primeiro_gol,
                                 chance_primeiro_gol_prevista = round(predict(modelo_primeiro_gol, newdata = dataset_teste_primeiro_gol, type = "response"), digits = 4))

resultados_primeiro_gol <- resultados_primeiro_gol %>%
  select(time, chance_primeiro_gol_prevista) %>%
  arrange(desc(chance_primeiro_gol_prevista)) %>%
  as_tibble()
