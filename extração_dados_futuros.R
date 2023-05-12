setwd("C:/Users/itamar.oliveira/Desktop/PESSOAL/UNIVESP/PROJETO_INTEGRADOR_IV/dados_manaciais")

library(rvest)
library(tidyverse)


#### BOT para extração de dados temperatura e chuva para 7 dias #######################
webpage = read_html("https://www.tempo.com/piracaia.htm")

data = c(NA)
chuva = c(NA)
temperatura = c(NA)

dadosAtualizados <- data.frame(data, chuva, temperatura)
df_temp <- data.frame(data, chuva, temperatura)
count = 1


for (i in 2:7){
  data = html_node(webpage, paste0(".dos-semanas .dia.d", i, " .cuando"))
  data2 = html_text(data)
  df_temp[count, 1] = data2
  
  chuva = html_node(webpage, paste0(".dos-semanas .dia.d", i, " .prediccion .probabilidad-lluvia"))
  chuva2 = html_text(chuva)
  df_temp[count, 2] = chuva2
  
  
  temperatura = html_node(webpage, paste0(".dos-semanas .dia.d", i, " .temperatura .maxima"))
  temperatura2 = html_text(temperatura)
  df_temp[count, 3] = temperatura2
  
  count = count+1
}

webpage2 = read_html("https://www.tempo.com/piracaia.htm?d=proxima-semana")

data = c(NA)
chuva = c(NA)
temperatura = c(NA)

dadosAtualizados2 <- data.frame(data, chuva, temperatura)
df_temp2 <- data.frame(data, chuva, temperatura)
count = 1


for (i in 8:14){
  data = html_node(webpage2, paste0(".dia.d", i, " .cuando"))
  data2 = html_text(data)
  df_temp2[count, 1] = data2
  
  chuva = html_node(webpage2, paste0(".dia.d", i, " .prediccion .probabilidad-lluvia"))
  chuva2 = html_text(chuva)
  df_temp2[count, 2] = chuva2
  
  
  temperatura = html_node(webpage2, paste0(".dia.d", i, " .temperatura .maxima"))
  temperatura2 = html_text(temperatura)
  df_temp2[count, 3] = temperatura2
  
  count = count+1
}

dadosAtualizados <- rbind(df_temp, df_temp2)

dadosAtualizados[ , 1] <- str_replace(dadosAtualizados[ , 1], "Amanhã", "")
dadosAtualizados[ , 1] <- str_replace(dadosAtualizados[ , 1], "Domingo", "")
dadosAtualizados[ , 1] <- str_replace(dadosAtualizados[ , 1], "Segunda", "")
dadosAtualizados[ , 1] <- str_replace(dadosAtualizados[ , 1], "Terça", "")
dadosAtualizados[ , 1] <- str_replace(dadosAtualizados[ , 1], "Quarta", "")
dadosAtualizados[ , 1] <- str_replace(dadosAtualizados[ , 1], "Quinta", "")
dadosAtualizados[ , 1] <- str_replace(dadosAtualizados[ , 1], "Sexta", "")
dadosAtualizados[ , 1] <- str_replace(dadosAtualizados[ , 1], "Sábado", "")


dadosAtualizados <- dadosAtualizados %>% 
  separate(data, into = c("dia", "mes")) %>% 
  separate(chuva, into = c("probablidade", "chuva"), sep = "%")

dadosAtualizados$mes <- ifelse(dadosAtualizados$mes=="Maio", 5, dadosAtualizados$mes)
dadosAtualizados$ano <- 2023
dadosAtualizados <- dadosAtualizados %>% 
  unite(data, ano, mes, dia, sep = "-") %>% 
  mutate(data = as.Date(data))


dadosAtualizados$chuva <- as.numeric(str_replace(dadosAtualizados$chuva, " mm", ""))
dadosAtualizados$probablidade <- as.numeric(dadosAtualizados$probablidade)
dadosAtualizados$temperatura <- as.numeric(str_replace(dadosAtualizados$temperatura, "°", ""))


#####Junção de bases histórica e futura
library(readxl)
dados_PI4_historico <-read_excel("dados_PI4_historico.xlsx")

names(dadosAtualizados) <- c("data", "probabilidade", "pluviometria", "temperaturaMedia")
names(dados_PI4_historico)

baseUnica <- full_join(dados_PI4_historico, dadosAtualizados[ ,c(1, 3, 4)], by = "data")
str(baseUnica)

baseUnica$pluviometria.x <- ifelse(is.na(baseUnica$pluviometria.x), baseUnica$pluviometria.y, baseUnica$pluviometria.x)
baseUnica$pluviometria.x <- ifelse(is.na(baseUnica$pluviometria.x), 0, baseUnica$pluviometria.x)

baseUnica$temperaturaMedia.x <- ifelse(is.na(baseUnica$temperaturaMedia.x), baseUnica$temperaturaMedia.y, baseUnica$temperaturaMedia.x)


names(baseUnica)
baseUnica <- baseUnica[ ,c(1:9)]
baseUnica <- rename(baseUnica, "pluviometria"="pluviometria.x")
baseUnica <- rename(baseUnica, "temperaturaMedia"="temperaturaMedia.x")
baseUnica$diferenca <- as.numeric(baseUnica$diferenca)
baseUnica$medMov.pluviometria <- as.numeric(baseUnica$medMov.pluviometria)
baseUnica$medMov.diferenca <- as.numeric(baseUnica$medMov.diferenca)
baseUnica$medMov.temperatura <- as.numeric(baseUnica$medMov.temperatura)

library(zoo)

tail(baseUnica)

baseUnica <- baseUnica %>% 
  mutate(medMov.pluviometria = rollmean(pluviometria, 15, fill = NA, align = "right"),
         medMov.temperatura = rollmean(temperaturaMedia, 15, fill = NA, align = "right"))

names(baseUnica)

#### MACHINE LEARNING

#biblioteca de random forest
library(randomForest)
library(caret)
library(MLmetrics) #medida de acurácia

dados_rf = na.omit(baseUnica[basePredita$data<"2023-05-05",-c(1:6)])


set.seed(345) 
flag=createDataPartition(dados_rf$medMov.diferenca, p=0.9, list = F)
df_treinamento = dados_rf[flag,]; dim(df_treinamento)
df_teste = dados_rf[-flag,]; dim(df_teste)

regressor_rf = randomForest(medMov.diferenca~., data = df_treinamento, ntree = 40, mtry=2, importance = TRUE)

regressor_rf; plot(regressor_rf)

prev_rf = predict(regressor_rf, newdata = df_teste)

MLmetrics::R2_Score(prev_rf, df_teste$medMov.diferenca)
MLmetrics::MAE(prev_rf, df_teste$medMov.diferenca)

predict = predict(regressor_rf, newdata = baseUnica[ ,-c(1:6)])

baseUnica$predito <- predict


#### Ajustando a base final
baseUnica <- baseUnica[ , 2:10]

baseUnica2 <- baseUnica[baseUnica$data>"2000-01-16",]

baseUnica2$volumeOperacionalPredito <- NA
names(baseUnica2)
baseUnica2[1, 10] <- baseUnica2[1, 2]

for (i in seq.int(2,nrow(baseUnica2))) {
  baseUnica2[i, 10] = baseUnica2[i-1, 10] + baseUnica2[i-1, 9]
}

basePredita1 %>% 
  select(data, volumeOperacional, volumeOperacionalPredito) %>% 
  mutate(data = ymd(data)) %>% 
  pivot_longer(cols = c(volumeOperacional, volumeOperacionalPredito), 
               names_to = "variaveis", values_to = "valores") %>% 
  ggplot() +
  geom_line(aes(x = data, y = valores, color = variaveis))+
  theme_minimal()+
  theme(legend.title = element_blank())+
  labs(x = NULL,
       y = "Volume Armazenado (em HM3)",
       title = "Comparação entre volume real e predito",
       subtitle = "Coincidênca nos padrões, embora não nos valores")
  

write.csv(baseUnica2, "basePredita.csv")

basePredita1 <- read.csv("basePredita.csv")

names(basePredita)

basePredita1 <- basePredita1[ ,c(2:10)]

basePredita <- basePredita[basePredita$data>"2023-05-04",]

names(basePredita1)
tail(basePredita, 20)

basePredita1$volumeOperacionalPredito <- NA

basePredita1$volumeOperacionalPredito[1] <- basePredita1$volumeOperacional[1]

for (i in seq.int(2,nrow(basePredita1))) {
  basePredita1[i, 10] = basePredita1[i-1, 10] + basePredita1[i-1, 9]
}

basePredita3 <- rbind(basePredita1[basePredita1$data<"2023-05-05",], basePredita)

view(tail(as_tibble(basePredita3), 20))

basePredita3$volumeOperacionalPredito <- ifelse(basePredita3$data>"2023-05-05", basePredita3$volumeOperacional, NA)
basePredita3$volumeOperacional <- ifelse(basePredita3$data<="2023-05-06", basePredita3$volumeOperacional, NA)

#write.csv(basePredita3, "basePredita2.csv", row.names = F)

basePredita3 %>% 
  select(data, temperaturaMedia, pluviometria, diferenca) %>% 
  mutate(data = ymd(data),
         temperaturaMedia = scale(temperaturaMedia),
         pluviometria = scale(pluviometria), 
         diferenca = scale(diferenca)) %>% 
  filter(data>"2018-01-01") %>% 
  pivot_longer(cols = c(2:4), names_to = "variaveis", values_to = "valores") %>% 
  ggplot() +
  geom_line(aes(x = data, y = valores, color = variaveis))+
  scale_color_manual(breaks = c("temperaturaMedia", "diferenca", "pluviometria"),
                     values = c("red2", "green2", "blue2"))+
  theme_minimal()+
  labs(x = NULL,
       y = "valores normalizados",
       title = "Comparação das variáveis por valores diários",
       subtitle = "Recorte de tempo > 2017 (para melhor visualização)")



basePredita3 %>% 
  select(data, medMov.diferenca, medMov.pluviometria, medMov.temperatura) %>% 
  mutate(data = ymd(data),
         medMov.temperatura = scale(medMov.temperatura),
         medMov.pluviometria = scale(medMov.pluviometria), 
         medMov.diferenca = scale(medMov.diferenca)) %>% 
  filter(data>"2018-01-01") %>% 
  pivot_longer(cols = c(2:4), names_to = "variaveis", values_to = "valores") %>% 
  ggplot() +
  geom_line(aes(x = data, y = valores, color = variaveis))+
  scale_color_manual(breaks = c("medMov.temperatura", "medMov.diferenca", "medMov.pluviometria"),
                     values = c("red2", "green2", "blue2"))+
  theme_minimal()+
  labs(x = NULL,
       y = "valores normalizados",
       title = "Comparação das variáveis por média móvel 15 dias",
       subtitle = "Recorte de tempo > 2017 (para melhor visualização)")

library(gridExtra)



plotVolume <- basePredita3 %>% 
  #select(data, temperaturaMedia, pluviometria, diferenca) %>% 
  mutate(data = ymd(data)) %>% 
  ggplot() +
  geom_line(aes(x = data, y = volumeOperacional), color = "green3")+
  theme_minimal()+
  labs(x = NULL,
       y = "Volume Armazenado em HM3",
       title = "Histórico de volumes armazenados")

plotTemperatura <- basePredita3 %>% 
  #select(data, temperaturaMedia, pluviometria, diferenca) %>% 
  mutate(data = ymd(data)) %>% 
  ggplot() +
  geom_line(aes(x = data, y = medMov.temperatura), color = "orange2")+
  theme_minimal()+
  labs(x = NULL,
       y = "Temperatura em ºC",
       title = "Histórico de temperaturas ambientes")

plotChuva <- basePredita3 %>% 
  #select(data, temperaturaMedia, pluviometria, diferenca) %>% 
  mutate(data = ymd(data)) %>% 
  ggplot() +
  geom_line(aes(x = data, y = medMov.pluviometria), color = "blue3")+
  theme_minimal()+
  labs(x = NULL,
       y = "Precipitação em mm",
       title = "Histórico de precipitação")


grid.arrange(plotTemperatura, plotChuva, ncol=2)


basePredita3 %>% 
  mutate(data=ymd(data)) %>% 
  filter(data > "2023-04-18") %>% 
  ggplot() +
  geom_line(aes(x = data, y = volumeOperacional), color = "green3")+
  geom_line(aes(x = data, y = volumeOperacionalPredito), color = "red3")




#arvore de regressao
library(rpart)
library(rpart.plot)
library(caret)

names(basePredita3)

basePredita3$data <- ymd(basePredita3$data)


dados_ad = basePredita3[,-c(1:5, 9:10)]
dados_ad <- na.omit(dados_ad)

set.seed(123)
flag=createDataPartition(dados_ad$medMov.diferenca, p=0.8, list = F)
df_treinamento = dados_ad[flag,]; dim(df_treinamento)
df_teste = dados_ad[-flag,]; dim(df_teste)

regressor_ad = rpart(medMov.diferenca~., data = df_treinamento)
rpart.plot(regressor_ad)

printcp(regressor_ad) #verificar a necessidade de poda

prev_ad = predict(regressor_ad, newdata = df_teste)

MLmetrics::R2_Score(prev_ad, df_teste$medMov.diferenca) #0.71
MLmetrics::MAE(prev_ad, df_teste$medMov.diferenca) #0.72

