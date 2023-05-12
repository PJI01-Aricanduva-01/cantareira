setwd("C:/Users/itamar.oliveira/Desktop/PESSOAL/UNIVESP/PROJETO_INTEGRADOR_IV/dados_manaciais")

#biblioteca
library(tidyverse)
library(readxl)
library(zoo)

list.files()

dados <- as_tibble(read_excel("dados_PI4_historico.xlsx"))

#Ajustes na base de dados

str(dados)
dados$data <- ymd(dados$data)
dados$diferenca <- as.numeric(dados$diferenca)
dados$medMov.pluviometria <- as.numeric(dados$medMov.pluviometria)
dados$medMov.diferenca <- as.numeric(dados$medMov.diferenca)
dados$medMov.temperatura <- as.numeric(dados$medMov.temperatura)

dados <- na.omit(dados)


dados <- dados[, c(2, 3, 4, 5, 6)]


names(dados) <- c("data", "volume", "chuva", "temp.max", "diff")


dados2 <- dados %>% 
  #filter(data>"2016-12-31") %>% 
  mutate(medMov.chuva = rollmean(chuva, 15, fill = NA, align = "right"),
         medMov.diff = rollmean(diff, 15, fill = NA, align = "right"),
         medMov.temp = rollmean(temp.max, 15, fill = NA, align = "right"))


dados2 <- na.omit(dados2)


#Biblioteca regressao
library(caTools)
library(car)


set.seed(1)
divisao = sample.split(dados2$medMov.diff, SplitRatio = 0.70)
df_treinamento = subset(dados2, divisao==TRUE)
df_teste = subset(dados2, divisao==FALSE)


regressor = lm(formula = scale(medMov.diff) ~ scale(medMov.chuva)+scale(medMov.temp),
               data = df_treinamento)



summary(regressor)


previsoes = predict(regressor, newdata = df_teste[-1])#realizar previsões

mean(abs(df_teste[["medMov.diff"]]-previsoes), na.rm = T)#Calcular o MAE (Erro absoluto médio)


#biblioteca GAM
library(mgcv)

set.seed(1)
divisao = sample.split(dados2$medMov.diff, SplitRatio = 0.70)
df_treinamento = subset(dados2, divisao==TRUE)
df_teste = subset(dados2, divisao==FALSE)


regressor = gam(formula = scale(medMov.diff) ~ scale(medMov.chuva),
                data = df_treinamento, family = gaussian, method = "REML")


previsoes = predict(regressor, newdata = df_teste[-1])

#rSquared(df_teste[["medMov.diff"]], resid = df_teste[["medMov.diff"]]-previsoes)
mean(abs(df_teste[["medMov.diff"]]-previsoes), na.rm = T)#Calcular o MAE (Erro absoluto médio)

summary(regressor)

par(mfrow=c(2,2))
plot(regressor)
gam.check(regressor)




#arvore de regressao
library(rpart)
library(rpart.plot)

dados_ad = dados2[,-c(1:5)]

set.seed(123)
flag=createDataPartition(dados_ad$medMov.diff, p=0.8, list = F)
df_treinamento = dados_ad[flag,]; dim(df_treinamento)
df_teste = dados_ad[-flag,]; dim(df_teste)

regressor_ad = rpart(medMov.diff~., data = df_treinamento)
rpart.plot(regressor_ad)

printcp(regressor_ad) #verificar a necessidade de poda

prev_ad = predict(regressor_ad, newdata = df_teste)

MLmetrics::R2_Score(prev_ad, df_teste$medMov.diff) #0.71
MLmetrics::MAE(prev_ad, df_teste$medMov.diff) #0.72



#biblioteca de random forest
library(randomForest)
library(caret)

dados_rf = dados2[,-c(1:5)]

set.seed(123) 
flag=createDataPartition(dados_rf$medMov.diff, p=0.9, list = F)
df_treinamento = dados_rf[flag,]; dim(df_treinamento)
df_teste = dados_rf[-flag,]; dim(df_teste)

regressor_rf = randomForest(medMov.diff~., data = df_treinamento, ntree = 40, mtry=4, importance = TRUE)

regressor_rf; plot(regressor_rf)

prev_rf = predict(regressor_rf, newdata = df_teste)

library(MLmetrics) #medida de acurácia

MLmetrics::R2_Score(prev_rf, df_teste$medMov.diff)
MLmetrics::MAE(prev_rf, df_teste$medMov.diff)



#gradiente boosting
library(gbm)

dados_gb = dados2[,-c(1:5)]

set.seed(123) 
flag=createDataPartition(dados_gb$medMov.diff, p=0.8, list = F)
df_treinamento = dados_gb[flag,]; dim(df_treinamento)
df_teste = dados_gb[-flag,]; dim(df_teste)

regressor_gb = gbm(data = df_treinamento, medMov.diff~.,
                   n.trees = 80,
                   interaction.depth = 2, shrinkage = 0.1,
                   bag.fraction = 0.80, cv.folds = 5)

print(regressor_gb)
summary(regressor_gb)

prev_gb = predict(regressor_gb, newdata = df_teste)

MLmetrics::R2_Score(prev_gb, df_teste$medMov.diff)#0.76
MLmetrics::MAE(prev_gb, df_teste$medMov.diff) #0.67



#biblioteca de redes neurais

library(h2o)

dados_rna = dados2[,-c(1:5)]

set.seed(123) 
flag=createDataPartition(dados_rna$medMov.diff, p=0.8, list = F)
df_treinamento = dados_rna[flag,]; dim(df_treinamento)
df_teste = dados_rna[-flag,]; dim(df_teste)

h2o.init(nthreads = -1)#para rodar o modelo na nuvem

regressor_rna = h2o.deeplearning(y = "medMov.diff", training_frame = as.h2o(df_treinamento), activation = "Rectifier", hidden = c(50, 50, 10), epochs = 700, nfolds = 5)

regressor_rna
plot(regressor_rna)