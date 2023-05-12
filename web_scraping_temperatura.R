# Atribuição de diretório

setwd("C:/Users/itamar.oliveira/Desktop/PESSOAL/UNIVESP/PROJETO_INTEGRADOR_IV/dados_manaciais")

#'Biblioteca

library(tidyverse)
library(rvest)
library(lubridate)


###### COLETA DE DADOS DA SÉRIE HISTÓRICA DE TEMPERATURA NA CAPITAL #############################


#' Utilizaremos o site www.tempo.com (não há restrições)

#' O site se estrutura da seguinte forma: ao selecionar um local (apenas capitais)
#' o site direciona para uma request retornando os dados que podem ser diários, mensais, anuais
#' Na request, a url mantém um texto padrão, com alterações de código da estação, dia, mês e ano
#' para consultas históricas.
#' Há histórico desde 1980, aproximadamente
#' Não há dados por município, mas por estação metereológica
#' Para São Paulo, três estações: Congonhas, Campo de Marte, Guarulhos (os aeroportos locais)
#'       
#'       1. Aeroporto São Paulo - Campo de Marte 
#'          código da estação: 571e07b8c76c49177837d4fe
#'          latitude -23,509119
#'          longitude. -46,637753
#'          
#'       2. Aeroporto São Paulo - Guarulhos
#'          código da estação: 571e07b8c76c49177837d523
#'          latitude -23,432333
#'          longitude -46,469677
#'          
#'       3. Aeroporto São Paulo - Congonhas
#'          código da estação: 571e07b8c76c49177837d4e1
#'          latitude -23,626694
#'          longitude -46,655376




#' Base de datas desejadas
datas_dia    <- seq(from=as.Date("2000-01-01"), to=as.Date("2023-03-25"), by="day")

#' lista com códigos das estações metereológicas
estacao_meter <- c("571e07b8c76c49177837d4fe","571e07b8c76c49177837d523","571e07b8c76c49177837d4e1")

#' criação de dataframe para agregar dados do loop
temperatura <- data.frame()

#' criação de dataframe para dados temporários do loop
df_temp <- data.frame()

#' vamos coletar temperaturas mínimas, máximas e médias por dia da série
for(j in estacao_meter){
  for(i in datas_dia) {
    data_dia_temp <- as.Date(i, origin = "1970-01-01")
    url_tempo <- paste0("https://www.tempo.com/peticiones/historico.php?id_estacion=", j,"&accion=DIA&dia=", as.character(format(data_dia_temp, "%d")), "&mes=", 
                        as.character(format(data_dia_temp, "%m")), "&anno=", as.character(format(data_dia_temp, "%Y")))
    gru_tempo <- httr::GET(url_tempo)
    results_temp <- as.character(httr::content(gru_tempo, simplifyDataFrame = TRUE))
    data <- data_dia_temp
    estacao <- ifelse(j == "571e07b8c76c49177837d4fe", "Campo de Marte", 
                      ifelse (j == "571e07b8c76c49177837d523", "Guarulhos", 
                              ifelse(j == "571e07b8c76c49177837d4e1", "Congonhas", "outro")))
    long <- ifelse(j == "571e07b8c76c49177837d4fe", "-46,637753", 
                   ifelse (j == "571e07b8c76c49177837d523", "-46,469677", 
                           ifelse(j == "571e07b8c76c49177837d4e1", "-46,655376", "outro")))
    lat <- ifelse(j == "571e07b8c76c49177837d4fe", "-23,509119", 
                  ifelse (j == "571e07b8c76c49177837d523", "-23,432333", 
                          ifelse(j == "571e07b8c76c49177837d4e1", "-23,626694", "outro")))
    ano <- str_extract_all((str_extract_all(results_temp, "anno\":[[:digit:]]{1,}")), "[[:digit:]]{1,}")
    mes <- str_extract_all((str_extract_all(results_temp, "mes\":[[:digit:]]{1,}")), "[[:digit:]]{1,}")
    dia <- str_extract_all((str_extract_all(results_temp, "\"dia\":[[:digit:]]{1,}")), "[[:digit:]]{1,}")
    temp_max <- str_extract_all((str_extract_all(results_temp, "temp_max\":[[:digit:]]{1,}")), "[[:digit:]]{1,}")
    temp_min <- str_extract_all((str_extract_all(results_temp, "temp_min\":[[:digit:]]{1,}")), "[[:digit:]]{1,}")
    temp_med <- str_extract_all((str_extract_all(results_temp, "temp_media\":[[:digit:]]{1,}")), "[[:digit:]]{1,}")
    df_temp <- data.frame(data, estacao, long, lat, ano, mes, dia, temp_max, temp_min, temp_med)
    df_temp <- unique(df_temp)
    names(df_temp) <- c("data", "estacao", "long", "lat", "ano", "mes", "dia", "temp_max", "temp_min", "temp_med")
    temperatura <- rbind(temperatura, df_temp)
    print(paste(estacao, as.Date(i, origin = "1970-01-01"), sep = " - "))
  }
}

write.csv(temperatura, "temperatura.csv", sep = ";", row.names = FALSE)


######### ATUALIZAÇÃO DA BASE TEMPERATURA #############################################################################

#' O custo computacional do web scraping é elevado. Gasta-se várias horas para extração completa da base.
#' Dessa forma, atualizações devem ser feitas utilizando o código abaixo que apenas incrementará as
#' informações ausentes.

#' Importando última versão da base temperatura
temperatura <- read.csv("temperatura.csv")

#' Base de datas desejadas
datas_dia <- seq(from=as.Date(max(temperatura$data)+1), to=as.Date(Sys.Date()), by="day")

#' lista com códigos das estações metereológicas
estacao_meter <- c("571e07b8c76c49177837d4fe","571e07b8c76c49177837d523","571e07b8c76c49177837d4e1")

#' criação de dataframe para agregar dados do loop
temperatura_atual <- data.frame()

#' criação de dataframe para dados temporários do loop
df_temp <- data.frame()

#' vamos coletar temperaturas mínimas, máximas e médias por dia da série
for(j in estacao_meter){
  for(i in datas_dia) {
    data_dia_temp <- as.Date(i, origin = "1970-01-01")
    url_tempo <- paste0("https://www.tempo.com/peticiones/historico.php?id_estacion=", j,"&accion=DIA&dia=", as.character(format(data_dia_temp, "%d")), "&mes=", 
                        as.character(format(data_dia_temp, "%m")), "&anno=", as.character(format(data_dia_temp, "%Y")))
    gru_tempo <- httr::GET(url_tempo)
    results_temp <- as.character(httr::content(gru_tempo, simplifyDataFrame = TRUE))
    data <- data_dia_temp
    estacao <- ifelse(j == "571e07b8c76c49177837d4fe", "Campo de Marte", 
                      ifelse (j == "571e07b8c76c49177837d523", "Guarulhos", 
                              ifelse(j == "571e07b8c76c49177837d4e1", "Congonhas", "outro")))
    long <- ifelse(j == "571e07b8c76c49177837d4fe", "-46,637753", 
                   ifelse (j == "571e07b8c76c49177837d523", "-46,469677", 
                           ifelse(j == "571e07b8c76c49177837d4e1", "-46,655376", "outro")))
    lat <- ifelse(j == "571e07b8c76c49177837d4fe", "-23,509119", 
                  ifelse (j == "571e07b8c76c49177837d523", "-23,432333", 
                          ifelse(j == "571e07b8c76c49177837d4e1", "-23,626694", "outro")))
    ano <- str_extract_all((str_extract_all(results_temp, "anno\":[[:digit:]]{1,}")), "[[:digit:]]{1,}")
    mes <- str_extract_all((str_extract_all(results_temp, "mes\":[[:digit:]]{1,}")), "[[:digit:]]{1,}")
    dia <- str_extract_all((str_extract_all(results_temp, "\"dia\":[[:digit:]]{1,}")), "[[:digit:]]{1,}")
    temp_max <- str_extract_all((str_extract_all(results_temp, "temp_max\":[[:digit:]]{1,}")), "[[:digit:]]{1,}")
    temp_min <- str_extract_all((str_extract_all(results_temp, "temp_min\":[[:digit:]]{1,}")), "[[:digit:]]{1,}")
    temp_med <- str_extract_all((str_extract_all(results_temp, "temp_media\":[[:digit:]]{1,}")), "[[:digit:]]{1,}")
    df_temp <- data.frame(data, estacao, long, lat, ano, mes, dia, temp_max, temp_min, temp_med)
    df_temp <- unique(df_temp)
    names(df_temp) <- c("data", "estacao", "long", "lat", "ano", "mes", "dia", "temp_max", "temp_min", "temp_med")
    temperatura_atual <- rbind(temperatura_atual, df_temp)
    print(paste(estacao, as.Date(i, origin = "1970-01-01"), sep = " - "))
  }
}

#' união das bases
temperatura <- rbind(temperatura, temperatura_atual)

#salvando nova versão atualizada
write.csv(temperatura, file = "temperatura_2.csv", sep = ";")