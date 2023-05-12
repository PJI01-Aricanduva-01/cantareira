#ARQUIVO PARA WEB SCRAPING DA BASE DE DADOS DE TODOS OS MANACIAIS DA RMSP

# Atribuição de diretório

setwd("C:/Users/itamar.oliveira/Desktop/PESSOAL/UNIVESP/PROJETO_INTEGRADOR_IV/dados_manaciais")

#'Biblioteca

library(tidyverse)
library(rvest)
library(lubridate)


##############################  COLETA DE DADOS ############################


#' Uso de web scraping com API disponibilizado pelo site
#' Arquivos em JSON

#criando base final limpa
dados_sistema <- data.frame() 

#criando base temporária para for
df_temp <- data.frame() 

#criando lista com períodos (para evitar sobrecarga no site)
periodos <- list(list(2000, 2005), list(2006, 2010), list(2011, 2015), list(2016, 2020), list(2021, 2023))

#criando lista com os códigos dos mananciais
sistemas <- c(0, 1, 2, 3, 4, 5, 17)

#estabelecendo o loop para extração
for (i in sistemas) {
  for (j in seq(1, 5)) {
    u_sabesp <- paste0("http://mananciais.sabesp.com.br/api/Mananciais/RepresasSistemasNivel/", periodos[[j]][[1]], "-01-01/", periodos[[j]][[2]], "-12-31/", i)
    r_sabesp <- httr::GET(u_sabesp)
    results <- httr::content(r_sabesp, simplifyDataFrame = TRUE)
    df_temp <- results$ReturnObj$ListaDadosSistema$objSistema
    df_temp$SistemaId <- ifelse(df_temp$SistemaId==0, "Cantareira",
                                ifelse(df_temp$SistemaId==1, "Alto Tiete",
                                       ifelse(df_temp$SistemaId==2, "Guarapiranga",
                                              ifelse(df_temp$SistemaId==3, "Cotia",
                                                     ifelse(df_temp$SistemaId==4, "Rio Grande",
                                                            ifelse(df_temp$SistemaId==5, "Rio Claro",
                                                                   ifelse(df_temp$SistemaId==17, "são Lourenço", "Outro")))))))
    df_temp$Data <- ymd(str_extract_all(df_temp$Data, "^[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}"))
    dados_sistema <- rbind(dados_sistema, df_temp)
    print(paste(i, periodos[[j]], sep = "-"))
  }
}

#salvando o arquivo em csv
write.csv(dados_sistema, "dados_sistemas_sabesp.csv", sep = ";", row.names = FALSE)



######################  ATUALIZAÇÃO DA BASE DE DADOS ############################

#' A criação da base de dados deve ser feita apenas uma vez.
#' Para as atualizações, deve-se usar o código abaixo para reduzir o
#' custo computacional.

#' importar o arquivo completo
dados_sistema <- as_tibble(read.csv("dados_sistemas_sabesp.csv"))

max(dados_sistema$Data)+1

#'cria lista de datas que não constam na última versão do arquivo
data_dia <- max(ymd(str_extract_all(as.character(dados_sistema$Data), "^[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}")))+1


#' cria um data frame para atualização
dados_sistema_atual <- data.frame()

#' cria um data frame temporário
df_temp <- data.frame()

#' loop para atualização
u_sabesp <- paste0("http://mananciais.sabesp.com.br/api/Mananciais/RepresasSistemasNivel/", data_dia, "/", Sys.Date(), "/", "0")
r_sabesp <- httr::GET(u_sabesp)
results <- httr::content(r_sabesp, simplifyDataFrame = TRUE)
dados_sistema_atual <- results$ReturnObj$ListaDadosSistema$objSistema

#' união das bases
dados_sistema <- rbind(dados_sistema_atual, dados_sistema)

#salvando nova versão atualizada
write.csv(dados_sistema, file = "dados_sistema_sabesp.csv", sep = ";", row.names = FALSE)
