#' Author: Itamar Ap. Oliveira
#' Subject: Painel Cantareira

setwd("C:/Users/itamar.oliveira/Desktop/PESSOAL/UNIVESP/PROJETO_INTEGRADOR_IV/dados_manaciais")


library(tidyverse)
library(zoo)
library(lubridate)
library(ggpubr)


# Import -----------------------------------------------------------------------
cantareira <- as_tibble(read.csv("dados_PI4.csv"))

cantareira <- cantareira[cantareira$SistemaId=="Cantareira",]
cantareira$Data <- ymd(cantareira$Data)
cantareira <- rename(cantareira, data = Data)

# Tidy -------------------------------------------------------------------------


# Visualize --------------------------------------------------------------------
dias <- 30 # periodo desejado para janela movel

cantareira$medMov.VolAcum <- rollmean(cantareira$VolumeOperacionalHm3, dias, fill = NA, align = "right")
cantareira$medMov.VolPerc <- rollmean(cantareira$VolumePorcentagem, dias, fill = NA, align = "right")
#cantareira$medMov.VazRet <- rollmean(cantareira$VazaoRetirada, dias, fill = NA, align = "right")
#cantareira$medMov.VazAflu <- rollmean(cantareira$VazaoAfluente, dias, fill = NA, align = "right")
cantareira$medMov.Precip <- rollmean(cantareira$Precipitacao, dias, fill = NA, align = "right")
cantareira$medMov.tempMax <- rollapply(cantareira$temp_max, dias, mean, fill = NA, align = "right")

#' dia e mês que se deseja comparar entre os anos do histórico
dia <- 25
mês <- 03
alpha <- 0.4 #palides da linha vertica

#'gráfico volume em percentual
cantareira.plot.perc <- cantareira %>%
  dplyr::mutate(ano = format(data, "%Y")) %>% 
  dplyr::group_by(ano, data) %>% 
  ggplot(aes(x = data))+
  annotate("rect", 
           xmin = min(cantareira$data), 
           xmax = max(cantareira$data), 
           ymin = 60, 
           ymax = 100, 
           fill = "green4",
           alpha = 0.2) +
  annotate("rect", 
           xmin = min(cantareira$data), 
           xmax = max(cantareira$data), 
           ymin = 40, 
           ymax = 60, 
           fill = "yellow",
           alpha = 0.2) +
  annotate("rect", 
           xmin = min(cantareira$data), 
           xmax = max(cantareira$data), 
           ymin = 30, 
           ymax = 40, 
           fill = "orange",
           alpha = 0.2) +
  annotate("rect", 
           xmin = min(cantareira$data), 
           xmax = max(cantareira$data), 
           ymin = 20, 
           ymax = 30, 
           fill = "red4",
           alpha = 0.2) +
  annotate("rect", 
           xmin = min(cantareira$data), 
           xmax = max(cantareira$data), 
           ymin = -Inf, 
           ymax = 20, 
           fill = "gray4",
           alpha = 0.2) +
  geom_line(aes(y = medMov.VolPerc))+
  geom_vline(aes(xintercept = as.numeric(as.Date(paste((as.numeric(format(Sys.Date(), "%Y"))-5), mês, dia, sep = "-" )))), color = "red", alpha = alpha) +
  geom_vline(aes(xintercept = as.numeric(as.Date(paste((as.numeric(format(Sys.Date(), "%Y"))-4), mês, dia, sep = "-" )))), color = "red", alpha = alpha) +
  geom_vline(aes(xintercept = as.numeric(as.Date(paste((as.numeric(format(Sys.Date(), "%Y"))-3), mês, dia, sep = "-" )))), color = "red", alpha = alpha) +
  geom_vline(aes(xintercept = as.numeric(as.Date(paste((as.numeric(format(Sys.Date(), "%Y"))-2), mês, dia, sep = "-" )))), color = "red", alpha = alpha) +
  geom_vline(aes(xintercept = as.numeric(as.Date(paste((as.numeric(format(Sys.Date(), "%Y"))-1), mês, dia, sep = "-" )))), color = "red", alpha = alpha) +
  geom_vline(aes(xintercept = as.numeric(as.Date(paste((as.numeric(format(Sys.Date(), "%Y"))-0), mês, dia, sep = "-" )))), color = "red", alpha = alpha) +
  annotate("text", x = as.Date(paste(format(Sys.Date(), "%Y"), mês, dia, sep = "-" )), 
           y = 0, angle = 90, label = paste0(dia, "-", mês, "-", format(Sys.Date(), "%Y")), 
           vjust = -0.1, hjust = 0.5, size = 3)+
  geom_hline(aes(yintercept = medMov.VolPerc[nrow(cantareira)]), color = "green4")+
  labs(title="Série histórica de volume operacional (%) no manancial Cantareira (2000 - 2023)",
       subtitle = "Média móvel de 30 dias")+
  xlab(NULL) +
  ylab("Percetual") +
  scale_x_date(date_labels = "%Y", 
               breaks = "1 year",
               date_minor_breaks = "1 year")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 75, 
                                   vjust = 0.2,
                                   hjust = 0.1)) +
  theme(panel.grid.minor.y = element_blank())+
  theme(legend.position = "bottom")


#gráfico precipitação ---------------------------------------------------
precipitacao.plot <- cantareira %>%
  dplyr::mutate(ano = format(data, "%Y")) %>% 
  dplyr::group_by(ano, data) %>% 
  ggplot(aes(x = data))+
  geom_line(aes(y = medMov.Precip), color = "blue")+
  geom_vline(aes(xintercept = as.numeric(as.Date(paste((as.numeric(format(Sys.Date(), "%Y"))-5), mês, dia, sep = "-" )))), color = "red", alpha = alpha) +
  geom_vline(aes(xintercept = as.numeric(as.Date(paste((as.numeric(format(Sys.Date(), "%Y"))-4), mês, dia, sep = "-" )))), color = "red", alpha = alpha) +
  geom_vline(aes(xintercept = as.numeric(as.Date(paste((as.numeric(format(Sys.Date(), "%Y"))-3), mês, dia, sep = "-" )))), color = "red", alpha = alpha) +
  geom_vline(aes(xintercept = as.numeric(as.Date(paste((as.numeric(format(Sys.Date(), "%Y"))-2), mês, dia, sep = "-" )))), color = "red", alpha = alpha) +
  geom_vline(aes(xintercept = as.numeric(as.Date(paste((as.numeric(format(Sys.Date(), "%Y"))-1), mês, dia, sep = "-" )))), color = "red", alpha = alpha) +
  geom_vline(aes(xintercept = as.numeric(as.Date(paste((as.numeric(format(Sys.Date(), "%Y"))-0), mês, dia, sep = "-" )))), color = "red", alpha = alpha) +
  annotate("text", x = as.Date(paste(format(Sys.Date(), "%Y"), mês, dia, sep = "-" )), 
           y = 0, angle = 90, label = paste0(dia, "-", mês, "-", format(Sys.Date(), "%Y")), 
           vjust = -0.1, hjust = -2, size = 3)+
  geom_hline(aes(yintercept = medMov.Precip[nrow(cantareira)]), color = "red4")+
  labs(title="Série histórica de precipitação (mm) no manancial Cantareira (2000 - 2023)",
       subtitle = "Média móvel de 30 dias")+
  xlab(NULL) +
  ylab("Precipitação em mm") +
  scale_x_date(date_labels = "%Y", 
               breaks = "1 year",
               date_minor_breaks = "1 year")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 75, 
                                   vjust = 0.2,
                                   hjust = 0.1)) +
  theme(panel.grid.minor.y = element_blank())+
  theme(legend.position = "bottom")

#'gráfico de temperatura


temperatura.plot <- cantareira %>%
  dplyr::mutate(ano = format(data, "%Y")) %>% 
  dplyr::group_by(ano, data) %>% 
  ggplot(aes(x = data))+
  geom_line(aes(y = medMov.tempMax), color = "orange4")+
  geom_vline(aes(xintercept = as.numeric(as.Date(paste((as.numeric(format(Sys.Date(), "%Y"))-5), mês, dia, sep = "-" )))), color = "red", alpha = alpha) +
  geom_vline(aes(xintercept = as.numeric(as.Date(paste((as.numeric(format(Sys.Date(), "%Y"))-4), mês, dia, sep = "-" )))), color = "red", alpha = alpha) +
  geom_vline(aes(xintercept = as.numeric(as.Date(paste((as.numeric(format(Sys.Date(), "%Y"))-3), mês, dia, sep = "-" )))), color = "red", alpha = alpha) +
  geom_vline(aes(xintercept = as.numeric(as.Date(paste((as.numeric(format(Sys.Date(), "%Y"))-2), mês, dia, sep = "-" )))), color = "red", alpha = alpha) +
  geom_vline(aes(xintercept = as.numeric(as.Date(paste((as.numeric(format(Sys.Date(), "%Y"))-1), mês, dia, sep = "-" )))), color = "red", alpha = alpha) +
  geom_vline(aes(xintercept = as.numeric(as.Date(paste((as.numeric(format(Sys.Date(), "%Y"))-0), mês, dia, sep = "-" )))), color = "red", alpha = alpha) +
  annotate("text", x = as.Date(paste(format(Sys.Date(), "%Y"), mês, dia, sep = "-" )), 
           y = 0, angle = 90, label = paste0(dia, "-", mês, "-", format(Sys.Date(), "%Y")), 
           vjust = -0.1, hjust = -2, size = 3)+
  geom_hline(aes(yintercept = medMov.tempMax[nrow(cantareira)]), color = "red4")+
  labs(title="Série histórica de temperatura (Celsius) na área de consumo (2000 - 2023)",
       subtitle = "Média móvel de 30 dias")+
  xlab(NULL) +
  ylab("Temperatura em C") +
  scale_x_date(date_labels = "%Y", 
               breaks = "1 year",
               date_minor_breaks = "1 year")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 75, 
                                   vjust = 0.2,
                                   hjust = 0.1)) +
  theme(panel.grid.minor.y = element_blank())+
  theme(legend.position = "bottom")




#gráfico vazao retirada ETA
vazaoRetirada.plot <- cantareira %>%
  dplyr::mutate(ano = format(data, "%Y")) %>% 
  dplyr::group_by(ano, data) %>% 
  ggplot(aes(x = data))+
  geom_line(aes(y = medMov.VazRet), color = "green4")+
  geom_vline(aes(xintercept = as.numeric(as.Date(paste((as.numeric(format(Sys.Date(), "%Y"))-5), mês, dia, sep = "-" )))), color = "red", alpha = alpha) +
  geom_vline(aes(xintercept = as.numeric(as.Date(paste((as.numeric(format(Sys.Date(), "%Y"))-4), mês, dia, sep = "-" )))), color = "red", alpha = alpha) +
  geom_vline(aes(xintercept = as.numeric(as.Date(paste((as.numeric(format(Sys.Date(), "%Y"))-3), mês, dia, sep = "-" )))), color = "red", alpha = alpha) +
  geom_vline(aes(xintercept = as.numeric(as.Date(paste((as.numeric(format(Sys.Date(), "%Y"))-2), mês, dia, sep = "-" )))), color = "red", alpha = alpha) +
  geom_vline(aes(xintercept = as.numeric(as.Date(paste((as.numeric(format(Sys.Date(), "%Y"))-1), mês, dia, sep = "-" )))), color = "red", alpha = alpha) +
  geom_vline(aes(xintercept = as.numeric(as.Date(paste((as.numeric(format(Sys.Date(), "%Y"))-0), mês, dia, sep = "-" )))), color = "red", alpha = alpha) +
  annotate("text", x = as.Date(paste(format(Sys.Date(), "%Y"), mês, dia, sep = "-" )), 
           y = 0, angle = 90, label = paste0(dia, "-", mês, "-", format(Sys.Date(), "%Y")), 
           vjust = -0.1, hjust = -0.5, size = 3)+
  geom_hline(aes(yintercept = medMov.VazRet[nrow(cantareira)]), color = "red4")+
  labs(title="Série histórica de vazão retirada (m3/s) na ETA Guaraú (2000 - 2023)",
       subtitle = "Média móvel de 30 dias")+
  xlab(NULL) +
  ylab(NULL) +
  scale_x_date(date_labels = "%Y", 
               breaks = "1 year",
               date_minor_breaks = "1 year")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 75, 
                                   vjust = 0.2,
                                   hjust = 0.1)) +
  theme(panel.grid.minor.y = element_blank())+
  theme(legend.position = "bottom")

#'Gráfico pie percentual 2-7-2021

cantareira %>% 
  ggplot(aes(x = data, y = VolumeOperacionalHm3, color = nome))+
  geom_line()


ggarrange(cantareira.plot.perc, precipitacao.plot, temperatura.plot,  nrow = 3, ncol = 1)

# Model ------------------------------------------------------------------------

total <- data.frame()

datas <- seq.Date(from = min(dados_sistema$Data), to = max(dados_sistema$Data), by = "day")

for(i in datas){
  SistemaId <- "Total"
  Data <- as.Date(i, origin = "1970-01-01")
  VolumeTotalHm3 <- sum(dados_sistema[dados_sistema$Data==i, 3], na.rm = T)
  VolumeOperacionalHm3 <- sum(dados_sistema[dados_sistema$Data==i, 4], na.rm = T)
  VolumePorcentagem <- ifelse(dim(dados_sistema[dados_sistema$Data==i, 1])[1]>6,
                              sum(dados_sistema[dados_sistema$Data==i, 4], na.rm = T)/1944.19,
                              sum(dados_sistema[dados_sistema$Data==i, 4], na.rm = T)/1855.369)
  VariacaoVolumePorcentagem <- NA
  Precipitacao <- sum(dados_sistema[dados_sistema$Data==i, 7], na.rm = T)
  VazaoJusante <- sum(dados_sistema[dados_sistema$Data==i, 8], na.rm = T)
  VazaoNatural <- sum(dados_sistema[dados_sistema$Data==i, 9], na.rm = T)
  VazaoAfluente <- sum(dados_sistema[dados_sistema$Data==i, 10], na.rm = T)
  VazaoProduzida <- sum(dados_sistema[dados_sistema$Data==i, 11], na.rm = T)
  VazaoRetirada <- sum(dados_sistema[dados_sistema$Data==i, 12], na.rm = T)
  df_temp <- data.frame(SistemaId, Data, VolumeTotalHm3, VolumeOperacionalHm3, VolumePorcentagem,
                        VariacaoVolumePorcentagem, Precipitacao, VazaoJusante,
                        VazaoNatural, VazaoAfluente, VazaoProduzida, VazaoRetirada)
  colnames(df_temp) <- colnames(dados_sistema)
  total <- rbind(total, df_temp)
  print(as.Date(i, origin = "1970-01-01"))
}


  


# Export -----------------------------------------------------------------------

# readr::write_rds(d, "")
