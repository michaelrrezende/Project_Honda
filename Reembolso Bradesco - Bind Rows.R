require(dplyr)
require(data.table)
require(ggplot2)
require(RColorBrewer)
require(psych)
require(bit64)

# reembolso1 <- fread("d:/Users/sb046971/Desktop/reembolso/08062019-141531-436035-71015-
#                     Sinistros em Processamento e Encerrados.txt",
#                     sep = "|", colClasses = c("CNPJ da Contratante" = "character",
#                                               "Cartao do Segurado" = "character",
#                                               "Numero do Sinistro" = "character",
#                                               "Conta Corrente" = "character",
#                                               "Valor Apresentado" = "numeric",
#                                               "Valor Pago" = "numeric"))
# 
# reembolso1$`Valor Apresentado` <- as.numeric(reembolso1$`Valor Apresentado`, is.na = T)
# reembolso1$`Valor Pago` <- as.numeric(reembolso1$`Valor Pago`, is.na = T)
# 
# reembolso2 <- fread("d:/Users/sb046971/Desktop/reembolso/15062019-143701-436035-71015-
#                     Sinistros em Processamento e Encerrados.txt",
#                     sep = "|", colClasses = c("CNPJ da Contratante" = "character",
#                                               "Cartao do Segurado" = "character",
#                                               "Numero do Sinistro" = "character",
#                                               "Conta Corrente" = "character",
#                                               "Valor Apresentado" = "numeric",
#                                               "Valor Pago" = "numeric"))
# 
# reembolso3 <- fread("d:/Users/sb046971/Desktop/reembolso/22062019-141220-436035-71015-
#                     Sinistros em Processamento e Encerrados.txt",
#                     sep = "|", colClasses = c("CNPJ da Contratante" = "character",
#                                               "Cartao do Segurado" = "character",
#                                               "Numero do Sinistro" = "character",
#                                               "Conta Corrente" = "character",
#                                               "Valor Apresentado" = "numeric",
#                                               "Valor Pago" = "numeric"))
# 
# reembolso <- bind_rows(reembolso1,reembolso2,reembolso3)
# 
# fwrite(reembolso, file = "reembolso_bind.csv", sep = "|", dec = ",")

#################


reembolso <- fread("d:/Users/sb046971/Desktop/reembolso/reembolso_bind2.csv", dec = ",")



reembolso$`Valor Apresentado` <- as.numeric(reembolso$`Valor Apresentado`, is.na = T)
reembolso$`Valor Pago` <- as.numeric(reembolso$`Valor Pago`, is.na = T)


reemb_th <- reembolso %>% filter(
  `Nome do Titular` == "THIAGO BOGOMOLOW") %>% group_by(
    `Nome do Segurado`) %>% summarise(Valor_Cobr = sum(
      `Valor Apresentado`), Valor_Reemb = sum(`Valor Pago`, na.rm = T))

###################

bind_colab <- fread("d:/Users/sb046971/Documents/thiago_bind.csv", dec = ",")

grupo1 <- bind_colab %>% group_by(Comp,Segurado) %>% summarise(
  Valor_Cobrado = sum(`Valor apresentado`), 
  Valor_Reembolso = sum(`Valor pago`),
  '%Reemb' = Valor_Reembolso/Valor_Cobrado)

grupo2 <- bind_colab %>% group_by(Segurado) %>% summarise(
  Valor_Cobrado = sum(`Valor apresentado`), 
  Valor_Reembolso = sum(`Valor pago`), 
  '%Reemb' = Valor_Reembolso/Valor_Cobrado)

geometric.mean(grupo2$`%Reemb`)


##################

base_proc.th1 <- fread(
  "d:/Users/sb046971/Documents/base - proced Th/paciente_eventos3031559324078.csv", 
  dec = ",",colClasses = c("CODIGO DO PRESTADOR" = "character",
                           "ANO DE REFERENCIA" = "character"))

base_proc.th2 <- fread(
  "d:/Users/sb046971/Documents/base - proced Th/paciente_eventos3031559324084.csv",
  dec = ",",colClasses = c("CODIGO DO PRESTADOR" = "character",
                           "ANO DE REFERENCIA" = "character"))

base_proc.th <- bind_rows(base_proc.th1,base_proc.th2)

base_proc.th$"VALOR FIM" <- base_proc.th$`VALOR DO EVENTO` + base_proc.th$`VALOR EXTRA 1`

agrup.analytics1 <- base_proc.th %>% group_by(`NOME DO BENEFICIARIO`,
                                              `ANO DE REFERENCIA`) %>% summarise(
                                                Soma.Evento = sum(`VALOR DO EVENTO`), 
                                                Soma.Final = sum(`VALOR FIM`))

agrup.analytics2 <- base_proc.th %>% group_by(`NOME DO BENEFICIARIO`,
                                              `ANO DE REFERENCIA`,
                                              `TIPO (HEALTHBIT)`,
                                              `TIPO (SEGURADORA)`) %>% summarise(
                                                Soma.Evento = sum(`VALOR DO EVENTO`), 
                                                Soma.Final = sum(`VALOR FIM`))

agrup.analytics3 <- base_proc.th %>% group_by(`NOME DO BENEFICIARIO`,
                                              `ANO DE REFERENCIA`,
                                              `TIPO (SEGURADORA)`) %>% summarise(
                                                Soma.Evento = sum(`VALOR DO EVENTO`), 
                                                Soma.Final = sum(`VALOR FIM`))

fwrite(base_proc.th,
       "D:/Users/sb046971/Documents/Estudos - Reembolso CC -TH/Base_Fim_Th.csv", 
       sep = "|", dec = ",")

