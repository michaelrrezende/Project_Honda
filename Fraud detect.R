#### PACKAGES ####

require(dplyr)
require(data.table)
require(ggplot2)
require(RColorBrewer)
require(readr)
require(stringr)
require(chron)
require(extrafont)

reembolsos_bradesco <- fread(
  "D:/Users/sb046971/Documents/Sinistro Bradesco/reembolso_bradesco.txt",
  dec = ",", encoding = "UTF-8", colClasses = c("CNPJ" = "character"))

names(reembolsos_bradesco)[names(reembolsos_bradesco) == "Segurado"] <- "NOME DO PACIENTE"

pacie <- bradesco_consolidado %>% select(`NOME DO PACIENTE`,`NOME DO SEGURADO`) %>% distinct()

fwrite(pacie, file = "D:/Users/sb046971/Documents/icl.csv", sep = ";", dec = ",")

pacie <- fread("D:/Users/sb046971/Documents/pacie.txt")

incl <- inner_join(reembolsos_bradesco,pacie)

incl <- incl[-c(1852,1855,1857,1859,1861,1863,1865,1867,
                1869,1871,1873,1875,1877,1879,1881,1883),]

############ new base ##########

reembolsos_bradesco <- fread(
  "D:/Users/sb046971/Documents/Reembolso_v3.csv",dec = ",",encoding = "Latin-1", 
  colClasses = c("CPF/CNPJ_PRESTADOR" = "character", "CARTAO" = "character"))

reembolsos_bradesco$duplicados <- duplicated(reembolsos_bradesco)

reemb <- reembolsos_bradesco %>% filter(duplicados == "FALSE")

# reemb <- reembolsos_bradesco %>% filter(PRESTADOR == "")

reemb$PRESTADOR <- str_replace_all(reemb$PRESTADOR, ";", "")

fwrite(reemb, file = "D:/Users/sb046971/Documents/Reemb.csv", sep = "|", dec = ",")

reemb$cartaofixed <- substr(reemb$CARTAO,1,13)