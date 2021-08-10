#### PACKAGES ####

require(dplyr)
require(data.table)
require(ggplot2)
require(RColorBrewer)
require(readr)
require(stringr)
require(chron)
require(extrafont)
require(tidyr)

#### CREATE LIST.FILES FROM BIND LINES IN 1 ARCHIVE ####

setwd("D:/Users/sb046971/OneDrive - Honda/Documentos/Douglas - Honda/")

base_oleo <- list.files(pattern = "os_oleo") %>% lapply(
  fread, h = T, encoding = "UTF-8") %>% bind_rows()

base_oleo2 <- base_oleo %>% select(NUM_CHASSI,`Quantidade de Peças de Serviços`,ano_os)

oleo <- base_oleo2 %>% group_by(NUM_CHASSI,
                                ano_os) %>% summarise(
                                  qtde = sum(`Quantidade de Peças de Serviços`)) %>% spread(
                                    ano_os, qtde)

separate = split(oleo, rep(1:2, each = 808453))

base_oleo_1 <- subset(separate$`1`)
base_oleo_2 <- subset(separate$`2`)

fwrite(base_oleo_1, file = "D:/Users/sb046971/Documents/Douglas - Honda/os_oleo_base1.csv", 
       sep = "|", dec = ",")
fwrite(base_oleo_2, file = "D:/Users/sb046971/Documents/Douglas - Honda/os_oleo_base2.csv", 
       sep = "|", dec = ",")


#### SAUDE MENTAL ####

x <- fread(file = "D:/Users/sb046971/OneDrive - Honda/Documentos/Archives - Honda/Cópia de Relatório PDCA Honda - 20.07.21-Final.v1xlsx.csv", h = T)

z <- x %>% select(-Empresa,-`Colababorador ou  Dependente`,-`Gênero`) %>% pivot_longer(-Unidade)

w <- z %>% group_by(Unidade,value) %>% tally()
