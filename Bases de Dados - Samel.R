#### PACKAGES ####

require(dplyr)
require(data.table)
require(ggplot2)
require(RColorBrewer)
require(readr)
require(stringr)
require(chron)
require(extrafont)
loadfonts(device = "win")

#### DROP WARNINGS ####

assign("last.warning", NULL, envir = baseenv())

#### CHANGE CUT CENTURY FOR AGE // BECAUSE THE DEFAULT IS 1970 - 2070 ####

options(chron.year.expand =
          function (y, cut.off = 21, century = c(1900, 2000), ...) {
            chron:::year.expand(y, cut.off = cut.off, century = century, ...)
          }
)

#### CREATE LIST.FILES FROM BIND LINES IN 1 ARCHIVE ####

setwd("D:/Users/sb046971/Documents/Sinistro Samel/honda/honda/")

base_samel <- list.files(pattern = "Moto Honda") %>% lapply(
  fread, h = T, encoding = "UTF-8") %>% bind_rows()


#### new data ####

setwd("D:/Users/sb046971/Documents/Sinistro Samel/honda/2018/honda componente/")

base_samel_comp_18 <- list.files(pattern = "hond") %>% lapply(
  fread, h = T, encoding = "Latin-1") %>% bind_rows()

setwd("D:/Users/sb046971/Documents/Sinistro Samel/honda/2018/honda trading/")

base_samel_trad_18 <- list.files(pattern = "hond") %>% lapply(
  fread, h = T, encoding = "Latin-1") %>% bind_rows()

setwd("D:/Users/sb046971/Documents/Sinistro Samel/honda/2018/moto honda/")

base_samel_motoh_18 <- list.files(pattern = "hond") %>% lapply(
  fread, h = T, encoding = "Latin-1") %>% bind_rows()

setwd("D:/Users/sb046971/Documents/Sinistro Samel/honda/2019/honda componente/")

base_samel_comp_19 <- list.files(pattern = "hond") %>% lapply(
  fread, h = T, encoding = "Latin-1") %>% bind_rows()

setwd("D:/Users/sb046971/Documents/Sinistro Samel/honda/2019/honda trading/")

base_samel_trad_19 <- list.files(pattern = "hond") %>% lapply(
  fread, h = T, encoding = "Latin-1") %>% bind_rows()

setwd("D:/Users/sb046971/Documents/Sinistro Samel/honda/2019/moto honda/")

base_samel_motoh_19 <- list.files(pattern = "hond") %>% lapply(
  fread, h = T, encoding = "Latin-1") %>% bind_rows()

setwd("D:/Users/sb046971/Documents/Sinistro Samel/honda/")

#### REPLACE NUMERIC VALUES #####

base_samel$VALOR <- str_replace_all(base_samel$VALOR, "\\.", "")

base_samel$VALOR <- str_replace_all(base_samel$VALOR, ",", "\\.")

base_samel$VALOR <- str_replace_all(base_samel$VALOR, "[R$ ]", "")

base_samel$VALORTOTAL <- str_replace_all(base_samel$VALORTOTAL, "\\.", "")

base_samel$VALORTOTAL <- str_replace_all(base_samel$VALORTOTAL, ",", "\\.")

base_samel$VALORTOTAL <- str_replace_all(base_samel$VALORTOTAL, "[R$ ]", "")

base_samel$`VALOR TOTAL` <- str_replace_all(base_samel$`VALOR TOTAL`, "\\.", "")

base_samel$`VALOR TOTAL` <- str_replace_all(base_samel$`VALOR TOTAL`, ",", "\\.")

base_samel$`VALOR TOTAL` <- str_replace_all(base_samel$`VALOR TOTAL`, "[R$ ]", "")

base_samel$VALOR <- as.numeric(base_samel$VALOR)

base_samel$VALORTOTAL <- as.numeric(base_samel$VALORTOTAL)

base_samel$`VALOR TOTAL` <- as.numeric(base_samel$`VALOR TOTAL`)


#### TREATMENT DATABASE DATES ####

base_samel$DATA <- as.Date(base_samel$DATA, "%d/%m/%Y")

base_samel$DATAALTA <- as.Date(base_samel$DATAALTA, "%d/%m/%Y")

base_samel$DATA_PROCEDIMENTO <- as.Date(base_samel$DATA_PROCEDIMENTO, "%d/%m/%Y")

base_samel$DATANASCIMENTO <- as.Date(base_samel$DATANASCIMENTO, "%d/%m/%Y")

base_samel$DATA_NASCIMENTO <- as.Date(base_samel$DATA_NASCIMENTO, "%d/%m/%Y")

#### SELECT DUPLICATED LINES TO DATA AND DROP ####

base_samel$DUPLICADOS <- duplicated(base_samel)

base_samel$VALORFIM <- base_samel$VALOR*base_samel$QTDE

base_samel %>% filter(DUPLICADOS == 
                        "TRUE") %>% group_by(.) %>% summarise(sum(VALORFIM,na.rm = T))

#### ANALYSIS IN DATA ####

base_samel$diaEvxPg <- difftime(base_samel$DATA,
                                 base_samel$DATA_PROCEDIMENTO,units = "days")

base_samel$diaEvxPg <- as.numeric(base_samel$diaEvxPg)

base_samel$mesEvxPg <-floor((as.double(base_samel$diaEvxPg)/365)*12)

base_samel$flag12meses <- if_else(base_samel$mesEvxPg > 11,"+","0")

base_samel$flag4meses <- if_else(base_samel$diaEvxPg > 120,"+","0")

base_samel$flag6meses <- if_else(base_samel$diaEvxPg > 180,"+","0")

table(base_samel$flag12meses)

table(base_samel$flag4meses)

table(base_samel$flag6meses)

# base_samel %>% filter(`Tipo da produção médica` != "RESSARC. SUS" & 
#                          flag12meses == "+") %>% group_by(.) %>% summarise(sum(Valor))

base_samel %>% filter(flag4meses == "+") %>% group_by(.) %>% summarise(sum(VALORFIM))

base_samel %>% filter(flag6meses == "+") %>% group_by(.) %>% summarise(sum(VALORFIM))

fwrite(base_samel, file = "D:/Users/sb046971/Documents/base_samel.txt", 
       sep = "|", dec = ",")

samel_sd <- base_samel %>% filter(DUPLICADOS == "FALSE")

analysis1 <- samel_sd %>% group_by(TITULAR,PRESTADOR) %>% summarise(
  med_dias = mean(diaEvxPg),
  med_meses = mean(mesEvxPg),
  valor_sinistro = sum(VALORFIM))

analysis2 <- samel_sd %>% group_by(PRESTADOR,mesEvxPg) %>% summarise(
  valor_sinistro = sum(VALORFIM))

analysis3 <- samel_sd %>% group_by(PRESTADOR) %>% summarise(
  med_dias = mean(diaEvxPg),
  med_meses = mean(mesEvxPg),
  valor_sinistro = sum(VALORFIM))

analysis4 <- samel_sd %>% group_by(COD_TUSS,diaEvxPg) %>% summarise(
  cont_proc = sum(QTDE)) 

analysis4$flag <- if_else(analysis4$diaEvxPg > 120, "+","0")

analysis5 <- analysis4 %>% filter(flag == "+") %>% group_by(
  COD_TUSS) %>% summarise(qtde_proc = sum(cont_proc))

analysis6 <- samel_sd %>% group_by(COD_TUSS,diaEvxPg) %>% summarise(
  cont_proc = n())

analysis6$flag <- if_else(analysis6$diaEvxPg > 120, "+","0")

analysis7 <- analysis6 %>% filter(COD_TUSS == "10101012" & 
                                    flag == "+") %>% group_by(
                                    ) %>% summarise(
                                      qtde_proc = sum(cont_proc))

analysis8 <- analysis6 %>% filter(flag == "+") %>% group_by(
  COD_TUSS) %>% summarise(qtde_proc = sum(cont_proc))

analysis9 <- samel_sd %>% select(PRESTADOR,
                                       diaEvxPg,mesEvxPg,
                                       VALORFIM) %>% filter(diaEvxPg > 120)

sum(analysis9$VALORFIM) ## grafico

analysis10 <- samel_sd %>% select(PRESTADOR,diaEvxPg,
                                       mesEvxPg,VALORFIM) %>% filter(diaEvxPg < 6000)

sum(analysis10$VALORFIM)

analysis11 <- samel_sd %>% filter(
  COD_TUSS == "10101039") %>% group_by(TITULAR) %>% summarise(
    qt_cons = sum(QTDE),valor = sum(VALORFIM))

analysis12 <- samel_sd %>% filter(
  COD_TUSS == "10101012") %>% group_by(TITULAR) %>% summarise(
    qt_cons = sum(QTDE),valor = sum(VALORFIM))

analysis13 <- samel_sd %>% group_by(BENEFICIARIO,MAT_CLIENTE) %>% summarise(sum(VALORFIM))

#### GRAPH DAYS AND VALUES ####

ggplot(analysis10,aes(diaEvxPg,VALORFIM)) + 
  geom_point(mapping = aes(analysis11$diaEvxPg,analysis11$VALORFIM)) + 
  scale_x_continuous(name = "Days", breaks = seq(0,1500,100)) +
  scale_y_continuous(name = "Values", breaks = seq(0,150000,20000)) + 
  ggtitle("") + theme_bw() + 
  theme(axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size = 14, family = "Tahoma",
                                  face = "bold", hjust = 0.5),
        text=element_text(family="Tahoma"),
        axis.text.x=element_text(colour="black", size = 9),
        axis.text.y=element_text(colour="black", size = 9)) +
  geom_vline(xintercept = 365, size = 1, colour = "#FF3721", linetype = "dashed")
