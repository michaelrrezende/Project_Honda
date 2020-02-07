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
          function (y, cut.off = 20, century = c(1900, 2000), ...) {
            chron:::year.expand(y, cut.off = cut.off, century = century, ...)
          }
)

#### READ MONTHS TREATMENT ####

utilizacoes_hapvida <- fread(file = "D:/users/sb046971/documents/Sinistro Hapvida/
             RELATORIO DE UTILIZAÇÃO - ACUMULADO_julho.2018-junho.2019 - matriz Honda.txt",
                             h = T, dec = ",", colClasses = c("BENEFICIARIO" = "character",
                                                          "CD_PROCEDIMENTO" = "character"))

julho_hap <- fread(file = "D:/users/sb046971/documents/Sinistro Hapvida/
                   23 MOTO HONDA DA AMAZONIA (88871)_julho.txt",
                   h = T, dec = ",", colClasses = c("BENEFICIARIO" = "character",
                                                    "CD_PROCEDIMENTO" = "character"))

base_hapvida <- bind_rows(utilizacoes_hapvida,julho_hap)

#### TREATMENT DATABASE DATES ####

base_hapvida$PAGAMENTO <- as.Date(base_hapvida$PAGAMENTO,"%d/%m/%Y")
base_hapvida$ATENDIMENTO <- as.Date(base_hapvida$ATENDIMENTO,"%d/%m/%Y")

#### SELECT DUPLICATED LINES TO DATA AND DROP ####

base_hapvida$DUPLICADOS <- duplicated(base_hapvida)

#### ANALYSIS IN DATA ####

sum(base_hapvida$VL_PROCEDIMENTO)

base_hapvida$diaEvxPg <- difftime(base_hapvida$PAGAMENTO,
                                  base_hapvida$ATENDIMENTO,units = "days")

base_hapvida$diaEvxPg <- as.numeric(base_hapvida$diaEvxPg)

base_hapvida$mesEvxPg <-floor((as.double(base_hapvida$diaEvxPg)/365)*12)

base_hapvida$flag12meses <- if_else(base_hapvida$mesEvxPg > 11,"+","0")

base_hapvida$flag4meses <- if_else(base_hapvida$diaEvxPg > 120,"+","0")

base_hapvida$flag6meses <- if_else(base_hapvida$diaEvxPg > 180,"+","0")

table(base_hapvida$flag12meses)

table(base_hapvida$flag4meses)

table(base_hapvida$flag6meses)

base_hapvida %>% filter(flag12meses == "+") %>% 
  group_by(.) %>% summarise(sum(VL_PROCEDIMENTO))

base_hapvida %>% filter(flag4meses == "+") %>% 
  group_by(.) %>% summarise(sum(VL_PROCEDIMENTO))

base_hapvida %>% filter(flag6meses == "+") %>% 
  group_by(.) %>% summarise(sum(VL_PROCEDIMENTO))

analysis1 <- base_hapvida %>% group_by(PRESTADOR) %>% summarise(
                                      med_dias = mean(diaEvxPg),
                                      med_meses = mean(mesEvxPg),
                                      valor_sinistro = sum(VL_PROCEDIMENTO))

analysis2 <- base_hapvida %>% group_by(PRESTADOR,mesEvxPg) %>% summarise(
  valor_sinistro = sum(VL_PROCEDIMENTO))

analysis3 <- base_hapvida %>% group_by(PRESTADOR) %>% summarise(
  med_dias = mean(diaEvxPg),
  med_meses = mean(mesEvxPg),
  valor_sinistro = sum(VL_PROCEDIMENTO))

analysis4 <- base_hapvida %>% group_by(CD_PROCEDIMENTO,diaEvxPg) %>% summarise(
  cont_proc = sum(QT_PROCEDIMENTO)) 

analysis4$flag <- if_else(analysis4$diaEvxPg > 120, "+","0")

analysis5 <- analysis4 %>% filter(CD_PROCEDIMENTO == "10101012" & 
                                      flag == "+") %>% group_by(
                                        CD_PROCEDIMENTO) %>% summarise(
                                          qtde_proc = sum(cont_proc))

analysis6 <- analysis4 %>% filter(flag == "+") %>% group_by(
  CD_PROCEDIMENTO) %>% summarise(qtde_proc = sum(cont_proc))


analysis7 <- base_hapvida %>% select(PRESTADOR,diaEvxPg,mesEvxPg,
                                   VL_PROCEDIMENTO) %>% filter(diaEvxPg > 120)

sum(analysis7$VL_PROCEDIMENTO)

hip_consult <- base_hapvida %>% filter(CD_PROCEDIMENTO %in% 
                                       c("10101012", "10014",
                                         "10600")) %>% group_by(NOME,
                                                                `T/D`) %>% summarise(
                                           sum(QT_PROCEDIMENTO),sum(VL_PROCEDIMENTO))

hip_consult %>% filter(`sum(QT_PROCEDIMENTO)` > 4) %>% group_by(.)

hip_ps <- base_hapvida %>% filter(CD_PROCEDIMENTO %in% c("10101039", 
                                                            "10081")) %>% group_by(
                                                              NOME,
                                                              `T/D`) %>% summarise(
                                                                sum(QT_PROCEDIMENTO),
                                                                sum(VL_PROCEDIMENTO))

hip_gast <- base_hapvida %>% group_by(NOME,`T/D`, MES_ANO_PAGAMENTO) %>% summarise(sum(VL_PROCEDIMENTO))

#### FINAL DATABASE ####

fwrite(base_hapvida, file = "d:/Users/sb046971/Documents/Sinistro Hapvida/BaseFimHAP.csv",
       sep = "|", dec = ",")

#### GRAPH DAYS AND VALUES || ONLY BETWEEN DATES ####

excedente_hapvida <- fread("d:/Users/sb046971/Documents/Sinistro Hapvida/excedente.csv",
                              dec = ",")

ggplot(excedente_hapvida,aes(diaEvxPg,VL_PROCEDIMENTO)) + 
  geom_point(mapping = aes(excedente_hapvida$diaEvxPg,
                           excedente_hapvida$VL_PROCEDIMENTO)) + 
  scale_x_continuous(name = "Days", breaks = seq(0,365,20)) +
  scale_y_continuous(name = "Values", breaks = seq(0,1000,100)) + 
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
  geom_vline(xintercept = 365, size = 1, colour = "#FF3721", linetype = "dashed")+
  geom_vline(xintercept = 120, size = 1, colour = "#FF3721", linetype = "dashed")

#### GRAPH DAYS AND VALUES ####

analysis8 <- base_hapvida %>% select(PRESTADOR,diaEvxPg,mesEvxPg,VL_PROCEDIMENTO)

ggplot(analysis8,aes(diaEvxPg,VL_PROCEDIMENTO)) + 
  geom_point(mapping = aes(analysis8$diaEvxPg,analysis8$VL_PROCEDIMENTO)) + 
  scale_x_continuous(name = "Days", breaks = seq(0,900,50)) +
  scale_y_continuous(name = "Values", breaks = seq(0,90000,5000)) + 
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



#### ANALYSIS CODES ####

analysis9 <- base_hapvida %>% filter(CD_PROCEDIMENTO %in% c("10014","10081",
                                                            "10138","10600",
                                                            "20010","20028",
                                                            "30015","30031",
                                                            "40010","27")) %>% group_by(
                                                    .) %>% summarise(sum(VL_PROCEDIMENTO))

