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
          function (y, cut.off = 22, century = c(1900, 2000), ...) {
            chron:::year.expand(y, cut.off = cut.off, century = century, ...)
          }
)

#### ONLY HIGH USERS BRADESCO - DATA ####

bradesco_highusers <- fread("D:/Users/sb046971/OneDrive - Honda/Documentos/bradesco_highusers.csv", 
                            dec = ",")

#### ANALYSIS DATA HIGH USERS ####

dynamics <- bradesco_highusers %>% group_by(Referenciado) %>% summarise(
  med_dias = mean(`Dias Evento x Pagamento`),
  med_meses = mean(`Meses Evento x Pagamento`),
  valor_total = sum(`Valor Total`),
  valor_recibo = sum(`Valor Recibo`))

dynamics2 <- bradesco_highusers %>% group_by(Referenciado,Paciente) %>% summarise(
  med_dias = mean(`Dias Evento x Pagamento`),
  med_meses = mean(`Meses Evento x Pagamento`),
  valor_total = sum(`Valor Total`),
  valor_recibo = sum(`Valor Recibo`))

dynamics3 <- bradesco_highusers %>% group_by(Referenciado,
                                             `Dias Evento x Pagamento`,
                                             `Meses Evento x Pagamento`) %>% 
  summarise(total = sum(`Valor Total`))

dynamics3$dias <- ifelse(dynamics3$`Dias Evento x Pagamento` > 120, "true","false")

dynamics4 <- dynamics3 %>% filter(
  dias %in% "true") %>% group_by(
    Referenciado,`Dias Evento x Pagamento`,`Meses Evento x Pagamento`) %>% summarise(
      total = sum(total))

#### SEND ANALYSIS TO EXCEL ####

fwrite(dynamics4, file = "D:/Users/sb046971/OneDrive - Honda/Documentos/Dinamica4.csv", sep = "|",
       dec = ",")
fwrite(dynamics, file = "D:/Users/sb046971/OneDrive - Honda/Documentos/Dinamica1.csv", sep = "|", 
       dec = ",")
fwrite(dynamics2, file = "D:/Users/sb046971/OneDrive - Honda/Documentos/Dinamica2.csv", sep = "|",
       dec = ",")

#### PLOT GRAPHS HIGH USERS 

plot(density(dynamics$med_dias, na.rm = T),xlim = c(0,333),
     col = "red",
     main = "Incidência da média de dias",
     xlab = "Média", ylab="Incidência",lwd=2)
lines(density(dynamics$med_meses, na.rm = T), col = "black",lwd=2)
legend("topright", legend=c("Média de Dias"),lty=1, lwd = 2,
       col=c("red"))

ggplot(dynamics, aes(x = med_dias)) + 
  geom_density(fill = "#4271AE", colour = "#1F3552",alpha = 0.6) + 
  scale_x_continuous(name = "Mean", breaks = seq(0,300,25)) + 
  scale_y_continuous(name = "Incidence") + ggtitle("Average Days Incidence") +
  theme_bw() + theme(axis.line = element_line(size=1, colour = "black"),
                     panel.grid.major = element_line(colour = "#d3d3d3"),
                     panel.grid.minor = element_blank(),
                     panel.border = element_blank(),
                     panel.background = element_blank(),
                     plot.title = element_text(size = 14, family = "Tahoma", 
                                               face = "bold", hjust = 0.5),
                     text=element_text(family="Tahoma"),
                     axis.text.x=element_text(colour="black", size = 9),
                     axis.text.y=element_text(colour="black", size = 9)) +
  geom_vline(xintercept = 120, size = 1, colour = "#FF3721", linetype = "dashed")
  

### COMPLETE DATA ###
#### CHANGE DIRECTORY ####

setwd("D:/Users/sb046971/OneDrive - Honda/Documentos/Sinistro Bradesco/")

# lapply doesn't function to read_fwf
# teste <- list.files(pattern = ".txt") %>% 
#   lapply(read_file_raw,#colClasses = c(),
#          stringsAsFactors=F,skip = 1,header = F,sep = "$")  %>% bind_rows

#### WIDTHS AND NAMES LAYOUT BRADESCO ####

widths = c(1,3,35,2,7,10,35,2,35,35,1,10,8,2,6,14,6,6,10,114,
           6,1,1,2,14,12,2,2,14,1,14,14,20,8,4,8,8,8,8,1,10)

names = c("TIPO DE REGISTRO","NÚMERO DA SUBFATURA","NOME DA SUBFATURA",
          "TIPO DA SUBFATURA","NÚMERO DO CERTIFICADO","MATRICULA",
          "NOME DO SEGURADO","CÓDIGO DO PACIENTE","NOME DO PACIENTE",
          "PRESTADOR EXECUTANTE","TIPO DE EVENTO","NÚMERO DO DOCUMENTO",
          "CÓDIGO DO PROCEDIMENTO","QUANTIDADE PROCEDIMENTOS",
          "DATA DO PAGAMENTO","VALOR PAGO","DATA DO EVENTO",
          "NÚMERO DO CONTRATO","CÓDIGO DO REFERENCIADO","FILLER",
          "DATA DE NASCIMENTO","SEXO","GRAU DE PARENTESCO",
          "ESPECIALIDADE","VALOR DO SINISTRO","MATRICULA ESPECIAL",
          "FILLER_1","CODIGO AUTORIZAÇÃO","CPF/CGC DO REFERENCIADO",
          "TIPO DO REFERENCIADO","VALOR DE INSS OU ISS ($)",
          "VALOR DE INSS OU ISS (FAJ-TR)","CARGO DO SEGURADO","DATA DE ADMISSÃO",
          "PLANO DO SEGURADO","CDB","DATA DO PAGAMENTO(Y2K)",
          "DATA DO EVENTO(Y2K)","DATA DE NASCIMENTO(Y2K)",
          "TROCA DE ACOMODAÇÃO","VALOR DO RECIBO")

#### READ ALL MONTHS DATABASE ABOUT CLAIM #########
### mes 05/2018

mes0518 <- readr:: read_fwf(
  file ="D:/Users/sb046971/OneDrive - Honda/Documentos/Sinistro Bradesco/SN1805_D071015.txt",
  fwf_widths(widths, col_names = names), skip_empty_rows = T,skip = 1,
    col_types = cols("VALOR PAGO" = col_integer(),
                     "VALOR DO SINISTRO" = col_integer(),
                     "VALOR DO RECIBO" = col_integer(), 
                     "VALOR DE INSS OU ISS ($)" = col_integer(),
                     "VALOR DE INSS OU ISS (FAJ-TR)" = col_integer(),
                     "QUANTIDADE PROCEDIMENTOS" = col_integer(),
                     "DATA DO PAGAMENTO" = col_character(),
                     "DATA DE NASCIMENTO" = col_character(),
                     "DATA DE ADMISSÃO" = col_character(),
                     "DATA DE NASCIMENTO(Y2K)" = col_character(),
                     "DATA DO EVENTO(Y2K)" = col_character(),
                     "DATA DO PAGAMENTO(Y2K)" = col_character())) %>% filter(
                       !str_detect(`TIPO DE REGISTRO`, "T"))

mes0518$"VALOR PAGO" <- mes0518$"VALOR PAGO"/100
mes0518$`VALOR DO RECIBO` <- mes0518$`VALOR DO RECIBO`/100
mes0518$`VALOR DO SINISTRO` <- mes0518$`VALOR DO SINISTRO`/100
mes0518$`VALOR DE INSS OU ISS ($)` <- mes0518$`VALOR DE INSS OU ISS ($)`/100

#mes 06/2018

mes0618 <- readr:: read_fwf(
  file ="D:/Users/sb046971/OneDrive - Honda/Documentos/Sinistro Bradesco/SN1806_D071015.txt",
  fwf_widths(widths, col_names = names), skip_empty_rows = T,skip = 1,
  col_types = cols("VALOR PAGO" = col_integer(),
                   "VALOR DO SINISTRO" = col_integer(),
                   "VALOR DO RECIBO" = col_integer(), 
                   "VALOR DE INSS OU ISS ($)" = col_integer(),
                   "VALOR DE INSS OU ISS (FAJ-TR)" = col_integer(),
                   "QUANTIDADE PROCEDIMENTOS" = col_integer(),
                   "DATA DO PAGAMENTO" = col_character(),
                   "DATA DE NASCIMENTO" = col_character(),
                   "DATA DE ADMISSÃO" = col_character(),
                   "DATA DE NASCIMENTO(Y2K)" = col_character(),
                   "DATA DO EVENTO(Y2K)" = col_character(),
                   "DATA DO PAGAMENTO(Y2K)" = col_character())) %>% filter(
                     !str_detect(`TIPO DE REGISTRO`, "T"))

mes0618$"VALOR PAGO" <- mes0618$"VALOR PAGO"/100
mes0618$`VALOR DO RECIBO` <- mes0618$`VALOR DO RECIBO`/100
mes0618$`VALOR DO SINISTRO` <- mes0618$`VALOR DO SINISTRO`/100
mes0618$`VALOR DE INSS OU ISS ($)` <- mes0618$`VALOR DE INSS OU ISS ($)`/100

#mes 07/2018

mes0718 <- readr:: read_fwf(
  file ="D:/Users/sb046971/OneDrive - Honda/Documentos/Sinistro Bradesco/SN1807_D071015.txt",
  fwf_widths(widths, col_names = names), skip_empty_rows = T,skip = 1,
  col_types = cols("VALOR PAGO" = col_integer(),
                   "VALOR DO SINISTRO" = col_integer(),
                   "VALOR DO RECIBO" = col_integer(), 
                   "VALOR DE INSS OU ISS ($)" = col_integer(),
                   "VALOR DE INSS OU ISS (FAJ-TR)" = col_integer(),
                   "QUANTIDADE PROCEDIMENTOS" = col_integer(),
                   "DATA DO PAGAMENTO" = col_character(),
                   "DATA DE NASCIMENTO" = col_character(),
                   "DATA DE ADMISSÃO" = col_character(),
                   "DATA DE NASCIMENTO(Y2K)" = col_character(),
                   "DATA DO EVENTO(Y2K)" = col_character(),
                   "DATA DO PAGAMENTO(Y2K)" = col_character())) %>% filter(
                     !str_detect(`TIPO DE REGISTRO`, "T"))

mes0718$"VALOR PAGO" <- mes0718$"VALOR PAGO"/100
mes0718$`VALOR DO RECIBO` <- mes0718$`VALOR DO RECIBO`/100
mes0718$`VALOR DO SINISTRO` <- mes0718$`VALOR DO SINISTRO`/100
mes0718$`VALOR DE INSS OU ISS ($)` <- mes0718$`VALOR DE INSS OU ISS ($)`/100

#mes 08/2018

mes0818 <- readr:: read_fwf(
  file ="D:/Users/sb046971/OneDrive - Honda/Documentos/Sinistro Bradesco/SN1808_D071015.txt",
  fwf_widths(widths, col_names = names), skip_empty_rows = T,skip = 1,
  col_types = cols("VALOR PAGO" = col_integer(),
                   "VALOR DO SINISTRO" = col_integer(),
                   "VALOR DO RECIBO" = col_integer(), 
                   "VALOR DE INSS OU ISS ($)" = col_integer(),
                   "VALOR DE INSS OU ISS (FAJ-TR)" = col_integer(),
                   "QUANTIDADE PROCEDIMENTOS" = col_integer(),
                   "DATA DO PAGAMENTO" = col_character(),
                   "DATA DE NASCIMENTO" = col_character(),
                   "DATA DE ADMISSÃO" = col_character(),
                   "DATA DE NASCIMENTO(Y2K)" = col_character(),
                   "DATA DO EVENTO(Y2K)" = col_character(),
                   "DATA DO PAGAMENTO(Y2K)" = col_character())) %>% filter(
                     !str_detect(`TIPO DE REGISTRO`, "T"))

mes0818$"VALOR PAGO" <- mes0818$"VALOR PAGO"/100
mes0818$`VALOR DO RECIBO` <- mes0818$`VALOR DO RECIBO`/100
mes0818$`VALOR DO SINISTRO` <- mes0818$`VALOR DO SINISTRO`/100
mes0818$`VALOR DE INSS OU ISS ($)` <- mes0818$`VALOR DE INSS OU ISS ($)`/100

#mes 09/2018

mes0918 <- readr:: read_fwf(
  file ="D:/Users/sb046971/OneDrive - Honda/Documentos/Sinistro Bradesco/SN1809_D071015.txt",
  fwf_widths(widths, col_names = names), skip_empty_rows = T,skip = 1,
  col_types = cols("VALOR PAGO" = col_integer(),
                   "VALOR DO SINISTRO" = col_integer(),
                   "VALOR DO RECIBO" = col_integer(), 
                   "VALOR DE INSS OU ISS ($)" = col_integer(),
                   "VALOR DE INSS OU ISS (FAJ-TR)" = col_integer(),
                   "QUANTIDADE PROCEDIMENTOS" = col_integer(),
                   "DATA DO PAGAMENTO" = col_character(),
                   "DATA DE NASCIMENTO" = col_character(),
                   "DATA DE ADMISSÃO" = col_character(),
                   "DATA DE NASCIMENTO(Y2K)" = col_character(),
                   "DATA DO EVENTO(Y2K)" = col_character(),
                   "DATA DO PAGAMENTO(Y2K)" = col_character())) %>% filter(
                     !str_detect(`TIPO DE REGISTRO`, "T"))

mes0918$"VALOR PAGO" <- mes0918$"VALOR PAGO"/100
mes0918$`VALOR DO RECIBO` <- mes0918$`VALOR DO RECIBO`/100
mes0918$`VALOR DO SINISTRO` <- mes0918$`VALOR DO SINISTRO`/100
mes0918$`VALOR DE INSS OU ISS ($)` <- mes0918$`VALOR DE INSS OU ISS ($)`/100

#mes 10/2018

mes1018 <- readr:: read_fwf(
  file ="D:/Users/sb046971/OneDrive - Honda/Documentos/Sinistro Bradesco/SN1810_D071015.txt",
  fwf_widths(widths, col_names = names), skip_empty_rows = T,skip = 1,
  col_types = cols("VALOR PAGO" = col_integer(),
                   "VALOR DO SINISTRO" = col_integer(),
                   "VALOR DO RECIBO" = col_integer(), 
                   "VALOR DE INSS OU ISS ($)" = col_integer(),
                   "VALOR DE INSS OU ISS (FAJ-TR)" = col_integer(),
                   "QUANTIDADE PROCEDIMENTOS" = col_integer(),
                   "DATA DO PAGAMENTO" = col_character(),
                   "DATA DE NASCIMENTO" = col_character(),
                   "DATA DE ADMISSÃO" = col_character(),
                   "DATA DE NASCIMENTO(Y2K)" = col_character(),
                   "DATA DO EVENTO(Y2K)" = col_character(),
                   "DATA DO PAGAMENTO(Y2K)" = col_character())) %>% filter(
                     !str_detect(`TIPO DE REGISTRO`, "T"))

mes1018$"VALOR PAGO" <- mes1018$"VALOR PAGO"/100
mes1018$`VALOR DO RECIBO` <- mes1018$`VALOR DO RECIBO`/100
mes1018$`VALOR DO SINISTRO` <- mes1018$`VALOR DO SINISTRO`/100
mes1018$`VALOR DE INSS OU ISS ($)` <- mes1018$`VALOR DE INSS OU ISS ($)`/100

#mes 11/2018

mes1118 <- readr:: read_fwf(
  file ="D:/Users/sb046971/OneDrive - Honda/Documentos/Sinistro Bradesco/SN1811_D071015.txt",
  fwf_widths(widths, col_names = names), skip_empty_rows = T,skip = 1,
  col_types = cols("VALOR PAGO" = col_integer(),
                   "VALOR DO SINISTRO" = col_integer(),
                   "VALOR DO RECIBO" = col_integer(), 
                   "VALOR DE INSS OU ISS ($)" = col_integer(),
                   "VALOR DE INSS OU ISS (FAJ-TR)" = col_integer(),
                   "QUANTIDADE PROCEDIMENTOS" = col_integer(),
                   "DATA DO PAGAMENTO" = col_character(),
                   "DATA DE NASCIMENTO" = col_character(),
                   "DATA DE ADMISSÃO" = col_character(),
                   "DATA DE NASCIMENTO(Y2K)" = col_character(),
                   "DATA DO EVENTO(Y2K)" = col_character(),
                   "DATA DO PAGAMENTO(Y2K)" = col_character())) %>% filter(
                     !str_detect(`TIPO DE REGISTRO`, "T"))

mes1118$"VALOR PAGO" <- mes1118$"VALOR PAGO"/100
mes1118$`VALOR DO RECIBO` <- mes1118$`VALOR DO RECIBO`/100
mes1118$`VALOR DO SINISTRO` <- mes1118$`VALOR DO SINISTRO`/100
mes1118$`VALOR DE INSS OU ISS ($)` <- mes1118$`VALOR DE INSS OU ISS ($)`/100

#mes 12/2018

mes1218 <- readr:: read_fwf(
  file ="D:/Users/sb046971/OneDrive - Honda/Documentos/Sinistro Bradesco/SN1812_D071015.txt",
  fwf_widths(widths, col_names = names), skip_empty_rows = T,skip = 1,
  col_types = cols("VALOR PAGO" = col_integer(),
                   "VALOR DO SINISTRO" = col_integer(),
                   "VALOR DO RECIBO" = col_integer(), 
                   "VALOR DE INSS OU ISS ($)" = col_integer(),
                   "VALOR DE INSS OU ISS (FAJ-TR)" = col_integer(),
                   "QUANTIDADE PROCEDIMENTOS" = col_integer(),
                   "DATA DO PAGAMENTO" = col_character(),
                   "DATA DE NASCIMENTO" = col_character(),
                   "DATA DE ADMISSÃO" = col_character(),
                   "DATA DE NASCIMENTO(Y2K)" = col_character(),
                   "DATA DO EVENTO(Y2K)" = col_character(),
                   "DATA DO PAGAMENTO(Y2K)" = col_character())) %>% filter(
                     !str_detect(`TIPO DE REGISTRO`, "T"))

mes1218$"VALOR PAGO" <- mes1218$"VALOR PAGO"/100
mes1218$`VALOR DO RECIBO` <- mes1218$`VALOR DO RECIBO`/100
mes1218$`VALOR DO SINISTRO` <- mes1218$`VALOR DO SINISTRO`/100
mes1218$`VALOR DE INSS OU ISS ($)` <- mes1218$`VALOR DE INSS OU ISS ($)`/100

#mes 01/2019

mes0119 <- readr:: read_fwf(
file ="D:/Users/sb046971/OneDrive - Honda/Documentos/Sinistro Bradesco/SN1901_D071015.txt",
fwf_widths(widths, col_names = names), skip_empty_rows = T,skip = 1,
col_types = cols("VALOR PAGO" = col_integer(),
                 "VALOR DO SINISTRO" = col_integer(),
                 "VALOR DO RECIBO" = col_integer(), 
                 "VALOR DE INSS OU ISS ($)" = col_integer(),
                 "VALOR DE INSS OU ISS (FAJ-TR)" = col_integer(),
                 "QUANTIDADE PROCEDIMENTOS" = col_integer(),
                 "DATA DO PAGAMENTO" = col_character(),
                 "DATA DE NASCIMENTO" = col_character(),
                 "DATA DE ADMISSÃO" = col_character(),
                 "DATA DE NASCIMENTO(Y2K)" = col_character(),
                 "DATA DO EVENTO(Y2K)" = col_character(),
                 "DATA DO PAGAMENTO(Y2K)" = col_character())) %>% filter(
                   !str_detect(`TIPO DE REGISTRO`, "T"))

mes0119$"VALOR PAGO" <- mes0119$"VALOR PAGO"/100
mes0119$`VALOR DO RECIBO` <- mes0119$`VALOR DO RECIBO`/100
mes0119$`VALOR DO SINISTRO` <- mes0119$`VALOR DO SINISTRO`/100
mes0119$`VALOR DE INSS OU ISS ($)` <- mes0119$`VALOR DE INSS OU ISS ($)`/100

#mes 02/2019

mes0219 <- readr:: read_fwf(
  file ="D:/Users/sb046971/OneDrive - Honda/Documentos/Sinistro Bradesco/SN1902_D071015.txt",
  fwf_widths(widths, col_names = names), skip_empty_rows = T,skip = 1,
  col_types = cols("VALOR PAGO" = col_integer(),
                   "VALOR DO SINISTRO" = col_integer(),
                   "VALOR DO RECIBO" = col_integer(), 
                   "VALOR DE INSS OU ISS ($)" = col_integer(),
                   "VALOR DE INSS OU ISS (FAJ-TR)" = col_integer(),
                   "QUANTIDADE PROCEDIMENTOS" = col_integer(),
                   "DATA DO PAGAMENTO" = col_character(),
                   "DATA DE NASCIMENTO" = col_character(),
                   "DATA DE ADMISSÃO" = col_character(),
                   "DATA DE NASCIMENTO(Y2K)" = col_character(),
                   "DATA DO EVENTO(Y2K)" = col_character(),
                   "DATA DO PAGAMENTO(Y2K)" = col_character())) %>% filter(
                     !str_detect(`TIPO DE REGISTRO`, "T"))

mes0219$"VALOR PAGO" <- mes0219$"VALOR PAGO"/100
mes0219$`VALOR DO RECIBO` <- mes0219$`VALOR DO RECIBO`/100
mes0219$`VALOR DO SINISTRO` <- mes0219$`VALOR DO SINISTRO`/100
mes0219$`VALOR DE INSS OU ISS ($)` <- mes0219$`VALOR DE INSS OU ISS ($)`/100

#mes 03/2019

mes0319 <- readr:: read_fwf(
  file ="D:/Users/sb046971/OneDrive - Honda/Documentos/Sinistro Bradesco/SN1903_D071015.txt",
  fwf_widths(widths, col_names = names), skip_empty_rows = T,skip = 1,
  col_types = cols("VALOR PAGO" = col_integer(),
                   "VALOR DO SINISTRO" = col_integer(),
                   "VALOR DO RECIBO" = col_integer(), 
                   "VALOR DE INSS OU ISS ($)" = col_integer(),
                   "VALOR DE INSS OU ISS (FAJ-TR)" = col_integer(),
                   "QUANTIDADE PROCEDIMENTOS" = col_integer(),
                   "DATA DO PAGAMENTO" = col_character(),
                   "DATA DE NASCIMENTO" = col_character(),
                   "DATA DE ADMISSÃO" = col_character(),
                   "DATA DE NASCIMENTO(Y2K)" = col_character(),
                   "DATA DO EVENTO(Y2K)" = col_character(),
                   "DATA DO PAGAMENTO(Y2K)" = col_character())) %>% filter(
                     !str_detect(`TIPO DE REGISTRO`, "T"))

mes0319$"VALOR PAGO" <- mes0319$"VALOR PAGO"/100
mes0319$`VALOR DO RECIBO` <- mes0319$`VALOR DO RECIBO`/100
mes0319$`VALOR DO SINISTRO` <- mes0319$`VALOR DO SINISTRO`/100
mes0319$`VALOR DE INSS OU ISS ($)` <- mes0319$`VALOR DE INSS OU ISS ($)`/100

#mes 04/2019

mes0419 <- readr:: read_fwf(
  file ="D:/Users/sb046971/OneDrive - Honda/Documentos/Sinistro Bradesco/SN1904_D071015.txt",
  fwf_widths(widths, col_names = names), skip_empty_rows = T,skip = 1,
  col_types = cols("VALOR PAGO" = col_integer(),
                   "VALOR DO SINISTRO" = col_integer(),
                   "VALOR DO RECIBO" = col_integer(), 
                   "VALOR DE INSS OU ISS ($)" = col_integer(),
                   "VALOR DE INSS OU ISS (FAJ-TR)" = col_integer(),
                   "QUANTIDADE PROCEDIMENTOS" = col_integer(),
                   "DATA DO PAGAMENTO" = col_character(),
                   "DATA DE NASCIMENTO" = col_character(),
                   "DATA DE ADMISSÃO" = col_character(),
                   "DATA DE NASCIMENTO(Y2K)" = col_character(),
                   "DATA DO EVENTO(Y2K)" = col_character(),
                   "DATA DO PAGAMENTO(Y2K)" = col_character())) %>% filter(
                     !str_detect(`TIPO DE REGISTRO`, "T"))

mes0419$"VALOR PAGO" <- mes0419$"VALOR PAGO"/100
mes0419$`VALOR DO RECIBO` <- mes0419$`VALOR DO RECIBO`/100
mes0419$`VALOR DO SINISTRO` <- mes0419$`VALOR DO SINISTRO`/100
mes0419$`VALOR DE INSS OU ISS ($)` <- mes0419$`VALOR DE INSS OU ISS ($)`/100

#mes 05/2019

mes0519 <- readr:: read_fwf(
  file ="D:/Users/sb046971/OneDrive - Honda/Documentos/Sinistro Bradesco/SN1905_D071015.txt",
  fwf_widths(widths, col_names = names), skip_empty_rows = T,skip = 1,
  col_types = cols("VALOR PAGO" = col_integer(),
                   "VALOR DO SINISTRO" = col_integer(),
                   "VALOR DO RECIBO" = col_integer(), 
                   "VALOR DE INSS OU ISS ($)" = col_integer(),
                   "VALOR DE INSS OU ISS (FAJ-TR)" = col_integer(),
                   "QUANTIDADE PROCEDIMENTOS" = col_integer(),
                   "DATA DO PAGAMENTO" = col_character(),
                   "DATA DE NASCIMENTO" = col_character(),
                   "DATA DE ADMISSÃO" = col_character(),
                   "DATA DE NASCIMENTO(Y2K)" = col_character(),
                   "DATA DO EVENTO(Y2K)" = col_character(),
                   "DATA DO PAGAMENTO(Y2K)" = col_character())) %>% filter(
                     !str_detect(`TIPO DE REGISTRO`, "T"))

mes0519$"VALOR PAGO" <- mes0519$"VALOR PAGO"/100
mes0519$`VALOR DO RECIBO` <- mes0519$`VALOR DO RECIBO`/100
mes0519$`VALOR DO SINISTRO` <- mes0519$`VALOR DO SINISTRO`/100
mes0519$`VALOR DE INSS OU ISS ($)` <- mes0519$`VALOR DE INSS OU ISS ($)`/100

#mes 06/2019

mes0619 <- readr:: read_fwf(
  file ="D:/Users/sb046971/OneDrive - Honda/Documentos/Sinistro Bradesco/SN1906_D071015.txt",
  fwf_widths(widths, col_names = names), skip_empty_rows = T,skip = 1,
  col_types = cols("VALOR PAGO" = col_integer(),
                   "VALOR DO SINISTRO" = col_integer(),
                   "VALOR DO RECIBO" = col_integer(), 
                   "VALOR DE INSS OU ISS ($)" = col_integer(),
                   "VALOR DE INSS OU ISS (FAJ-TR)" = col_integer(),
                   "QUANTIDADE PROCEDIMENTOS" = col_integer(),
                   "DATA DO PAGAMENTO" = col_character(),
                   "DATA DE NASCIMENTO" = col_character(),
                   "DATA DE ADMISSÃO" = col_character(),
                   "DATA DE NASCIMENTO(Y2K)" = col_character(),
                   "DATA DO EVENTO(Y2K)" = col_character(),
                   "DATA DO PAGAMENTO(Y2K)" = col_character())) %>% filter(
                     !str_detect(`TIPO DE REGISTRO`, "T"))

mes0619$"VALOR PAGO" <- mes0619$"VALOR PAGO"/100
mes0619$`VALOR DO RECIBO` <- mes0619$`VALOR DO RECIBO`/100
mes0619$`VALOR DO SINISTRO` <- mes0619$`VALOR DO SINISTRO`/100
mes0619$`VALOR DE INSS OU ISS ($)` <- mes0619$`VALOR DE INSS OU ISS ($)`/100

#mes 07/2019

mes0719 <- readr:: read_fwf(
  file ="D:/Users/sb046971/OneDrive - Honda/Documentos/Sinistro Bradesco/SN1907_D071015.txt",
  fwf_widths(widths, col_names = names), skip_empty_rows = T,skip = 1,
  col_types = cols("VALOR PAGO" = col_integer(),
                   "VALOR DO SINISTRO" = col_integer(),
                   "VALOR DO RECIBO" = col_integer(), 
                   "VALOR DE INSS OU ISS ($)" = col_integer(),
                   "VALOR DE INSS OU ISS (FAJ-TR)" = col_integer(),
                   "QUANTIDADE PROCEDIMENTOS" = col_integer(),
                   "DATA DO PAGAMENTO" = col_character(),
                   "DATA DE NASCIMENTO" = col_character(),
                   "DATA DE ADMISSÃO" = col_character(),
                   "DATA DE NASCIMENTO(Y2K)" = col_character(),
                   "DATA DO EVENTO(Y2K)" = col_character(),
                   "DATA DO PAGAMENTO(Y2K)" = col_character())) %>% filter(
                     !str_detect(`TIPO DE REGISTRO`, "T"))

mes0719$"VALOR PAGO" <- mes0719$"VALOR PAGO"/100
mes0719$`VALOR DO RECIBO` <- mes0719$`VALOR DO RECIBO`/100
mes0719$`VALOR DO SINISTRO` <- mes0719$`VALOR DO SINISTRO`/100
mes0719$`VALOR DE INSS OU ISS ($)` <- mes0719$`VALOR DE INSS OU ISS ($)`/100

#mes 08/2019

mes0819 <- readr:: read_fwf(
  file ="D:/Users/sb046971/OneDrive - Honda/Documentos/Sinistro Bradesco/SN1908_D071015.txt",
  fwf_widths(widths, col_names = names), skip_empty_rows = T,skip = 1,
  col_types = cols("VALOR PAGO" = col_integer(),
                   "VALOR DO SINISTRO" = col_integer(),
                   "VALOR DO RECIBO" = col_integer(), 
                   "VALOR DE INSS OU ISS ($)" = col_integer(),
                   "VALOR DE INSS OU ISS (FAJ-TR)" = col_integer(),
                   "QUANTIDADE PROCEDIMENTOS" = col_integer(),
                   "DATA DO PAGAMENTO" = col_character(),
                   "DATA DE NASCIMENTO" = col_character(),
                   "DATA DE ADMISSÃO" = col_character(),
                   "DATA DE NASCIMENTO(Y2K)" = col_character(),
                   "DATA DO EVENTO(Y2K)" = col_character(),
                   "DATA DO PAGAMENTO(Y2K)" = col_character())) %>% filter(
                     !str_detect(`TIPO DE REGISTRO`, "T"))

mes0819$"VALOR PAGO" <- mes0819$"VALOR PAGO"/100
mes0819$`VALOR DO RECIBO` <- mes0819$`VALOR DO RECIBO`/100
mes0819$`VALOR DO SINISTRO` <- mes0819$`VALOR DO SINISTRO`/100
mes0819$`VALOR DE INSS OU ISS ($)` <- mes0819$`VALOR DE INSS OU ISS ($)`/100

#mes 09/2019

mes0919 <- readr:: read_fwf(
  file ="D:/Users/sb046971/OneDrive - Honda/Documentos/Sinistro Bradesco/SN1909_D071015.txt",
  fwf_widths(widths, col_names = names), skip_empty_rows = T,skip = 1,
  col_types = cols("VALOR PAGO" = col_integer(),
                   "VALOR DO SINISTRO" = col_integer(),
                   "VALOR DO RECIBO" = col_integer(), 
                   "VALOR DE INSS OU ISS ($)" = col_integer(),
                   "VALOR DE INSS OU ISS (FAJ-TR)" = col_integer(),
                   "QUANTIDADE PROCEDIMENTOS" = col_integer(),
                   "DATA DO PAGAMENTO" = col_character(),
                   "DATA DE NASCIMENTO" = col_character(),
                   "DATA DE ADMISSÃO" = col_character(),
                   "DATA DE NASCIMENTO(Y2K)" = col_character(),
                   "DATA DO EVENTO(Y2K)" = col_character(),
                   "DATA DO PAGAMENTO(Y2K)" = col_character())) %>% filter(
                     !str_detect(`TIPO DE REGISTRO`, "T"))

mes0919$"VALOR PAGO" <- mes0919$"VALOR PAGO"/100
mes0919$`VALOR DO RECIBO` <- mes0919$`VALOR DO RECIBO`/100
mes0919$`VALOR DO SINISTRO` <- mes0919$`VALOR DO SINISTRO`/100
mes0919$`VALOR DE INSS OU ISS ($)` <- mes0919$`VALOR DE INSS OU ISS ($)`/100

#mes 10/2019

mes1019 <- readr:: read_fwf(
  file ="D:/Users/sb046971/OneDrive - Honda/Documentos/Sinistro Bradesco/SN1910_D071015.txt",
  fwf_widths(widths, col_names = names), skip_empty_rows = T,skip = 1,
  col_types = cols("VALOR PAGO" = col_integer(),
                   "VALOR DO SINISTRO" = col_integer(),
                   "VALOR DO RECIBO" = col_integer(), 
                   "VALOR DE INSS OU ISS ($)" = col_integer(),
                   "VALOR DE INSS OU ISS (FAJ-TR)" = col_integer(),
                   "QUANTIDADE PROCEDIMENTOS" = col_integer(),
                   "DATA DO PAGAMENTO" = col_character(),
                   "DATA DE NASCIMENTO" = col_character(),
                   "DATA DE ADMISSÃO" = col_character(),
                   "DATA DE NASCIMENTO(Y2K)" = col_character(),
                   "DATA DO EVENTO(Y2K)" = col_character(),
                   "DATA DO PAGAMENTO(Y2K)" = col_character())) %>% filter(
                     !str_detect(`TIPO DE REGISTRO`, "T"))

mes1019$"VALOR PAGO" <- mes1019$"VALOR PAGO"/100
mes1019$`VALOR DO RECIBO` <- mes1019$`VALOR DO RECIBO`/100
mes1019$`VALOR DO SINISTRO` <- mes1019$`VALOR DO SINISTRO`/100
mes1019$`VALOR DE INSS OU ISS ($)` <- mes1019$`VALOR DE INSS OU ISS ($)`/100

#mes 11/2019

mes1119 <- readr:: read_fwf(
  file ="D:/Users/sb046971/OneDrive - Honda/Documentos/Sinistro Bradesco/SN1911_D071015.txt",
  fwf_widths(widths, col_names = names), skip_empty_rows = T,skip = 1,
  col_types = cols("VALOR PAGO" = col_integer(),
                   "VALOR DO SINISTRO" = col_integer(),
                   "VALOR DO RECIBO" = col_integer(), 
                   "VALOR DE INSS OU ISS ($)" = col_integer(),
                   "VALOR DE INSS OU ISS (FAJ-TR)" = col_integer(),
                   "QUANTIDADE PROCEDIMENTOS" = col_integer(),
                   "DATA DO PAGAMENTO" = col_character(),
                   "DATA DE NASCIMENTO" = col_character(),
                   "DATA DE ADMISSÃO" = col_character(),
                   "DATA DE NASCIMENTO(Y2K)" = col_character(),
                   "DATA DO EVENTO(Y2K)" = col_character(),
                   "DATA DO PAGAMENTO(Y2K)" = col_character())) %>% filter(
                     !str_detect(`TIPO DE REGISTRO`, "T"))

mes1119$"VALOR PAGO" <- mes1119$"VALOR PAGO"/100
mes1119$`VALOR DO RECIBO` <- mes1119$`VALOR DO RECIBO`/100
mes1119$`VALOR DO SINISTRO` <- mes1119$`VALOR DO SINISTRO`/100
mes1119$`VALOR DE INSS OU ISS ($)` <- mes1119$`VALOR DE INSS OU ISS ($)`/100

#mes 12/2019

mes1219 <- readr:: read_fwf(
  file ="D:/Users/sb046971/OneDrive - Honda/Documentos/Sinistro Bradesco/SN1912_D071015.txt",
  fwf_widths(widths, col_names = names), skip_empty_rows = T,skip = 1,
  col_types = cols("VALOR PAGO" = col_integer(),
                   "VALOR DO SINISTRO" = col_integer(),
                   "VALOR DO RECIBO" = col_integer(), 
                   "VALOR DE INSS OU ISS ($)" = col_integer(),
                   "VALOR DE INSS OU ISS (FAJ-TR)" = col_integer(),
                   "QUANTIDADE PROCEDIMENTOS" = col_integer(),
                   "DATA DO PAGAMENTO" = col_character(),
                   "DATA DE NASCIMENTO" = col_character(),
                   "DATA DE ADMISSÃO" = col_character(),
                   "DATA DE NASCIMENTO(Y2K)" = col_character(),
                   "DATA DO EVENTO(Y2K)" = col_character(),
                   "DATA DO PAGAMENTO(Y2K)" = col_character())) %>% filter(
                     !str_detect(`TIPO DE REGISTRO`, "T"))

mes1219$"VALOR PAGO" <- mes1219$"VALOR PAGO"/100
mes1219$`VALOR DO RECIBO` <- mes1219$`VALOR DO RECIBO`/100
mes1219$`VALOR DO SINISTRO` <- mes1219$`VALOR DO SINISTRO`/100
mes1219$`VALOR DE INSS OU ISS ($)` <- mes1219$`VALOR DE INSS OU ISS ($)`/100

#mes 01/2020

mes0120 <- readr:: read_fwf(
  file ="D:/Users/sb046971/OneDrive - Honda/Documentos/Sinistro Bradesco/SN2001_D071015.txt",
  fwf_widths(widths, col_names = names), skip_empty_rows = T,skip = 1,
  col_types = cols("VALOR PAGO" = col_integer(),
                   "VALOR DO SINISTRO" = col_integer(),
                   "VALOR DO RECIBO" = col_integer(), 
                   "VALOR DE INSS OU ISS ($)" = col_integer(),
                   "VALOR DE INSS OU ISS (FAJ-TR)" = col_integer(),
                   "QUANTIDADE PROCEDIMENTOS" = col_integer(),
                   "DATA DO PAGAMENTO" = col_character(),
                   "DATA DE NASCIMENTO" = col_character(),
                   "DATA DE ADMISSÃO" = col_character(),
                   "DATA DE NASCIMENTO(Y2K)" = col_character(),
                   "DATA DO EVENTO(Y2K)" = col_character(),
                   "DATA DO PAGAMENTO(Y2K)" = col_character())) %>% filter(
                     !str_detect(`TIPO DE REGISTRO`, "T"))

mes0120$"VALOR PAGO" <- mes0120$"VALOR PAGO"/100
mes0120$`VALOR DO RECIBO` <- mes0120$`VALOR DO RECIBO`/100
mes0120$`VALOR DO SINISTRO` <- mes0120$`VALOR DO SINISTRO`/100
mes0120$`VALOR DE INSS OU ISS ($)` <- mes0120$`VALOR DE INSS OU ISS ($)`/100

#mes 02/2020

mes0220 <- readr:: read_fwf(
  file ="D:/Users/sb046971/OneDrive - Honda/Documentos/Sinistro Bradesco/SN2002_D071015.txt",
  fwf_widths(widths, col_names = names), skip_empty_rows = T,skip = 1,
  col_types = cols("VALOR PAGO" = col_integer(),
                   "VALOR DO SINISTRO" = col_integer(),
                   "VALOR DO RECIBO" = col_integer(), 
                   "VALOR DE INSS OU ISS ($)" = col_integer(),
                   "VALOR DE INSS OU ISS (FAJ-TR)" = col_integer(),
                   "QUANTIDADE PROCEDIMENTOS" = col_integer(),
                   "DATA DO PAGAMENTO" = col_character(),
                   "DATA DE NASCIMENTO" = col_character(),
                   "DATA DE ADMISSÃO" = col_character(),
                   "DATA DE NASCIMENTO(Y2K)" = col_character(),
                   "DATA DO EVENTO(Y2K)" = col_character(),
                   "DATA DO PAGAMENTO(Y2K)" = col_character())) %>% filter(
                     !str_detect(`TIPO DE REGISTRO`, "T"))

mes0220$"VALOR PAGO" <- mes0220$"VALOR PAGO"/100
mes0220$`VALOR DO RECIBO` <- mes0220$`VALOR DO RECIBO`/100
mes0220$`VALOR DO SINISTRO` <- mes0220$`VALOR DO SINISTRO`/100
mes0220$`VALOR DE INSS OU ISS ($)` <- mes0220$`VALOR DE INSS OU ISS ($)`/100

#mes 03/2020

mes0320 <- readr:: read_fwf(
  file ="D:/Users/sb046971/OneDrive - Honda/Documentos/Sinistro Bradesco/SN2003_D071015.txt",
  fwf_widths(widths, col_names = names), skip_empty_rows = T,skip = 1,
  col_types = cols("VALOR PAGO" = col_integer(),
                   "VALOR DO SINISTRO" = col_integer(),
                   "VALOR DO RECIBO" = col_integer(), 
                   "VALOR DE INSS OU ISS ($)" = col_integer(),
                   "VALOR DE INSS OU ISS (FAJ-TR)" = col_integer(),
                   "QUANTIDADE PROCEDIMENTOS" = col_integer(),
                   "DATA DO PAGAMENTO" = col_character(),
                   "DATA DE NASCIMENTO" = col_character(),
                   "DATA DE ADMISSÃO" = col_character(),
                   "DATA DE NASCIMENTO(Y2K)" = col_character(),
                   "DATA DO EVENTO(Y2K)" = col_character(),
                   "DATA DO PAGAMENTO(Y2K)" = col_character())) %>% filter(
                     !str_detect(`TIPO DE REGISTRO`, "T"))

mes0320$"VALOR PAGO" <- mes0320$"VALOR PAGO"/100
mes0320$`VALOR DO RECIBO` <- mes0320$`VALOR DO RECIBO`/100
mes0320$`VALOR DO SINISTRO` <- mes0320$`VALOR DO SINISTRO`/100
mes0320$`VALOR DE INSS OU ISS ($)` <- mes0320$`VALOR DE INSS OU ISS ($)`/100

#mes 04/2020

mes0420 <- readr:: read_fwf(
  file ="D:/Users/sb046971/OneDrive - Honda/Documentos/Sinistro Bradesco/SN2004_D071015.txt",
  fwf_widths(widths, col_names = names), skip_empty_rows = T,skip = 1,
  col_types = cols("VALOR PAGO" = col_integer(),
                   "VALOR DO SINISTRO" = col_integer(),
                   "VALOR DO RECIBO" = col_integer(), 
                   "VALOR DE INSS OU ISS ($)" = col_integer(),
                   "VALOR DE INSS OU ISS (FAJ-TR)" = col_integer(),
                   "QUANTIDADE PROCEDIMENTOS" = col_integer(),
                   "DATA DO PAGAMENTO" = col_character(),
                   "DATA DE NASCIMENTO" = col_character(),
                   "DATA DE ADMISSÃO" = col_character(),
                   "DATA DE NASCIMENTO(Y2K)" = col_character(),
                   "DATA DO EVENTO(Y2K)" = col_character(),
                   "DATA DO PAGAMENTO(Y2K)" = col_character())) %>% filter(
                     !str_detect(`TIPO DE REGISTRO`, "T"))

mes0420$"VALOR PAGO" <- mes0420$"VALOR PAGO"/100
mes0420$`VALOR DO RECIBO` <- mes0420$`VALOR DO RECIBO`/100
mes0420$`VALOR DO SINISTRO` <- mes0420$`VALOR DO SINISTRO`/100
mes0420$`VALOR DE INSS OU ISS ($)` <- mes0420$`VALOR DE INSS OU ISS ($)`/100

#mes 05/2020

mes0520 <- readr:: read_fwf(
  file ="D:/Users/sb046971/OneDrive - Honda/Documentos/Sinistro Bradesco/SN2005_D071015.txt",
  fwf_widths(widths, col_names = names), skip_empty_rows = T,skip = 1,
  col_types = cols("VALOR PAGO" = col_integer(),
                   "VALOR DO SINISTRO" = col_integer(),
                   "VALOR DO RECIBO" = col_integer(), 
                   "VALOR DE INSS OU ISS ($)" = col_integer(),
                   "VALOR DE INSS OU ISS (FAJ-TR)" = col_integer(),
                   "QUANTIDADE PROCEDIMENTOS" = col_integer(),
                   "DATA DO PAGAMENTO" = col_character(),
                   "DATA DE NASCIMENTO" = col_character(),
                   "DATA DE ADMISSÃO" = col_character(),
                   "DATA DE NASCIMENTO(Y2K)" = col_character(),
                   "DATA DO EVENTO(Y2K)" = col_character(),
                   "DATA DO PAGAMENTO(Y2K)" = col_character())) %>% filter(
                     !str_detect(`TIPO DE REGISTRO`, "T"))

mes0520$"VALOR PAGO" <- mes0520$"VALOR PAGO"/100
mes0520$`VALOR DO RECIBO` <- mes0520$`VALOR DO RECIBO`/100
mes0520$`VALOR DO SINISTRO` <- mes0520$`VALOR DO SINISTRO`/100
mes0520$`VALOR DE INSS OU ISS ($)` <- mes0520$`VALOR DE INSS OU ISS ($)`/100

#mes 06/2020

mes0620 <- readr:: read_fwf(
  file ="D:/Users/sb046971/OneDrive - Honda/Documentos/Sinistro Bradesco/SN2006_D071015.txt",
  fwf_widths(widths, col_names = names), skip_empty_rows = T,skip = 1,
  col_types = cols("VALOR PAGO" = col_integer(),
                   "VALOR DO SINISTRO" = col_integer(),
                   "VALOR DO RECIBO" = col_integer(), 
                   "VALOR DE INSS OU ISS ($)" = col_integer(),
                   "VALOR DE INSS OU ISS (FAJ-TR)" = col_integer(),
                   "QUANTIDADE PROCEDIMENTOS" = col_integer(),
                   "DATA DO PAGAMENTO" = col_character(),
                   "DATA DE NASCIMENTO" = col_character(),
                   "DATA DE ADMISSÃO" = col_character(),
                   "DATA DE NASCIMENTO(Y2K)" = col_character(),
                   "DATA DO EVENTO(Y2K)" = col_character(),
                   "DATA DO PAGAMENTO(Y2K)" = col_character())) %>% filter(
                     !str_detect(`TIPO DE REGISTRO`, "T"))

mes0620$"VALOR PAGO" <- mes0620$"VALOR PAGO"/100
mes0620$`VALOR DO RECIBO` <- mes0620$`VALOR DO RECIBO`/100
mes0620$`VALOR DO SINISTRO` <- mes0620$`VALOR DO SINISTRO`/100
mes0620$`VALOR DE INSS OU ISS ($)` <- mes0620$`VALOR DE INSS OU ISS ($)`/100

#mes 07/2020

mes0720 <- readr:: read_fwf(
  file ="D:/Users/sb046971/OneDrive - Honda/Documentos/Sinistro Bradesco/SN2007_D071015.txt",
  fwf_widths(widths, col_names = names), skip_empty_rows = T,skip = 1,
  col_types = cols("VALOR PAGO" = col_integer(),
                   "VALOR DO SINISTRO" = col_integer(),
                   "VALOR DO RECIBO" = col_integer(), 
                   "VALOR DE INSS OU ISS ($)" = col_integer(),
                   "VALOR DE INSS OU ISS (FAJ-TR)" = col_integer(),
                   "QUANTIDADE PROCEDIMENTOS" = col_integer(),
                   "DATA DO PAGAMENTO" = col_character(),
                   "DATA DE NASCIMENTO" = col_character(),
                   "DATA DE ADMISSÃO" = col_character(),
                   "DATA DE NASCIMENTO(Y2K)" = col_character(),
                   "DATA DO EVENTO(Y2K)" = col_character(),
                   "DATA DO PAGAMENTO(Y2K)" = col_character())) %>% filter(
                     !str_detect(`TIPO DE REGISTRO`, "T"))

mes0720$"VALOR PAGO" <- mes0720$"VALOR PAGO"/100
mes0720$`VALOR DO RECIBO` <- mes0720$`VALOR DO RECIBO`/100
mes0720$`VALOR DO SINISTRO` <- mes0720$`VALOR DO SINISTRO`/100
mes0720$`VALOR DE INSS OU ISS ($)` <- mes0720$`VALOR DE INSS OU ISS ($)`/100

#mes 08/2020

mes0820 <- readr:: read_fwf(
  file ="D:/Users/sb046971/OneDrive - Honda/Documentos/Sinistro Bradesco/SN2008_D071015.txt",
  fwf_widths(widths, col_names = names), skip_empty_rows = T,skip = 1,
  col_types = cols("VALOR PAGO" = col_integer(),
                   "VALOR DO SINISTRO" = col_integer(),
                   "VALOR DO RECIBO" = col_integer(), 
                   "VALOR DE INSS OU ISS ($)" = col_integer(),
                   "VALOR DE INSS OU ISS (FAJ-TR)" = col_integer(),
                   "QUANTIDADE PROCEDIMENTOS" = col_integer(),
                   "DATA DO PAGAMENTO" = col_character(),
                   "DATA DE NASCIMENTO" = col_character(),
                   "DATA DE ADMISSÃO" = col_character(),
                   "DATA DE NASCIMENTO(Y2K)" = col_character(),
                   "DATA DO EVENTO(Y2K)" = col_character(),
                   "DATA DO PAGAMENTO(Y2K)" = col_character())) %>% filter(
                     !str_detect(`TIPO DE REGISTRO`, "T"))

mes0820$"VALOR PAGO" <- mes0820$"VALOR PAGO"/100
mes0820$`VALOR DO RECIBO` <- mes0820$`VALOR DO RECIBO`/100
mes0820$`VALOR DO SINISTRO` <- mes0820$`VALOR DO SINISTRO`/100
mes0820$`VALOR DE INSS OU ISS ($)` <- mes0820$`VALOR DE INSS OU ISS ($)`/100

#mes 09/2020

mes0920 <- readr:: read_fwf(
  file ="D:/Users/sb046971/OneDrive - Honda/Documentos/Sinistro Bradesco/SN2009_D071015.txt",
  fwf_widths(widths, col_names = names), skip_empty_rows = T,skip = 1,
  col_types = cols("VALOR PAGO" = col_integer(),
                   "VALOR DO SINISTRO" = col_integer(),
                   "VALOR DO RECIBO" = col_integer(), 
                   "VALOR DE INSS OU ISS ($)" = col_integer(),
                   "VALOR DE INSS OU ISS (FAJ-TR)" = col_integer(),
                   "QUANTIDADE PROCEDIMENTOS" = col_integer(),
                   "DATA DO PAGAMENTO" = col_character(),
                   "DATA DE NASCIMENTO" = col_character(),
                   "DATA DE ADMISSÃO" = col_character(),
                   "DATA DE NASCIMENTO(Y2K)" = col_character(),
                   "DATA DO EVENTO(Y2K)" = col_character(),
                   "DATA DO PAGAMENTO(Y2K)" = col_character())) %>% filter(
                     !str_detect(`TIPO DE REGISTRO`, "T"))

mes0920$"VALOR PAGO" <- mes0920$"VALOR PAGO"/100
mes0920$`VALOR DO RECIBO` <- mes0920$`VALOR DO RECIBO`/100
mes0920$`VALOR DO SINISTRO` <- mes0920$`VALOR DO SINISTRO`/100
mes0920$`VALOR DE INSS OU ISS ($)` <- mes0920$`VALOR DE INSS OU ISS ($)`/100

#mes 10/2020

mes1020 <- readr:: read_fwf(
  file ="D:/Users/sb046971/OneDrive - Honda/Documentos/Sinistro Bradesco/SN2010_D071015.txt",
  fwf_widths(widths, col_names = names), skip_empty_rows = T,skip = 1,
  col_types = cols("VALOR PAGO" = col_integer(),
                   "VALOR DO SINISTRO" = col_integer(),
                   "VALOR DO RECIBO" = col_integer(), 
                   "VALOR DE INSS OU ISS ($)" = col_integer(),
                   "VALOR DE INSS OU ISS (FAJ-TR)" = col_integer(),
                   "QUANTIDADE PROCEDIMENTOS" = col_integer(),
                   "DATA DO PAGAMENTO" = col_character(),
                   "DATA DE NASCIMENTO" = col_character(),
                   "DATA DE ADMISSÃO" = col_character(),
                   "DATA DE NASCIMENTO(Y2K)" = col_character(),
                   "DATA DO EVENTO(Y2K)" = col_character(),
                   "DATA DO PAGAMENTO(Y2K)" = col_character())) %>% filter(
                     !str_detect(`TIPO DE REGISTRO`, "T"))

mes1020$"VALOR PAGO" <- mes1020$"VALOR PAGO"/100
mes1020$`VALOR DO RECIBO` <- mes1020$`VALOR DO RECIBO`/100
mes1020$`VALOR DO SINISTRO` <- mes1020$`VALOR DO SINISTRO`/100
mes1020$`VALOR DE INSS OU ISS ($)` <- mes1020$`VALOR DE INSS OU ISS ($)`/100

#mes 11/2020

mes1120 <- readr:: read_fwf(
  file ="D:/Users/sb046971/OneDrive - Honda/Documentos/Sinistro Bradesco/SN2011_D071015.txt",
  fwf_widths(widths, col_names = names), skip_empty_rows = T,skip = 1,
  col_types = cols("VALOR PAGO" = col_integer(),
                   "VALOR DO SINISTRO" = col_integer(),
                   "VALOR DO RECIBO" = col_integer(), 
                   "VALOR DE INSS OU ISS ($)" = col_integer(),
                   "VALOR DE INSS OU ISS (FAJ-TR)" = col_integer(),
                   "QUANTIDADE PROCEDIMENTOS" = col_integer(),
                   "DATA DO PAGAMENTO" = col_character(),
                   "DATA DE NASCIMENTO" = col_character(),
                   "DATA DE ADMISSÃO" = col_character(),
                   "DATA DE NASCIMENTO(Y2K)" = col_character(),
                   "DATA DO EVENTO(Y2K)" = col_character(),
                   "DATA DO PAGAMENTO(Y2K)" = col_character())) %>% filter(
                     !str_detect(`TIPO DE REGISTRO`, "T"))

mes1120$"VALOR PAGO" <- mes1120$"VALOR PAGO"/100
mes1120$`VALOR DO RECIBO` <- mes1120$`VALOR DO RECIBO`/100
mes1120$`VALOR DO SINISTRO` <- mes1120$`VALOR DO SINISTRO`/100
mes1120$`VALOR DE INSS OU ISS ($)` <- mes1120$`VALOR DE INSS OU ISS ($)`/100

#mes 12/2020

mes1220 <- readr:: read_fwf(
  file ="D:/Users/sb046971/OneDrive - Honda/Documentos/Sinistro Bradesco/SN2012_D071015.txt",
  fwf_widths(widths, col_names = names), skip_empty_rows = T,skip = 1,
  col_types = cols("VALOR PAGO" = col_integer(),
                   "VALOR DO SINISTRO" = col_integer(),
                   "VALOR DO RECIBO" = col_integer(), 
                   "VALOR DE INSS OU ISS ($)" = col_integer(),
                   "VALOR DE INSS OU ISS (FAJ-TR)" = col_integer(),
                   "QUANTIDADE PROCEDIMENTOS" = col_integer(),
                   "DATA DO PAGAMENTO" = col_character(),
                   "DATA DE NASCIMENTO" = col_character(),
                   "DATA DE ADMISSÃO" = col_character(),
                   "DATA DE NASCIMENTO(Y2K)" = col_character(),
                   "DATA DO EVENTO(Y2K)" = col_character(),
                   "DATA DO PAGAMENTO(Y2K)" = col_character())) %>% filter(
                     !str_detect(`TIPO DE REGISTRO`, "T"))

mes1220$"VALOR PAGO" <- mes1220$"VALOR PAGO"/100
mes1220$`VALOR DO RECIBO` <- mes1220$`VALOR DO RECIBO`/100
mes1220$`VALOR DO SINISTRO` <- mes1220$`VALOR DO SINISTRO`/100
mes1220$`VALOR DE INSS OU ISS ($)` <- mes1220$`VALOR DE INSS OU ISS ($)`/100

#mes 01/2021

mes0121 <- readr:: read_fwf(
  file ="D:/Users/sb046971/OneDrive - Honda/Documentos/Sinistro Bradesco/SN2101_D071015.txt",
  fwf_widths(widths, col_names = names), skip_empty_rows = T,skip = 1,
  col_types = cols("VALOR PAGO" = col_integer(),
                   "VALOR DO SINISTRO" = col_integer(),
                   "VALOR DO RECIBO" = col_integer(), 
                   "VALOR DE INSS OU ISS ($)" = col_integer(),
                   "VALOR DE INSS OU ISS (FAJ-TR)" = col_integer(),
                   "QUANTIDADE PROCEDIMENTOS" = col_integer(),
                   "DATA DO PAGAMENTO" = col_character(),
                   "DATA DE NASCIMENTO" = col_character(),
                   "DATA DE ADMISSÃO" = col_character(),
                   "DATA DE NASCIMENTO(Y2K)" = col_character(),
                   "DATA DO EVENTO(Y2K)" = col_character(),
                   "DATA DO PAGAMENTO(Y2K)" = col_character())) %>% filter(
                     !str_detect(`TIPO DE REGISTRO`, "T"))

mes0121$"VALOR PAGO" <- mes0121$"VALOR PAGO"/100
mes0121$`VALOR DO RECIBO` <- mes0121$`VALOR DO RECIBO`/100
mes0121$`VALOR DO SINISTRO` <- mes0121$`VALOR DO SINISTRO`/100
mes0121$`VALOR DE INSS OU ISS ($)` <- mes0121$`VALOR DE INSS OU ISS ($)`/100

#mes 02/2021

mes0221 <- readr:: read_fwf(
  file ="D:/Users/sb046971/OneDrive - Honda/Documentos/Sinistro Bradesco/SN2102_D071015.txt",
  fwf_widths(widths, col_names = names), skip_empty_rows = T,skip = 1,
  col_types = cols("VALOR PAGO" = col_integer(),
                   "VALOR DO SINISTRO" = col_integer(),
                   "VALOR DO RECIBO" = col_integer(), 
                   "VALOR DE INSS OU ISS ($)" = col_integer(),
                   "VALOR DE INSS OU ISS (FAJ-TR)" = col_integer(),
                   "QUANTIDADE PROCEDIMENTOS" = col_integer(),
                   "DATA DO PAGAMENTO" = col_character(),
                   "DATA DE NASCIMENTO" = col_character(),
                   "DATA DE ADMISSÃO" = col_character(),
                   "DATA DE NASCIMENTO(Y2K)" = col_character(),
                   "DATA DO EVENTO(Y2K)" = col_character(),
                   "DATA DO PAGAMENTO(Y2K)" = col_character())) %>% filter(
                     !str_detect(`TIPO DE REGISTRO`, "T"))

mes0221$"VALOR PAGO" <- mes0221$"VALOR PAGO"/100
mes0221$`VALOR DO RECIBO` <- mes0221$`VALOR DO RECIBO`/100
mes0221$`VALOR DO SINISTRO` <- mes0221$`VALOR DO SINISTRO`/100
mes0221$`VALOR DE INSS OU ISS ($)` <- mes0221$`VALOR DE INSS OU ISS ($)`/100

#### BIND DATABASE ALL MONTHS ####

bradesco_consolidado <- bind_rows(mes0518,mes0618,mes0718,mes0818,mes0918,mes1018,
                                  mes1118,mes1218,mes0119,mes0219,mes0319,mes0419,
                                  mes0519,mes0619,mes0719,mes0819,mes0919,mes1019,
                                  mes1119,mes1219,mes0120,mes0220,mes0320,mes0420,
                                  mes0520,mes0620,mes0720,mes0820,mes0920,mes1020,
                                  mes1120,mes1220,mes0121,mes0221)

#### TREATMENT DATABASE FACTORS ####

bradesco_consolidado$`TIPO DE EVENTO` <- factor(bradesco_consolidado$`TIPO DE EVENTO`,
                                                label = c("Sin. Manual","Consulta",
                                                          "Evento","Desp. Hosp.",
                                                          "Hon. Med.","Exame Simp.",
                                                          "Exame Espec.","Cl. Espec.",
                                                          "Farmacia","Ter. Inf."), 
                                                levels = 0:9)

bradesco_consolidado$`TIPO DA SUBFATURA` <- as.numeric(
  bradesco_consolidado$`TIPO DA SUBFATURA`)

bradesco_consolidado$`TIPO DA SUBFATURA`<-factor(bradesco_consolidado$`TIPO DA SUBFATURA`,
                                                label = c("Técnica","Cancelada",
                                                          "Administrativa"), 
                                                levels = 1:3)

bradesco_consolidado$SEXO <- factor(bradesco_consolidado$SEXO,
                                    label = c("M","F"),levels = 1:2)

bradesco_consolidado$`GRAU DE PARENTESCO` <- factor(
  bradesco_consolidado$`GRAU DE PARENTESCO`,
  label = c("Titular","Conjuge","Filho","Mãe","Pai",
            "Sogro","Sogra","Tutelado","Outros"),levels = 0:8)

bradesco_consolidado$newtype <- paste(bradesco_consolidado$`TIPO DE EVENTO`,
                                      bradesco_consolidado$`CÓDIGO DO PROCEDIMENTO`)

proced <- fread(file = "D:/Users/sb046971/OneDrive - Honda/Documentos/Sinistro Bradesco/Tabela Procedimentos - ORG.txt",
                colClasses = c("CÓDIGO DO PROCEDIMENTO" = "character"))

bradesco_consolidado <- left_join(bradesco_consolidado,proced,
                                  by = "CÓDIGO DO PROCEDIMENTO")

bradesco_consolidado$Tipo_evento <-if_else(
  bradesco_consolidado$ESPECIALIDADE == "99","Reembolso",
  if_else(grepl('CONSULTA',bradesco_consolidado$`NOME DO PROCEDIMENTO`),"Consulta",
          if_else(startsWith(bradesco_consolidado$`CÓDIGO DO PROCEDIMENTO`,"4"),"Exame",
                  if_else(bradesco_consolidado$`TIPO DE EVENTO` == "Desp. Hosp.","Internação",
                          if_else(grepl('SESSAO',bradesco_consolidado$`NOME DO PROCEDIMENTO`),"Terapia",
                                  if_else(grepl('ATENDIMENTO',bradesco_consolidado$`NOME DO PROCEDIMENTO`),"Consulta",
                                          if_else(bradesco_consolidado$`TIPO DE EVENTO` == "Consulta", "Consulta",
                                                  if_else(bradesco_consolidado$`TIPO DE EVENTO` == "Cl. Espec.", "Desp. Hosp.",
                                                          if_else(bradesco_consolidado$`TIPO DE EVENTO` == "Exame Espec.", "Exame", 
                                                                  if_else(bradesco_consolidado$`TIPO DE EVENTO` == "Hon. Med.", "Internação","Desp. Hosp."))))))))))

# bradesco_consolidado$`CODIGO AUTORIZAÇÃO` <- as.numeric(
#   bradesco_consolidado$`CODIGO AUTORIZAÇÃO`)
# 
# bradesco_consolidado$`CODIGO AUTORIZAÇÃO` <- factor(
#   bradesco_consolidado$`CODIGO AUTORIZAÇÃO`,label = c("Segurado Novo",
#                                                       "Extravio ou perda do cartão",
#                                                       "Admissional","Demissional",
#                                                       "Periódico",
#                                                       "Acidente de Trabalho",
#                                                       "Outros",
#                                                       "Assistência a demitidos",
#                                                       "Medicina Social"),levels = 1:9)

# bradesco_consolidado$ESPECIALIDADE <- as.numeric(
#   bradesco_consolidado$ESPECIALIDADE)
# 
# bradesco_consolidado$ESPECIALIDADE<-factor(bradesco_consolidado$ESPECIALIDADE,
#           label = c("Acupuntura","Alergia e Imunologia","Anestesiologia",
#                     "Angiologia","Cancerologia (oncologia)","Cardiologia",
#                     "Cirurgia Cardiovascular","Cirurgia da Mão",
#                     "Cirurgia de cabeça e pescoço","Cirurgia do Aparelho Digestivo",
#                     "Cirurgia Geral","Cirurgia Pediátrica","Cirurgia Plástica",
#                     "Cirurgia Torácica","Cirurgia Vascular",
#                     "Clínica Médica (Medicina interna)","Coloproctologia",
#                     "Dermatologia","Endocrinologia e Metabologia","Endoscopia",
#                     "Gastroenterologia","Genética médica","Geriatria",
#                     "Ginecologia e obstetrícia","Hematologia e Hemoterapia",
#                     "Homeopatia","Infectologia","Mastologia",
#                     "Medicina de Família e Comunidade","Medicina de Emergência",
#                     "Medicina do Trabalho","Medicina do Tráfego",
#                     "Medicina Esportiva","Medicina Física e Reabilitação",
#                     "Medicina Intensiva","Medicina Legal e Perícia Médica",
#                     "Medicina Nuclear","Medicina Preventiva e Social",
#                     "Nefrologia","Neurocirurgia","Neurologia","Nutrologia",
#                     "Obstetrícia","Oftalmologia","Ortopedia e Traumatologia",
#                     "Otorrinolaringologia","Patologia",
#                     "Patologia Clínica/Medicina laboratorial","Pediatria",
#                     "Pneumologia","Psiquiatria","Radiologia e Diagnóstico por Imagem",
#                     "Radioterapia","Reumatologia","Urologia"),levels = 1:55)

#### TREATMENT DATABASE DATES ####

#### AAMMDD

bradesco_consolidado$`DATA DO PAGAMENTO` <- as.character(
  bradesco_consolidado$`DATA DO PAGAMENTO`)

bradesco_consolidado$`DATA DO PAGAMENTO` <- as.Date(chron(format(as.Date(
  bradesco_consolidado$`DATA DO PAGAMENTO`, format = "%y%m%d"),"%m/%d/%y")))

bradesco_consolidado$`DATA DE NASCIMENTO` <- as.character(
  bradesco_consolidado$`DATA DE NASCIMENTO`)

bradesco_consolidado$`DATA DE NASCIMENTO` <- as.Date(chron(format(as.Date(
  bradesco_consolidado$`DATA DE NASCIMENTO`, format = "%y%m%d"),"%m/%d/%y")))

bradesco_consolidado$`DATA DO EVENTO` <- as.character(
  bradesco_consolidado$`DATA DO EVENTO`)

bradesco_consolidado$`DATA DO EVENTO` <- as.Date(chron(format(as.Date(
  bradesco_consolidado$`DATA DO EVENTO`, format = "%y%m%d"),"%m/%d/%y")))

#### AAAAMMDD

bradesco_consolidado$`DATA DE ADMISSÃO` <- as.character(
  bradesco_consolidado$`DATA DE ADMISSÃO`)

bradesco_consolidado$`DATA DE ADMISSÃO` <- as.Date(chron(format(as.Date(
  bradesco_consolidado$`DATA DE ADMISSÃO`, format = "%Y%m%d"),"%m/%d/%y")))

bradesco_consolidado$`DATA DE NASCIMENTO(Y2K)` <- as.character(
  bradesco_consolidado$`DATA DE NASCIMENTO(Y2K)`)

bradesco_consolidado$`DATA DE NASCIMENTO(Y2K)` <- as.Date(chron(format(as.Date(
  bradesco_consolidado$`DATA DE NASCIMENTO(Y2K)`, format = "%Y%m%d"),"%m/%d/%y")))

bradesco_consolidado$`DATA DO EVENTO(Y2K)` <- as.character(
  bradesco_consolidado$`DATA DO EVENTO(Y2K)`)

bradesco_consolidado$`DATA DO EVENTO(Y2K)` <- as.Date(chron(format(as.Date(
  bradesco_consolidado$`DATA DO EVENTO(Y2K)`, format = "%Y%m%d"),"%m/%d/%y")))

bradesco_consolidado$`DATA DO PAGAMENTO(Y2K)` <- as.character(
  bradesco_consolidado$`DATA DO PAGAMENTO(Y2K)`)

bradesco_consolidado$`DATA DO PAGAMENTO(Y2K)` <- as.Date(chron(format(as.Date(
  bradesco_consolidado$`DATA DO PAGAMENTO(Y2K)`, format = "%Y%m%d"),"%m/%d/%y")))


### CREATE DATES MON/YEAR

bradesco_consolidado$COMPETENCIA <- format(bradesco_consolidado$`DATA DO PAGAMENTO`,"%d/%m/%Y")

bradesco_consolidado$COMPETENCIA <- substr(bradesco_consolidado$COMPETENCIA,
                                           start = 4, stop = 11)

bradesco_consolidado$COMPETENCIA2 <- format(bradesco_consolidado$`DATA DO EVENTO(Y2K)`,
                                            "%d/%m/%Y")
bradesco_consolidado$COMPETENCIA2 <- substr(bradesco_consolidado$COMPETENCIA2, 
                                            start = 4, stop = 11)

#### SELECT DUPLICATED LINES TO DATA AND DROP ####

bradesco_consolidado$DUPLICADOS <- duplicated(bradesco_consolidado)

fwrite(bradesco_consolidado, 
       file = "D:/Users/sb046971/OneDrive - Honda/Documentos/Bradesco - Consolidado.csv", 
       sep = "|", dec = ",")

bradesco_sd <- bradesco_consolidado %>% filter(DUPLICADOS == "FALSE") %>% select(
                                             -DUPLICADOS,-`VALOR DO RECIBO`,-FILLER,
                                             -FILLER_1,-CDB,-`CODIGO AUTORIZAÇÃO`,
                                             -`TROCA DE ACOMODAÇÃO`,-`DATA DO EVENTO`)

#### DUPLICATED VERIFYING ####

soment_dupli <- bradesco_consolidado %>% filter(DUPLICADOS == "TRUE")

conta.hospit <- fread("D:/Users/sb046971/OneDrive - Honda/Documentos/ContaHosp.csv",dec = ",")

names(conta.hospit)[names(
  conta.hospit) == "Senha Autorização"] <- "NÚMERO DO DOCUMENTO"

verify <- inner_join(soment_dupli,conta.hospit, by = "NÚMERO DO DOCUMENTO")

#### ANALYSIS IN DATA ####

sum(bradesco_consolidado$`VALOR DO SINISTRO`)
sum(bradesco_consolidado$`VALOR DO RECIBO`)

analysis1 <- bradesco_consolidado %>% filter(
  DUPLICADOS == "TRUE") %>% group_by(
    `CÓDIGO DO PROCEDIMENTO`) %>% summarise(
      sinistro = sum(`VALOR DO SINISTRO`) , recibo = sum(`VALOR DO RECIBO`))

sum(analysis1$sinistro)
sum(analysis1$recibo)

analysis2 <- bradesco_consolidado %>% filter(
  DUPLICADOS == "TRUE") %>% group_by(`TIPO DE EVENTO`) %>% summarise(
      sinistro = sum(`VALOR DO SINISTRO`))

analysis3 <- bradesco_consolidado %>% filter(
  DUPLICADOS == "TRUE") %>% group_by(`GRAU DE PARENTESCO`) %>% summarise(
    sinistro = sum(`VALOR DO SINISTRO`))

analysis4 <- bradesco_consolidado %>% filter(
  DUPLICADOS == "TRUE") %>% group_by(`DATA DO PAGAMENTO`) %>% summarise(
    sinistro = sum(`VALOR DO SINISTRO`))

bradesco_consolidado$diaEvxPg <- difftime(bradesco_consolidado$`DATA DO PAGAMENTO`,
                                          bradesco_consolidado$`DATA DO EVENTO(Y2K)`,units = "days")

bradesco_consolidado$diaEvxPg <- as.numeric(bradesco_consolidado$diaEvxPg)

bradesco_consolidado$mesEvxPg <-floor((as.double(bradesco_consolidado$diaEvxPg)/365)*12)

bradesco_consolidado$flag12meses <- if_else(bradesco_consolidado$mesEvxPg > 11,"+","0")

bradesco_consolidado$flag4meses <- if_else(bradesco_consolidado$diaEvxPg > 120,"+","0")

bradesco_consolidado$flag6meses <- if_else(bradesco_consolidado$diaEvxPg > 180,"+","0")

table(bradesco_consolidado$flag12meses)

table(bradesco_consolidado$flag4meses)

table(bradesco_consolidado$flag6meses)

analysis5 <- bradesco_consolidado %>% filter(flag12meses == "+") %>% 
  group_by(.) %>% summarise(sum(`VALOR DO SINISTRO`))

analysis6 <- bradesco_consolidado %>% filter(flag4meses == "+") %>% 
  group_by(.) %>% summarise(sum(`VALOR DO SINISTRO`))

analysis7 <- bradesco_consolidado %>% filter(flag6meses == "+") %>% 
  group_by(.) %>% summarise(sum(`VALOR DO SINISTRO`))

analysis8 <- bradesco_consolidado %>% group_by(`NÚMERO DO CERTIFICADO`,
                                    `PRESTADOR EXECUTANTE`) %>% summarise(
                                      med_dias = mean(diaEvxPg),
                                      med_meses = mean(mesEvxPg),
                                      valor_sinistro = sum(`VALOR DO SINISTRO`))

analysis9 <- bradesco_consolidado %>% group_by(`PRESTADOR EXECUTANTE`,mesEvxPg) %>% summarise(
                                      valor_sinistro = sum(`VALOR DO SINISTRO`))

analysis10 <- bradesco_consolidado %>% group_by(`PRESTADOR EXECUTANTE`) %>% summarise(
                                      med_dias = mean(diaEvxPg),
                                      med_meses = mean(mesEvxPg),
                                      valor_sinistro = sum(`VALOR DO SINISTRO`))

analysis11 <- bradesco_sd %>% group_by(`CÓDIGO DO PROCEDIMENTO`,diaEvxPg) %>% summarise(
                                      cont_proc = sum(`QUANTIDADE PROCEDIMENTOS`)) 

analysis11$flag <- if_else(analysis11$diaEvxPg > 120, "+","0")

analysis12 <- analysis11 %>% filter(`CÓDIGO DO PROCEDIMENTO` == "10101012" & 
                                      flag == "+") %>% group_by(
                                        `CÓDIGO DO PROCEDIMENTO`) %>% summarise(
                                          qtde_proc = sum(cont_proc))

analysis13 <- analysis11 %>% filter(flag == "+") %>% group_by(
  `CÓDIGO DO PROCEDIMENTO`) %>% summarise(qtde_proc = sum(cont_proc))

analysis14 <- bradesco_sd %>% group_by(`CÓDIGO DO PROCEDIMENTO`,diaEvxPg) %>% summarise(
  cont_proc = n())

analysis14$flag <- if_else(analysis14$diaEvxPg > 120, "+","0")

analysis15 <- analysis14 %>% filter(`CÓDIGO DO PROCEDIMENTO` == "10101012" & 
                                      flag == "+") %>% group_by(
                                        `CÓDIGO DO PROCEDIMENTO`) %>% summarise(
                                          qtde_proc = sum(cont_proc))

analysis16 <- analysis14 %>% filter(flag == "+") %>% group_by(
  `CÓDIGO DO PROCEDIMENTO`) %>% summarise(qtde_proc = sum(cont_proc))

analysis17 <- bradesco_sd %>% select(`PRESTADOR EXECUTANTE`,
                                   diaEvxPg,mesEvxPg,
                                   `VALOR PAGO`) %>% filter(diaEvxPg > 120)

sum(analysis17$`VALOR PAGO`)

analysis18 <- bradesco_sd %>% select(`PRESTADOR EXECUTANTE`,
                                   diaEvxPg,mesEvxPg,
                                   `VALOR PAGO`)

analysis19 <- teste %>% filter(
  `DATA DO PAGAMENTO(Y2K)` > "01/05/18",
    `DATA DO PAGAMENTO(Y2K)` <= "31/06/18") %>% filter(
      `DATA DO EVENTO(Y2K)` < "31/12/18")

sum(analysis19$`VALOR DO SINISTRO`)

# bradesco_sd$teste <- format(bradesco_sd$`DATA DO PAGAMENTO`,"%d/%m/%Y")
# 
# bradesco_sd$teste <- as.Date(bradesco_sd$teste)

analysis20 <- bradesco_consolidado %>% filter(ESPECIALIDADE == "99") %>% group_by(.) %>% summarise(
  valor = sum(`VALOR DO SINISTRO`))

analysis20 <- analysis20 %>% filter(`TIPO DE EVENTO` == "Consulta")

fwrite(analysis20, file = "D:/Users/sb046971/OneDrive - Honda/Documentos/Reemb.csv", sep = ";", dec = ",")

fwrite(bradesco_sd, file = "D:/Users/sb046971/OneDrive - Honda/Documentos/teste2.csv", sep = ";", dec = ",")

#### GRAPH MEAN DAYS AND VALUES ####

ggplot(analysis10, aes(x = med_dias)) + 
  geom_density(fill = "#4271AE", colour = "#1F3552",alpha = 0.6) + 
  scale_x_continuous(name = "Days Mean", breaks = seq(0,900,50)) + 
  scale_y_continuous(name = "Incidence") + ggtitle("Average Days Incidence") +
  theme_bw() + theme(axis.line = element_line(size=1, colour = "black"),
                     panel.grid.major = element_line(colour = "#d3d3d3"),
                     panel.grid.minor = element_blank(),
                     panel.border = element_blank(),
                     panel.background = element_blank(),
                     plot.title = element_text(size = 14, family = "Tahoma", 
                                               face = "bold", hjust = 0.5),
                     text=element_text(family="Tahoma"),
                     axis.text.x=element_text(colour="black", size = 9),
                     axis.text.y=element_text(colour="black", size = 9)) +
  geom_vline(xintercept = 120, size = 1, colour = "#FF3721", linetype = "dashed")

ggplot(analysis10,aes(med_dias,valor_sinistro)) + 
  geom_point(mapping = aes(analysis10$med_dias,analysis10$valor_sinistro)) + 
  scale_x_continuous(name = "Days Mean", breaks = seq(0,900,50)) +
  scale_y_continuous(name = "Values", breaks = seq(0,1900000,100000)) + 
  ggtitle("Values Incidence") + theme_bw() + 
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
  geom_vline(xintercept = 120, size = 1, colour = "#FF3721", linetype = "dashed")

#### GRAPH DAYS ####

ggplot(analysis17, aes(x = diaEvxPg)) + 
  geom_density(fill = "#4271AE", colour = "#1F3552",alpha = 0.6) + 
  scale_x_continuous(name = "Days", breaks = seq(0,900,50)) + 
  scale_y_continuous(name = "Incidence") + ggtitle("Average Days Incidence") +
  theme_bw() + theme(axis.line = element_line(size=1, colour = "black"),
                     panel.grid.major = element_line(colour = "#d3d3d3"),
                     panel.grid.minor = element_blank(),
                     panel.border = element_blank(),
                     panel.background = element_blank(),
                     plot.title = element_text(size = 14, family = "Tahoma", 
                                               face = "bold", hjust = 0.5),
                     text=element_text(family="Tahoma"),
                     axis.text.x=element_text(colour="black", size = 9),
                     axis.text.y=element_text(colour="black", size = 9)) +
  geom_vline(xintercept = 120, size = 1, colour = "#FF3721", linetype = "dashed")

ggplot(analysis18, aes(x = diaEvxPg)) + 
  geom_density(fill = "#4271AE", colour = "#1F3552",alpha = 0.6) + 
  scale_x_continuous(name = "Days", breaks = seq(0,900,50)) + 
  scale_y_continuous(name = "Incidence") + ggtitle("Average Days Incidence") +
  theme_bw() + theme(axis.line = element_line(size=1, colour = "black"),
                     panel.grid.major = element_line(colour = "#d3d3d3"),
                     panel.grid.minor = element_blank(),
                     panel.border = element_blank(),
                     panel.background = element_blank(),
                     plot.title = element_text(size = 14, family = "Tahoma", 
                                               face = "bold", hjust = 0.5),
                     text=element_text(family="Tahoma"),
                     axis.text.x=element_text(colour="black", size = 9),
                     axis.text.y=element_text(colour="black", size = 9)) +
  geom_vline(xintercept = 120, size = 1, colour = "#FF3721", linetype = "dashed")

#### GRAPH DAYS AND VALUES ####

ggplot(analysis17,aes(diaEvxPg,`VALOR PAGO`)) + 
  geom_point(mapping = aes(analysis17$diaEvxPg,analysis17$`VALOR PAGO`)) + 
  scale_x_continuous(name = "Days", breaks = seq(0,900,50)) +
  scale_y_continuous(name = "Values") + 
  ggtitle("Values Incidence") + theme_bw() + 
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
  geom_vline(xintercept = 120, size = 1, colour = "#FF3721", linetype = "dashed")

ggplot(analysis18,aes(diaEvxPg,`VALOR PAGO`)) + 
  geom_point(mapping = aes(analysis18$diaEvxPg,analysis18$`VALOR PAGO`)) + 
  scale_x_continuous(name = "Days", breaks = seq(0,900,50)) +
  scale_y_continuous(name = "Values", breaks = seq(0,60000,5000)) + 
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

#### GRAPH DAYS AND VALUES || ONLY BETWEEN DATES #### 

exceed_between_dates <- fread("D:/Users/sb046971/OneDrive - Honda/Documentos/Bases Testes/
                              Base_Entre_datas.csv", dec = ",")

ggplot(exceed_between_dates,aes(diaEvxPg,`VALOR DO SINISTRO`)) + 
  geom_point(mapping = aes(exceed_between_dates$diaEvxPg,
                           exceed_between_dates$`VALOR DO SINISTRO`)) + 
  scale_x_continuous(name = "Days", breaks = seq(0,900,50)) +
  scale_y_continuous(name = "Values", breaks = seq(0,80000,5000)) + 
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

#### new analysis ####

cost.p.person <- bradesco_sd %>% group_by(`NOME DO PACIENTE`) %>% summarise(
  valor = sum(`VALOR DO SINISTRO`))

analysis21 <- bradesco_consolidado %>% filter(
  `CÓDIGO DO PROCEDIMENTO` == "10101039") %>% group_by(
    `NOME DO PACIENTE`) %>% summarise(qt_cons = sum(`QUANTIDADE PROCEDIMENTOS`),
                                      valor = sum(`VALOR DO SINISTRO`))

analysis22 <- bradesco_consolidado %>% filter(
  `CÓDIGO DO PROCEDIMENTO` == "10101012") %>% group_by(
    `NOME DO PACIENTE`) %>% summarise(qt_cons = sum(`QUANTIDADE PROCEDIMENTOS`),
                                      valor = sum(`VALOR DO SINISTRO`))

analysis23 <- bradesco_consolidado %>% filter(
  `CÓDIGO DO PROCEDIMENTO` == "10106146") %>% group_by(
    `NOME DO PACIENTE`) %>% summarise(qt_cons = sum(`QUANTIDADE PROCEDIMENTOS`),
                                      valor = sum(`VALOR DO SINISTRO`))

analysis24 <- bradesco_consolidado %>% filter(
  `NOME DO PACIENTE` == "MONICA OLIVEIRA SOUZA") 

fwrite(analysis24, file = "D:/Users/sb046971/OneDrive - Honda/Documentos/Utilizacao - Monica.csv", 
       sep = "|", dec = ",")

analysis25 <- bradesco_sd %>% filter(is.na(
  `TIPO DO REFERENCIADO`)) %>% group_by(.) %>% summarise(
  valor = sum(`VALOR DO SINISTRO`))

analysis26 <- bradesco_sd %>% filter(
  `CPF/CGC DO REFERENCIADO` == "00000000000000") %>% group_by(.) %>% summarise(
  valor = sum(`VALOR DO SINISTRO`))

reembolsos <- bradesco_consolidado %>% filter(ESPECIALIDADE == "99")

fwrite(reembolsos, file = "D:/Users/sb046971/OneDrive - Honda/Documentos/Reembolsos - Base sinistro.csv",
       sep = "|", dec = ",")

detalhado <- bradesco_consolidado %>% filter(`NOME DO PACIENTE` == "JOAO KOYTY OJI WADA")

fwrite(detalhado, file = "D:/Users/sb046971/OneDrive - Honda/Documentos/Detalhado J.csv", 
       sep = ";", dec = ",")

analysis27 <- bradesco_consolidado %>% group_by(COMPETENCIA,
                                       `NÚMERO DA SUBFATURA`,
                                       `NOME DA SUBFATURA`,
                                       `NOME DO SEGURADO`,`TIPO DE EVENTO`,
                                       `NOME DO PACIENTE`,newtype,
                                        `PRESTADOR EXECUTANTE`,
                                       `NOME DO PROCEDIMENTO`) %>% summarise(
                                         valor_gasto = sum(
                                           `VALOR DO SINISTRO`))

fwrite(analysis27, file = "D:/Users/sb046971/OneDrive - Honda/Documentos/HighUsers_Bradesco.csv",
       sep = ";", dec = ",")

analysis28 <- bradesco_sd %>% filter(`NOME DO SEGURADO` == "GISELE SEGALA")

fwrite(analysis28, file = "D:/Users/sb046971/OneDrive - Honda/Documentos/lcap_G.csv",
       sep = ";", dec = ",")

graph <- bradesco_consolidado %>% group_by(COMPETENCIA,
                                           Tipo_evento) %>% summarise(
                                             valor = sum(`VALOR DO SINISTRO`))

require(tidyr)

graph2 <- graph %>% spread(COMPETENCIA, valor)

fwrite(graph2,file = "D:/Users/sb046971/OneDrive - Honda/Documentos/Grafico-pdca-jan.csv", sep = ";", dec = ",")

rank_proc <- bradesco_consolidado %>% filter(COMPETENCIA2 %in% c("07/2019", "08/2019", "09/2019",
                                                                "10/2019", "11/2019", "12/2019")
                                             & `NÚMERO DA SUBFATURA` %in% c("001","020",
                                                                            "031")) %>% 
  select(-newtype,-`TROCA DE ACOMODAÇÃO`,-`VALOR DO RECIBO`,-`DATA DE NASCIMENTO(Y2K)`,
         -`VALOR DE INSS OU ISS ($)`,-`VALOR DE INSS OU ISS (FAJ-TR)`,-CDB,-`VALOR PAGO`,
         -`PLANO DO SEGURADO`,-`DATA DE ADMISSÃO`,-`CARGO DO SEGURADO`,-FILLER_1,
         -`MATRICULA ESPECIAL`,-`GRAU DE PARENTESCO`,-SEXO,-`DATA DE NASCIMENTO`,
         -FILLER,-`NÚMERO DO CONTRATO`,-`DATA DO EVENTO`,-`NÚMERO DO DOCUMENTO`,
         -`NOME DO PACIENTE`,-`CÓDIGO DO PACIENTE`,-`NOME DO SEGURADO`,-MATRICULA,
         -`NÚMERO DO CERTIFICADO`,-`TIPO DE REGISTRO`,-`CÓDIGO DO REFERENCIADO`,
         -`CODIGO AUTORIZAÇÃO`,-`TIPO DA SUBFATURA`,-`TIPO DO REFERENCIADO`,-ESPECIALIDADE)

fwrite(rank_proc, file = "D:/Users/sb046971/OneDrive - Honda/Documentos/Ranking_proc.csv", dec = ",", sep = "|")

lcap_JTA <- bradesco_consolidado %>% filter(`NOME DO SEGURADO` == "JORGE TSUGUO ADATI")

fwrite(lcap_JTA, file = "D:/Users/sb046971/OneDrive - Honda/Documentos/lcap_JTA.csv", sep = "|", dec = ",")

lcap_SBJ <- bradesco_consolidado %>% filter(`NOME DO SEGURADO` == "SERGIO BELLETTI JUNIOR")

fwrite(lcap_SBJ, file = "D:/Users/sb046971/OneDrive - Honda/Documentos/lcap_SBJ.csv", sep = "|", dec = ",")

lcap_MRM <- bradesco_consolidado %>% filter(`NOME DO SEGURADO` == "MATEUS RIOITI MORITA")

fwrite(lcap_MRM, file = "D:/Users/sb046971/OneDrive - Honda/Documentos/lcap_MRM.csv", sep = "|", dec = ",")

lcap_ENV <- bradesco_consolidado %>% filter(`NOME DO SEGURADO` == "EDUARDO DE NOVAIS VITORINO")

fwrite(lcap_ENV, file = "D:/Users/sb046971/OneDrive - Honda/Documentos/lcap_ENV.csv", sep = "|", dec = ",")

lcap_MSTK <- bradesco_consolidado %>% filter(`NOME DO PACIENTE` == "MARCIA SETSUKO TANIGAWA KOGA")

fwrite(lcap_MSTK, file = "D:/Users/sb046971/OneDrive - Honda/Documentos/lcap_MSTK.csv", sep = "|", dec = ",")

lcap_SMRZ <- bradesco_consolidado %>% filter(`NOME DO PACIENTE` == "SANDRA MARA RODRIGUES ZAMPIERI")

fwrite(lcap_SMRZ, file = "D:/Users/sb046971/OneDrive - Honda/Documentos/lcap_SMRZ.csv", sep = "|", dec = ",")

lcap_MNGS <- bradesco_consolidado %>% filter(`NOME DO SEGURADO` == "MARIA NUBIA GOMES DE SOUZA")

fwrite(lcap_MNGS, file = "D:/Users/sb046971/OneDrive - Honda/Documentos/lcap_MNGS.csv", sep = "|", dec = ",")

lcap_LSR <- bradesco_consolidado %>% filter(`NOME DO SEGURADO` == "LETICIA SILVA DA ROCHA")

fwrite(lcap_LSR, file = "D:/Users/sb046971/OneDrive - Honda/Documentos/lcap_LSR.csv", sep = "|", dec = ",")

lcap_PKFT <- bradesco_consolidado %>% filter(`NOME DO PACIENTE` == "PAULO KATSUO FONSECA TAKAHASHI")

fwrite(lcap_PKFT, file = "D:/Users/sb046971/OneDrive - Honda/Documentos/lcap_PKFT.csv", sep = "|", dec = ",")

lcap_PFS <- bradesco_consolidado %>% filter(`NOME DO PACIENTE` == "PEDRO FARIAS DOS SANTOS")

fwrite(lcap_PFS, file = "D:/Users/sb046971/OneDrive - Honda/Documentos/lcap_PFS.csv", sep = "|", dec = ",")

lcap_GAM <- bradesco_consolidado %>% filter(`NOME DO PACIENTE` == "GILBERTO APARECIDO MANFRE")

fwrite(lcap_GAM, file = "D:/Users/sb046971/OneDrive - Honda/Documentos/lcap_GAM.csv", sep = "|", dec = ",")

	

analysis29 <- bradesco_consolidado %>% filter(grepl("QUIMIO", `NOME DO PROCEDIMENTO`) 
                                              | grepl("RADIOT", `NOME DO PROCEDIMENTO`)) %>% 
  select(`NOME DO SEGURADO`,`NOME DO PACIENTE`,`NOME DO PROCEDIMENTO`) %>% distinct()

fwrite(analysis29, file = "D:/Users/sb046971/OneDrive - Honda/Documentos/Terap_Colab_bradesco.csv", sep ="|")

#### forecast claim ####


analytic <- fread("D:/Users/sb046971/OneDrive - Honda/Documentos/prev_brad.txt")

tseries <- ts(analytic$Bradesco, frequency = 12, start = c(2007,5))


require(car)
require(tseries)
require(astsa)
require(forecast)
require(lattice)
require(lmtest)
require(randtests)

adf.test(tseries)
adf.test(diff(tseries))

cox.stuart.test(tseries)

sazo=tseries #matriz deve ser completa row x column
sazo
fried = matrix((sazo),nrow=12,ncol=12,byrow=TRUE,dimnames=NULL)
friedman.test(fried)

par(mfrow=c(2,1))

acf(tseries)
pacf(tseries)

acf(diff(tseries)) #1ª diferença
pacf(diff(tseries))

acf(diff(diff(tseries, lag=12)), lag.max = 360) #1ª diferença
pacf(diff(diff(tseries, lag=12)))

plot(diff(diff(tseries, lag=12)))
plot(decompose(tseries))

#modelo = ar(dados.consultas$Valor)

modelo = arima(tseries, order = c(2,1,3), seasonal = list(order = c(1,0,1)))
modelo

plot(as.numeric(modelo$residuals))
shapiro.test(as.numeric(modelo$residuals))

coeftest(modelo)

previsao = forecast(modelo, h=7)
previsao
plot(previsao,lwd=3,xlab="Meses",ylab="Premio",col="gray")

# previsao2 = predict(modelo, n.ahead=12)
# ts.plot(serie.consultas,previsao2$pred)

coeftest(modelo)

previsao <- as.data.frame(previsao, row.names = TRUE)