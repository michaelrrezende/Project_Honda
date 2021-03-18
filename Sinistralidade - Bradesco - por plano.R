#### PACKAGES ####

require(dplyr)
require(data.table)
require(ggplot2)
require(RColorBrewer)
require(readr)
require(stringr)
require(chron)
require(readxl)
require(extrafont)
require(tidyr)
require(readxl)

#### faturas técnicas ####

widths = c(1,4,7,2,35,4,8,1,1,1,4,8,2,6,15,15,2,20,12,32)

names =  c("TIPO DO REGISTRO","NUMERO DA SUBFATURA","NUMERO DO CERTIFICADO",
           "COMPLEMENTO DO CERTIFICADO","NOME SEGURADO/DEPENDENTE",
           "INDIC. SUBF. ANTER/ATUAL","DATA DE NASCIMENTO",
           "CODIGO DO SEXO","ESTADO CIVIL","COD. GRAU PARENT.DEP.",
           "CODIGO DO PLANO","DATA INICIO VIGENCIA","TIPO DE LANÇAMENTO",
           "DATA DE LANCAMENTO","VALOR DO LANCAMENTO","PARTE DO SEGURADO",
           "CODIGO DO LANCAMENTO","CARGO / OCUPACAO","Matrícula","FILLER")


fat.tec0 <- readr:: read_fwf(
  file ="D:/Users/sb046971/Documents/fat/FM01071015.TXT",
  fwf_widths(widths, col_names = names), skip_empty_rows = T,skip = 1,
  col_types = cols("TIPO DO REGISTRO" = col_integer(),
                   "NUMERO DA SUBFATURA" = col_integer(),
                   "NUMERO DO CERTIFICADO" = col_integer(),
                   "COMPLEMENTO DO CERTIFICADO" = col_integer(),
                   "NOME SEGURADO/DEPENDENTE" = col_character(),
                   "INDIC. SUBF. ANTER/ATUAL" = col_character(),
                   "DATA DE NASCIMENTO" = col_character(),
                   "CODIGO DO SEXO" = col_integer(),
                   "ESTADO CIVIL" = col_integer(),
                   "COD. GRAU PARENT.DEP." = col_integer(),
                   "CODIGO DO PLANO" = col_character(),
                   "DATA INICIO VIGENCIA" = col_character(),
                   "TIPO DE LANÇAMENTO" = col_character(),
                   "DATA DE LANCAMENTO" = col_character(),
                   "VALOR DO LANCAMENTO" = col_integer(),
                   "PARTE DO SEGURADO" = col_integer(),
                   "CODIGO DO LANCAMENTO" = col_character(),
                   "CARGO / OCUPACAO" = col_character(),
                   "Matrícula" = col_character(),
                   "FILLER" = col_character())) %>% filter(
                     (`TIPO DO REGISTRO` == 3))

fat.tec0$`DATA DE LANCAMENTO` <- "02/2019"

fat.tec0$`VALOR DO LANCAMENTO` <- fat.tec0$`VALOR DO LANCAMENTO`/100

fat.tec0$`VALOR DO LANCAMENTO` <- ifelse(
  fat.tec0$`CODIGO DO LANCAMENTO` > 11,
  fat.tec0$`VALOR DO LANCAMENTO`*(-1),
  fat.tec0$`VALOR DO LANCAMENTO`)

fat.tec0 <- fat.tec0 %>% group_by(`NUMERO DA SUBFATURA`,`NUMERO DO CERTIFICADO`,
                                  `COMPLEMENTO DO CERTIFICADO`,`NOME SEGURADO/DEPENDENTE`,
                                  `DATA DE NASCIMENTO`,`CODIGO DO SEXO`,`ESTADO CIVIL`,
                                  `COD. GRAU PARENT.DEP.`,`CODIGO DO PLANO`,
                                  `DATA DE LANCAMENTO`,`CARGO / OCUPACAO`) %>% summarise(
                                    `VALOR DO LANCAMENTO` = sum(`VALOR DO LANCAMENTO`))

calculo <- fat.tec0 %>% filter(`NUMERO DO CERTIFICADO` == 0)

fat.tec0 <- fat.tec0 %>% filter(!`NUMERO DO CERTIFICADO` == 0)

fat.tec0$tit <- if_else(fat.tec0$`COMPLEMENTO DO CERTIFICADO` == 0, 1,0)

fat.tec0$conta <- fat.tec0$tit/sum(fat.tec0$tit)*sum(calculo$`VALOR DO LANCAMENTO`)

fat.tec0$`VALOR DO LANCAMENTO` <- fat.tec0$`VALOR DO LANCAMENTO` + fat.tec0$conta

fat.tec0$conta <- NULL
fat.tec0$tit <- NULL

fat.tec1 <- readr:: read_fwf(
  file ="D:/Users/sb046971/Documents/fat/FM02071015.TXT",
  fwf_widths(widths, col_names = names), skip_empty_rows = T,skip = 1,
  col_types = cols("TIPO DO REGISTRO" = col_integer(),
                   "NUMERO DA SUBFATURA" = col_integer(),
                   "NUMERO DO CERTIFICADO" = col_integer(),
                   "COMPLEMENTO DO CERTIFICADO" = col_integer(),
                   "NOME SEGURADO/DEPENDENTE" = col_character(),
                   "INDIC. SUBF. ANTER/ATUAL" = col_character(),
                   "DATA DE NASCIMENTO" = col_character(),
                   "CODIGO DO SEXO" = col_integer(),
                   "ESTADO CIVIL" = col_integer(),
                   "COD. GRAU PARENT.DEP." = col_integer(),
                   "CODIGO DO PLANO" = col_character(),
                   "DATA INICIO VIGENCIA" = col_character(),
                   "TIPO DE LANÇAMENTO" = col_character(),
                   "DATA DE LANCAMENTO" = col_character(),
                   "VALOR DO LANCAMENTO" = col_integer(),
                   "PARTE DO SEGURADO" = col_integer(),
                   "CODIGO DO LANCAMENTO" = col_character(),
                   "CARGO / OCUPACAO" = col_character(),
                   "Matrícula" = col_character(),
                   "FILLER" = col_character())) %>% filter(
                     (`TIPO DO REGISTRO` == 3))

fat.tec1$`DATA DE LANCAMENTO` <- "02/2020"

fat.tec1$`VALOR DO LANCAMENTO` <- fat.tec1$`VALOR DO LANCAMENTO`/100

fat.tec1$`VALOR DO LANCAMENTO` <- ifelse(
  fat.tec1$`CODIGO DO LANCAMENTO` > 11,
  fat.tec1$`VALOR DO LANCAMENTO`*(-1),
  fat.tec1$`VALOR DO LANCAMENTO`)

fat.tec1 <- fat.tec1 %>% group_by(`NUMERO DA SUBFATURA`,`NUMERO DO CERTIFICADO`,
                                  `COMPLEMENTO DO CERTIFICADO`,`NOME SEGURADO/DEPENDENTE`,
                                  `DATA DE NASCIMENTO`,`CODIGO DO SEXO`,`ESTADO CIVIL`,
                                  `COD. GRAU PARENT.DEP.`,`CODIGO DO PLANO`,
                                  `DATA DE LANCAMENTO`,`CARGO / OCUPACAO`) %>% summarise(
                                    `VALOR DO LANCAMENTO` = sum(`VALOR DO LANCAMENTO`))

calculo <- fat.tec1 %>% filter(`NUMERO DO CERTIFICADO` == 0)

fat.tec1 <- fat.tec1 %>% filter(!`NUMERO DO CERTIFICADO` == 0)

fat.tec1$tit <- if_else(fat.tec1$`COMPLEMENTO DO CERTIFICADO` == 0, 1,0)

fat.tec1$conta <- fat.tec1$tit/sum(fat.tec1$tit)*sum(calculo$`VALOR DO LANCAMENTO`)

fat.tec1$`VALOR DO LANCAMENTO` <- fat.tec1$`VALOR DO LANCAMENTO` + fat.tec1$conta

fat.tec1$conta <- NULL
fat.tec1$tit <- NULL

fat.tec2 <- readr:: read_fwf(
  file ="D:/Users/sb046971/Documents/fat/FM07071015.TXT",
  fwf_widths(widths, col_names = names), skip_empty_rows = T,skip = 1,
  col_types = cols("TIPO DO REGISTRO" = col_integer(),
                   "NUMERO DA SUBFATURA" = col_integer(),
                   "NUMERO DO CERTIFICADO" = col_integer(),
                   "COMPLEMENTO DO CERTIFICADO" = col_integer(),
                   "NOME SEGURADO/DEPENDENTE" = col_character(),
                   "INDIC. SUBF. ANTER/ATUAL" = col_character(),
                   "DATA DE NASCIMENTO" = col_character(),
                   "CODIGO DO SEXO" = col_integer(),
                   "ESTADO CIVIL" = col_integer(),
                   "COD. GRAU PARENT.DEP." = col_integer(),
                   "CODIGO DO PLANO" = col_character(),
                   "DATA INICIO VIGENCIA" = col_character(),
                   "TIPO DE LANÇAMENTO" = col_character(),
                   "DATA DE LANCAMENTO" = col_character(),
                   "VALOR DO LANCAMENTO" = col_integer(),
                   "PARTE DO SEGURADO" = col_integer(),
                   "CODIGO DO LANCAMENTO" = col_character(),
                   "CARGO / OCUPACAO" = col_character(),
                   "Matrícula" = col_character(),
                   "FILLER" = col_character())) %>% filter(
                     (`TIPO DO REGISTRO` == 3))

fat.tec2$`DATA DE LANCAMENTO` <- "08/2019"

fat.tec2$`VALOR DO LANCAMENTO` <- fat.tec2$`VALOR DO LANCAMENTO`/100

fat.tec2$`VALOR DO LANCAMENTO` <- ifelse(
  fat.tec2$`CODIGO DO LANCAMENTO` > 22,
  fat.tec2$`VALOR DO LANCAMENTO`*(-1),
  fat.tec2$`VALOR DO LANCAMENTO`)

fat.tec2 <- fat.tec2 %>% group_by(`NUMERO DA SUBFATURA`,`NUMERO DO CERTIFICADO`,
                                  `COMPLEMENTO DO CERTIFICADO`,`NOME SEGURADO/DEPENDENTE`,
                                  `DATA DE NASCIMENTO`,`CODIGO DO SEXO`,`ESTADO CIVIL`,
                                  `COD. GRAU PARENT.DEP.`,`CODIGO DO PLANO`,
                                  `DATA DE LANCAMENTO`,`CARGO / OCUPACAO`) %>% summarise(
                                    `VALOR DO LANCAMENTO` = sum(`VALOR DO LANCAMENTO`))

calculo <- fat.tec2 %>% filter(`NUMERO DO CERTIFICADO` == 0)

fat.tec2 <- fat.tec2 %>% filter(!`NUMERO DO CERTIFICADO` == 0)

fat.tec2$tit <- if_else(fat.tec2$`COMPLEMENTO DO CERTIFICADO` == 0, 1,0)

fat.tec2$conta <- fat.tec2$tit/sum(fat.tec2$tit)*sum(calculo$`VALOR DO LANCAMENTO`)

fat.tec2$`VALOR DO LANCAMENTO` <- fat.tec2$`VALOR DO LANCAMENTO` + fat.tec2$conta

fat.tec2$conta <- NULL
fat.tec2$tit <- NULL

fat.tec3 <- readr:: read_fwf(
  file ="D:/Users/sb046971/Documents/fat/FM071015_D180411.TXT",
  fwf_widths(widths, col_names = names), skip_empty_rows = T,skip = 1,
  col_types = cols("TIPO DO REGISTRO" = col_integer(),
                   "NUMERO DA SUBFATURA" = col_integer(),
                   "NUMERO DO CERTIFICADO" = col_integer(),
                   "COMPLEMENTO DO CERTIFICADO" = col_integer(),
                   "NOME SEGURADO/DEPENDENTE" = col_character(),
                   "INDIC. SUBF. ANTER/ATUAL" = col_character(),
                   "DATA DE NASCIMENTO" = col_character(),
                   "CODIGO DO SEXO" = col_integer(),
                   "ESTADO CIVIL" = col_integer(),
                   "COD. GRAU PARENT.DEP." = col_integer(),
                   "CODIGO DO PLANO" = col_character(),
                   "DATA INICIO VIGENCIA" = col_character(),
                   "TIPO DE LANÇAMENTO" = col_character(),
                   "DATA DE LANCAMENTO" = col_character(),
                   "VALOR DO LANCAMENTO" = col_integer(),
                   "PARTE DO SEGURADO" = col_integer(),
                   "CODIGO DO LANCAMENTO" = col_character(),
                   "CARGO / OCUPACAO" = col_character(),
                   "Matrícula" = col_character(),
                   "FILLER" = col_character())) %>% filter(
                     (`TIPO DO REGISTRO` == 3))

fat.tec3$`DATA DE LANCAMENTO` <- "05/2018"

fat.tec3$`VALOR DO LANCAMENTO` <- fat.tec3$`VALOR DO LANCAMENTO`/100

fat.tec3$`VALOR DO LANCAMENTO` <- ifelse(
  fat.tec3$`CODIGO DO LANCAMENTO` > 33,
  fat.tec3$`VALOR DO LANCAMENTO`*(-1),
  fat.tec3$`VALOR DO LANCAMENTO`)

fat.tec3 <- fat.tec3 %>% group_by(`NUMERO DA SUBFATURA`,`NUMERO DO CERTIFICADO`,
                                  `COMPLEMENTO DO CERTIFICADO`,`NOME SEGURADO/DEPENDENTE`,
                                  `DATA DE NASCIMENTO`,`CODIGO DO SEXO`,`ESTADO CIVIL`,
                                  `COD. GRAU PARENT.DEP.`,`CODIGO DO PLANO`,
                                  `DATA DE LANCAMENTO`,`CARGO / OCUPACAO`) %>% summarise(
                                    `VALOR DO LANCAMENTO` = sum(`VALOR DO LANCAMENTO`))

calculo <- fat.tec3 %>% filter(`NUMERO DO CERTIFICADO` == 0)

fat.tec3 <- fat.tec3 %>% filter(!`NUMERO DO CERTIFICADO` == 0)

fat.tec3$tit <- if_else(fat.tec3$`COMPLEMENTO DO CERTIFICADO` == 0, 1,0)

fat.tec3$conta <- fat.tec3$tit/sum(fat.tec3$tit)*sum(calculo$`VALOR DO LANCAMENTO`)

fat.tec3$`VALOR DO LANCAMENTO` <- fat.tec3$`VALOR DO LANCAMENTO` + fat.tec3$conta

fat.tec3$conta <- NULL
fat.tec3$tit <- NULL

fat.tec4 <- readr:: read_fwf(
  file ="D:/Users/sb046971/Documents/fat/FM071015_D180515.TXT",
  fwf_widths(widths, col_names = names), skip_empty_rows = T,skip = 1,
  col_types = cols("TIPO DO REGISTRO" = col_integer(),
                   "NUMERO DA SUBFATURA" = col_integer(),
                   "NUMERO DO CERTIFICADO" = col_integer(),
                   "COMPLEMENTO DO CERTIFICADO" = col_integer(),
                   "NOME SEGURADO/DEPENDENTE" = col_character(),
                   "INDIC. SUBF. ANTER/ATUAL" = col_character(),
                   "DATA DE NASCIMENTO" = col_character(),
                   "CODIGO DO SEXO" = col_integer(),
                   "ESTADO CIVIL" = col_integer(),
                   "COD. GRAU PARENT.DEP." = col_integer(),
                   "CODIGO DO PLANO" = col_character(),
                   "DATA INICIO VIGENCIA" = col_character(),
                   "TIPO DE LANÇAMENTO" = col_character(),
                   "DATA DE LANCAMENTO" = col_character(),
                   "VALOR DO LANCAMENTO" = col_integer(),
                   "PARTE DO SEGURADO" = col_integer(),
                   "CODIGO DO LANCAMENTO" = col_character(),
                   "CARGO / OCUPACAO" = col_character(),
                   "Matrícula" = col_character(),
                   "FILLER" = col_character())) %>% filter(
                     (`TIPO DO REGISTRO` == 3))

fat.tec4$`DATA DE LANCAMENTO` <- "06/2019"

fat.tec4$`VALOR DO LANCAMENTO` <- fat.tec4$`VALOR DO LANCAMENTO`/100

fat.tec4$`VALOR DO LANCAMENTO` <- ifelse(
  fat.tec4$`CODIGO DO LANCAMENTO` > 44,
  fat.tec4$`VALOR DO LANCAMENTO`*(-1),
  fat.tec4$`VALOR DO LANCAMENTO`)

fat.tec4 <- fat.tec4 %>% group_by(`NUMERO DA SUBFATURA`,`NUMERO DO CERTIFICADO`,
                                  `COMPLEMENTO DO CERTIFICADO`,`NOME SEGURADO/DEPENDENTE`,
                                  `DATA DE NASCIMENTO`,`CODIGO DO SEXO`,`ESTADO CIVIL`,
                                  `COD. GRAU PARENT.DEP.`,`CODIGO DO PLANO`,
                                  `DATA DE LANCAMENTO`,`CARGO / OCUPACAO`) %>% summarise(
                                    `VALOR DO LANCAMENTO` = sum(`VALOR DO LANCAMENTO`))

calculo <- fat.tec4 %>% filter(`NUMERO DO CERTIFICADO` == 0)

fat.tec4 <- fat.tec4 %>% filter(!`NUMERO DO CERTIFICADO` == 0)

fat.tec4$tit <- if_else(fat.tec4$`COMPLEMENTO DO CERTIFICADO` == 0, 1,0)

fat.tec4$conta <- fat.tec4$tit/sum(fat.tec4$tit)*sum(calculo$`VALOR DO LANCAMENTO`)

fat.tec4$`VALOR DO LANCAMENTO` <- fat.tec4$`VALOR DO LANCAMENTO` + fat.tec4$conta

fat.tec4$conta <- NULL
fat.tec4$tit <- NULL

fat.tec5 <- readr:: read_fwf(
  file ="D:/Users/sb046971/Documents/fat/FM071015_D180629.TXT",
  fwf_widths(widths, col_names = names), skip_empty_rows = T,skip = 1,
  col_types = cols("TIPO DO REGISTRO" = col_integer(),
                   "NUMERO DA SUBFATURA" = col_integer(),
                   "NUMERO DO CERTIFICADO" = col_integer(),
                   "COMPLEMENTO DO CERTIFICADO" = col_integer(),
                   "NOME SEGURADO/DEPENDENTE" = col_character(),
                   "INDIC. SUBF. ANTER/ATUAL" = col_character(),
                   "DATA DE NASCIMENTO" = col_character(),
                   "CODIGO DO SEXO" = col_integer(),
                   "ESTADO CIVIL" = col_integer(),
                   "COD. GRAU PARENT.DEP." = col_integer(),
                   "CODIGO DO PLANO" = col_character(),
                   "DATA INICIO VIGENCIA" = col_character(),
                   "TIPO DE LANÇAMENTO" = col_character(),
                   "DATA DE LANCAMENTO" = col_character(),
                   "VALOR DO LANCAMENTO" = col_integer(),
                   "PARTE DO SEGURADO" = col_integer(),
                   "CODIGO DO LANCAMENTO" = col_character(),
                   "CARGO / OCUPACAO" = col_character(),
                   "Matrícula" = col_character(),
                   "FILLER" = col_character())) %>% filter(
                     (`TIPO DO REGISTRO` == 3))

fat.tec5$`DATA DE LANCAMENTO` <- "06/2018"

fat.tec5$`VALOR DO LANCAMENTO` <- fat.tec5$`VALOR DO LANCAMENTO`/100

fat.tec5$`VALOR DO LANCAMENTO` <- ifelse(
  fat.tec5$`CODIGO DO LANCAMENTO` > 45,
  fat.tec5$`VALOR DO LANCAMENTO`*(-1),
  fat.tec5$`VALOR DO LANCAMENTO`)

fat.tec5 <- fat.tec5 %>% group_by(`NUMERO DA SUBFATURA`,`NUMERO DO CERTIFICADO`,
                                  `COMPLEMENTO DO CERTIFICADO`,`NOME SEGURADO/DEPENDENTE`,
                                  `DATA DE NASCIMENTO`,`CODIGO DO SEXO`,`ESTADO CIVIL`,
                                  `COD. GRAU PARENT.DEP.`,`CODIGO DO PLANO`,
                                  `DATA DE LANCAMENTO`,`CARGO / OCUPACAO`) %>% summarise(
                                    `VALOR DO LANCAMENTO` = sum(`VALOR DO LANCAMENTO`))

calculo <- fat.tec5 %>% filter(`NUMERO DO CERTIFICADO` == 0)

fat.tec5 <- fat.tec5 %>% filter(!`NUMERO DO CERTIFICADO` == 0)

fat.tec5$tit <- if_else(fat.tec5$`COMPLEMENTO DO CERTIFICADO` == 0, 1,0)

fat.tec5$conta <- fat.tec5$tit/sum(fat.tec5$tit)*sum(calculo$`VALOR DO LANCAMENTO`)

fat.tec5$`VALOR DO LANCAMENTO` <- fat.tec5$`VALOR DO LANCAMENTO` + fat.tec5$conta

fat.tec5$conta <- NULL
fat.tec5$tit <- NULL

fat.tec6 <- readr:: read_fwf(
  file ="D:/Users/sb046971/Documents/fat/FM071015_D180713.TXT",
  fwf_widths(widths, col_names = names), skip_empty_rows = T,skip = 1,
  col_types = cols("TIPO DO REGISTRO" = col_integer(),
                   "NUMERO DA SUBFATURA" = col_integer(),
                   "NUMERO DO CERTIFICADO" = col_integer(),
                   "COMPLEMENTO DO CERTIFICADO" = col_integer(),
                   "NOME SEGURADO/DEPENDENTE" = col_character(),
                   "INDIC. SUBF. ANTER/ATUAL" = col_character(),
                   "DATA DE NASCIMENTO" = col_character(),
                   "CODIGO DO SEXO" = col_integer(),
                   "ESTADO CIVIL" = col_integer(),
                   "COD. GRAU PARENT.DEP." = col_integer(),
                   "CODIGO DO PLANO" = col_character(),
                   "DATA INICIO VIGENCIA" = col_character(),
                   "TIPO DE LANÇAMENTO" = col_character(),
                   "DATA DE LANCAMENTO" = col_character(),
                   "VALOR DO LANCAMENTO" = col_integer(),
                   "PARTE DO SEGURADO" = col_integer(),
                   "CODIGO DO LANCAMENTO" = col_character(),
                   "CARGO / OCUPACAO" = col_character(),
                   "Matrícula" = col_character(),
                   "FILLER" = col_character())) %>% filter(
                     (`TIPO DO REGISTRO` == 3))

fat.tec6$`DATA DE LANCAMENTO` <- "08/2018"

fat.tec6$`VALOR DO LANCAMENTO` <- fat.tec6$`VALOR DO LANCAMENTO`/100

fat.tec6$`VALOR DO LANCAMENTO` <- ifelse(
  fat.tec6$`CODIGO DO LANCAMENTO` > 46,
  fat.tec6$`VALOR DO LANCAMENTO`*(-1),
  fat.tec6$`VALOR DO LANCAMENTO`)

fat.tec6 <- fat.tec6 %>% group_by(`NUMERO DA SUBFATURA`,`NUMERO DO CERTIFICADO`,
                                  `COMPLEMENTO DO CERTIFICADO`,`NOME SEGURADO/DEPENDENTE`,
                                  `DATA DE NASCIMENTO`,`CODIGO DO SEXO`,`ESTADO CIVIL`,
                                  `COD. GRAU PARENT.DEP.`,`CODIGO DO PLANO`,
                                  `DATA DE LANCAMENTO`,`CARGO / OCUPACAO`) %>% summarise(
                                    `VALOR DO LANCAMENTO` = sum(`VALOR DO LANCAMENTO`))

calculo <- fat.tec6 %>% filter(`NUMERO DO CERTIFICADO` == 0)

fat.tec6 <- fat.tec6 %>% filter(!`NUMERO DO CERTIFICADO` == 0)

fat.tec6$tit <- if_else(fat.tec6$`COMPLEMENTO DO CERTIFICADO` == 0, 1,0)

fat.tec6$conta <- fat.tec6$tit/sum(fat.tec6$tit)*sum(calculo$`VALOR DO LANCAMENTO`)

fat.tec6$`VALOR DO LANCAMENTO` <- fat.tec6$`VALOR DO LANCAMENTO` + fat.tec6$conta

fat.tec6$conta <- NULL
fat.tec6$tit <- NULL

fat.tec7 <- readr:: read_fwf(
  file ="D:/Users/sb046971/Documents/fat/FM071015_D180821.TXT",
  fwf_widths(widths, col_names = names), skip_empty_rows = T,skip = 1,
  col_types = cols("TIPO DO REGISTRO" = col_integer(),
                   "NUMERO DA SUBFATURA" = col_integer(),
                   "NUMERO DO CERTIFICADO" = col_integer(),
                   "COMPLEMENTO DO CERTIFICADO" = col_integer(),
                   "NOME SEGURADO/DEPENDENTE" = col_character(),
                   "INDIC. SUBF. ANTER/ATUAL" = col_character(),
                   "DATA DE NASCIMENTO" = col_character(),
                   "CODIGO DO SEXO" = col_integer(),
                   "ESTADO CIVIL" = col_integer(),
                   "COD. GRAU PARENT.DEP." = col_integer(),
                   "CODIGO DO PLANO" = col_character(),
                   "DATA INICIO VIGENCIA" = col_character(),
                   "TIPO DE LANÇAMENTO" = col_character(),
                   "DATA DE LANCAMENTO" = col_character(),
                   "VALOR DO LANCAMENTO" = col_integer(),
                   "PARTE DO SEGURADO" = col_integer(),
                   "CODIGO DO LANCAMENTO" = col_character(),
                   "CARGO / OCUPACAO" = col_character(),
                   "Matrícula" = col_character(),
                   "FILLER" = col_character())) %>% filter(
                     (`TIPO DO REGISTRO` == 3))

fat.tec7$`DATA DE LANCAMENTO` <- "09/2018"

fat.tec7$`VALOR DO LANCAMENTO` <- fat.tec7$`VALOR DO LANCAMENTO`/100

fat.tec7$`VALOR DO LANCAMENTO` <- ifelse(
  fat.tec7$`CODIGO DO LANCAMENTO` > 47,
  fat.tec7$`VALOR DO LANCAMENTO`*(-1),
  fat.tec7$`VALOR DO LANCAMENTO`)

fat.tec7 <- fat.tec7 %>% group_by(`NUMERO DA SUBFATURA`,`NUMERO DO CERTIFICADO`,
                                  `COMPLEMENTO DO CERTIFICADO`,`NOME SEGURADO/DEPENDENTE`,
                                  `DATA DE NASCIMENTO`,`CODIGO DO SEXO`,`ESTADO CIVIL`,
                                  `COD. GRAU PARENT.DEP.`,`CODIGO DO PLANO`,
                                  `DATA DE LANCAMENTO`,`CARGO / OCUPACAO`) %>% summarise(
                                    `VALOR DO LANCAMENTO` = sum(`VALOR DO LANCAMENTO`))

calculo <- fat.tec7 %>% filter(`NUMERO DO CERTIFICADO` == 0)

fat.tec7 <- fat.tec7 %>% filter(!`NUMERO DO CERTIFICADO` == 0)

fat.tec7$tit <- if_else(fat.tec7$`COMPLEMENTO DO CERTIFICADO` == 0, 1,0)

fat.tec7$conta <- fat.tec7$tit/sum(fat.tec7$tit)*sum(calculo$`VALOR DO LANCAMENTO`)

fat.tec7$`VALOR DO LANCAMENTO` <- fat.tec7$`VALOR DO LANCAMENTO` + fat.tec7$conta

fat.tec7$conta <- NULL
fat.tec7$tit <- NULL

fat.tec8 <- readr:: read_fwf(
  file ="D:/Users/sb046971/Documents/fat/FM05071015.txt",
  fwf_widths(widths, col_names = names), skip_empty_rows = T,skip = 1,
  col_types = cols("TIPO DO REGISTRO" = col_integer(),
                   "NUMERO DA SUBFATURA" = col_integer(),
                   "NUMERO DO CERTIFICADO" = col_integer(),
                   "COMPLEMENTO DO CERTIFICADO" = col_integer(),
                   "NOME SEGURADO/DEPENDENTE" = col_character(),
                   "INDIC. SUBF. ANTER/ATUAL" = col_character(),
                   "DATA DE NASCIMENTO" = col_character(),
                   "CODIGO DO SEXO" = col_integer(),
                   "ESTADO CIVIL" = col_integer(),
                   "COD. GRAU PARENT.DEP." = col_integer(),
                   "CODIGO DO PLANO" = col_character(),
                   "DATA INICIO VIGENCIA" = col_character(),
                   "TIPO DE LANÇAMENTO" = col_character(),
                   "DATA DE LANCAMENTO" = col_character(),
                   "VALOR DO LANCAMENTO" = col_integer(),
                   "PARTE DO SEGURADO" = col_integer(),
                   "CODIGO DO LANCAMENTO" = col_character(),
                   "CARGO / OCUPACAO" = col_character(),
                   "Matrícula" = col_character(),
                   "FILLER" = col_character())) %>% filter(
                     (`TIPO DO REGISTRO` == 3))

fat.tec8$`DATA DE LANCAMENTO` <- "06/2018"

fat.tec8$`VALOR DO LANCAMENTO` <- fat.tec8$`VALOR DO LANCAMENTO`/100

fat.tec8$`VALOR DO LANCAMENTO` <- ifelse(
  fat.tec8$`CODIGO DO LANCAMENTO` > 48,
  fat.tec8$`VALOR DO LANCAMENTO`*(-1),
  fat.tec8$`VALOR DO LANCAMENTO`)

fat.tec8 <- fat.tec8 %>% group_by(`NUMERO DA SUBFATURA`,`NUMERO DO CERTIFICADO`,
                                  `COMPLEMENTO DO CERTIFICADO`,`NOME SEGURADO/DEPENDENTE`,
                                  `DATA DE NASCIMENTO`,`CODIGO DO SEXO`,`ESTADO CIVIL`,
                                  `COD. GRAU PARENT.DEP.`,`CODIGO DO PLANO`,
                                  `DATA DE LANCAMENTO`,`CARGO / OCUPACAO`) %>% summarise(
                                    `VALOR DO LANCAMENTO` = sum(`VALOR DO LANCAMENTO`))

calculo <- fat.tec8 %>% filter(`NUMERO DO CERTIFICADO` == 0)

fat.tec8 <- fat.tec8 %>% filter(!`NUMERO DO CERTIFICADO` == 0)

fat.tec8$tit <- if_else(fat.tec8$`COMPLEMENTO DO CERTIFICADO` == 0, 1,0)

fat.tec8$conta <- fat.tec8$tit/sum(fat.tec8$tit)*sum(calculo$`VALOR DO LANCAMENTO`)

fat.tec8$`VALOR DO LANCAMENTO` <- fat.tec8$`VALOR DO LANCAMENTO` + fat.tec8$conta

fat.tec8$conta <- NULL
fat.tec8$tit <- NULL

fat.tec9 <- readr:: read_fwf(
  file ="D:/Users/sb046971/Documents/fat/FM071015_D181010.TXT",
  fwf_widths(widths, col_names = names), skip_empty_rows = T,skip = 1,
  col_types = cols("TIPO DO REGISTRO" = col_integer(),
                   "NUMERO DA SUBFATURA" = col_integer(),
                   "NUMERO DO CERTIFICADO" = col_integer(),
                   "COMPLEMENTO DO CERTIFICADO" = col_integer(),
                   "NOME SEGURADO/DEPENDENTE" = col_character(),
                   "INDIC. SUBF. ANTER/ATUAL" = col_character(),
                   "DATA DE NASCIMENTO" = col_character(),
                   "CODIGO DO SEXO" = col_integer(),
                   "ESTADO CIVIL" = col_integer(),
                   "COD. GRAU PARENT.DEP." = col_integer(),
                   "CODIGO DO PLANO" = col_character(),
                   "DATA INICIO VIGENCIA" = col_character(),
                   "TIPO DE LANÇAMENTO" = col_character(),
                   "DATA DE LANCAMENTO" = col_character(),
                   "VALOR DO LANCAMENTO" = col_integer(),
                   "PARTE DO SEGURADO" = col_integer(),
                   "CODIGO DO LANCAMENTO" = col_character(),
                   "CARGO / OCUPACAO" = col_character(),
                   "Matrícula" = col_character(),
                   "FILLER" = col_character())) %>% filter(
                     (`TIPO DO REGISTRO` == 3))

fat.tec9$`DATA DE LANCAMENTO` <- "11/2018"

fat.tec9$`VALOR DO LANCAMENTO` <- fat.tec9$`VALOR DO LANCAMENTO`/100

fat.tec9$`VALOR DO LANCAMENTO` <- ifelse(
  fat.tec9$`CODIGO DO LANCAMENTO` > 49,
  fat.tec9$`VALOR DO LANCAMENTO`*(-1),
  fat.tec9$`VALOR DO LANCAMENTO`)

fat.tec9 <- fat.tec9 %>% group_by(`NUMERO DA SUBFATURA`,`NUMERO DO CERTIFICADO`,
                                  `COMPLEMENTO DO CERTIFICADO`,`NOME SEGURADO/DEPENDENTE`,
                                  `DATA DE NASCIMENTO`,`CODIGO DO SEXO`,`ESTADO CIVIL`,
                                  `COD. GRAU PARENT.DEP.`,`CODIGO DO PLANO`,
                                  `DATA DE LANCAMENTO`,`CARGO / OCUPACAO`) %>% summarise(
                                    `VALOR DO LANCAMENTO` = sum(`VALOR DO LANCAMENTO`))

calculo <- fat.tec9 %>% filter(`NUMERO DO CERTIFICADO` == 0)

fat.tec9 <- fat.tec9 %>% filter(!`NUMERO DO CERTIFICADO` == 0)

fat.tec9$tit <- if_else(fat.tec9$`COMPLEMENTO DO CERTIFICADO` == 0, 1,0)

fat.tec9$conta <- fat.tec9$tit/sum(fat.tec9$tit)*sum(calculo$`VALOR DO LANCAMENTO`)

fat.tec9$`VALOR DO LANCAMENTO` <- fat.tec9$`VALOR DO LANCAMENTO` + fat.tec9$conta

fat.tec9$conta <- NULL
fat.tec9$tit <- NULL

fat.tec10 <- readr:: read_fwf(
  file ="D:/Users/sb046971/Documents/fat/FM071015_D181207.TXT",
  fwf_widths(widths, col_names = names), skip_empty_rows = T,skip = 1,
  col_types = cols("TIPO DO REGISTRO" = col_integer(),
                   "NUMERO DA SUBFATURA" = col_integer(),
                   "NUMERO DO CERTIFICADO" = col_integer(),
                   "COMPLEMENTO DO CERTIFICADO" = col_integer(),
                   "NOME SEGURADO/DEPENDENTE" = col_character(),
                   "INDIC. SUBF. ANTER/ATUAL" = col_character(),
                   "DATA DE NASCIMENTO" = col_character(),
                   "CODIGO DO SEXO" = col_integer(),
                   "ESTADO CIVIL" = col_integer(),
                   "COD. GRAU PARENT.DEP." = col_integer(),
                   "CODIGO DO PLANO" = col_character(),
                   "DATA INICIO VIGENCIA" = col_character(),
                   "TIPO DE LANÇAMENTO" = col_character(),
                   "DATA DE LANCAMENTO" = col_character(),
                   "VALOR DO LANCAMENTO" = col_integer(),
                   "PARTE DO SEGURADO" = col_integer(),
                   "CODIGO DO LANCAMENTO" = col_character(),
                   "CARGO / OCUPACAO" = col_character(),
                   "Matrícula" = col_character(),
                   "FILLER" = col_character())) %>% filter(
                     (`TIPO DO REGISTRO` == 3))

fat.tec10$`DATA DE LANCAMENTO` <- "01/2019"

fat.tec10$`VALOR DO LANCAMENTO` <- fat.tec10$`VALOR DO LANCAMENTO`/100

fat.tec10$`VALOR DO LANCAMENTO` <- ifelse(
  fat.tec10$`CODIGO DO LANCAMENTO` > 49,
  fat.tec10$`VALOR DO LANCAMENTO`*(-1),
  fat.tec10$`VALOR DO LANCAMENTO`)

fat.tec10 <- fat.tec10 %>% group_by(`NUMERO DA SUBFATURA`,`NUMERO DO CERTIFICADO`,
                                    `COMPLEMENTO DO CERTIFICADO`,`NOME SEGURADO/DEPENDENTE`,
                                    `DATA DE NASCIMENTO`,`CODIGO DO SEXO`,`ESTADO CIVIL`,
                                    `COD. GRAU PARENT.DEP.`,`CODIGO DO PLANO`,
                                    `DATA DE LANCAMENTO`,`CARGO / OCUPACAO`) %>% summarise(
                                      `VALOR DO LANCAMENTO` = sum(`VALOR DO LANCAMENTO`))

calculo <- fat.tec10 %>% filter(`NUMERO DO CERTIFICADO` == 0)

fat.tec10 <- fat.tec10 %>% filter(!`NUMERO DO CERTIFICADO` == 0)

fat.tec10$tit <- if_else(fat.tec10$`COMPLEMENTO DO CERTIFICADO` == 0, 1,0)

fat.tec10$conta <- fat.tec10$tit/sum(fat.tec10$tit)*sum(calculo$`VALOR DO LANCAMENTO`)

fat.tec10$`VALOR DO LANCAMENTO` <- fat.tec10$`VALOR DO LANCAMENTO` + fat.tec10$conta

fat.tec10$conta <- NULL
fat.tec10$tit <- NULL

fat.tec11 <- readr:: read_fwf(
  file ="D:/Users/sb046971/Documents/fat/FM071015_D190214.TXT",
  fwf_widths(widths, col_names = names), skip_empty_rows = T,skip = 1,
  col_types = cols("TIPO DO REGISTRO" = col_integer(),
                   "NUMERO DA SUBFATURA" = col_integer(),
                   "NUMERO DO CERTIFICADO" = col_integer(),
                   "COMPLEMENTO DO CERTIFICADO" = col_integer(),
                   "NOME SEGURADO/DEPENDENTE" = col_character(),
                   "INDIC. SUBF. ANTER/ATUAL" = col_character(),
                   "DATA DE NASCIMENTO" = col_character(),
                   "CODIGO DO SEXO" = col_integer(),
                   "ESTADO CIVIL" = col_integer(),
                   "COD. GRAU PARENT.DEP." = col_integer(),
                   "CODIGO DO PLANO" = col_character(),
                   "DATA INICIO VIGENCIA" = col_character(),
                   "TIPO DE LANÇAMENTO" = col_character(),
                   "DATA DE LANCAMENTO" = col_character(),
                   "VALOR DO LANCAMENTO" = col_integer(),
                   "PARTE DO SEGURADO" = col_integer(),
                   "CODIGO DO LANCAMENTO" = col_character(),
                   "CARGO / OCUPACAO" = col_character(),
                   "Matrícula" = col_character(),
                   "FILLER" = col_character())) %>% filter(
                     (`TIPO DO REGISTRO` == 3))

fat.tec11$`DATA DE LANCAMENTO` <- "03/2019"

fat.tec11$`VALOR DO LANCAMENTO` <- fat.tec11$`VALOR DO LANCAMENTO`/100

fat.tec11$`VALOR DO LANCAMENTO` <- ifelse(
  fat.tec11$`CODIGO DO LANCAMENTO` > 49,
  fat.tec11$`VALOR DO LANCAMENTO`*(-1),
  fat.tec11$`VALOR DO LANCAMENTO`)

fat.tec11 <- fat.tec11 %>% group_by(`NUMERO DA SUBFATURA`,`NUMERO DO CERTIFICADO`,
                                    `COMPLEMENTO DO CERTIFICADO`,`NOME SEGURADO/DEPENDENTE`,
                                    `DATA DE NASCIMENTO`,`CODIGO DO SEXO`,`ESTADO CIVIL`,
                                    `COD. GRAU PARENT.DEP.`,`CODIGO DO PLANO`,
                                    `DATA DE LANCAMENTO`,`CARGO / OCUPACAO`) %>% summarise(
                                      `VALOR DO LANCAMENTO` = sum(`VALOR DO LANCAMENTO`))

calculo <- fat.tec11 %>% filter(`NUMERO DO CERTIFICADO` == 0)

fat.tec11 <- fat.tec11 %>% filter(!`NUMERO DO CERTIFICADO` == 0)

fat.tec11$tit <- if_else(fat.tec11$`COMPLEMENTO DO CERTIFICADO` == 0, 1,0)

fat.tec11$conta <- fat.tec11$tit/sum(fat.tec11$tit)*sum(calculo$`VALOR DO LANCAMENTO`)

fat.tec11$`VALOR DO LANCAMENTO` <- fat.tec11$`VALOR DO LANCAMENTO` + fat.tec11$conta

fat.tec11$conta <- NULL
fat.tec11$tit <- NULL

fat.tec12 <- readr:: read_fwf(
  file ="D:/Users/sb046971/Documents/fat/FM071015_D190312.TXT",
  fwf_widths(widths, col_names = names), skip_empty_rows = T,skip = 1,
  col_types = cols("TIPO DO REGISTRO" = col_integer(),
                   "NUMERO DA SUBFATURA" = col_integer(),
                   "NUMERO DO CERTIFICADO" = col_integer(),
                   "COMPLEMENTO DO CERTIFICADO" = col_integer(),
                   "NOME SEGURADO/DEPENDENTE" = col_character(),
                   "INDIC. SUBF. ANTER/ATUAL" = col_character(),
                   "DATA DE NASCIMENTO" = col_character(),
                   "CODIGO DO SEXO" = col_integer(),
                   "ESTADO CIVIL" = col_integer(),
                   "COD. GRAU PARENT.DEP." = col_integer(),
                   "CODIGO DO PLANO" = col_character(),
                   "DATA INICIO VIGENCIA" = col_character(),
                   "TIPO DE LANÇAMENTO" = col_character(),
                   "DATA DE LANCAMENTO" = col_character(),
                   "VALOR DO LANCAMENTO" = col_integer(),
                   "PARTE DO SEGURADO" = col_integer(),
                   "CODIGO DO LANCAMENTO" = col_character(),
                   "CARGO / OCUPACAO" = col_character(),
                   "Matrícula" = col_character(),
                   "FILLER" = col_character())) %>% filter(
                     (`TIPO DO REGISTRO` == 3))

fat.tec12$`DATA DE LANCAMENTO` <- "04/2019"

fat.tec12$`VALOR DO LANCAMENTO` <- fat.tec12$`VALOR DO LANCAMENTO`/100

fat.tec12$`VALOR DO LANCAMENTO` <- ifelse(
  fat.tec12$`CODIGO DO LANCAMENTO` > 49,
  fat.tec12$`VALOR DO LANCAMENTO`*(-1),
  fat.tec12$`VALOR DO LANCAMENTO`)

fat.tec12 <- fat.tec12 %>% group_by(`NUMERO DA SUBFATURA`,`NUMERO DO CERTIFICADO`,
                                    `COMPLEMENTO DO CERTIFICADO`,`NOME SEGURADO/DEPENDENTE`,
                                    `DATA DE NASCIMENTO`,`CODIGO DO SEXO`,`ESTADO CIVIL`,
                                    `COD. GRAU PARENT.DEP.`,`CODIGO DO PLANO`,
                                    `DATA DE LANCAMENTO`,`CARGO / OCUPACAO`) %>% summarise(
                                      `VALOR DO LANCAMENTO` = sum(`VALOR DO LANCAMENTO`))

calculo <- fat.tec12 %>% filter(`NUMERO DO CERTIFICADO` == 0)

fat.tec12 <- fat.tec12 %>% filter(!`NUMERO DO CERTIFICADO` == 0)

fat.tec12$tit <- if_else(fat.tec12$`COMPLEMENTO DO CERTIFICADO` == 0, 1,0)

fat.tec12$conta <- fat.tec12$tit/sum(fat.tec12$tit)*sum(calculo$`VALOR DO LANCAMENTO`)

fat.tec12$`VALOR DO LANCAMENTO` <- fat.tec12$`VALOR DO LANCAMENTO` + fat.tec12$conta

fat.tec12$conta <- NULL
fat.tec12$tit <- NULL

fat.tec13 <- readr:: read_fwf(
  file ="D:/Users/sb046971/Documents/fat/FM071015_D190410.TXT",
  fwf_widths(widths, col_names = names), skip_empty_rows = T,skip = 1,
  col_types = cols("TIPO DO REGISTRO" = col_integer(),
                   "NUMERO DA SUBFATURA" = col_integer(),
                   "NUMERO DO CERTIFICADO" = col_integer(),
                   "COMPLEMENTO DO CERTIFICADO" = col_integer(),
                   "NOME SEGURADO/DEPENDENTE" = col_character(),
                   "INDIC. SUBF. ANTER/ATUAL" = col_character(),
                   "DATA DE NASCIMENTO" = col_character(),
                   "CODIGO DO SEXO" = col_integer(),
                   "ESTADO CIVIL" = col_integer(),
                   "COD. GRAU PARENT.DEP." = col_integer(),
                   "CODIGO DO PLANO" = col_character(),
                   "DATA INICIO VIGENCIA" = col_character(),
                   "TIPO DE LANÇAMENTO" = col_character(),
                   "DATA DE LANCAMENTO" = col_character(),
                   "VALOR DO LANCAMENTO" = col_integer(),
                   "PARTE DO SEGURADO" = col_integer(),
                   "CODIGO DO LANCAMENTO" = col_character(),
                   "CARGO / OCUPACAO" = col_character(),
                   "Matrícula" = col_character(),
                   "FILLER" = col_character())) %>% filter(
                     (`TIPO DO REGISTRO` == 3))

fat.tec13$`DATA DE LANCAMENTO` <- "05/2019"

fat.tec13$`VALOR DO LANCAMENTO` <- fat.tec13$`VALOR DO LANCAMENTO`/100

fat.tec13$`VALOR DO LANCAMENTO` <- ifelse(
  fat.tec13$`CODIGO DO LANCAMENTO` > 49,
  fat.tec13$`VALOR DO LANCAMENTO`*(-1),
  fat.tec13$`VALOR DO LANCAMENTO`)

fat.tec13 <- fat.tec13 %>% group_by(`NUMERO DA SUBFATURA`,`NUMERO DO CERTIFICADO`,
                                    `COMPLEMENTO DO CERTIFICADO`,`NOME SEGURADO/DEPENDENTE`,
                                    `DATA DE NASCIMENTO`,`CODIGO DO SEXO`,`ESTADO CIVIL`,
                                    `COD. GRAU PARENT.DEP.`,`CODIGO DO PLANO`,
                                    `DATA DE LANCAMENTO`,`CARGO / OCUPACAO`) %>% summarise(
                                      `VALOR DO LANCAMENTO` = sum(`VALOR DO LANCAMENTO`))

calculo <- fat.tec13 %>% filter(`NUMERO DO CERTIFICADO` == 0)

fat.tec13 <- fat.tec13 %>% filter(!`NUMERO DO CERTIFICADO` == 0)

fat.tec13$tit <- if_else(fat.tec13$`COMPLEMENTO DO CERTIFICADO` == 0, 1,0)

fat.tec13$conta <- fat.tec13$tit/sum(fat.tec13$tit)*sum(calculo$`VALOR DO LANCAMENTO`)

fat.tec13$`VALOR DO LANCAMENTO` <- fat.tec13$`VALOR DO LANCAMENTO` + fat.tec13$conta

fat.tec13$conta <- NULL
fat.tec13$tit <- NULL

fat.tec14 <- readr:: read_fwf(
  file ="D:/Users/sb046971/Documents/fat/FM071015_D190514.TXT",
  fwf_widths(widths, col_names = names), skip_empty_rows = T,skip = 1,
  col_types = cols("TIPO DO REGISTRO" = col_integer(),
                   "NUMERO DA SUBFATURA" = col_integer(),
                   "NUMERO DO CERTIFICADO" = col_integer(),
                   "COMPLEMENTO DO CERTIFICADO" = col_integer(),
                   "NOME SEGURADO/DEPENDENTE" = col_character(),
                   "INDIC. SUBF. ANTER/ATUAL" = col_character(),
                   "DATA DE NASCIMENTO" = col_character(),
                   "CODIGO DO SEXO" = col_integer(),
                   "ESTADO CIVIL" = col_integer(),
                   "COD. GRAU PARENT.DEP." = col_integer(),
                   "CODIGO DO PLANO" = col_character(),
                   "DATA INICIO VIGENCIA" = col_character(),
                   "TIPO DE LANÇAMENTO" = col_character(),
                   "DATA DE LANCAMENTO" = col_character(),
                   "VALOR DO LANCAMENTO" = col_integer(),
                   "PARTE DO SEGURADO" = col_integer(),
                   "CODIGO DO LANCAMENTO" = col_character(),
                   "CARGO / OCUPACAO" = col_character(),
                   "Matrícula" = col_character(),
                   "FILLER" = col_character())) %>% filter(
                     (`TIPO DO REGISTRO` == 3))

fat.tec14$`DATA DE LANCAMENTO` <- "06/2019"

fat.tec14$`VALOR DO LANCAMENTO` <- fat.tec14$`VALOR DO LANCAMENTO`/100

fat.tec14$`VALOR DO LANCAMENTO` <- ifelse(
  fat.tec14$`CODIGO DO LANCAMENTO` > 49,
  fat.tec14$`VALOR DO LANCAMENTO`*(-1),
  fat.tec14$`VALOR DO LANCAMENTO`)

fat.tec14 <- fat.tec14 %>% group_by(`NUMERO DA SUBFATURA`,`NUMERO DO CERTIFICADO`,
                                    `COMPLEMENTO DO CERTIFICADO`,`NOME SEGURADO/DEPENDENTE`,
                                    `DATA DE NASCIMENTO`,`CODIGO DO SEXO`,`ESTADO CIVIL`,
                                    `COD. GRAU PARENT.DEP.`,`CODIGO DO PLANO`,
                                    `DATA DE LANCAMENTO`,`CARGO / OCUPACAO`) %>% summarise(
                                      `VALOR DO LANCAMENTO` = sum(`VALOR DO LANCAMENTO`))

calculo <- fat.tec14 %>% filter(`NUMERO DO CERTIFICADO` == 0)

fat.tec14 <- fat.tec14 %>% filter(!`NUMERO DO CERTIFICADO` == 0)

fat.tec14$tit <- if_else(fat.tec14$`COMPLEMENTO DO CERTIFICADO` == 0, 1,0)

fat.tec14$conta <- fat.tec14$tit/sum(fat.tec14$tit)*sum(calculo$`VALOR DO LANCAMENTO`)

fat.tec14$`VALOR DO LANCAMENTO` <- fat.tec14$`VALOR DO LANCAMENTO` + fat.tec14$conta

fat.tec14$conta <- NULL
fat.tec14$tit <- NULL

fat.tec15 <- readr:: read_fwf(
  file ="D:/Users/sb046971/Documents/fat/FM071015_D190611.TXT",
  fwf_widths(widths, col_names = names), skip_empty_rows = T,skip = 1,
  col_types = cols("TIPO DO REGISTRO" = col_integer(),
                   "NUMERO DA SUBFATURA" = col_integer(),
                   "NUMERO DO CERTIFICADO" = col_integer(),
                   "COMPLEMENTO DO CERTIFICADO" = col_integer(),
                   "NOME SEGURADO/DEPENDENTE" = col_character(),
                   "INDIC. SUBF. ANTER/ATUAL" = col_character(),
                   "DATA DE NASCIMENTO" = col_character(),
                   "CODIGO DO SEXO" = col_integer(),
                   "ESTADO CIVIL" = col_integer(),
                   "COD. GRAU PARENT.DEP." = col_integer(),
                   "CODIGO DO PLANO" = col_character(),
                   "DATA INICIO VIGENCIA" = col_character(),
                   "TIPO DE LANÇAMENTO" = col_character(),
                   "DATA DE LANCAMENTO" = col_character(),
                   "VALOR DO LANCAMENTO" = col_integer(),
                   "PARTE DO SEGURADO" = col_integer(),
                   "CODIGO DO LANCAMENTO" = col_character(),
                   "CARGO / OCUPACAO" = col_character(),
                   "Matrícula" = col_character(),
                   "FILLER" = col_character())) %>% filter(
                     (`TIPO DO REGISTRO` == 3))

fat.tec15$`DATA DE LANCAMENTO` <- "07/2019"

fat.tec15$`VALOR DO LANCAMENTO` <- fat.tec15$`VALOR DO LANCAMENTO`/100

fat.tec15$`VALOR DO LANCAMENTO` <- ifelse(
  fat.tec15$`CODIGO DO LANCAMENTO` > 49,
  fat.tec15$`VALOR DO LANCAMENTO`*(-1),
  fat.tec15$`VALOR DO LANCAMENTO`)

fat.tec15 <- fat.tec15 %>% group_by(`NUMERO DA SUBFATURA`,`NUMERO DO CERTIFICADO`,
                                    `COMPLEMENTO DO CERTIFICADO`,`NOME SEGURADO/DEPENDENTE`,
                                    `DATA DE NASCIMENTO`,`CODIGO DO SEXO`,`ESTADO CIVIL`,
                                    `COD. GRAU PARENT.DEP.`,`CODIGO DO PLANO`,
                                    `DATA DE LANCAMENTO`,`CARGO / OCUPACAO`) %>% summarise(
                                      `VALOR DO LANCAMENTO` = sum(`VALOR DO LANCAMENTO`))

calculo <- fat.tec15 %>% filter(`NUMERO DO CERTIFICADO` == 0)

fat.tec15 <- fat.tec15 %>% filter(!`NUMERO DO CERTIFICADO` == 0)

fat.tec15$tit <- if_else(fat.tec15$`COMPLEMENTO DO CERTIFICADO` == 0, 1,0)

fat.tec15$conta <- fat.tec15$tit/sum(fat.tec15$tit)*sum(calculo$`VALOR DO LANCAMENTO`)

fat.tec15$`VALOR DO LANCAMENTO` <- fat.tec15$`VALOR DO LANCAMENTO` + fat.tec15$conta

fat.tec15$conta <- NULL
fat.tec15$tit <- NULL

fat.tec16 <- readr:: read_fwf(
  file ="D:/Users/sb046971/Documents/fat/FM071015_D190827.TXT",
  fwf_widths(widths, col_names = names), skip_empty_rows = T,skip = 1,
  col_types = cols("TIPO DO REGISTRO" = col_integer(),
                   "NUMERO DA SUBFATURA" = col_integer(),
                   "NUMERO DO CERTIFICADO" = col_integer(),
                   "COMPLEMENTO DO CERTIFICADO" = col_integer(),
                   "NOME SEGURADO/DEPENDENTE" = col_character(),
                   "INDIC. SUBF. ANTER/ATUAL" = col_character(),
                   "DATA DE NASCIMENTO" = col_character(),
                   "CODIGO DO SEXO" = col_integer(),
                   "ESTADO CIVIL" = col_integer(),
                   "COD. GRAU PARENT.DEP." = col_integer(),
                   "CODIGO DO PLANO" = col_character(),
                   "DATA INICIO VIGENCIA" = col_character(),
                   "TIPO DE LANÇAMENTO" = col_character(),
                   "DATA DE LANCAMENTO" = col_character(),
                   "VALOR DO LANCAMENTO" = col_integer(),
                   "PARTE DO SEGURADO" = col_integer(),
                   "CODIGO DO LANCAMENTO" = col_character(),
                   "CARGO / OCUPACAO" = col_character(),
                   "Matrícula" = col_character(),
                   "FILLER" = col_character())) %>% filter(
                     (`TIPO DO REGISTRO` == 3))

fat.tec16$`DATA DE LANCAMENTO` <- "09/2019"

fat.tec16$`VALOR DO LANCAMENTO` <- fat.tec16$`VALOR DO LANCAMENTO`/100

fat.tec16$`VALOR DO LANCAMENTO` <- ifelse(
  fat.tec16$`CODIGO DO LANCAMENTO` > 49,
  fat.tec16$`VALOR DO LANCAMENTO`*(-1),
  fat.tec16$`VALOR DO LANCAMENTO`)

fat.tec16 <- fat.tec16 %>% group_by(`NUMERO DA SUBFATURA`,`NUMERO DO CERTIFICADO`,
                                    `COMPLEMENTO DO CERTIFICADO`,`NOME SEGURADO/DEPENDENTE`,
                                    `DATA DE NASCIMENTO`,`CODIGO DO SEXO`,`ESTADO CIVIL`,
                                    `COD. GRAU PARENT.DEP.`,`CODIGO DO PLANO`,
                                    `DATA DE LANCAMENTO`,`CARGO / OCUPACAO`) %>% summarise(
                                      `VALOR DO LANCAMENTO` = sum(`VALOR DO LANCAMENTO`))

calculo <- fat.tec16 %>% filter(`NUMERO DO CERTIFICADO` == 0)

fat.tec16 <- fat.tec16 %>% filter(!`NUMERO DO CERTIFICADO` == 0)

fat.tec16$tit <- if_else(fat.tec16$`COMPLEMENTO DO CERTIFICADO` == 0, 1,0)

fat.tec16$conta <- fat.tec16$tit/sum(fat.tec16$tit)*sum(calculo$`VALOR DO LANCAMENTO`)

fat.tec16$`VALOR DO LANCAMENTO` <- fat.tec16$`VALOR DO LANCAMENTO` + fat.tec16$conta

fat.tec16$conta <- NULL
fat.tec16$tit <- NULL

fat.tec17 <- readr:: read_fwf(
  file ="D:/Users/sb046971/Documents/fat/FM071015_D190930.TXT",
  fwf_widths(widths, col_names = names), skip_empty_rows = T,skip = 1,
  col_types = cols("TIPO DO REGISTRO" = col_integer(),
                   "NUMERO DA SUBFATURA" = col_integer(),
                   "NUMERO DO CERTIFICADO" = col_integer(),
                   "COMPLEMENTO DO CERTIFICADO" = col_integer(),
                   "NOME SEGURADO/DEPENDENTE" = col_character(),
                   "INDIC. SUBF. ANTER/ATUAL" = col_character(),
                   "DATA DE NASCIMENTO" = col_character(),
                   "CODIGO DO SEXO" = col_integer(),
                   "ESTADO CIVIL" = col_integer(),
                   "COD. GRAU PARENT.DEP." = col_integer(),
                   "CODIGO DO PLANO" = col_character(),
                   "DATA INICIO VIGENCIA" = col_character(),
                   "TIPO DE LANÇAMENTO" = col_character(),
                   "DATA DE LANCAMENTO" = col_character(),
                   "VALOR DO LANCAMENTO" = col_integer(),
                   "PARTE DO SEGURADO" = col_integer(),
                   "CODIGO DO LANCAMENTO" = col_character(),
                   "CARGO / OCUPACAO" = col_character(),
                   "Matrícula" = col_character(),
                   "FILLER" = col_character())) %>% filter(
                     (`TIPO DO REGISTRO` == 3))

fat.tec17$`DATA DE LANCAMENTO` <- "09/2019"

fat.tec17$`VALOR DO LANCAMENTO` <- fat.tec17$`VALOR DO LANCAMENTO`/100

fat.tec17$`VALOR DO LANCAMENTO` <- ifelse(
  fat.tec17$`CODIGO DO LANCAMENTO` > 49,
  fat.tec17$`VALOR DO LANCAMENTO`*(-1),
  fat.tec17$`VALOR DO LANCAMENTO`)

fat.tec17 <- fat.tec17 %>% group_by(`NUMERO DA SUBFATURA`,`NUMERO DO CERTIFICADO`,
                                    `COMPLEMENTO DO CERTIFICADO`,`NOME SEGURADO/DEPENDENTE`,
                                    `DATA DE NASCIMENTO`,`CODIGO DO SEXO`,`ESTADO CIVIL`,
                                    `COD. GRAU PARENT.DEP.`,`CODIGO DO PLANO`,
                                    `DATA DE LANCAMENTO`,`CARGO / OCUPACAO`) %>% summarise(
                                      `VALOR DO LANCAMENTO` = sum(`VALOR DO LANCAMENTO`))

calculo <- fat.tec17 %>% filter(`NUMERO DO CERTIFICADO` == 0)

fat.tec17 <- fat.tec17 %>% filter(!`NUMERO DO CERTIFICADO` == 0)

fat.tec17$tit <- if_else(fat.tec17$`COMPLEMENTO DO CERTIFICADO` == 0, 1,0)

fat.tec17$conta <- fat.tec17$tit/sum(fat.tec17$tit)*sum(calculo$`VALOR DO LANCAMENTO`)

fat.tec17$`VALOR DO LANCAMENTO` <- fat.tec17$`VALOR DO LANCAMENTO` + fat.tec17$conta

fat.tec17$conta <- NULL
fat.tec17$tit <- NULL

fat.tec18 <- readr:: read_fwf(
  file ="D:/Users/sb046971/Documents/fat/FM071015_D191028.TXT",
  fwf_widths(widths, col_names = names), skip_empty_rows = T,skip = 1,
  col_types = cols("TIPO DO REGISTRO" = col_integer(),
                   "NUMERO DA SUBFATURA" = col_integer(),
                   "NUMERO DO CERTIFICADO" = col_integer(),
                   "COMPLEMENTO DO CERTIFICADO" = col_integer(),
                   "NOME SEGURADO/DEPENDENTE" = col_character(),
                   "INDIC. SUBF. ANTER/ATUAL" = col_character(),
                   "DATA DE NASCIMENTO" = col_character(),
                   "CODIGO DO SEXO" = col_integer(),
                   "ESTADO CIVIL" = col_integer(),
                   "COD. GRAU PARENT.DEP." = col_integer(),
                   "CODIGO DO PLANO" = col_character(),
                   "DATA INICIO VIGENCIA" = col_character(),
                   "TIPO DE LANÇAMENTO" = col_character(),
                   "DATA DE LANCAMENTO" = col_character(),
                   "VALOR DO LANCAMENTO" = col_integer(),
                   "PARTE DO SEGURADO" = col_integer(),
                   "CODIGO DO LANCAMENTO" = col_character(),
                   "CARGO / OCUPACAO" = col_character(),
                   "Matrícula" = col_character(),
                   "FILLER" = col_character())) %>% filter(
                     (`TIPO DO REGISTRO` == 3))

fat.tec18$`DATA DE LANCAMENTO` <- "11/2019"

fat.tec18$`VALOR DO LANCAMENTO` <- fat.tec18$`VALOR DO LANCAMENTO`/100

fat.tec18$`VALOR DO LANCAMENTO` <- ifelse(
  fat.tec18$`CODIGO DO LANCAMENTO` > 49,
  fat.tec18$`VALOR DO LANCAMENTO`*(-1),
  fat.tec18$`VALOR DO LANCAMENTO`)

fat.tec18 <- fat.tec18 %>% group_by(`NUMERO DA SUBFATURA`,`NUMERO DO CERTIFICADO`,
                                    `COMPLEMENTO DO CERTIFICADO`,`NOME SEGURADO/DEPENDENTE`,
                                    `DATA DE NASCIMENTO`,`CODIGO DO SEXO`,`ESTADO CIVIL`,
                                    `COD. GRAU PARENT.DEP.`,`CODIGO DO PLANO`,
                                    `DATA DE LANCAMENTO`,`CARGO / OCUPACAO`) %>% summarise(
                                      `VALOR DO LANCAMENTO` = sum(`VALOR DO LANCAMENTO`))

calculo <- fat.tec18 %>% filter(`NUMERO DO CERTIFICADO` == 0)

fat.tec18 <- fat.tec18 %>% filter(!`NUMERO DO CERTIFICADO` == 0)

fat.tec18$tit <- if_else(fat.tec18$`COMPLEMENTO DO CERTIFICADO` == 0, 1,0)

fat.tec18$conta <- fat.tec18$tit/sum(fat.tec18$tit)*sum(calculo$`VALOR DO LANCAMENTO`)

fat.tec18$`VALOR DO LANCAMENTO` <- fat.tec18$`VALOR DO LANCAMENTO` + fat.tec18$conta

fat.tec18$conta <- NULL
fat.tec18$tit <- NULL

fat.tec19 <- readr:: read_fwf(
  file ="D:/Users/sb046971/Documents/fat/FM071015_D191108.TXT",
  fwf_widths(widths, col_names = names), skip_empty_rows = T,skip = 1,
  col_types = cols("TIPO DO REGISTRO" = col_integer(),
                   "NUMERO DA SUBFATURA" = col_integer(),
                   "NUMERO DO CERTIFICADO" = col_integer(),
                   "COMPLEMENTO DO CERTIFICADO" = col_integer(),
                   "NOME SEGURADO/DEPENDENTE" = col_character(),
                   "INDIC. SUBF. ANTER/ATUAL" = col_character(),
                   "DATA DE NASCIMENTO" = col_character(),
                   "CODIGO DO SEXO" = col_integer(),
                   "ESTADO CIVIL" = col_integer(),
                   "COD. GRAU PARENT.DEP." = col_integer(),
                   "CODIGO DO PLANO" = col_character(),
                   "DATA INICIO VIGENCIA" = col_character(),
                   "TIPO DE LANÇAMENTO" = col_character(),
                   "DATA DE LANCAMENTO" = col_character(),
                   "VALOR DO LANCAMENTO" = col_integer(),
                   "PARTE DO SEGURADO" = col_integer(),
                   "CODIGO DO LANCAMENTO" = col_character(),
                   "CARGO / OCUPACAO" = col_character(),
                   "Matrícula" = col_character(),
                   "FILLER" = col_character())) %>% filter(
                     (`TIPO DO REGISTRO` == 3))

fat.tec19$`DATA DE LANCAMENTO` <- "12/2019"

fat.tec19$`VALOR DO LANCAMENTO` <- fat.tec19$`VALOR DO LANCAMENTO`/100

fat.tec19$`VALOR DO LANCAMENTO` <- ifelse(
  fat.tec19$`CODIGO DO LANCAMENTO` > 49,
  fat.tec19$`VALOR DO LANCAMENTO`*(-1),
  fat.tec19$`VALOR DO LANCAMENTO`)

fat.tec19 <- fat.tec19 %>% group_by(`NUMERO DA SUBFATURA`,`NUMERO DO CERTIFICADO`,
                                    `COMPLEMENTO DO CERTIFICADO`,`NOME SEGURADO/DEPENDENTE`,
                                    `DATA DE NASCIMENTO`,`CODIGO DO SEXO`,`ESTADO CIVIL`,
                                    `COD. GRAU PARENT.DEP.`,`CODIGO DO PLANO`,
                                    `DATA DE LANCAMENTO`,`CARGO / OCUPACAO`) %>% summarise(
                                      `VALOR DO LANCAMENTO` = sum(`VALOR DO LANCAMENTO`))

calculo <- fat.tec19 %>% filter(`NUMERO DO CERTIFICADO` == 0)

fat.tec19 <- fat.tec19 %>% filter(!`NUMERO DO CERTIFICADO` == 0)

fat.tec19$tit <- if_else(fat.tec19$`COMPLEMENTO DO CERTIFICADO` == 0, 1,0)

fat.tec19$conta <- fat.tec19$tit/sum(fat.tec19$tit)*sum(calculo$`VALOR DO LANCAMENTO`)

fat.tec19$`VALOR DO LANCAMENTO` <- fat.tec19$`VALOR DO LANCAMENTO` + fat.tec19$conta

fat.tec19$conta <- NULL
fat.tec19$tit <- NULL


fat.tec20 <- readr:: read_fwf(
  file ="D:/Users/sb046971/Documents/fat/FM071015_D191211.TXT",
  fwf_widths(widths, col_names = names), skip_empty_rows = T,skip = 1,
  col_types = cols("TIPO DO REGISTRO" = col_integer(),
                   "NUMERO DA SUBFATURA" = col_integer(),
                   "NUMERO DO CERTIFICADO" = col_integer(),
                   "COMPLEMENTO DO CERTIFICADO" = col_integer(),
                   "NOME SEGURADO/DEPENDENTE" = col_character(),
                   "INDIC. SUBF. ANTER/ATUAL" = col_character(),
                   "DATA DE NASCIMENTO" = col_character(),
                   "CODIGO DO SEXO" = col_integer(),
                   "ESTADO CIVIL" = col_integer(),
                   "COD. GRAU PARENT.DEP." = col_integer(),
                   "CODIGO DO PLANO" = col_character(),
                   "DATA INICIO VIGENCIA" = col_character(),
                   "TIPO DE LANÇAMENTO" = col_character(),
                   "DATA DE LANCAMENTO" = col_character(),
                   "VALOR DO LANCAMENTO" = col_integer(),
                   "PARTE DO SEGURADO" = col_integer(),
                   "CODIGO DO LANCAMENTO" = col_character(),
                   "CARGO / OCUPACAO" = col_character(),
                   "Matrícula" = col_character(),
                   "FILLER" = col_character())) %>% filter(
                     (`TIPO DO REGISTRO` == 3))

fat.tec20$`DATA DE LANCAMENTO` <- "01/2020"

fat.tec20$`VALOR DO LANCAMENTO` <- fat.tec20$`VALOR DO LANCAMENTO`/100

fat.tec20$`VALOR DO LANCAMENTO` <- ifelse(
  fat.tec20$`CODIGO DO LANCAMENTO` > 49,
  fat.tec20$`VALOR DO LANCAMENTO`*(-1),
  fat.tec20$`VALOR DO LANCAMENTO`)

fat.tec20 <- fat.tec20 %>% group_by(`NUMERO DA SUBFATURA`,`NUMERO DO CERTIFICADO`,
                                    `COMPLEMENTO DO CERTIFICADO`,`NOME SEGURADO/DEPENDENTE`,
                                    `DATA DE NASCIMENTO`,`CODIGO DO SEXO`,`ESTADO CIVIL`,
                                    `COD. GRAU PARENT.DEP.`,`CODIGO DO PLANO`,
                                    `DATA DE LANCAMENTO`,`CARGO / OCUPACAO`) %>% summarise(
                                      `VALOR DO LANCAMENTO` = sum(`VALOR DO LANCAMENTO`))

calculo <- fat.tec20 %>% filter(`NUMERO DO CERTIFICADO` == 0)

fat.tec20 <- fat.tec20 %>% filter(!`NUMERO DO CERTIFICADO` == 0)

fat.tec20$tit <- if_else(fat.tec20$`COMPLEMENTO DO CERTIFICADO` == 0, 1,0)

fat.tec20$conta <- fat.tec20$tit/sum(fat.tec20$tit)*sum(calculo$`VALOR DO LANCAMENTO`)

fat.tec20$`VALOR DO LANCAMENTO` <- fat.tec20$`VALOR DO LANCAMENTO` + fat.tec20$conta

fat.tec20$conta <- NULL
fat.tec20$tit <- NULL


fat.tec21 <- readr:: read_fwf(
  file ="D:/Users/sb046971/Documents/fat/FM071015_D200211.TXT",
  fwf_widths(widths, col_names = names), skip_empty_rows = T,skip = 1,
  col_types = cols("TIPO DO REGISTRO" = col_integer(),
                   "NUMERO DA SUBFATURA" = col_integer(),
                   "NUMERO DO CERTIFICADO" = col_integer(),
                   "COMPLEMENTO DO CERTIFICADO" = col_integer(),
                   "NOME SEGURADO/DEPENDENTE" = col_character(),
                   "INDIC. SUBF. ANTER/ATUAL" = col_character(),
                   "DATA DE NASCIMENTO" = col_character(),
                   "CODIGO DO SEXO" = col_integer(),
                   "ESTADO CIVIL" = col_integer(),
                   "COD. GRAU PARENT.DEP." = col_integer(),
                   "CODIGO DO PLANO" = col_character(),
                   "DATA INICIO VIGENCIA" = col_character(),
                   "TIPO DE LANÇAMENTO" = col_character(),
                   "DATA DE LANCAMENTO" = col_character(),
                   "VALOR DO LANCAMENTO" = col_integer(),
                   "PARTE DO SEGURADO" = col_integer(),
                   "CODIGO DO LANCAMENTO" = col_character(),
                   "CARGO / OCUPACAO" = col_character(),
                   "Matrícula" = col_character(),
                   "FILLER" = col_character())) %>% filter(
                     (`TIPO DO REGISTRO` == 3))

fat.tec21$`DATA DE LANCAMENTO` <- "03/2020"

fat.tec21$`VALOR DO LANCAMENTO` <- fat.tec21$`VALOR DO LANCAMENTO`/100

fat.tec21$`VALOR DO LANCAMENTO` <- ifelse(
  fat.tec21$`CODIGO DO LANCAMENTO` > 49,
  fat.tec21$`VALOR DO LANCAMENTO`*(-1),
  fat.tec21$`VALOR DO LANCAMENTO`)

fat.tec21 <- fat.tec21 %>% group_by(`NUMERO DA SUBFATURA`,`NUMERO DO CERTIFICADO`,
                                    `COMPLEMENTO DO CERTIFICADO`,`NOME SEGURADO/DEPENDENTE`,
                                    `DATA DE NASCIMENTO`,`CODIGO DO SEXO`,`ESTADO CIVIL`,
                                    `COD. GRAU PARENT.DEP.`,`CODIGO DO PLANO`,
                                    `DATA DE LANCAMENTO`,`CARGO / OCUPACAO`) %>% summarise(
                                      `VALOR DO LANCAMENTO` = sum(`VALOR DO LANCAMENTO`))

calculo <- fat.tec21 %>% filter(`NUMERO DO CERTIFICADO` == 0)

fat.tec21 <- fat.tec21 %>% filter(!`NUMERO DO CERTIFICADO` == 0)

fat.tec21$tit <- if_else(fat.tec21$`COMPLEMENTO DO CERTIFICADO` == 0, 1,0)

fat.tec21$conta <- fat.tec21$tit/sum(fat.tec21$tit)*sum(calculo$`VALOR DO LANCAMENTO`)

fat.tec21$`VALOR DO LANCAMENTO` <- fat.tec21$`VALOR DO LANCAMENTO` + fat.tec21$conta

fat.tec21$conta <- NULL
fat.tec21$tit <- NULL


fat.tec22 <- readr:: read_fwf(
  file ="D:/Users/sb046971/Documents/fat/FM071015_D200311.TXT",
  fwf_widths(widths, col_names = names), skip_empty_rows = T,skip = 1,
  col_types = cols("TIPO DO REGISTRO" = col_integer(),
                   "NUMERO DA SUBFATURA" = col_integer(),
                   "NUMERO DO CERTIFICADO" = col_integer(),
                   "COMPLEMENTO DO CERTIFICADO" = col_integer(),
                   "NOME SEGURADO/DEPENDENTE" = col_character(),
                   "INDIC. SUBF. ANTER/ATUAL" = col_character(),
                   "DATA DE NASCIMENTO" = col_character(),
                   "CODIGO DO SEXO" = col_integer(),
                   "ESTADO CIVIL" = col_integer(),
                   "COD. GRAU PARENT.DEP." = col_integer(),
                   "CODIGO DO PLANO" = col_character(),
                   "DATA INICIO VIGENCIA" = col_character(),
                   "TIPO DE LANÇAMENTO" = col_character(),
                   "DATA DE LANCAMENTO" = col_character(),
                   "VALOR DO LANCAMENTO" = col_integer(),
                   "PARTE DO SEGURADO" = col_integer(),
                   "CODIGO DO LANCAMENTO" = col_character(),
                   "CARGO / OCUPACAO" = col_character(),
                   "Matrícula" = col_character(),
                   "FILLER" = col_character())) %>% filter(
                     (`TIPO DO REGISTRO` == 3))

fat.tec22$`DATA DE LANCAMENTO` <- "04/2020"

fat.tec22$`VALOR DO LANCAMENTO` <- fat.tec22$`VALOR DO LANCAMENTO`/100

fat.tec22$`VALOR DO LANCAMENTO` <- ifelse(
  fat.tec22$`CODIGO DO LANCAMENTO` > 49,
  fat.tec22$`VALOR DO LANCAMENTO`*(-1),
  fat.tec22$`VALOR DO LANCAMENTO`)

fat.tec22 <- fat.tec22 %>% group_by(`NUMERO DA SUBFATURA`,`NUMERO DO CERTIFICADO`,
                                    `COMPLEMENTO DO CERTIFICADO`,`NOME SEGURADO/DEPENDENTE`,
                                    `DATA DE NASCIMENTO`,`CODIGO DO SEXO`,`ESTADO CIVIL`,
                                    `COD. GRAU PARENT.DEP.`,`CODIGO DO PLANO`,
                                    `DATA DE LANCAMENTO`,`CARGO / OCUPACAO`) %>% summarise(
                                      `VALOR DO LANCAMENTO` = sum(`VALOR DO LANCAMENTO`))

calculo <- fat.tec22 %>% filter(`NUMERO DO CERTIFICADO` == 0)

fat.tec22 <- fat.tec22 %>% filter(!`NUMERO DO CERTIFICADO` == 0)

fat.tec22$tit <- if_else(fat.tec22$`COMPLEMENTO DO CERTIFICADO` == 0, 1,0)

fat.tec22$conta <- fat.tec22$tit/sum(fat.tec22$tit)*sum(calculo$`VALOR DO LANCAMENTO`)

fat.tec22$`VALOR DO LANCAMENTO` <- fat.tec22$`VALOR DO LANCAMENTO` + fat.tec22$conta

fat.tec22$conta <- NULL
fat.tec22$tit <- NULL


fat.tec23 <- readr:: read_fwf(
  file ="D:/Users/sb046971/Documents/fat/FM11071015 15 12 2018.TXT",
  fwf_widths(widths, col_names = names), skip_empty_rows = T,skip = 1,
  col_types = cols("TIPO DO REGISTRO" = col_integer(),
                   "NUMERO DA SUBFATURA" = col_integer(),
                   "NUMERO DO CERTIFICADO" = col_integer(),
                   "COMPLEMENTO DO CERTIFICADO" = col_integer(),
                   "NOME SEGURADO/DEPENDENTE" = col_character(),
                   "INDIC. SUBF. ANTER/ATUAL" = col_character(),
                   "DATA DE NASCIMENTO" = col_character(),
                   "CODIGO DO SEXO" = col_integer(),
                   "ESTADO CIVIL" = col_integer(),
                   "COD. GRAU PARENT.DEP." = col_integer(),
                   "CODIGO DO PLANO" = col_character(),
                   "DATA INICIO VIGENCIA" = col_character(),
                   "TIPO DE LANÇAMENTO" = col_character(),
                   "DATA DE LANCAMENTO" = col_character(),
                   "VALOR DO LANCAMENTO" = col_integer(),
                   "PARTE DO SEGURADO" = col_integer(),
                   "CODIGO DO LANCAMENTO" = col_character(),
                   "CARGO / OCUPACAO" = col_character(),
                   "Matrícula" = col_character(),
                   "FILLER" = col_character())) %>% filter(
                     (`TIPO DO REGISTRO` == 3))

fat.tec23$`DATA DE LANCAMENTO` <- "12/2018"

fat.tec23$`VALOR DO LANCAMENTO` <- fat.tec23$`VALOR DO LANCAMENTO`/100

fat.tec23$`VALOR DO LANCAMENTO` <- ifelse(
  fat.tec23$`CODIGO DO LANCAMENTO` > 49,
  fat.tec23$`VALOR DO LANCAMENTO`*(-1),
  fat.tec23$`VALOR DO LANCAMENTO`)

fat.tec23 <- fat.tec23 %>% group_by(`NUMERO DA SUBFATURA`,`NUMERO DO CERTIFICADO`,
                                `COMPLEMENTO DO CERTIFICADO`,`NOME SEGURADO/DEPENDENTE`,
                                `DATA DE NASCIMENTO`,`CODIGO DO SEXO`,`ESTADO CIVIL`,
                                `COD. GRAU PARENT.DEP.`,`CODIGO DO PLANO`,
                                `DATA DE LANCAMENTO`,`CARGO / OCUPACAO`) %>% summarise(
                                  `VALOR DO LANCAMENTO` = sum(`VALOR DO LANCAMENTO`))

calculo <- fat.tec23 %>% filter(`NUMERO DO CERTIFICADO` == 0)

fat.tec23 <- fat.tec23 %>% filter(!`NUMERO DO CERTIFICADO` == 0)

fat.tec23$tit <- if_else(fat.tec23$`COMPLEMENTO DO CERTIFICADO` == 0, 1,0)

fat.tec23$conta <- fat.tec23$tit/sum(fat.tec23$tit)*sum(calculo$`VALOR DO LANCAMENTO`)

fat.tec23$`VALOR DO LANCAMENTO` <- fat.tec23$`VALOR DO LANCAMENTO` + fat.tec23$conta

fat.tec23$conta <- NULL
fat.tec23$tit <- NULL

fat.tec <- bind_rows(fat.tec0,fat.tec1,fat.tec2,fat.tec3,fat.tec5,fat.tec6,fat.tec7,
                     fat.tec8,fat.tec9,fat.tec10,fat.tec11,fat.tec12,fat.tec13,
                     fat.tec14,fat.tec15,fat.tec17,fat.tec18,fat.tec19,fat.tec20,
                     fat.tec21,fat.tec22,fat.tec23)

fat.tec$`VALOR DO LANCAMENTO` <- round(fat.tec$`VALOR DO LANCAMENTO`,2)

premio <- fat.tec %>% group_by(`CODIGO DO PLANO`,
                               `DATA DE LANCAMENTO`) %>% summarise(
                                 Premio = sum(`VALOR DO LANCAMENTO`))

fwrite(premio, "D:/Users/sb046971/Documents/premio.csv", sep = "|", dec = ",")

