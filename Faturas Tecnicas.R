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

widths = c(1,4,7,2,35,4,8,1,1,1,4,8,2,6,15,15,2,20,12,32)

names =  c("TIPO DO REGISTRO","NUMERO DA SUBFATURA","NUMERO DO CERTIFICADO",
           "COMPLEMENTO DO CERTIFICADO","NOME SEGURADO/DEPENDENTE",
           "INDIC. SUBF. ANTER/ATUAL","DATA DE NASCIMENTO",
           "CODIGO DO SEXO","ESTADO CIVIL","COD. GRAU PARENT.DEP.",
           "CODIGO DO PLANO","DATA INICIO VIGENCIA","TIPO DE LANÇAMENTO",
           "DATA DE LANCAMENTO","VALOR DO LANCAMENTO","PARTE DO SEGURADO",
           "CODIGO DO LANCAMENTO","CARGO / OCUPACAO","Matrícula","FILLER")

ft1 <- readr:: read_fwf(
  file ="D:/Users/sb046971/Documents/Faturas Tecnicas/FM01071015.TXT",
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

ft1$`VALOR DO LANCAMENTO` <- ft1$`VALOR DO LANCAMENTO`/100

ft1$`VALOR DO LANCAMENTO` <- ifelse(
  ft1$`CODIGO DO LANCAMENTO` >49,
  ft1$`VALOR DO LANCAMENTO`*(-1),
  ft1$`VALOR DO LANCAMENTO`)

ft1 <- ft1 %>% group_by(
  `TIPO DO REGISTRO`,`NUMERO DA SUBFATURA`,`NUMERO DO CERTIFICADO`,
  `COMPLEMENTO DO CERTIFICADO`,`NOME SEGURADO/DEPENDENTE`,`CODIGO DO SEXO`,
  `ESTADO CIVIL`,`COD. GRAU PARENT.DEP.`,`CODIGO DO PLANO`,
  `DATA INICIO VIGENCIA`,`TIPO DE LANÇAMENTO`,`PARTE DO SEGURADO`,
  `CARGO / OCUPACAO`,Matrícula) %>% summarise(Valor = sum(
    `VALOR DO LANCAMENTO`))

ft2 <- readr:: read_fwf(
  file ="D:/Users/sb046971/Documents/Faturas Tecnicas/FM07071015.TXT",
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

ft2$`VALOR DO LANCAMENTO` <- ft2$`VALOR DO LANCAMENTO`/100

ft2$`VALOR DO LANCAMENTO` <- ifelse(
  ft2$`CODIGO DO LANCAMENTO` >49,
  ft2$`VALOR DO LANCAMENTO`*(-1),
  ft2$`VALOR DO LANCAMENTO`)

ft2 <- ft2 %>% group_by(
  `TIPO DO REGISTRO`,`NUMERO DA SUBFATURA`,`NUMERO DO CERTIFICADO`,
  `COMPLEMENTO DO CERTIFICADO`,`NOME SEGURADO/DEPENDENTE`,`CODIGO DO SEXO`,
  `ESTADO CIVIL`,`COD. GRAU PARENT.DEP.`,`CODIGO DO PLANO`,
  `DATA INICIO VIGENCIA`,`TIPO DE LANÇAMENTO`,`PARTE DO SEGURADO`,
  `CARGO / OCUPACAO`,Matrícula) %>% summarise(Valor = sum(
    `VALOR DO LANCAMENTO`))

ft3 <- readr:: read_fwf(
  file ="D:/Users/sb046971/Documents/Faturas Tecnicas/FM071015_D190312.TXT",
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

ft3$`VALOR DO LANCAMENTO` <- ft3$`VALOR DO LANCAMENTO`/100

ft3$`VALOR DO LANCAMENTO` <- ifelse(
  ft3$`CODIGO DO LANCAMENTO` >49,
  ft3$`VALOR DO LANCAMENTO`*(-1),
  ft3$`VALOR DO LANCAMENTO`)

ft3 <- ft3 %>% group_by(
  `TIPO DO REGISTRO`,`NUMERO DA SUBFATURA`,`NUMERO DO CERTIFICADO`,
  `COMPLEMENTO DO CERTIFICADO`,`NOME SEGURADO/DEPENDENTE`,`CODIGO DO SEXO`,
  `ESTADO CIVIL`,`COD. GRAU PARENT.DEP.`,`CODIGO DO PLANO`,
  `DATA INICIO VIGENCIA`,`TIPO DE LANÇAMENTO`,`PARTE DO SEGURADO`,
  `CARGO / OCUPACAO`,Matrícula) %>% summarise(Valor = sum(
    `VALOR DO LANCAMENTO`))

ft4 <- readr:: read_fwf(
  file ="D:/Users/sb046971/Documents/Faturas Tecnicas/FM071015_D190410.TXT",
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

ft4$`VALOR DO LANCAMENTO` <- ft4$`VALOR DO LANCAMENTO`/100

ft4$`VALOR DO LANCAMENTO` <- ifelse(
  ft4$`CODIGO DO LANCAMENTO` >49,
  ft4$`VALOR DO LANCAMENTO`*(-1),
  ft4$`VALOR DO LANCAMENTO`)

ft4 <- ft4 %>% group_by(
  `TIPO DO REGISTRO`,`NUMERO DA SUBFATURA`,`NUMERO DO CERTIFICADO`,
  `COMPLEMENTO DO CERTIFICADO`,`NOME SEGURADO/DEPENDENTE`,`CODIGO DO SEXO`,
  `ESTADO CIVIL`,`COD. GRAU PARENT.DEP.`,`CODIGO DO PLANO`,
  `DATA INICIO VIGENCIA`,`TIPO DE LANÇAMENTO`,`PARTE DO SEGURADO`,
  `CARGO / OCUPACAO`,Matrícula) %>% summarise(Valor = sum(
    `VALOR DO LANCAMENTO`))

ft5 <- readr:: read_fwf(
  file ="D:/Users/sb046971/Documents/Faturas Tecnicas/FM071015_D190514.TXT",
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

ft5$`VALOR DO LANCAMENTO` <- ft5$`VALOR DO LANCAMENTO`/100

ft5$`VALOR DO LANCAMENTO` <- ifelse(
  ft5$`CODIGO DO LANCAMENTO` >49,
  ft5$`VALOR DO LANCAMENTO`*(-1),
  ft5$`VALOR DO LANCAMENTO`)

ft5 <- ft5 %>% group_by(
  `TIPO DO REGISTRO`,`NUMERO DA SUBFATURA`,`NUMERO DO CERTIFICADO`,
  `COMPLEMENTO DO CERTIFICADO`,`NOME SEGURADO/DEPENDENTE`,`CODIGO DO SEXO`,
  `ESTADO CIVIL`,`COD. GRAU PARENT.DEP.`,`CODIGO DO PLANO`,
  `DATA INICIO VIGENCIA`,`TIPO DE LANÇAMENTO`,`PARTE DO SEGURADO`,
  `CARGO / OCUPACAO`,Matrícula) %>% summarise(Valor = sum(
    `VALOR DO LANCAMENTO`))

ft6 <- readr:: read_fwf(
  file ="D:/Users/sb046971/Documents/Faturas Tecnicas/FM071015_D190611.TXT",
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

ft6$`VALOR DO LANCAMENTO` <- ft6$`VALOR DO LANCAMENTO`/100

ft6$`VALOR DO LANCAMENTO` <- ifelse(
  ft6$`CODIGO DO LANCAMENTO` >49,
  ft6$`VALOR DO LANCAMENTO`*(-1),
  ft6$`VALOR DO LANCAMENTO`)

ft6 <- ft6 %>% group_by(
  `TIPO DO REGISTRO`,`NUMERO DA SUBFATURA`,`NUMERO DO CERTIFICADO`,
  `COMPLEMENTO DO CERTIFICADO`,`NOME SEGURADO/DEPENDENTE`,`CODIGO DO SEXO`,
  `ESTADO CIVIL`,`COD. GRAU PARENT.DEP.`,`CODIGO DO PLANO`,
  `DATA INICIO VIGENCIA`,`TIPO DE LANÇAMENTO`,`PARTE DO SEGURADO`,
  `CARGO / OCUPACAO`,Matrícula) %>% summarise(Valor = sum(
    `VALOR DO LANCAMENTO`))

ft7 <- readr:: read_fwf(
  file ="D:/Users/sb046971/Documents/Faturas Tecnicas/FM071015_D190827.TXT",
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

ft7$`VALOR DO LANCAMENTO` <- ft7$`VALOR DO LANCAMENTO`/100

ft7$`VALOR DO LANCAMENTO` <- ifelse(
  ft7$`CODIGO DO LANCAMENTO` >49,
  ft7$`VALOR DO LANCAMENTO`*(-1),
  ft7$`VALOR DO LANCAMENTO`)

ft7 <- ft7 %>% group_by(
  `TIPO DO REGISTRO`,`NUMERO DA SUBFATURA`,`NUMERO DO CERTIFICADO`,
  `COMPLEMENTO DO CERTIFICADO`,`NOME SEGURADO/DEPENDENTE`,`CODIGO DO SEXO`,
  `ESTADO CIVIL`,`COD. GRAU PARENT.DEP.`,`CODIGO DO PLANO`,
  `DATA INICIO VIGENCIA`,`TIPO DE LANÇAMENTO`,`PARTE DO SEGURADO`,
  `CARGO / OCUPACAO`,Matrícula) %>% summarise(Valor = sum(
    `VALOR DO LANCAMENTO`))

ft8 <- readr:: read_fwf(
  file ="D:/Users/sb046971/Documents/Faturas Tecnicas/FM071015_D190930.TXT",
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

ft8$`VALOR DO LANCAMENTO` <- ft8$`VALOR DO LANCAMENTO`/100

ft8$`VALOR DO LANCAMENTO` <- ifelse(
  ft8$`CODIGO DO LANCAMENTO` >49,
  ft8$`VALOR DO LANCAMENTO`*(-1),
  ft8$`VALOR DO LANCAMENTO`)

ft8 <- ft8 %>% group_by(
  `TIPO DO REGISTRO`,`NUMERO DA SUBFATURA`,`NUMERO DO CERTIFICADO`,
  `COMPLEMENTO DO CERTIFICADO`,`NOME SEGURADO/DEPENDENTE`,`CODIGO DO SEXO`,
  `ESTADO CIVIL`,`COD. GRAU PARENT.DEP.`,`CODIGO DO PLANO`,
  `DATA INICIO VIGENCIA`,`TIPO DE LANÇAMENTO`,`PARTE DO SEGURADO`,
  `CARGO / OCUPACAO`,Matrícula) %>% summarise(Valor = sum(
    `VALOR DO LANCAMENTO`))

ft9 <- readr:: read_fwf(
  file ="D:/Users/sb046971/Documents/Faturas Tecnicas/FM071015_D191028.TXT",
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

ft9$`VALOR DO LANCAMENTO` <- ft9$`VALOR DO LANCAMENTO`/100

ft9$`VALOR DO LANCAMENTO` <- ifelse(
  ft9$`CODIGO DO LANCAMENTO` >49,
  ft9$`VALOR DO LANCAMENTO`*(-1),
  ft9$`VALOR DO LANCAMENTO`)

ft9 <- ft9 %>% group_by(
  `TIPO DO REGISTRO`,`NUMERO DA SUBFATURA`,`NUMERO DO CERTIFICADO`,
  `COMPLEMENTO DO CERTIFICADO`,`NOME SEGURADO/DEPENDENTE`,`CODIGO DO SEXO`,
  `ESTADO CIVIL`,`COD. GRAU PARENT.DEP.`,`CODIGO DO PLANO`,
  `DATA INICIO VIGENCIA`,`TIPO DE LANÇAMENTO`,`PARTE DO SEGURADO`,
  `CARGO / OCUPACAO`,Matrícula) %>% summarise(Valor = sum(
    `VALOR DO LANCAMENTO`))

ft10 <- readr:: read_fwf(
  file ="D:/Users/sb046971/Documents/Faturas Tecnicas/FM071015_D191108.TXT",
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

ft10$`VALOR DO LANCAMENTO` <- ft10$`VALOR DO LANCAMENTO`/100

ft10$`VALOR DO LANCAMENTO` <- ifelse(
  ft10$`CODIGO DO LANCAMENTO` >49,
  ft10$`VALOR DO LANCAMENTO`*(-1),
  ft10$`VALOR DO LANCAMENTO`)

ft10 <- ft10 %>% group_by(
  `TIPO DO REGISTRO`,`NUMERO DA SUBFATURA`,`NUMERO DO CERTIFICADO`,
  `COMPLEMENTO DO CERTIFICADO`,`NOME SEGURADO/DEPENDENTE`,`CODIGO DO SEXO`,
  `ESTADO CIVIL`,`COD. GRAU PARENT.DEP.`,`CODIGO DO PLANO`,
  `DATA INICIO VIGENCIA`,`TIPO DE LANÇAMENTO`,`PARTE DO SEGURADO`,
  `CARGO / OCUPACAO`,Matrícula) %>% summarise(Valor = sum(
    `VALOR DO LANCAMENTO`))

ft11 <- readr:: read_fwf(
  file ="D:/Users/sb046971/Documents/Faturas Tecnicas/FM071015_D191211.TXT",
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

ft11$`VALOR DO LANCAMENTO` <- ft11$`VALOR DO LANCAMENTO`/100

ft11$`VALOR DO LANCAMENTO` <- ifelse(
  ft11$`CODIGO DO LANCAMENTO` >49,
  ft11$`VALOR DO LANCAMENTO`*(-1),
  ft11$`VALOR DO LANCAMENTO`)

ft11 <- ft11 %>% group_by(
  `TIPO DO REGISTRO`,`NUMERO DA SUBFATURA`,`NUMERO DO CERTIFICADO`,
  `COMPLEMENTO DO CERTIFICADO`,`NOME SEGURADO/DEPENDENTE`,`CODIGO DO SEXO`,
  `ESTADO CIVIL`,`COD. GRAU PARENT.DEP.`,`CODIGO DO PLANO`,
  `DATA INICIO VIGENCIA`,`TIPO DE LANÇAMENTO`,`PARTE DO SEGURADO`,
  `CARGO / OCUPACAO`,Matrícula) %>% summarise(Valor = sum(
    `VALOR DO LANCAMENTO`))

ft12 <- readr:: read_fwf(
  file ="D:/Users/sb046971/Documents/Faturas Tecnicas/FM071015_D200211.TXT",
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

ft12$`VALOR DO LANCAMENTO` <- ft12$`VALOR DO LANCAMENTO`/100

ft12$`VALOR DO LANCAMENTO` <- ifelse(
  ft12$`CODIGO DO LANCAMENTO` >49,
  ft12$`VALOR DO LANCAMENTO`*(-1),
  ft12$`VALOR DO LANCAMENTO`)

ft12 <- ft12 %>% group_by(
  `TIPO DO REGISTRO`,`NUMERO DA SUBFATURA`,`NUMERO DO CERTIFICADO`,
  `COMPLEMENTO DO CERTIFICADO`,`NOME SEGURADO/DEPENDENTE`,`CODIGO DO SEXO`,
  `ESTADO CIVIL`,`COD. GRAU PARENT.DEP.`,`CODIGO DO PLANO`,
  `DATA INICIO VIGENCIA`,`TIPO DE LANÇAMENTO`,`PARTE DO SEGURADO`,
  `CARGO / OCUPACAO`,Matrícula) %>% summarise(Valor = sum(
    `VALOR DO LANCAMENTO`))

ft13 <- readr:: read_fwf(
  file ="D:/Users/sb046971/Documents/Faturas Tecnicas/FM071015_D200311.TXT",
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

ft13$`VALOR DO LANCAMENTO` <- ft13$`VALOR DO LANCAMENTO`/100

ft13$`VALOR DO LANCAMENTO` <- ifelse(
  ft13$`CODIGO DO LANCAMENTO` >49,
  ft13$`VALOR DO LANCAMENTO`*(-1),
  ft13$`VALOR DO LANCAMENTO`)

ft13 <- ft13 %>% group_by(
  `TIPO DO REGISTRO`,`NUMERO DA SUBFATURA`,`NUMERO DO CERTIFICADO`,
  `COMPLEMENTO DO CERTIFICADO`,`NOME SEGURADO/DEPENDENTE`,`CODIGO DO SEXO`,
  `ESTADO CIVIL`,`COD. GRAU PARENT.DEP.`,`CODIGO DO PLANO`,
  `DATA INICIO VIGENCIA`,`TIPO DE LANÇAMENTO`,`PARTE DO SEGURADO`,
  `CARGO / OCUPACAO`,Matrícula) %>% summarise(Valor = sum(
    `VALOR DO LANCAMENTO`))

ft <- bind_rows(ft1,ft2,ft3,ft4,ft5,ft6,ft7,ft8,ft9,ft10,ft11,ft12,ft13)

ft1$eleg <- if_else(ft1$`COD. GRAU PARENT.DEP.` == 0,
                         "TITULAR", 
                         "DEPENDENTE") 

ft.fim1 <-  ft1 %>% group_by(
  `NOME SEGURADO/DEPENDENTE`,
  `CODIGO DO PLANO`,eleg) %>% summarise(Valor = sum(Valor)) %>% filter(
    !is.na(`CODIGO DO PLANO`)) %>% group_by(
      `CODIGO DO PLANO`, eleg) %>% mutate(
      qtde.titulares = n()) %>% summarise(
        valor = sum(Valor),n.pessoas = n()) %>% spread(
            eleg,n.pessoas, fill = 0) %>% group_by(
              `CODIGO DO PLANO`) %>% summarise(dep = sum(DEPENDENTE), 
                                               tit = sum(TITULAR), 
                                               total = tit + dep)

ft2$eleg <- if_else(ft2$`COD. GRAU PARENT.DEP.` == 0,
                    "TITULAR", 
                    "DEPENDENTE") 

ft.fim2 <-  ft2 %>% group_by(
  `NOME SEGURADO/DEPENDENTE`,
  `CODIGO DO PLANO`,eleg) %>% summarise(Valor = sum(Valor)) %>% filter(
  !is.na(`CODIGO DO PLANO`)) %>% group_by(
    `CODIGO DO PLANO`, eleg) %>% mutate(
      qtde.titulares = n()) %>% summarise(
        valor = sum(Valor),n.pessoas = n()) %>% spread(
          eleg,n.pessoas, fill = 0) %>% group_by(
            `CODIGO DO PLANO`) %>% summarise(dep = sum(DEPENDENTE), 
                                             tit = sum(TITULAR), 
                                             total = tit + dep)

ft3$eleg <- if_else(ft3$`COD. GRAU PARENT.DEP.` == 0,
                    "TITULAR", 
                    "DEPENDENTE") 

ft.fimft3 <-  ft3 %>% group_by(
  `NOME SEGURADO/DEPENDENTE`,
  `CODIGO DO PLANO`,eleg) %>% summarise(Valor = sum(Valor)) %>% filter(
  !is.na(`CODIGO DO PLANO`)) %>% group_by(
    `CODIGO DO PLANO`, eleg) %>% mutate(
      qtde.titulares = n()) %>% summarise(
        valor = sum(Valor),n.pessoas = n()) %>% spread(
          eleg,n.pessoas, fill = 0) %>% group_by(
            `CODIGO DO PLANO`) %>% summarise(dep = sum(DEPENDENTE), 
                                             tit = sum(TITULAR), 
                                             total = tit + dep)

ft4$eleg <- if_else(ft4$`COD. GRAU PARENT.DEP.` == 0,
                    "TITULAR", 
                    "DEPENDENTE") 

ft.fimft4 <-  ft4 %>% group_by(
  `NOME SEGURADO/DEPENDENTE`,
  `CODIGO DO PLANO`,eleg) %>% summarise(Valor = sum(Valor)) %>% filter(
  !is.na(`CODIGO DO PLANO`)) %>% group_by(
    `CODIGO DO PLANO`, eleg) %>% mutate(
      qtde.titulares = n()) %>% summarise(
        valor = sum(Valor),n.pessoas = n()) %>% spread(
          eleg,n.pessoas, fill = 0) %>% group_by(
            `CODIGO DO PLANO`) %>% summarise(dep = sum(DEPENDENTE), 
                                             tit = sum(TITULAR), 
                                             total = tit + dep)

ft5$eleg <- if_else(ft5$`COD. GRAU PARENT.DEP.` == 0,
                    "TITULAR", 
                    "DEPENDENTE") 

ft.fimft5 <-  ft5 %>% group_by(
  `NOME SEGURADO/DEPENDENTE`,
  `CODIGO DO PLANO`,eleg) %>% summarise(Valor = sum(Valor)) %>% filter(
  !is.na(`CODIGO DO PLANO`)) %>% group_by(
    `CODIGO DO PLANO`, eleg) %>% mutate(
      qtde.titulares = n()) %>% summarise(
        valor = sum(Valor),n.pessoas = n()) %>% spread(
          eleg,n.pessoas, fill = 0) %>% group_by(
            `CODIGO DO PLANO`) %>% summarise(dep = sum(DEPENDENTE), 
                                             tit = sum(TITULAR), 
                                             total = tit + dep)

ft6$eleg <- if_else(ft6$`COD. GRAU PARENT.DEP.` == 0,
                    "TITULAR", 
                    "DEPENDENTE") 

ft.fimft6 <-  ft6 %>% group_by(
  `NOME SEGURADO/DEPENDENTE`,
  `CODIGO DO PLANO`,eleg) %>% summarise(Valor = sum(Valor)) %>% filter(
  !is.na(`CODIGO DO PLANO`)) %>% group_by(
    `CODIGO DO PLANO`, eleg) %>% mutate(
      qtde.titulares = n()) %>% summarise(
        valor = sum(Valor),n.pessoas = n()) %>% spread(
          eleg,n.pessoas, fill = 0) %>% group_by(
            `CODIGO DO PLANO`) %>% summarise(dep = sum(DEPENDENTE), 
                                             tit = sum(TITULAR), 
                                             total = tit + dep)

ft7$eleg <- if_else(ft7$`COD. GRAU PARENT.DEP.` == 0,
                    "TITULAR", 
                    "DEPENDENTE") 

ft.fimft7 <-  ft7 %>% group_by(
  `NOME SEGURADO/DEPENDENTE`,
  `CODIGO DO PLANO`,eleg) %>% summarise(Valor = sum(Valor)) %>% filter(
  !is.na(`CODIGO DO PLANO`)) %>% group_by(
    `CODIGO DO PLANO`, eleg) %>% mutate(
      qtde.titulares = n()) %>% summarise(
        valor = sum(Valor),n.pessoas = n()) %>% spread(
          eleg,n.pessoas, fill = 0) %>% group_by(
            `CODIGO DO PLANO`) %>% summarise(dep = sum(DEPENDENTE), 
                                             tit = sum(TITULAR), 
                                             total = tit + dep)

ft8$eleg <- if_else(ft8$`COD. GRAU PARENT.DEP.` == 0,
                    "TITULAR", 
                    "DEPENDENTE") 

ft.fimft8 <-  ft8 %>% group_by(
  `NOME SEGURADO/DEPENDENTE`,
  `CODIGO DO PLANO`,eleg) %>% summarise(Valor = sum(Valor)) %>% filter(
  !is.na(`CODIGO DO PLANO`)) %>% group_by(
    `CODIGO DO PLANO`, eleg) %>% mutate(
      qtde.titulares = n()) %>% summarise(
        valor = sum(Valor),n.pessoas = n()) %>% spread(
          eleg,n.pessoas, fill = 0) %>% group_by(
            `CODIGO DO PLANO`) %>% summarise(dep = sum(DEPENDENTE), 
                                             tit = sum(TITULAR), 
                                             total = tit + dep)

ft9$eleg <- if_else(ft9$`COD. GRAU PARENT.DEP.` == 0,
                    "TITULAR", 
                    "DEPENDENTE") 

ft.fimft9 <-  ft9 %>% group_by(
  `NOME SEGURADO/DEPENDENTE`,
  `CODIGO DO PLANO`,eleg) %>% summarise(Valor = sum(Valor)) %>% filter(
  !is.na(`CODIGO DO PLANO`)) %>% group_by(
    `CODIGO DO PLANO`, eleg) %>% mutate(
      qtde.titulares = n()) %>% summarise(
        valor = sum(Valor),n.pessoas = n()) %>% spread(
          eleg,n.pessoas, fill = 0) %>% group_by(
            `CODIGO DO PLANO`) %>% summarise(dep = sum(DEPENDENTE), 
                                             tit = sum(TITULAR), 
                                             total = tit + dep)

ft10$eleg <- if_else(ft10$`COD. GRAU PARENT.DEP.` == 0,
                     "TITULAR", 
                     "DEPENDENTE") 

ft.fimft10 <-  ft10 %>% group_by(
  `NOME SEGURADO/DEPENDENTE`,
  `CODIGO DO PLANO`,eleg) %>% summarise(Valor = sum(Valor)) %>% filter(
  !is.na(`CODIGO DO PLANO`)) %>% group_by(
    `CODIGO DO PLANO`, eleg) %>% mutate(
      qtde.titulares = n()) %>% summarise(
        valor = sum(Valor),n.pessoas = n()) %>% spread(
          eleg,n.pessoas, fill = 0) %>% group_by(
            `CODIGO DO PLANO`) %>% summarise(dep = sum(DEPENDENTE), 
                                             tit = sum(TITULAR), 
                                             total = tit + dep)

ft11$eleg <- if_else(ft11$`COD. GRAU PARENT.DEP.` == 0,
                     "TITULAR", 
                     "DEPENDENTE") 

ft.fimft11 <-  ft11 %>% group_by(
  `NOME SEGURADO/DEPENDENTE`,
  `CODIGO DO PLANO`,eleg) %>% summarise(Valor = sum(Valor)) %>% filter(
  !is.na(`CODIGO DO PLANO`)) %>% group_by(
    `CODIGO DO PLANO`, eleg) %>% mutate(
      qtde.titulares = n()) %>% summarise(
        valor = sum(Valor),n.pessoas = n()) %>% spread(
          eleg,n.pessoas, fill = 0) %>% group_by(
            `CODIGO DO PLANO`) %>% summarise(dep = sum(DEPENDENTE), 
                                             tit = sum(TITULAR), 
                                             total = tit + dep)

ft12$eleg <- if_else(ft12$`COD. GRAU PARENT.DEP.` == 0,
                     "TITULAR", 
                     "DEPENDENTE") 

ft.fimft12 <-  ft12 %>% group_by(
  `NOME SEGURADO/DEPENDENTE`,
  `CODIGO DO PLANO`,eleg) %>% summarise(Valor = sum(Valor)) %>% filter(
  !is.na(`CODIGO DO PLANO`)) %>% group_by(
    `CODIGO DO PLANO`, eleg) %>% mutate(
      qtde.titulares = n()) %>% summarise(
        valor = sum(Valor),n.pessoas = n()) %>% spread(
          eleg,n.pessoas, fill = 0) %>% group_by(
            `CODIGO DO PLANO`) %>% summarise(dep = sum(DEPENDENTE), 
                                             tit = sum(TITULAR), 
                                             total = tit + dep)

ft13$eleg <- if_else(ft13$`COD. GRAU PARENT.DEP.` == 0,
                     "TITULAR", 
                     "DEPENDENTE") 

ft.fimft13 <-  ft13 %>% group_by(
  `NOME SEGURADO/DEPENDENTE`,
  `CODIGO DO PLANO`,eleg) %>% summarise(Valor = sum(Valor)) %>% filter(
  !is.na(`CODIGO DO PLANO`)) %>% group_by(
    `CODIGO DO PLANO`, eleg) %>% mutate(
      qtde.titulares = n()) %>% summarise(
        valor = sum(Valor),n.pessoas = n()) %>% spread(
          eleg,n.pessoas, fill = 0) %>% group_by(
            `CODIGO DO PLANO`) %>% summarise(dep = sum(DEPENDENTE), 
                                             tit = sum(TITULAR), 
                                             total = tit + dep)



fattec <- ft.fim2 %>% select(`CODIGO DO PLANO`)

fattec$qtde.total <- ft.fim1$total + ft.fim2$total + ft.fimft10$total + ft.fimft11$total +ft.fimft12$total + ft.fimft13$total +ft.fimft3$total + ft.fimft4$total +ft.fimft5$total + ft.fimft6$total +ft.fimft7$total + ft.fimft8$total + ft.fimft9$total
fattec$qtde.media <- round(fattec$qtde.total/12,2)

fwrite(ft.fim1, file = "D:/Users/sb046971/Desktop/res1.txt", sep = "\t")
fwrite(ft.fim2, file = "D:/Users/sb046971/Desktop/res2.txt", sep = "\t")
fwrite(ft.fimft3, file = "D:/Users/sb046971/Desktop/res3.txt", sep = "\t")
fwrite(ft.fimft4, file = "D:/Users/sb046971/Desktop/res4.txt", sep = "\t")
fwrite(ft.fimft5, file = "D:/Users/sb046971/Desktop/res5.txt", sep = "\t")
fwrite(ft.fimft6, file = "D:/Users/sb046971/Desktop/res6.txt", sep = "\t")
fwrite(ft.fimft7, file = "D:/Users/sb046971/Desktop/res7.txt", sep = "\t")
fwrite(ft.fimft8, file = "D:/Users/sb046971/Desktop/res8.txt", sep = "\t")
fwrite(ft.fimft9, file = "D:/Users/sb046971/Desktop/res9.txt", sep = "\t")
fwrite(ft.fimft10, file = "D:/Users/sb046971/Desktop/res10.txt", sep = "\t")
fwrite(ft.fimft11, file = "D:/Users/sb046971/Desktop/res11.txt", sep = "\t")
fwrite(ft.fimft12, file = "D:/Users/sb046971/Desktop/res12.txt", sep = "\t")
fwrite(ft.fimft13, file = "D:/Users/sb046971/Desktop/res13.txt", sep = "\t")
