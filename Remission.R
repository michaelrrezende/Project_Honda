#### PACKAGES ####

require(dplyr)
require(data.table)
require(readxl)

#### READ ARCHIVES BRADESCO ####

setwd("D:/Users/sb046971/Documents/Remissao/Bradesco/2018/")

#### mes012018 #### 

mes0118 <- read_excel("Fatura técnica - 01.2018.xlsx")

mes0118$`VALOR DO LANCAMENTO` <- gsub("[.]", "",mes0118$`VALOR DO LANCAMENTO`)
mes0118$`VALOR DO LANCAMENTO` <- gsub(",", ".",mes0118$`VALOR DO LANCAMENTO`)

mes0118$`VALOR DO LANCAMENTO` <- as.numeric(mes0118$`VALOR DO LANCAMENTO`)

mes0118$competencia <- "01/2018"

#### mes022018 ####

mes0218 <- read_excel("Fatura técnica - 02.2018.xlsx")

mes0218$`VALOR DO LANCAMENTO` <- gsub("[.]", "",mes0218$`VALOR DO LANCAMENTO`)
mes0218$`VALOR DO LANCAMENTO` <- gsub(",", ".",mes0218$`VALOR DO LANCAMENTO`)

mes0218$`VALOR DO LANCAMENTO` <- as.numeric(mes0218$`VALOR DO LANCAMENTO`)

mes0218$competencia <- "02/2018"

#### mes032018 ####

mes0318 <- read_excel("Fatura técnica - 03.2018.xlsx")

mes0318$`VALOR DO LANCAMENTO` <- gsub("[.]", "",mes0318$`VALOR DO LANCAMENTO`)
mes0318$`VALOR DO LANCAMENTO` <- gsub(",", ".",mes0318$`VALOR DO LANCAMENTO`)

mes0318$`VALOR DO LANCAMENTO` <- as.numeric(mes0318$`VALOR DO LANCAMENTO`)

mes0318$competencia <- "03/2018"

#### mes042018 ####

mes0418 <- read_excel("Fatura técnica - 04.2018.xlsx")

mes0418$`VALOR DO LANCAMENTO` <- gsub("[.]", "",mes0418$`VALOR DO LANCAMENTO`)
mes0418$`VALOR DO LANCAMENTO` <- gsub(",", ".",mes0418$`VALOR DO LANCAMENTO`)

mes0418$`VALOR DO LANCAMENTO` <- as.numeric(mes0418$`VALOR DO LANCAMENTO`)

mes0418$competencia <- "04/2018"

#### mes052018 ####

mes0518 <- read_excel("Fatura técnica - 05.2018.xlsx")

mes0518$`VALOR DO LANCAMENTO` <- gsub("[.]", "",mes0518$`VALOR DO LANCAMENTO`)
mes0518$`VALOR DO LANCAMENTO` <- gsub(",", ".",mes0518$`VALOR DO LANCAMENTO`)

mes0518$`VALOR DO LANCAMENTO` <- as.numeric(mes0518$`VALOR DO LANCAMENTO`)

mes0518$competencia <- "05/2018"

#### mes062018 ####

mes0618 <- read_excel("Fatura técnica - 06.2018.xlsx")

mes0618$`VALOR DO LANCAMENTO` <- gsub("[.]", "",mes0618$`VALOR DO LANCAMENTO`)
mes0618$`VALOR DO LANCAMENTO` <- gsub(",", ".",mes0618$`VALOR DO LANCAMENTO`)

mes0618$`VALOR DO LANCAMENTO` <- as.numeric(mes0618$`VALOR DO LANCAMENTO`)

mes0618$competencia <- "06/2018"

#### mes072018 ####

mes0718 <- read_excel("Fatura técnica - 07.2018.xlsx")

mes0718$`VALOR DO LANCAMENTO` <- gsub("[.]", "",mes0718$`VALOR DO LANCAMENTO`)
mes0718$`VALOR DO LANCAMENTO` <- gsub(",", ".",mes0718$`VALOR DO LANCAMENTO`)

mes0718$`VALOR DO LANCAMENTO` <- as.numeric(mes0718$`VALOR DO LANCAMENTO`)

mes0718$competencia <- "07/2018"

#### mes082018 ####

mes0818 <- read_excel("Fatura técnica - 08.2018.xlsx")

mes0818$`VALOR DO LANCAMENTO` <- gsub("[.]", "",mes0818$`VALOR DO LANCAMENTO`)
mes0818$`VALOR DO LANCAMENTO` <- gsub(",", ".",mes0818$`VALOR DO LANCAMENTO`)

mes0818$`VALOR DO LANCAMENTO` <- as.numeric(mes0818$`VALOR DO LANCAMENTO`)

mes0818$competencia <- "08/2018"

#### mes092018 ####

mes0918 <- read_excel("Fatura técnica - 09.2018.xlsx")

mes0918$`VALOR DO LANCAMENTO` <- gsub("[.]", "",mes0918$`VALOR DO LANCAMENTO`)
mes0918$`VALOR DO LANCAMENTO` <- gsub(",", ".",mes0918$`VALOR DO LANCAMENTO`)

mes0918$`VALOR DO LANCAMENTO` <- as.numeric(mes0918$`VALOR DO LANCAMENTO`)

mes0918$competencia <- "09/2018"

#### mes102018 ####

mes1018 <- read_excel("Fatura técnica - 10.2018.xlsx")

mes1018$`VALOR DO LANCAMENTO` <- gsub("[.]", "",mes1018$`VALOR DO LANCAMENTO`)
mes1018$`VALOR DO LANCAMENTO` <- gsub(",", ".",mes1018$`VALOR DO LANCAMENTO`)

mes1018$`VALOR DO LANCAMENTO` <- as.numeric(mes1018$`VALOR DO LANCAMENTO`)

mes1018$competencia <- "10/2018"

#### mes112018 ####

mes1118 <- read_excel("Fatura técnica - 11.2018.xlsx")

mes1118$`VALOR DO LANCAMENTO` <- gsub("[.]", "",mes1118$`VALOR DO LANCAMENTO`)
mes1118$`VALOR DO LANCAMENTO` <- gsub(",", ".",mes1118$`VALOR DO LANCAMENTO`)

mes1118$`VALOR DO LANCAMENTO` <- as.numeric(mes1118$`VALOR DO LANCAMENTO`)

mes1118$competencia <- "11/2018"

#### mes122018 ####

mes1218 <- read_excel("Fatura técnica - 12.2018.xlsx")

mes1218$`VALOR DO LANCAMENTO` <- gsub("[.]", "",mes1218$`VALOR DO LANCAMENTO`)
mes1218$`VALOR DO LANCAMENTO` <- gsub(",", ".",mes1218$`VALOR DO LANCAMENTO`)

mes1218$`VALOR DO LANCAMENTO` <- as.numeric(mes1218$`VALOR DO LANCAMENTO`)

mes1218$competencia <- "12/2018"

#### CONSOLIDATED 2018 ####

ano2018 <- bind_rows(mes0118,mes0218,mes0318,mes0418,mes0518,mes0618,
                     mes0718,mes0818,mes0918,mes1018,mes1118,mes1218)

colnames(ano2018)[16] <- "Remissao"

dynamics <- ano2018 %>% group_by(.) %>% summarise(vlr_remissao = sum(Remissao), 
                                                  vlr_total = sum(`VALOR DO LANCAMENTO`))

#### READ ARCHIVES ####

setwd("D:/Users/sb046971/Documents/Remissao/Bradesco/2019/")

#### mes012019 ####

mes0119 <- read_excel("Fatura técnica - 01.2019.xlsx")

mes0119$`VALOR DO LANCAMENTO` <- gsub("[.]", "",mes0119$`VALOR DO LANCAMENTO`)
mes0119$`VALOR DO LANCAMENTO` <- gsub(",", ".",mes0119$`VALOR DO LANCAMENTO`)

mes0119$`VALOR DO LANCAMENTO` <- as.numeric(mes0119$`VALOR DO LANCAMENTO`)

mes0119$competencia <- "01/2019"

#### mes022019 ####

mes0219 <- read_excel("Fatura técnica - 02.2019.xlsx")

mes0219$`VALOR DO LANCAMENTO` <- gsub("[.]", "",mes0219$`VALOR DO LANCAMENTO`)
mes0219$`VALOR DO LANCAMENTO` <- gsub(",", ".",mes0219$`VALOR DO LANCAMENTO`)

mes0219$`VALOR DO LANCAMENTO` <- as.numeric(mes0219$`VALOR DO LANCAMENTO`)

mes0219$competencia <- "02/2019"

#### mes032019 ####

mes0319 <- read_excel("Fatura técnica - 03.2019.xlsx")

mes0319$`VALOR DO LANCAMENTO` <- gsub("[.]", "",mes0319$`VALOR DO LANCAMENTO`)
mes0319$`VALOR DO LANCAMENTO` <- gsub(",", ".",mes0319$`VALOR DO LANCAMENTO`)

mes0319$`VALOR DO LANCAMENTO` <- as.numeric(mes0319$`VALOR DO LANCAMENTO`)

mes0319$competencia <- "03/2019"

#### mes042019 ####

mes0419 <- read_excel("Fatura técnica - 04.2019.xlsx")

mes0419$`VALOR DO LANCAMENTO` <- gsub("[.]", "",mes0419$`VALOR DO LANCAMENTO`)
mes0419$`VALOR DO LANCAMENTO` <- gsub(",", ".",mes0419$`VALOR DO LANCAMENTO`)

mes0419$`VALOR DO LANCAMENTO` <- as.numeric(mes0419$`VALOR DO LANCAMENTO`)

mes0419$competencia <- "04/2019"

#### mes052019 ####

mes0519 <- read_excel("Fatura técnica - 05.2019.xlsx")

mes0519$`VALOR DO LANCAMENTO` <- gsub("[.]", "",mes0519$`VALOR DO LANCAMENTO`)
mes0519$`VALOR DO LANCAMENTO` <- gsub(",", ".",mes0519$`VALOR DO LANCAMENTO`)

mes0519$`VALOR DO LANCAMENTO` <- as.numeric(mes0519$`VALOR DO LANCAMENTO`)

mes0519$competencia <- "05/2019"

#### mes062019 ####

mes0619 <- read_excel("Fatura técnica - 06.2019.xlsx")

mes0619$`VALOR DO LANCAMENTO` <- gsub("[.]", "",mes0619$`VALOR DO LANCAMENTO`)
mes0619$`VALOR DO LANCAMENTO` <- gsub(",", ".",mes0619$`VALOR DO LANCAMENTO`)

mes0619$`VALOR DO LANCAMENTO` <- as.numeric(mes0619$`VALOR DO LANCAMENTO`)

mes0619$competencia <- "06/2019"

#### mes072019 ####

mes0719 <- read_excel("Fatura técnica - 07.2019.xlsx")

mes0719$`VALOR DO LANCAMENTO` <- gsub("[.]", "",mes0719$`VALOR DO LANCAMENTO`)
mes0719$`VALOR DO LANCAMENTO` <- gsub(",", ".",mes0719$`VALOR DO LANCAMENTO`)

mes0719$`VALOR DO LANCAMENTO` <- as.numeric(mes0719$`VALOR DO LANCAMENTO`)

mes0719$competencia <- "07/2019"

#### mes082019 ####

mes0819 <- read_excel("Fatura técnica - 08.2019.xlsx")

mes0819$`VALOR DO LANCAMENTO` <- gsub("[.]", "",mes0819$`VALOR DO LANCAMENTO`)
mes0819$`VALOR DO LANCAMENTO` <- gsub(",", ".",mes0819$`VALOR DO LANCAMENTO`)

mes0819$`VALOR DO LANCAMENTO` <- as.numeric(mes0819$`VALOR DO LANCAMENTO`)

mes0819$competencia <- "08/2019"

#### mes092019 ####

mes0919 <- read_excel("Fatura técnica - 09.2019.xlsx")

mes0919$`VALOR DO LANCAMENTO` <- gsub("[.]", "",mes0919$`VALOR DO LANCAMENTO`)
mes0919$`VALOR DO LANCAMENTO` <- gsub(",", ".",mes0919$`VALOR DO LANCAMENTO`)

mes0919$`VALOR DO LANCAMENTO` <- as.numeric(mes0919$`VALOR DO LANCAMENTO`)

mes0919$competencia <- "09/2019"

#### mes102019 ####

mes1019 <- read_excel("Fatura técnica - 10.2019.xlsx")

mes1019$`VALOR DO LANCAMENTO` <- gsub("[.]", "",mes1019$`VALOR DO LANCAMENTO`)
mes1019$`VALOR DO LANCAMENTO` <- gsub(",", ".",mes1019$`VALOR DO LANCAMENTO`)

mes1019$`VALOR DO LANCAMENTO` <- as.numeric(mes1019$`VALOR DO LANCAMENTO`)

mes1019$competencia <- "10/2019"

#### mes112019 ####

mes1119 <- read_excel("Fatura técnica - 11.2019.xlsx")

mes1119$`VALOR DO LANCAMENTO` <- gsub("[.]", "",mes1119$`VALOR DO LANCAMENTO`)
mes1119$`VALOR DO LANCAMENTO` <- gsub(",", ".",mes1119$`VALOR DO LANCAMENTO`)

mes1119$`VALOR DO LANCAMENTO` <- as.numeric(mes1119$`VALOR DO LANCAMENTO`)

mes1119$competencia <- "11/2019"

#### mes122019 ####

mes1219 <- read_excel("Fatura técnica - 12.2019.xlsx")

mes1219$`VALOR DO LANCAMENTO` <- gsub("[.]", "",mes1219$`VALOR DO LANCAMENTO`)
mes1219$`VALOR DO LANCAMENTO` <- gsub(",", ".",mes1219$`VALOR DO LANCAMENTO`)

mes1219$`VALOR DO LANCAMENTO` <- as.numeric(mes1219$`VALOR DO LANCAMENTO`)

mes1219$competencia <- "12/2019"

#### CONSOLIDATED 2019 ####

ano2019 <- bind_rows(mes0119,mes0219,mes0319,mes0419,mes0519,mes0619,
                     mes0719,mes0819,mes0919,mes1019,mes1119,mes1219)

colnames(ano2019)[16] <- "Remissao"

dynamics <- ano2018 %>% group_by(.) %>% summarise(vlr_remissao = sum(Remissao), 
                                                   vlr_total = sum(`VALOR DO LANCAMENTO`))

dynamics2 <- ano2019 %>% group_by(.) %>% summarise(vlr_remissao = sum(Remissao), 
                                                   vlr_total = sum(`VALOR DO LANCAMENTO`))

dynamics$ano <- "2018"
dynamics2$ano <- "2019"

dynamic <- bind_rows(dynamics,dynamics2)

dynamic %>% group_by(.) %>% summarise(vlr_remissao,vlr_total)

#### per month claim remission ####

x <- bradesco_consolidado %>% filter(
  `PLANO DO SEGURADO` == "QNR6") %>% group_by(COMPETENCIA) %>% summarise(
    vlr = sum(`VALOR DO SINISTRO`))
