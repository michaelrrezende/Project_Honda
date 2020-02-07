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

#### CREATE LIST.FILES FROM BIND LINES IN 1 ARCHIVE ####

setwd("D:/Users/sb046971/Documents/Sinistro Unimed/")

mes04.18 <- list.files(pattern = "PP_201804") %>% lapply(
  fread, h = F, sep = NULL) %>% bind_rows()

mes05.18 <- list.files(pattern = "PP_201805") %>% lapply(
  fread, h = F, sep = NULL) %>% bind_rows()

mes05.18$V1 <- str_replace_all(mes05.18$V1, "\"", " ")

mes06.18 <- list.files(pattern = "PP_201806") %>% lapply(
  fread, h = F, sep = NULL) %>% bind_rows()

mes06.18$V1 <- str_replace_all(mes06.18$V1, "\"", " ")

mes07.18 <- list.files(pattern = "PP_201807") %>% lapply(
  fread, h = F, sep = NULL) %>% bind_rows()

mes07.18$V1 <- str_replace_all(mes07.18$V1, "\"", " ")

mes08.18 <- list.files(pattern = "PP_201808") %>% lapply(
  fread, h = F, sep = NULL) %>% bind_rows()

mes08.18$V1 <- str_replace_all(mes08.18$V1, "\"", " ")

mes09.18 <- list.files(pattern = "PP_201809") %>% lapply(
  fread, h = F, sep = NULL) %>% bind_rows()

mes09.18$V1 <- str_replace_all(mes09.18$V1, "\"", " ")

mes10.18 <- list.files(pattern = "PP_201810") %>% lapply(
  fread, h = F, sep = NULL) %>% bind_rows()

mes10.18$V1 <- str_replace_all(mes10.18$V1, "\"", " ")

mes11.18 <- list.files(pattern = "PP_201811") %>% lapply(
  fread, h = F, sep = NULL) %>% bind_rows()

mes11.18$V1 <- str_replace_all(mes11.18$V1, "\"", " ")

mes12.18 <- list.files(pattern = "PP_201812") %>% lapply(
  fread, h = F, sep = NULL) %>% bind_rows()

mes12.18$V1 <- str_replace_all(mes12.18$V1, "\"", " ")

mes01.19 <- list.files(pattern = "PP_201901") %>% lapply(
  fread, h = F, sep = NULL) %>% bind_rows()

mes01.19$V1 <- str_replace_all(mes01.19$V1, "\"", " ")

mes02.19 <- list.files(pattern = "PP_201902") %>% lapply(
  fread, h = F, sep = NULL) %>% bind_rows()

mes02.19$V1 <- str_replace_all(mes02.19$V1, "\"", " ")

mes03.19 <- list.files(pattern = "PP_201903") %>% lapply(
  fread, h = F, sep = NULL) %>% bind_rows()

mes03.19$V1 <- str_replace_all(mes03.19$V1, "\"", " ")

mes04.19 <- list.files(pattern = "PP_201904") %>% lapply(
  fread, h = F, sep = NULL) %>% bind_rows()

mes04.19$V1 <- str_replace_all(mes04.19$V1, "\"", " ")

mes05.19 <- list.files(pattern = "PP_201905") %>% lapply(
  fread, h = F, sep = NULL) %>% bind_rows()

mes05.19$V1 <- str_replace_all(mes05.19$V1, "\"", " ")

mes06.19 <- list.files(pattern = "PP_201906") %>% lapply(
  fread, h = F, sep = NULL) %>% bind_rows()

mes06.19$V1 <- str_replace_all(mes06.19$V1, "\"", " ")

mes07.19 <- list.files(pattern = "PP_201907") %>% lapply(
  fread, h = F, sep = NULL) %>% bind_rows()

mes07.19$V1 <- str_replace_all(mes07.19$V1, "\"", " ")

mes08.19 <- list.files(pattern = "PP_201908") %>% lapply(
  fread, h = F, sep = NULL) %>% bind_rows()

mes08.19$V1 <- str_replace_all(mes08.19$V1, "\"", " ")

mes09.19 <- list.files(pattern = "PP_201909") %>% lapply(
  fread, h = F, sep = NULL) %>% bind_rows()

mes09.19$V1 <- str_replace_all(mes09.19$V1, "\"", " ")

mes10.19 <- list.files(pattern = "PP_201910") %>% lapply(
  fread, h = F, sep = NULL) %>% bind_rows()

mes10.19$V1 <- str_replace_all(mes10.19$V1, "\"", " ")

###### REPLACE SPECIAL CHARACTERS #######

mes04.18$V1 <- str_replace_all(mes04.18$V1, "ª", "a")
mes05.18$V1 <- str_replace_all(mes05.18$V1, "ª", "a")
mes06.18$V1 <- str_replace_all(mes06.18$V1, "ª", "a")
mes07.18$V1 <- str_replace_all(mes07.18$V1, "ª", "a")
mes08.18$V1 <- str_replace_all(mes08.18$V1, "ª", "a")
mes09.18$V1 <- str_replace_all(mes09.18$V1, "ª", "a")
mes10.18$V1 <- str_replace_all(mes10.18$V1, "ª", "a")
mes11.18$V1 <- str_replace_all(mes11.18$V1, "ª", "a")
mes12.18$V1 <- str_replace_all(mes12.18$V1, "ª", "a")
mes01.19$V1 <- str_replace_all(mes01.19$V1, "ª", "a")
mes02.19$V1 <- str_replace_all(mes02.19$V1, "ª", "a")
mes03.19$V1 <- str_replace_all(mes03.19$V1, "ª", "a")
mes04.19$V1 <- str_replace_all(mes04.19$V1, "ª", "a")
mes05.19$V1 <- str_replace_all(mes05.19$V1, "ª", "a")
mes06.19$V1 <- str_replace_all(mes06.19$V1, "ª", "a")
mes07.19$V1 <- str_replace_all(mes07.19$V1, "ª", "a")
mes08.19$V1 <- str_replace_all(mes08.19$V1, "ª", "a")
mes09.19$V1 <- str_replace_all(mes09.19$V1, "ª", "a")
mes10.19$V1 <- str_replace_all(mes10.19$V1, "ª", "a")

mes04.18$V1 <- str_replace_all(mes04.18$V1, "ã", "a")
mes04.18$V1 <- str_replace_all(mes04.18$V1, "à", "a")
mes04.18$V1 <- str_replace_all(mes04.18$V1, "á", "a")
mes04.18$V1 <- str_replace_all(mes04.18$V1, "Á", "A")
mes04.18$V1 <- str_replace_all(mes04.18$V1, "À", "A")
mes04.18$V1 <- str_replace_all(mes04.18$V1, "Ã", "A")
mes04.18$V1 <- str_replace_all(mes04.18$V1, "Ç", "C")
mes04.18$V1 <- str_replace_all(mes04.18$V1, "É", "E")
mes04.18$V1 <- str_replace_all(mes04.18$V1, "È", "E")
mes04.18$V1 <- str_replace_all(mes04.18$V1, "Ó", "O")
mes04.18$V1 <- str_replace_all(mes04.18$V1, "í", "i")
mes04.18$V1 <- str_replace_all(mes04.18$V1, "õ", "o")
mes04.18$V1 <- str_replace_all(mes04.18$V1, "Í", "I")
mes04.18$V1 <- str_replace_all(mes04.18$V1, "Ú", "U")
mes04.18$V1 <- str_replace_all(mes04.18$V1, "ú", "u")
mes04.18$V1 <- str_replace_all(mes04.18$V1, "õ", "o")
mes04.18$V1 <- str_replace_all(mes04.18$V1, "Ô", "O")
mes04.18$V1 <- str_replace_all(mes04.18$V1, "â", "a")
mes04.18$V1 <- str_replace_all(mes04.18$V1, "Â", "A")
mes05.18$V1 <- str_replace_all(mes05.18$V1, "ã", "a")
mes05.18$V1 <- str_replace_all(mes05.18$V1, "à", "a")
mes05.18$V1 <- str_replace_all(mes05.18$V1, "á", "a")
mes05.18$V1 <- str_replace_all(mes05.18$V1, "Á", "A")
mes05.18$V1 <- str_replace_all(mes05.18$V1, "À", "A")
mes05.18$V1 <- str_replace_all(mes05.18$V1, "Ã", "A")
mes05.18$V1 <- str_replace_all(mes05.18$V1, "Ç", "C")
mes05.18$V1 <- str_replace_all(mes05.18$V1, "É", "E")
mes05.18$V1 <- str_replace_all(mes05.18$V1, "È", "E")
mes05.18$V1 <- str_replace_all(mes05.18$V1, "Ó", "O")
mes05.18$V1 <- str_replace_all(mes05.18$V1, "í", "i")
mes05.18$V1 <- str_replace_all(mes05.18$V1, "õ", "o")
mes05.18$V1 <- str_replace_all(mes05.18$V1, "Í", "I")
mes05.18$V1 <- str_replace_all(mes05.18$V1, "Ú", "U")
mes05.18$V1 <- str_replace_all(mes05.18$V1, "ú", "u")
mes05.18$V1 <- str_replace_all(mes05.18$V1, "õ", "o")
mes05.18$V1 <- str_replace_all(mes05.18$V1, "Ô", "O")
mes05.18$V1 <- str_replace_all(mes05.18$V1, "â", "a")
mes05.18$V1 <- str_replace_all(mes05.18$V1, "Â", "A")
mes06.18$V1 <- str_replace_all(mes06.18$V1, "ã", "a")
mes06.18$V1 <- str_replace_all(mes06.18$V1, "à", "a")
mes06.18$V1 <- str_replace_all(mes06.18$V1, "á", "a")
mes06.18$V1 <- str_replace_all(mes06.18$V1, "Á", "A")
mes06.18$V1 <- str_replace_all(mes06.18$V1, "À", "A")
mes06.18$V1 <- str_replace_all(mes06.18$V1, "Ã", "A")
mes06.18$V1 <- str_replace_all(mes06.18$V1, "Ç", "C")
mes06.18$V1 <- str_replace_all(mes06.18$V1, "É", "E")
mes06.18$V1 <- str_replace_all(mes06.18$V1, "È", "E")
mes06.18$V1 <- str_replace_all(mes06.18$V1, "Ó", "O")
mes06.18$V1 <- str_replace_all(mes06.18$V1, "í", "i")
mes06.18$V1 <- str_replace_all(mes06.18$V1, "õ", "o")
mes06.18$V1 <- str_replace_all(mes06.18$V1, "Í", "I")
mes06.18$V1 <- str_replace_all(mes06.18$V1, "Ú", "U")
mes06.18$V1 <- str_replace_all(mes06.18$V1, "ú", "u")
mes06.18$V1 <- str_replace_all(mes06.18$V1, "õ", "o")
mes06.18$V1 <- str_replace_all(mes06.18$V1, "Ô", "O")
mes06.18$V1 <- str_replace_all(mes06.18$V1, "â", "a")
mes06.18$V1 <- str_replace_all(mes06.18$V1, "Â", "A")
mes07.18$V1 <- str_replace_all(mes07.18$V1, "ã", "a")
mes07.18$V1 <- str_replace_all(mes07.18$V1, "à", "a")
mes07.18$V1 <- str_replace_all(mes07.18$V1, "á", "a")
mes07.18$V1 <- str_replace_all(mes07.18$V1, "Á", "A")
mes07.18$V1 <- str_replace_all(mes07.18$V1, "À", "A")
mes07.18$V1 <- str_replace_all(mes07.18$V1, "Ã", "A")
mes07.18$V1 <- str_replace_all(mes07.18$V1, "Ç", "C")
mes07.18$V1 <- str_replace_all(mes07.18$V1, "É", "E")
mes07.18$V1 <- str_replace_all(mes07.18$V1, "È", "E")
mes07.18$V1 <- str_replace_all(mes07.18$V1, "Ó", "O")
mes07.18$V1 <- str_replace_all(mes07.18$V1, "í", "i")
mes07.18$V1 <- str_replace_all(mes07.18$V1, "õ", "o")
mes07.18$V1 <- str_replace_all(mes07.18$V1, "Í", "I")
mes07.18$V1 <- str_replace_all(mes07.18$V1, "Ú", "U")
mes07.18$V1 <- str_replace_all(mes07.18$V1, "ú", "u")
mes07.18$V1 <- str_replace_all(mes07.18$V1, "õ", "o")
mes07.18$V1 <- str_replace_all(mes07.18$V1, "Ô", "O")
mes07.18$V1 <- str_replace_all(mes07.18$V1, "â", "a")
mes07.18$V1 <- str_replace_all(mes07.18$V1, "Â", "A")
mes08.18$V1 <- str_replace_all(mes08.18$V1, "ã", "a")
mes08.18$V1 <- str_replace_all(mes08.18$V1, "à", "a")
mes08.18$V1 <- str_replace_all(mes08.18$V1, "á", "a")
mes08.18$V1 <- str_replace_all(mes08.18$V1, "Á", "A")
mes08.18$V1 <- str_replace_all(mes08.18$V1, "À", "A")
mes08.18$V1 <- str_replace_all(mes08.18$V1, "Ã", "A")
mes08.18$V1 <- str_replace_all(mes08.18$V1, "Ç", "C")
mes08.18$V1 <- str_replace_all(mes08.18$V1, "É", "E")
mes08.18$V1 <- str_replace_all(mes08.18$V1, "È", "E")
mes08.18$V1 <- str_replace_all(mes08.18$V1, "Ó", "O")
mes08.18$V1 <- str_replace_all(mes08.18$V1, "í", "i")
mes08.18$V1 <- str_replace_all(mes08.18$V1, "õ", "o")
mes08.18$V1 <- str_replace_all(mes08.18$V1, "Í", "I")
mes08.18$V1 <- str_replace_all(mes08.18$V1, "Ú", "U")
mes08.18$V1 <- str_replace_all(mes08.18$V1, "ú", "u")
mes08.18$V1 <- str_replace_all(mes08.18$V1, "õ", "o")
mes08.18$V1 <- str_replace_all(mes08.18$V1, "Ô", "O")
mes08.18$V1 <- str_replace_all(mes08.18$V1, "â", "a")
mes08.18$V1 <- str_replace_all(mes08.18$V1, "Â", "A")
mes09.18$V1 <- str_replace_all(mes09.18$V1, "ã", "a")
mes09.18$V1 <- str_replace_all(mes09.18$V1, "à", "a")
mes09.18$V1 <- str_replace_all(mes09.18$V1, "á", "a")
mes09.18$V1 <- str_replace_all(mes09.18$V1, "Á", "A")
mes09.18$V1 <- str_replace_all(mes09.18$V1, "À", "A")
mes09.18$V1 <- str_replace_all(mes09.18$V1, "Ã", "A")
mes09.18$V1 <- str_replace_all(mes09.18$V1, "Ç", "C")
mes09.18$V1 <- str_replace_all(mes09.18$V1, "É", "E")
mes09.18$V1 <- str_replace_all(mes09.18$V1, "È", "E")
mes09.18$V1 <- str_replace_all(mes09.18$V1, "Ó", "O")
mes09.18$V1 <- str_replace_all(mes09.18$V1, "í", "i")
mes09.18$V1 <- str_replace_all(mes09.18$V1, "õ", "o")
mes09.18$V1 <- str_replace_all(mes09.18$V1, "Í", "I")
mes09.18$V1 <- str_replace_all(mes09.18$V1, "Ú", "U")
mes09.18$V1 <- str_replace_all(mes09.18$V1, "ú", "u")
mes09.18$V1 <- str_replace_all(mes09.18$V1, "õ", "o")
mes09.18$V1 <- str_replace_all(mes09.18$V1, "Ô", "O")
mes09.18$V1 <- str_replace_all(mes09.18$V1, "â", "a")
mes09.18$V1 <- str_replace_all(mes09.18$V1, "Â", "A")
mes10.18$V1 <- str_replace_all(mes10.18$V1, "ã", "a")
mes10.18$V1 <- str_replace_all(mes10.18$V1, "à", "a")
mes10.18$V1 <- str_replace_all(mes10.18$V1, "á", "a")
mes10.18$V1 <- str_replace_all(mes10.18$V1, "Á", "A")
mes10.18$V1 <- str_replace_all(mes10.18$V1, "À", "A")
mes10.18$V1 <- str_replace_all(mes10.18$V1, "Ã", "A")
mes10.18$V1 <- str_replace_all(mes10.18$V1, "Ç", "C")
mes10.18$V1 <- str_replace_all(mes10.18$V1, "É", "E")
mes10.18$V1 <- str_replace_all(mes10.18$V1, "È", "E")
mes10.18$V1 <- str_replace_all(mes10.18$V1, "Ó", "O")
mes10.18$V1 <- str_replace_all(mes10.18$V1, "í", "i")
mes10.18$V1 <- str_replace_all(mes10.18$V1, "õ", "o")
mes10.18$V1 <- str_replace_all(mes10.18$V1, "Í", "I")
mes10.18$V1 <- str_replace_all(mes10.18$V1, "Ú", "U")
mes10.18$V1 <- str_replace_all(mes10.18$V1, "ú", "u")
mes10.18$V1 <- str_replace_all(mes10.18$V1, "õ", "o")
mes10.18$V1 <- str_replace_all(mes10.18$V1, "Ô", "O")
mes10.18$V1 <- str_replace_all(mes10.18$V1, "â", "a")
mes10.18$V1 <- str_replace_all(mes10.18$V1, "Â", "A")
mes11.18$V1 <- str_replace_all(mes11.18$V1, "ã", "a")
mes11.18$V1 <- str_replace_all(mes11.18$V1, "à", "a")
mes11.18$V1 <- str_replace_all(mes11.18$V1, "á", "a")
mes11.18$V1 <- str_replace_all(mes11.18$V1, "Á", "A")
mes11.18$V1 <- str_replace_all(mes11.18$V1, "À", "A")
mes11.18$V1 <- str_replace_all(mes11.18$V1, "Ã", "A")
mes11.18$V1 <- str_replace_all(mes11.18$V1, "Ç", "C")
mes11.18$V1 <- str_replace_all(mes11.18$V1, "É", "E")
mes11.18$V1 <- str_replace_all(mes11.18$V1, "È", "E")
mes11.18$V1 <- str_replace_all(mes11.18$V1, "Ó", "O")
mes11.18$V1 <- str_replace_all(mes11.18$V1, "í", "i")
mes11.18$V1 <- str_replace_all(mes11.18$V1, "õ", "o")
mes11.18$V1 <- str_replace_all(mes11.18$V1, "Í", "I")
mes11.18$V1 <- str_replace_all(mes11.18$V1, "Ú", "U")
mes11.18$V1 <- str_replace_all(mes11.18$V1, "ú", "u")
mes11.18$V1 <- str_replace_all(mes11.18$V1, "õ", "o")
mes11.18$V1 <- str_replace_all(mes11.18$V1, "Ô", "O")
mes11.18$V1 <- str_replace_all(mes11.18$V1, "â", "a")
mes11.18$V1 <- str_replace_all(mes11.18$V1, "Â", "A")
mes12.18$V1 <- str_replace_all(mes12.18$V1, "ã", "a")
mes12.18$V1 <- str_replace_all(mes12.18$V1, "à", "a")
mes12.18$V1 <- str_replace_all(mes12.18$V1, "á", "a")
mes12.18$V1 <- str_replace_all(mes12.18$V1, "Á", "A")
mes12.18$V1 <- str_replace_all(mes12.18$V1, "À", "A")
mes12.18$V1 <- str_replace_all(mes12.18$V1, "Ã", "A")
mes12.18$V1 <- str_replace_all(mes12.18$V1, "Ç", "C")
mes12.18$V1 <- str_replace_all(mes12.18$V1, "É", "E")
mes12.18$V1 <- str_replace_all(mes12.18$V1, "È", "E")
mes12.18$V1 <- str_replace_all(mes12.18$V1, "Ó", "O")
mes12.18$V1 <- str_replace_all(mes12.18$V1, "í", "i")
mes12.18$V1 <- str_replace_all(mes12.18$V1, "õ", "o")
mes12.18$V1 <- str_replace_all(mes12.18$V1, "Í", "I")
mes12.18$V1 <- str_replace_all(mes12.18$V1, "Ú", "U")
mes12.18$V1 <- str_replace_all(mes12.18$V1, "ú", "u")
mes12.18$V1 <- str_replace_all(mes12.18$V1, "õ", "o")
mes12.18$V1 <- str_replace_all(mes12.18$V1, "Ô", "O")
mes12.18$V1 <- str_replace_all(mes12.18$V1, "â", "a")
mes12.18$V1 <- str_replace_all(mes12.18$V1, "Â", "A")
mes01.19$V1 <- str_replace_all(mes01.19$V1, "ã", "a")
mes01.19$V1 <- str_replace_all(mes01.19$V1, "à", "a")
mes01.19$V1 <- str_replace_all(mes01.19$V1, "á", "a")
mes01.19$V1 <- str_replace_all(mes01.19$V1, "Á", "A")
mes01.19$V1 <- str_replace_all(mes01.19$V1, "À", "A")
mes01.19$V1 <- str_replace_all(mes01.19$V1, "Ã", "A")
mes01.19$V1 <- str_replace_all(mes01.19$V1, "Ç", "C")
mes01.19$V1 <- str_replace_all(mes01.19$V1, "É", "E")
mes01.19$V1 <- str_replace_all(mes01.19$V1, "È", "E")
mes01.19$V1 <- str_replace_all(mes01.19$V1, "Ó", "O")
mes01.19$V1 <- str_replace_all(mes01.19$V1, "í", "i")
mes01.19$V1 <- str_replace_all(mes01.19$V1, "õ", "o")
mes01.19$V1 <- str_replace_all(mes01.19$V1, "Í", "I")
mes01.19$V1 <- str_replace_all(mes01.19$V1, "Ú", "U")
mes01.19$V1 <- str_replace_all(mes01.19$V1, "ú", "u")
mes01.19$V1 <- str_replace_all(mes01.19$V1, "õ", "o")
mes01.19$V1 <- str_replace_all(mes01.19$V1, "Ô", "O")
mes01.19$V1 <- str_replace_all(mes01.19$V1, "â", "a")
mes01.19$V1 <- str_replace_all(mes01.19$V1, "Â", "A")
mes02.19$V1 <- str_replace_all(mes02.19$V1, "ã", "a")
mes02.19$V1 <- str_replace_all(mes02.19$V1, "à", "a")
mes02.19$V1 <- str_replace_all(mes02.19$V1, "á", "a")
mes02.19$V1 <- str_replace_all(mes02.19$V1, "Á", "A")
mes02.19$V1 <- str_replace_all(mes02.19$V1, "À", "A")
mes02.19$V1 <- str_replace_all(mes02.19$V1, "Ã", "A")
mes02.19$V1 <- str_replace_all(mes02.19$V1, "Ç", "C")
mes02.19$V1 <- str_replace_all(mes02.19$V1, "É", "E")
mes02.19$V1 <- str_replace_all(mes02.19$V1, "È", "E")
mes02.19$V1 <- str_replace_all(mes02.19$V1, "Ó", "O")
mes02.19$V1 <- str_replace_all(mes02.19$V1, "í", "i")
mes02.19$V1 <- str_replace_all(mes02.19$V1, "õ", "o")
mes02.19$V1 <- str_replace_all(mes02.19$V1, "Í", "I")
mes02.19$V1 <- str_replace_all(mes02.19$V1, "Ú", "U")
mes02.19$V1 <- str_replace_all(mes02.19$V1, "ú", "u")
mes02.19$V1 <- str_replace_all(mes02.19$V1, "õ", "o")
mes02.19$V1 <- str_replace_all(mes02.19$V1, "Ô", "O")
mes02.19$V1 <- str_replace_all(mes02.19$V1, "â", "a")
mes02.19$V1 <- str_replace_all(mes02.19$V1, "Â", "A")
mes03.19$V1 <- str_replace_all(mes03.19$V1, "ã", "a")
mes03.19$V1 <- str_replace_all(mes03.19$V1, "à", "a")
mes03.19$V1 <- str_replace_all(mes03.19$V1, "á", "a")
mes03.19$V1 <- str_replace_all(mes03.19$V1, "Á", "A")
mes03.19$V1 <- str_replace_all(mes03.19$V1, "À", "A")
mes03.19$V1 <- str_replace_all(mes03.19$V1, "Ã", "A")
mes03.19$V1 <- str_replace_all(mes03.19$V1, "Ç", "C")
mes03.19$V1 <- str_replace_all(mes03.19$V1, "É", "E")
mes03.19$V1 <- str_replace_all(mes03.19$V1, "È", "E")
mes03.19$V1 <- str_replace_all(mes03.19$V1, "Ó", "O")
mes03.19$V1 <- str_replace_all(mes03.19$V1, "í", "i")
mes03.19$V1 <- str_replace_all(mes03.19$V1, "õ", "o")
mes03.19$V1 <- str_replace_all(mes03.19$V1, "Í", "I")
mes03.19$V1 <- str_replace_all(mes03.19$V1, "Ú", "U")
mes03.19$V1 <- str_replace_all(mes03.19$V1, "ú", "u")
mes03.19$V1 <- str_replace_all(mes03.19$V1, "õ", "o")
mes03.19$V1 <- str_replace_all(mes03.19$V1, "Ô", "O")
mes03.19$V1 <- str_replace_all(mes03.19$V1, "â", "a")
mes03.19$V1 <- str_replace_all(mes03.19$V1, "Â", "A")
mes04.19$V1 <- str_replace_all(mes04.19$V1, "à", "a")
mes04.19$V1 <- str_replace_all(mes04.19$V1, "á", "a")
mes04.19$V1 <- str_replace_all(mes04.19$V1, "Á", "A")
mes04.19$V1 <- str_replace_all(mes04.19$V1, "À", "A")
mes04.19$V1 <- str_replace_all(mes04.19$V1, "Ã", "A")
mes04.19$V1 <- str_replace_all(mes04.19$V1, "Ç", "C")
mes04.19$V1 <- str_replace_all(mes04.19$V1, "É", "E")
mes04.19$V1 <- str_replace_all(mes04.19$V1, "È", "E")
mes04.19$V1 <- str_replace_all(mes04.19$V1, "Ó", "O")
mes04.19$V1 <- str_replace_all(mes04.19$V1, "í", "i")
mes04.19$V1 <- str_replace_all(mes04.19$V1, "õ", "o")
mes04.19$V1 <- str_replace_all(mes04.19$V1, "Í", "I")
mes04.19$V1 <- str_replace_all(mes04.19$V1, "Ú", "U")
mes04.19$V1 <- str_replace_all(mes04.19$V1, "ú", "u")
mes04.19$V1 <- str_replace_all(mes04.19$V1, "õ", "o")
mes04.19$V1 <- str_replace_all(mes04.19$V1, "Ô", "O")
mes04.19$V1 <- str_replace_all(mes04.19$V1, "â", "a")
mes04.19$V1 <- str_replace_all(mes04.19$V1, "Â", "A")
mes05.19$V1 <- str_replace_all(mes05.19$V1, "\"", "")
mes05.19$V1 <- str_replace_all(mes05.19$V1, "ã", "a")
mes05.19$V1 <- str_replace_all(mes05.19$V1, "à", "a")
mes05.19$V1 <- str_replace_all(mes05.19$V1, "á", "a")
mes05.19$V1 <- str_replace_all(mes05.19$V1, "Á", "A")
mes05.19$V1 <- str_replace_all(mes05.19$V1, "À", "A")
mes05.19$V1 <- str_replace_all(mes05.19$V1, "Ã", "A")
mes05.19$V1 <- str_replace_all(mes05.19$V1, "Ç", "C")
mes05.19$V1 <- str_replace_all(mes05.19$V1, "É", "E")
mes05.19$V1 <- str_replace_all(mes05.19$V1, "È", "E")
mes05.19$V1 <- str_replace_all(mes05.19$V1, "Ó", "O")
mes05.19$V1 <- str_replace_all(mes05.19$V1, "í", "i")
mes05.19$V1 <- str_replace_all(mes05.19$V1, "õ", "o")
mes05.19$V1 <- str_replace_all(mes05.19$V1, "Í", "I")
mes05.19$V1 <- str_replace_all(mes05.19$V1, "Ú", "U")
mes05.19$V1 <- str_replace_all(mes05.19$V1, "ú", "u")
mes05.19$V1 <- str_replace_all(mes05.19$V1, "õ", "o")
mes05.19$V1 <- str_replace_all(mes05.19$V1, "Ô", "O")
mes05.19$V1 <- str_replace_all(mes05.19$V1, "â", "a")
mes05.19$V1 <- str_replace_all(mes05.19$V1, "Â", "A")
mes06.19$V1 <- str_replace_all(mes06.19$V1, "\"", "")
mes06.19$V1 <- str_replace_all(mes06.19$V1, "ã", "a")
mes06.19$V1 <- str_replace_all(mes06.19$V1, "à", "a")
mes06.19$V1 <- str_replace_all(mes06.19$V1, "á", "a")
mes06.19$V1 <- str_replace_all(mes06.19$V1, "Á", "A")
mes06.19$V1 <- str_replace_all(mes06.19$V1, "À", "A")
mes06.19$V1 <- str_replace_all(mes06.19$V1, "Ã", "A")
mes06.19$V1 <- str_replace_all(mes06.19$V1, "Ç", "C")
mes06.19$V1 <- str_replace_all(mes06.19$V1, "É", "E")
mes06.19$V1 <- str_replace_all(mes06.19$V1, "È", "E")
mes06.19$V1 <- str_replace_all(mes06.19$V1, "Ó", "O")
mes06.19$V1 <- str_replace_all(mes06.19$V1, "í", "i")
mes06.19$V1 <- str_replace_all(mes06.19$V1, "õ", "o")
mes06.19$V1 <- str_replace_all(mes06.19$V1, "Í", "I")
mes06.19$V1 <- str_replace_all(mes06.19$V1, "Ú", "U")
mes06.19$V1 <- str_replace_all(mes06.19$V1, "ú", "u")
mes06.19$V1 <- str_replace_all(mes06.19$V1, "õ", "o")
mes06.19$V1 <- str_replace_all(mes06.19$V1, "Ô", "O")
mes06.19$V1 <- str_replace_all(mes06.19$V1, "â", "a")
mes06.19$V1 <- str_replace_all(mes06.19$V1, "Â", "A")
mes07.19$V1 <- str_replace_all(mes07.19$V1, "\"", "")
mes07.19$V1 <- str_replace_all(mes07.19$V1, "ã", "a")
mes07.19$V1 <- str_replace_all(mes07.19$V1, "à", "a")
mes07.19$V1 <- str_replace_all(mes07.19$V1, "á", "a")
mes07.19$V1 <- str_replace_all(mes07.19$V1, "Á", "A")
mes07.19$V1 <- str_replace_all(mes07.19$V1, "À", "A")
mes07.19$V1 <- str_replace_all(mes07.19$V1, "Ã", "A")
mes07.19$V1 <- str_replace_all(mes07.19$V1, "Ç", "C")
mes07.19$V1 <- str_replace_all(mes07.19$V1, "É", "E")
mes07.19$V1 <- str_replace_all(mes07.19$V1, "È", "E")
mes07.19$V1 <- str_replace_all(mes07.19$V1, "Ó", "O")
mes07.19$V1 <- str_replace_all(mes07.19$V1, "í", "i")
mes07.19$V1 <- str_replace_all(mes07.19$V1, "õ", "o")
mes07.19$V1 <- str_replace_all(mes07.19$V1, "Í", "I")
mes07.19$V1 <- str_replace_all(mes07.19$V1, "Ú", "U")
mes07.19$V1 <- str_replace_all(mes07.19$V1, "ú", "u")
mes07.19$V1 <- str_replace_all(mes07.19$V1, "õ", "o")
mes07.19$V1 <- str_replace_all(mes07.19$V1, "Ô", "O")
mes07.19$V1 <- str_replace_all(mes07.19$V1, "â", "a")
mes07.19$V1 <- str_replace_all(mes07.19$V1, "Â", "A")
mes08.19$V1 <- str_replace_all(mes08.19$V1, "\"", "")
mes08.19$V1 <- str_replace_all(mes08.19$V1, "ã", "a")
mes08.19$V1 <- str_replace_all(mes08.19$V1, "à", "a")
mes08.19$V1 <- str_replace_all(mes08.19$V1, "á", "a")
mes08.19$V1 <- str_replace_all(mes08.19$V1, "Á", "A")
mes08.19$V1 <- str_replace_all(mes08.19$V1, "À", "A")
mes08.19$V1 <- str_replace_all(mes08.19$V1, "Ã", "A")
mes08.19$V1 <- str_replace_all(mes08.19$V1, "Ç", "C")
mes08.19$V1 <- str_replace_all(mes08.19$V1, "É", "E")
mes08.19$V1 <- str_replace_all(mes08.19$V1, "È", "E")
mes08.19$V1 <- str_replace_all(mes08.19$V1, "Ó", "O")
mes08.19$V1 <- str_replace_all(mes08.19$V1, "í", "i")
mes08.19$V1 <- str_replace_all(mes08.19$V1, "õ", "o")
mes08.19$V1 <- str_replace_all(mes08.19$V1, "Í", "I")
mes08.19$V1 <- str_replace_all(mes08.19$V1, "Ú", "U")
mes08.19$V1 <- str_replace_all(mes08.19$V1, "ú", "u")
mes08.19$V1 <- str_replace_all(mes08.19$V1, "õ", "o")
mes08.19$V1 <- str_replace_all(mes08.19$V1, "Ô", "O")
mes08.19$V1 <- str_replace_all(mes08.19$V1, "â", "a")
mes08.19$V1 <- str_replace_all(mes08.19$V1, "Â", "A")
mes09.19$V1 <- str_replace_all(mes09.19$V1, "\"", "")
mes09.19$V1 <- str_replace_all(mes09.19$V1, "ã", "a")
mes09.19$V1 <- str_replace_all(mes09.19$V1, "à", "a")
mes09.19$V1 <- str_replace_all(mes09.19$V1, "á", "a")
mes09.19$V1 <- str_replace_all(mes09.19$V1, "Á", "A")
mes09.19$V1 <- str_replace_all(mes09.19$V1, "À", "A")
mes09.19$V1 <- str_replace_all(mes09.19$V1, "Ã", "A")
mes09.19$V1 <- str_replace_all(mes09.19$V1, "Ç", "C")
mes09.19$V1 <- str_replace_all(mes09.19$V1, "É", "E")
mes09.19$V1 <- str_replace_all(mes09.19$V1, "È", "E")
mes09.19$V1 <- str_replace_all(mes09.19$V1, "Ó", "O")
mes09.19$V1 <- str_replace_all(mes09.19$V1, "í", "i")
mes09.19$V1 <- str_replace_all(mes09.19$V1, "õ", "o")
mes09.19$V1 <- str_replace_all(mes09.19$V1, "Í", "I")
mes09.19$V1 <- str_replace_all(mes09.19$V1, "Ú", "U")
mes09.19$V1 <- str_replace_all(mes09.19$V1, "ú", "u")
mes09.19$V1 <- str_replace_all(mes09.19$V1, "õ", "o")
mes09.19$V1 <- str_replace_all(mes09.19$V1, "Ô", "O")
mes09.19$V1 <- str_replace_all(mes09.19$V1, "â", "a")
mes09.19$V1 <- str_replace_all(mes09.19$V1, "Â", "A")
mes10.19$V1 <- str_replace_all(mes10.19$V1, "\"", "")
mes10.19$V1 <- str_replace_all(mes10.19$V1, "ã", "a")
mes10.19$V1 <- str_replace_all(mes10.19$V1, "à", "a")
mes10.19$V1 <- str_replace_all(mes10.19$V1, "á", "a")
mes10.19$V1 <- str_replace_all(mes10.19$V1, "Á", "A")
mes10.19$V1 <- str_replace_all(mes10.19$V1, "À", "A")
mes10.19$V1 <- str_replace_all(mes10.19$V1, "Ã", "A")
mes10.19$V1 <- str_replace_all(mes10.19$V1, "Ç", "C")
mes10.19$V1 <- str_replace_all(mes10.19$V1, "É", "E")
mes10.19$V1 <- str_replace_all(mes10.19$V1, "È", "E")
mes10.19$V1 <- str_replace_all(mes10.19$V1, "Ó", "O")
mes10.19$V1 <- str_replace_all(mes10.19$V1, "í", "i")
mes10.19$V1 <- str_replace_all(mes10.19$V1, "õ", "o")
mes10.19$V1 <- str_replace_all(mes10.19$V1, "Í", "I")
mes10.19$V1 <- str_replace_all(mes10.19$V1, "Ú", "U")
mes10.19$V1 <- str_replace_all(mes10.19$V1, "ú", "u")
mes10.19$V1 <- str_replace_all(mes10.19$V1, "õ", "o")
mes10.19$V1 <- str_replace_all(mes10.19$V1, "Ô", "O")
mes10.19$V1 <- str_replace_all(mes10.19$V1, "â", "a")
mes10.19$V1 <- str_replace_all(mes10.19$V1, "Â", "A")

##### WRITE ARCHIVES #####


fwrite(mes04.18, file = "D:/Users/sb046971/Documents/Sinistro Unimed/201804_PP.txt",
       quote = F)
fwrite(mes05.18, file = "D:/Users/sb046971/Documents/Sinistro Unimed/201805_PP.txt",
       quote = F)
fwrite(mes06.18, file = "D:/Users/sb046971/Documents/Sinistro Unimed/201806_PP.txt",
       quote = F)
fwrite(mes07.18, file = "D:/Users/sb046971/Documents/Sinistro Unimed/201807_PP.txt",
       quote = F)
fwrite(mes08.18, file = "D:/Users/sb046971/Documents/Sinistro Unimed/201808_PP.txt",
       quote = F)
fwrite(mes09.18, file = "D:/Users/sb046971/Documents/Sinistro Unimed/201809_PP.txt",
       quote = F)
fwrite(mes10.18, file = "D:/Users/sb046971/Documents/Sinistro Unimed/201810_PP.txt",
       quote = F)
fwrite(mes11.18, file = "D:/Users/sb046971/Documents/Sinistro Unimed/201811_PP.txt", 
       quote = F)
fwrite(mes12.18, file = "D:/Users/sb046971/Documents/Sinistro Unimed/201812_PP.txt", 
       quote = F)
fwrite(mes01.19, file = "D:/Users/sb046971/Documents/Sinistro Unimed/201901_PP.txt", 
       quote = F)
fwrite(mes02.19, file = "D:/Users/sb046971/Documents/Sinistro Unimed/201902_PP.txt", 
       quote = F)
fwrite(mes03.19, file = "D:/Users/sb046971/Documents/Sinistro Unimed/201903_PP.txt",
       quote = F)
fwrite(mes04.19, file = "D:/Users/sb046971/Documents/Sinistro Unimed/201904_PP.txt",
       quote = F)
fwrite(mes05.19, file = "D:/Users/sb046971/Documents/Sinistro Unimed/201905_PP.txt",
       quote = F)
fwrite(mes06.19, file = "D:/Users/sb046971/Documents/Sinistro Unimed/201906_PP.txt", 
       quote = F)
fwrite(mes07.19, file = "D:/Users/sb046971/Documents/Sinistro Unimed/201907_PP.txt", 
       quote = F)
fwrite(mes08.19, file = "D:/Users/sb046971/Documents/Sinistro Unimed/201908_PP.txt", 
       quote = F)
fwrite(mes09.19, file = "D:/Users/sb046971/Documents/Sinistro Unimed/201909_PP.txt", 
       quote = F)
fwrite(mes10.19, file = "D:/Users/sb046971/Documents/Sinistro Unimed/201910_PP.txt", 
       quote = F)

#### WIDTHS AND NAMES LAYOUT BRADESCO ####

widths = c(6,50,14,20,17,50,17,50,6,8,8,15,10,6,14,12,100,8,
           11,14,10,6,60,6,8,8,10,10,50,1,8,6,6,5,10,20,15,
           10,14,10,10,8,50,50,8,1)

names = c("Competencia","Nome do Contratado","CGC contratado",
          "Centro de Custo","Código do titular","Nome do titular",
          "Código do paciente","Nome do paciente","Parentesco",
          "Data apresentação-prod med","Data de atendimento",
          "Identificador nota intercâmbio","Identificador da produção médica",
          "Tipo da produção médica","Número da solicitação","Código do serviço",
          "Descrição do serviço","Quantidade",
          "Qtd ch","Valor","Taxa Administrativa","Tipo de cobrança",
          "Módulos Contratados","RESERVADO","Data de internação",
          "Data de alta","Código do solicitante","Código do executante",
          "Nome do executante","Indicador de acidente de trab",
          "Data de exclusão","Código do local de atend",
          "Classificação do local atend","Porcentagem honorário",
          "Identificador Especialidade med","Matrícula","Número do Período",
          "Código do Contrato","Valor copart",
          "Indicador do Espelho","SubTipo Prod. Médica Espelho","Tipo de diária",
          "Unimed do prestador do atend","Local do atendimento","Data Nascimento","Sexo")

#### READ MONTHS TREATMENT ####

mes0518 <-readr::read_fwf("d:/Users/sb046971/Documents/Sinistro Unimed/201805_PP.txt",
  fwf_widths(widths, col_names = names), skip_empty_rows = T,skip = 1,
  col_types = cols("Competencia" = col_character(),
                   "Centro de Custo" = col_character(),
                   "CGC contratado" = col_character(),
                   "Código do titular" = col_character(), 
                   "Código do paciente" = col_character(),
                   "Data apresentação-prod med" = col_character(),
                   "Data de atendimento" = col_character(),
                   "Identificador nota intercâmbio" = col_character(),
                   "Identificador da produção médica" = col_character(),
                   "Indicador de acidente de trab" = col_character(),
                   "Número da solicitação" = col_character(),
                   "Código do serviço" = col_character(),
                   "Quantidade" = col_integer(),
                   "Matrícula" = col_character(),
                   "Qtd ch" = col_integer(),
                   "Valor" = col_integer(),
                   "Taxa Administrativa" = col_integer(),
                   "Data de internação" = col_character(),
                   "Data de alta" = col_character(),
                   "Código do solicitante" = col_character(),
                   "Código do executante" = col_character(),
                   "Data de exclusão" = col_character(),
                   "Código do Contrato" = col_character(),
                   "Valor copart" = col_integer(),
                   "Data Nascimento" = col_character(),
                   "Tipo de diária" = col_character()))

mes0518$`Quantidade` <- mes0518$`Quantidade`/100
mes0518$`Qtd ch` <- mes0518$`Qtd ch`/100
mes0518$`Valor` <- mes0518$`Valor`/100
mes0518$`Taxa Administrativa` <- mes0518$`Taxa Administrativa`/100000
mes0518$`Valor copart` <- mes0518$`Valor copart`/100

mes0618 <- readr:: read_fwf("d:/Users/sb046971/Documents/Sinistro Unimed/201806_PP.txt",
  fwf_widths(widths, col_names = names), skip_empty_rows = T,skip = 1,
  col_types = cols("Competencia" = col_character(),
                   "Centro de Custo" = col_character(),
                   "CGC contratado" = col_character(),
                   "Código do titular" = col_character(), 
                   "Código do paciente" = col_character(),
                   "Data apresentação-prod med" = col_character(),
                   "Data de atendimento" = col_character(),
                   "Identificador nota intercâmbio" = col_character(),
                   "Identificador da produção médica" = col_character(),    
                   "Indicador de acidente de trab" = col_character(),
                   "Número da solicitação" = col_character(),
                   "Código do serviço" = col_character(),
                   "Quantidade" = col_integer(),               
                   "Matrícula" = col_character(),
                   "Qtd ch" = col_integer(),
                   "Valor" = col_integer(),
                   "Taxa Administrativa" = col_integer(),
                   "Data de internação" = col_character(),
                   "Data de alta" = col_character(),
                   "Código do solicitante" = col_character(),
                   "Código do executante" = col_character(),
                   "Data de exclusão" = col_character(),
                   "Código do Contrato" = col_character(),
                   "Valor copart" = col_integer(),   
                   "Data Nascimento" = col_character(),    
                   "Tipo de diária" = col_character()))

mes0618$`Quantidade` <- mes0618$`Quantidade`/100
mes0618$`Qtd ch` <- mes0618$`Qtd ch`/100
mes0618$`Valor` <- mes0618$`Valor`/100
mes0618$`Taxa Administrativa` <- mes0618$`Taxa Administrativa`/100000
mes0618$`Valor copart` <- mes0618$`Valor copart`/100

mes0718 <- readr:: read_fwf("d:/Users/sb046971/Documents/Sinistro Unimed/201807_PP.txt",
  fwf_widths(widths, col_names = names), skip_empty_rows = T,skip = 1,
  col_types = cols("Competencia" = col_character(),          
                   "Centro de Custo" = col_character(),
                   "CGC contratado" = col_character(),
                   "Código do titular" = col_character(), 
                   "Código do paciente" = col_character(),
                   "Data apresentação-prod med" = col_character(),
                   "Data de atendimento" = col_character(),
                   "Identificador nota intercâmbio" = col_character(),
                   "Identificador da produção médica" = col_character(),  
                   "Indicador de acidente de trab" = col_character(),
                   "Número da solicitação" = col_character(),
                   "Código do serviço" = col_character(),
                   "Quantidade" = col_integer(),          
                   "Matrícula" = col_character(),
                   "Qtd ch" = col_integer(),
                   "Valor" = col_integer(),
                   "Taxa Administrativa" = col_integer(),
                   "Data de internação" = col_character(),
                   "Data de alta" = col_character(),
                   "Código do solicitante" = col_character(),
                   "Código do executante" = col_character(),
                   "Data de exclusão" = col_character(),
                   "Código do Contrato" = col_character(),
                   "Valor copart" = col_integer(),  
                   "Data Nascimento" = col_character(),         
                   "Tipo de diária" = col_character()))

mes0718$`Quantidade` <- mes0718$`Quantidade`/100
mes0718$`Qtd ch` <- mes0718$`Qtd ch`/100
mes0718$`Valor` <- mes0718$`Valor`/100
mes0718$`Taxa Administrativa` <- mes0718$`Taxa Administrativa`/100000
mes0718$`Valor copart` <- mes0718$`Valor copart`/100

mes0818 <- readr:: read_fwf("d:/Users/sb046971/Documents/Sinistro Unimed/201808_PP.txt",
  fwf_widths(widths, col_names = names), skip_empty_rows = T,skip = 1,
  col_types = cols("Competencia" = col_character(),              
                   "Centro de Custo" = col_character(),
                   "CGC contratado" = col_character(),
                   "Código do titular" = col_character(), 
                   "Código do paciente" = col_character(),
                   "Data apresentação-prod med" = col_character(),
                   "Data de atendimento" = col_character(),
                   "Identificador nota intercâmbio" = col_character(),
                   "Identificador da produção médica" = col_character(),    
                   "Indicador de acidente de trab" = col_character(),
                   "Número da solicitação" = col_character(),
                   "Código do serviço" = col_character(),
                   "Quantidade" = col_integer(),           
                   "Matrícula" = col_character(),
                   "Qtd ch" = col_integer(),
                   "Valor" = col_integer(),
                   "Taxa Administrativa" = col_integer(),
                   "Data de internação" = col_character(),
                   "Data de alta" = col_character(),
                   "Código do solicitante" = col_character(),
                   "Código do executante" = col_character(),
                   "Data de exclusão" = col_character(),
                   "Código do Contrato" = col_character(),
                   "Valor copart" = col_integer(),      
                   "Data Nascimento" = col_character(),          
                   "Tipo de diária" = col_character()))

mes0818$`Quantidade` <- mes0818$`Quantidade`/100
mes0818$`Qtd ch` <- mes0818$`Qtd ch`/100
mes0818$`Valor` <- mes0818$`Valor`/100
mes0818$`Taxa Administrativa` <- mes0818$`Taxa Administrativa`/100000
mes0818$`Valor copart` <- mes0818$`Valor copart`/100

mes0918 <- readr:: read_fwf("d:/Users/sb046971/Documents/Sinistro Unimed/201809_PP.txt",
  fwf_widths(widths, col_names = names), skip_empty_rows = T,skip = 1,
  col_types = cols("Competencia" = col_character(),               
                   "Centro de Custo" = col_character(),
                   "CGC contratado" = col_character(),
                   "Código do titular" = col_character(), 
                   "Código do paciente" = col_character(),
                   "Data apresentação-prod med" = col_character(),
                   "Data de atendimento" = col_character(),
                   "Identificador nota intercâmbio" = col_character(),
                   "Identificador da produção médica" = col_character(),      
                   "Indicador de acidente de trab" = col_character(),
                   "Número da solicitação" = col_character(),
                   "Código do serviço" = col_character(),
                   "Quantidade" = col_integer(),               
                   "Matrícula" = col_character(),
                   "Qtd ch" = col_integer(),
                   "Valor" = col_integer(),
                   "Taxa Administrativa" = col_integer(),
                   "Data de internação" = col_character(),
                   "Data de alta" = col_character(),
                   "Código do solicitante" = col_character(),
                   "Código do executante" = col_character(),
                   "Data de exclusão" = col_character(),
                   "Código do Contrato" = col_character(),
                   "Valor copart" = col_integer(),     
                   "Data Nascimento" = col_character(),             
                   "Tipo de diária" = col_character()))

mes0918$`Quantidade` <- mes0918$`Quantidade`/100
mes0918$`Qtd ch` <- mes0918$`Qtd ch`/100
mes0918$`Valor` <- mes0918$`Valor`/100
mes0918$`Taxa Administrativa` <- mes0918$`Taxa Administrativa`/100000
mes0918$`Valor copart` <- mes0918$`Valor copart`/100

mes1018 <- readr:: read_fwf("d:/Users/sb046971/Documents/Sinistro Unimed/201810_PP.txt",
  fwf_widths(widths, col_names = names), skip_empty_rows = T,skip = 1,
  col_types = cols("Competencia" = col_character(),               
                   "Centro de Custo" = col_character(),
                   "CGC contratado" = col_character(),
                   "Código do titular" = col_character(), 
                   "Código do paciente" = col_character(),
                   "Data apresentação-prod med" = col_character(),
                   "Data de atendimento" = col_character(),
                   "Identificador nota intercâmbio" = col_character(),
                   "Identificador da produção médica" = col_character(),      
                   "Indicador de acidente de trab" = col_character(),
                   "Número da solicitação" = col_character(),
                   "Código do serviço" = col_character(),
                   "Quantidade" = col_integer(),                
                   "Matrícula" = col_character(),
                   "Qtd ch" = col_integer(),
                   "Valor" = col_integer(),
                   "Taxa Administrativa" = col_integer(),
                   "Data de internação" = col_character(),
                   "Data de alta" = col_character(),
                   "Código do solicitante" = col_character(),
                   "Código do executante" = col_character(),
                   "Data de exclusão" = col_character(),
                   "Código do Contrato" = col_character(),
                   "Valor copart" = col_integer(),    
                   "Data Nascimento" = col_character(),
                   "Tipo de diária" = col_character()))

mes1018$`Quantidade` <- mes1018$`Quantidade`/100
mes1018$`Qtd ch` <- mes1018$`Qtd ch`/100
mes1018$`Valor` <- mes1018$`Valor`/100
mes1018$`Taxa Administrativa` <- mes1018$`Taxa Administrativa`/100000
mes1018$`Valor copart` <- mes1018$`Valor copart`/100

mes1118 <- readr:: read_fwf("d:/Users/sb046971/Documents/Sinistro Unimed/201811_PP.txt",
  fwf_widths(widths, col_names = names), skip_empty_rows = T,skip = 1,
  col_types = cols("Competencia" = col_character(),               
                   "Centro de Custo" = col_character(),
                   "CGC contratado" = col_character(),
                   "Código do titular" = col_character(), 
                   "Código do paciente" = col_character(),
                   "Data apresentação-prod med" = col_character(),
                   "Data de atendimento" = col_character(),
                   "Identificador nota intercâmbio" = col_character(),
                   "Identificador da produção médica" = col_character(),    
                   "Indicador de acidente de trab" = col_character(),
                   "Número da solicitação" = col_character(),
                   "Código do serviço" = col_character(),
                   "Quantidade" = col_integer(),              
                   "Matrícula" = col_character(),
                   "Qtd ch" = col_integer(),
                   "Valor" = col_integer(),
                   "Taxa Administrativa" = col_integer(),
                   "Data de internação" = col_character(),
                   "Data de alta" = col_character(),
                   "Código do solicitante" = col_character(),
                   "Código do executante" = col_character(),
                   "Data de exclusão" = col_character(),
                   "Código do Contrato" = col_character(),
                   "Valor copart" = col_integer(),   
                   "Data Nascimento" = col_character(),       
                   "Tipo de diária" = col_character()))

mes1118$`Quantidade` <- mes1118$`Quantidade`/100
mes1118$`Qtd ch` <- mes1118$`Qtd ch`/100
mes1118$`Valor` <- mes1118$`Valor`/100
mes1118$`Taxa Administrativa` <- mes1118$`Taxa Administrativa`/100000
mes1118$`Valor copart` <- mes1118$`Valor copart`/100

mes1218 <- readr:: read_fwf("D:/Users/sb046971/Documents/Sinistro Unimed/201812_PP.txt",
  fwf_widths(widths, col_names = names), skip_empty_rows = T,skip = 1,
  col_types = cols("Competencia" = col_character(),              
                   "Centro de Custo" = col_character(),
                   "CGC contratado" = col_character(),
                   "Código do titular" = col_character(), 
                   "Código do paciente" = col_character(),
                   "Data apresentação-prod med" = col_character(),
                   "Data de atendimento" = col_character(),
                   "Identificador nota intercâmbio" = col_character(),
                   "Identificador da produção médica" = col_character(),   
                   "Indicador de acidente de trab" = col_character(),
                   "Número da solicitação" = col_character(),
                   "Código do serviço" = col_character(),
                   "Quantidade" = col_integer(),             
                   "Matrícula" = col_character(),
                   "Qtd ch" = col_integer(),
                   "Valor" = col_integer(),
                   "Taxa Administrativa" = col_integer(),
                   "Data de internação" = col_character(),
                   "Data de alta" = col_character(),
                   "Código do solicitante" = col_character(),
                   "Código do executante" = col_character(),
                   "Data de exclusão" = col_character(),
                   "Código do Contrato" = col_character(),
                   "Valor copart" = col_integer(), 
                   "Data Nascimento" = col_character(),      
                   "Tipo de diária" = col_character()))

mes1218$`Quantidade` <- mes1218$`Quantidade`/100
mes1218$`Qtd ch` <- mes1218$`Qtd ch`/100
mes1218$`Valor` <- mes1218$`Valor`/100
mes1218$`Taxa Administrativa` <- mes1218$`Taxa Administrativa`/100000
mes1218$`Valor copart` <- mes1218$`Valor copart`/100

mes0119 <- readr:: read_fwf("d:/Users/sb046971/Documents/Sinistro Unimed/201901_PP.txt",
  fwf_widths(widths, col_names = names), skip_empty_rows = T,skip = 1,
  col_types = cols("Competencia" = col_character(),              
                   "Centro de Custo" = col_character(),
                   "CGC contratado" = col_character(),
                   "Código do titular" = col_character(), 
                   "Código do paciente" = col_character(),
                   "Data apresentação-prod med" = col_character(),
                   "Data de atendimento" = col_character(),
                   "Identificador nota intercâmbio" = col_character(),
                   "Identificador da produção médica" = col_character(),    
                   "Indicador de acidente de trab" = col_character(),
                   "Número da solicitação" = col_character(),
                   "Código do serviço" = col_character(),
                   "Quantidade" = col_integer(),             
                   "Matrícula" = col_character(),
                   "Qtd ch" = col_integer(),
                   "Valor" = col_integer(),
                   "Taxa Administrativa" = col_integer(),
                   "Data de internação" = col_character(),
                   "Data de alta" = col_character(),
                   "Código do solicitante" = col_character(),
                   "Código do executante" = col_character(),
                   "Data de exclusão" = col_character(),
                   "Código do Contrato" = col_character(),
                   "Valor copart" = col_integer(), 
                   "Data Nascimento" = col_character(),     
                   "Tipo de diária" = col_character()))

mes0119$`Quantidade` <- mes0119$`Quantidade`/100
mes0119$`Qtd ch` <- mes0119$`Qtd ch`/100
mes0119$`Valor` <- mes0119$`Valor`/100
mes0119$`Taxa Administrativa` <- mes0119$`Taxa Administrativa`/100000
mes0119$`Valor copart` <- mes0119$`Valor copart`/100

mes0219 <- readr:: read_fwf("D:/Users/sb046971/Documents/Sinistro Unimed/201902_PP.txt",
  fwf_widths(widths, col_names = names), skip_empty_rows = T,skip = 1,
  col_types = cols("Competencia" = col_character(),             
                   "Centro de Custo" = col_character(),
                   "CGC contratado" = col_character(),
                   "Código do titular" = col_character(), 
                   "Código do paciente" = col_character(),
                   "Data apresentação-prod med" = col_character(),
                   "Data de atendimento" = col_character(),
                   "Identificador nota intercâmbio" = col_character(),
                   "Identificador da produção médica" = col_character(),       
                   "Indicador de acidente de trab" = col_character(),
                   "Número da solicitação" = col_character(),
                   "Código do serviço" = col_character(),
                   "Quantidade" = col_integer(),                
                   "Matrícula" = col_character(),
                   "Qtd ch" = col_integer(),
                   "Valor" = col_integer(),
                   "Taxa Administrativa" = col_integer(),
                   "Data de internação" = col_character(),
                   "Data de alta" = col_character(),
                   "Código do solicitante" = col_character(),
                   "Código do executante" = col_character(),
                   "Data de exclusão" = col_character(),
                   "Código do Contrato" = col_character(),
                   "Valor copart" = col_integer(),    
                   "Data Nascimento" = col_character(),       
                   "Tipo de diária" = col_character()))

mes0219$`Quantidade` <- mes0219$`Quantidade`/100
mes0219$`Qtd ch` <- mes0219$`Qtd ch`/100
mes0219$`Valor` <- mes0219$`Valor`/100
mes0219$`Taxa Administrativa` <- mes0219$`Taxa Administrativa`/100000
mes0219$`Valor copart` <- mes0219$`Valor copart`/100

mes0319 <- readr:: read_fwf("d:/Users/sb046971/Documents/Sinistro Unimed/201903_PP.txt",
  fwf_widths(widths, col_names = names), skip_empty_rows = T,skip = 1,
  col_types = cols("Competencia" = col_character(),             
                   "Centro de Custo" = col_character(),
                   "CGC contratado" = col_character(),
                   "Código do titular" = col_character(), 
                   "Código do paciente" = col_character(),
                   "Data apresentação-prod med" = col_character(),
                   "Data de atendimento" = col_character(),
                   "Identificador nota intercâmbio" = col_character(),
                   "Identificador da produção médica" = col_character(),      
                   "Indicador de acidente de trab" = col_character(),
                   "Número da solicitação" = col_character(),
                   "Código do serviço" = col_character(),
                   "Quantidade" = col_integer(),             
                   "Matrícula" = col_character(),
                   "Qtd ch" = col_integer(),
                   "Valor" = col_integer(),
                   "Taxa Administrativa" = col_integer(),
                   "Data de internação" = col_character(),
                   "Data de alta" = col_character(),
                   "Código do solicitante" = col_character(),
                   "Código do executante" = col_character(),
                   "Data de exclusão" = col_character(),
                   "Código do Contrato" = col_character(),
                   "Valor copart" = col_integer(),  
                   "Data Nascimento" = col_character(),          
                   "Tipo de diária" = col_character()))

mes0319$`Quantidade` <- mes0319$`Quantidade`/100
mes0319$`Qtd ch` <- mes0319$`Qtd ch`/100
mes0319$`Valor` <- mes0319$`Valor`/100
mes0319$`Taxa Administrativa` <- mes0319$`Taxa Administrativa`/100000
mes0319$`Valor copart` <- mes0319$`Valor copart`/100

mes0419 <- readr:: read_fwf(
  file ="D:/Users/sb046971/Documents/Sinistro Unimed/201904_PP.txt",
  locale = readr::locale(encoding = "latin1"),
  fwf_widths(widths, col_names = names), skip_empty_rows = T,skip = 1,
  col_types = cols("Competencia" = col_character(),             
                   "Centro de Custo" = col_character(),
                   "CGC contratado" = col_character(),
                   "Código do titular" = col_character(), 
                   "Código do paciente" = col_character(),
                   "Data apresentação-prod med" = col_character(),
                   "Data de atendimento" = col_character(),
                   "Identificador nota intercâmbio" = col_character(),
                   "Identificador da produção médica" = col_character(),   
                   "Indicador de acidente de trab" = col_character(),
                   "Número da solicitação" = col_character(),
                   "Código do serviço" = col_character(),
                   "Quantidade" = col_integer(),             
                   "Matrícula" = col_character(),
                   "Qtd ch" = col_integer(),
                   "Valor" = col_integer(),
                   "Taxa Administrativa" = col_integer(),
                   "Data de internação" = col_character(),
                   "Data de alta" = col_character(),
                   "Código do solicitante" = col_character(),
                   "Código do executante" = col_character(),
                   "Data de exclusão" = col_character(),
                   "Código do Contrato" = col_character(),
                   "Valor copart" = col_integer(),   
                   "Data Nascimento" = col_character(),       
                   "Tipo de diária" = col_character()))

mes0419$`Quantidade` <- mes0419$`Quantidade`/100
mes0419$`Qtd ch` <- mes0419$`Qtd ch`/100
mes0419$`Valor` <- mes0419$`Valor`/100
mes0419$`Taxa Administrativa` <- mes0419$`Taxa Administrativa`/100000
mes0419$`Valor copart` <- mes0419$`Valor copart`/100

mes0519 <- readr:: read_fwf(
  file ="D:/Users/sb046971/Documents/Sinistro Unimed/201905_PP.txt",
  fwf_widths(widths, col_names = names), skip_empty_rows = T,skip = 1,
  col_types = cols("Competencia" = col_character(),            
                   "Centro de Custo" = col_character(),
                   "CGC contratado" = col_character(),
                   "Código do titular" = col_character(), 
                   "Código do paciente" = col_character(),
                   "Data apresentação-prod med" = col_character(),
                   "Data de atendimento" = col_character(),
                   "Identificador nota intercâmbio" = col_character(),
                   "Identificador da produção médica" = col_character(),     
                   "Indicador de acidente de trab" = col_character(),
                   "Número da solicitação" = col_character(),
                   "Código do serviço" = col_character(),
                   "Quantidade" = col_integer(),              
                   "Matrícula" = col_character(),
                   "Qtd ch" = col_integer(),
                   "Valor" = col_integer(),
                   "Taxa Administrativa" = col_integer(),
                   "Data de internação" = col_character(),
                   "Data de alta" = col_character(),
                   "Código do solicitante" = col_character(),
                   "Código do executante" = col_character(),
                   "Data de exclusão" = col_character(),
                   "Código do Contrato" = col_character(),
                   "Valor copart" = col_integer(),   
                   "Data Nascimento" = col_character(),      
                   "Tipo de diária" = col_character()))

mes0519$`Quantidade` <- mes0519$`Quantidade`/100
mes0519$`Qtd ch` <- mes0519$`Qtd ch`/100
mes0519$`Valor` <- mes0519$`Valor`/100
mes0519$`Taxa Administrativa` <- mes0519$`Taxa Administrativa`/100000
mes0519$`Valor copart` <- mes0519$`Valor copart`/100

mes0619 <- readr:: read_fwf(
  file ="D:/Users/sb046971/Documents/Sinistro Unimed/201906_PP.txt",
  fwf_widths(widths, col_names = names), skip_empty_rows = T,skip = 1,
  col_types = cols("Competencia" = col_character(),                 
                   "Centro de Custo" = col_character(),
                   "CGC contratado" = col_character(),
                   "Código do titular" = col_character(), 
                   "Código do paciente" = col_character(),
                   "Data apresentação-prod med" = col_character(),
                   "Data de atendimento" = col_character(),
                   "Identificador nota intercâmbio" = col_character(),
                   "Identificador da produção médica" = col_character(),    
                   "Indicador de acidente de trab" = col_character(),
                   "Número da solicitação" = col_character(),
                   "Código do serviço" = col_character(),
                   "Quantidade" = col_integer(),             
                   "Matrícula" = col_character(),
                   "Qtd ch" = col_integer(),
                   "Valor" = col_integer(),
                   "Taxa Administrativa" = col_integer(),
                   "Data de internação" = col_character(),
                   "Data de alta" = col_character(),
                   "Código do solicitante" = col_character(),
                   "Código do executante" = col_character(),
                   "Data de exclusão" = col_character(),
                   "Código do Contrato" = col_character(),
                   "Valor copart" = col_integer(),   
                   "Data Nascimento" = col_character(),     
                   "Tipo de diária" = col_character()))

mes0619$`Quantidade` <- mes0619$`Quantidade`/100
mes0619$`Qtd ch` <- mes0619$`Qtd ch`/100
mes0619$`Valor` <- mes0619$`Valor`/100
mes0619$`Taxa Administrativa` <- mes0619$`Taxa Administrativa`/100000
mes0619$`Valor copart` <- mes0619$`Valor copart`/100

mes0719 <- readr:: read_fwf(
  file ="D:/Users/sb046971/Documents/Sinistro Unimed/201907_PP.txt",
  fwf_widths(widths, col_names = names), skip_empty_rows = T,skip = 1,
  col_types = cols("Competencia" = col_character(),                 
                   "Centro de Custo" = col_character(),
                   "CGC contratado" = col_character(),
                   "Código do titular" = col_character(), 
                   "Código do paciente" = col_character(),
                   "Data apresentação-prod med" = col_character(),
                   "Data de atendimento" = col_character(),
                   "Identificador nota intercâmbio" = col_character(),
                   "Identificador da produção médica" = col_character(),    
                   "Indicador de acidente de trab" = col_character(),
                   "Número da solicitação" = col_character(),
                   "Código do serviço" = col_character(),
                   "Quantidade" = col_integer(),             
                   "Matrícula" = col_character(),
                   "Qtd ch" = col_integer(),
                   "Valor" = col_integer(),
                   "Taxa Administrativa" = col_integer(),
                   "Data de internação" = col_character(),
                   "Data de alta" = col_character(),
                   "Código do solicitante" = col_character(),
                   "Código do executante" = col_character(),
                   "Data de exclusão" = col_character(),
                   "Código do Contrato" = col_character(),
                   "Valor copart" = col_integer(),   
                   "Data Nascimento" = col_character(),     
                   "Tipo de diária" = col_character()))

mes0719$`Quantidade` <- mes0719$`Quantidade`/100
mes0719$`Qtd ch` <- mes0719$`Qtd ch`/100
mes0719$`Valor` <- mes0719$`Valor`/100
mes0719$`Taxa Administrativa` <- mes0719$`Taxa Administrativa`/100000
mes0719$`Valor copart` <- mes0719$`Valor copart`/100

mes0819 <- readr:: read_fwf(
  file ="D:/Users/sb046971/Documents/Sinistro Unimed/201908_PP.txt",
  fwf_widths(widths, col_names = names), skip_empty_rows = T,skip = 1,
  col_types = cols("Competencia" = col_character(),                 
                   "Centro de Custo" = col_character(),
                   "CGC contratado" = col_character(),
                   "Código do titular" = col_character(), 
                   "Código do paciente" = col_character(),
                   "Data apresentação-prod med" = col_character(),
                   "Data de atendimento" = col_character(),
                   "Identificador nota intercâmbio" = col_character(),
                   "Identificador da produção médica" = col_character(),    
                   "Indicador de acidente de trab" = col_character(),
                   "Número da solicitação" = col_character(),
                   "Código do serviço" = col_character(),
                   "Quantidade" = col_integer(),             
                   "Matrícula" = col_character(),
                   "Qtd ch" = col_integer(),
                   "Valor" = col_integer(),
                   "Taxa Administrativa" = col_integer(),
                   "Data de internação" = col_character(),
                   "Data de alta" = col_character(),
                   "Código do solicitante" = col_character(),
                   "Código do executante" = col_character(),
                   "Data de exclusão" = col_character(),
                   "Código do Contrato" = col_character(),
                   "Valor copart" = col_integer(),   
                   "Data Nascimento" = col_character(),     
                   "Tipo de diária" = col_character()))

mes0819$`Quantidade` <- mes0819$`Quantidade`/100
mes0819$`Qtd ch` <- mes0819$`Qtd ch`/100
mes0819$`Valor` <- mes0819$`Valor`/100
mes0819$`Taxa Administrativa` <- mes0819$`Taxa Administrativa`/100000
mes0819$`Valor copart` <- mes0819$`Valor copart`/100

mes0919 <- readr:: read_fwf(
  file ="D:/Users/sb046971/Documents/Sinistro Unimed/201909_PP.txt",
  fwf_widths(widths, col_names = names), skip_empty_rows = T,skip = 1,
  col_types = cols("Competencia" = col_character(),                 
                   "Centro de Custo" = col_character(),
                   "CGC contratado" = col_character(),
                   "Código do titular" = col_character(), 
                   "Código do paciente" = col_character(),
                   "Data apresentação-prod med" = col_character(),
                   "Data de atendimento" = col_character(),
                   "Identificador nota intercâmbio" = col_character(),
                   "Identificador da produção médica" = col_character(),    
                   "Indicador de acidente de trab" = col_character(),
                   "Número da solicitação" = col_character(),
                   "Código do serviço" = col_character(),
                   "Quantidade" = col_integer(),             
                   "Matrícula" = col_character(),
                   "Qtd ch" = col_integer(),
                   "Valor" = col_integer(),
                   "Taxa Administrativa" = col_integer(),
                   "Data de internação" = col_character(),
                   "Data de alta" = col_character(),
                   "Código do solicitante" = col_character(),
                   "Código do executante" = col_character(),
                   "Data de exclusão" = col_character(),
                   "Código do Contrato" = col_character(),
                   "Valor copart" = col_integer(),   
                   "Data Nascimento" = col_character(),     
                   "Tipo de diária" = col_character()))

mes0919$`Quantidade` <- mes0919$`Quantidade`/100
mes0919$`Qtd ch` <- mes0919$`Qtd ch`/100
mes0919$`Valor` <- mes0919$`Valor`/100
mes0919$`Taxa Administrativa` <- mes0919$`Taxa Administrativa`/100000
mes0919$`Valor copart` <- mes0919$`Valor copart`/100

mes1019 <- readr:: read_fwf(
  file ="D:/Users/sb046971/Documents/Sinistro Unimed/201910_PP.txt",
  fwf_widths(widths, col_names = names), skip_empty_rows = T,skip = 1,
  col_types = cols("Competencia" = col_character(),                 
                   "Centro de Custo" = col_character(),
                   "CGC contratado" = col_character(),
                   "Código do titular" = col_character(), 
                   "Código do paciente" = col_character(),
                   "Data apresentação-prod med" = col_character(),
                   "Data de atendimento" = col_character(),
                   "Identificador nota intercâmbio" = col_character(),
                   "Identificador da produção médica" = col_character(),    
                   "Indicador de acidente de trab" = col_character(),
                   "Número da solicitação" = col_character(),
                   "Código do serviço" = col_character(),
                   "Quantidade" = col_integer(),             
                   "Matrícula" = col_character(),
                   "Qtd ch" = col_integer(),
                   "Valor" = col_integer(),
                   "Taxa Administrativa" = col_integer(),
                   "Data de internação" = col_character(),
                   "Data de alta" = col_character(),
                   "Código do solicitante" = col_character(),
                   "Código do executante" = col_character(),
                   "Data de exclusão" = col_character(),
                   "Código do Contrato" = col_character(),
                   "Valor copart" = col_integer(),   
                   "Data Nascimento" = col_character(),     
                   "Tipo de diária" = col_character()))

mes1019$`Quantidade` <- mes1019$`Quantidade`/100
mes1019$`Qtd ch` <- mes1019$`Qtd ch`/100
mes1019$`Valor` <- mes1019$`Valor`/100
mes1019$`Taxa Administrativa` <- mes1019$`Taxa Administrativa`/100000
mes1019$`Valor copart` <- mes1019$`Valor copart`/100

#### BIND DATABASE ALL MONTHS ####

unimed_consolidado <- bind_rows(mes0518,mes0618,mes0718,mes0818,mes0918,mes1018,mes1118,
                                mes1218,mes0119,mes0219,mes0319,mes0419,mes0519,mes0619,
                                mes0719,mes0819,mes0919,mes1019)


unimed_consolidado$DUPLICADOS <- duplicated(unimed_consolidado)

sum(unimed_consolidado$Valor)

base_unimed <- unimed_consolidado %>% select(-`Nome do Contratado`,-`CGC contratado`,
                                             -`Módulos Contratados`,-RESERVADO,
                                             -`Número do Período`,-`Valor copart`,-`Qtd ch`,
                                             -`Tipo de cobrança`,-`Taxa Administrativa`,
                                             -`Indicador de acidente de trab`,
                                             -`Identificador nota intercâmbio`)

#### TREATMENT DATABASE DATES ####

base_unimed$`Data de atendimento` <- as.Date(base_unimed$`Data de atendimento`,"%d%m%Y")

base_unimed$`Data apresentação-prod med` <- as.Date(
  base_unimed$`Data apresentação-prod med`,"%d%m%Y")

base_unimed$`Data de exclusão` <- as.Date(base_unimed$`Data de exclusão`,"%d%m%Y")

base_unimed$`Data de internação` <- as.Date(base_unimed$`Data de internação`,"%d%m%Y")

base_unimed$`Data de alta` <- as.Date(base_unimed$`Data de alta`,"%d%m%Y")

base_unimed$`Data Nascimento` <- as.Date(base_unimed$`Data Nascimento`,"%d%m%Y")

base_unimed$Comp <- substr(base_unimed$Competencia,start = 1, stop = 2)
base_unimed$Comp2 <- substr(base_unimed$Competencia,start = 3, stop = 6)
base_unimed$Competencia2 <- paste(base_unimed$Comp,"/",base_unimed$Comp2, sep = "")
base_unimed$Comp <- NULL
base_unimed$Comp2 <- NULL

#### TREATMENT DATABASE ####

names(base_unimed)[names(
  base_unimed) == "Porcentagem honorário"] <- "Porcentagem honorário (%)"

base_unimed$`Porcentagem honorário (%)` <- as.numeric(
  base_unimed$`Porcentagem honorário (%)`)

base_unimed$`Porcentagem honorário (%)` <- base_unimed$`Porcentagem honorário (%)`/100

base_unimed$`Identificador Especialidade med` <- as.numeric(
  base_unimed$`Identificador Especialidade med`)

base_unimed$`Código do Contrato` <- as.numeric(base_unimed$`Código do Contrato`)

base_unimed$`Tipo da produção médica` <- factor(base_unimed$`Tipo da produção médica`,
                                                label = c("CONSULTA","DIARIA",
                                                          "GABARITO","HONORARIO",
                                                          "MATERIAL","MEDICAMENTO",
                                                          "PGT DIVIDA","REEMBOLSO",
                                                          "RESSARC. SUS","SERV. COMPL",
                                                          "TAXA"), 
                                                levels = c("CONSUL","DIARIA","GABARI",
                                                "HONORA","MATERI","MEDIC","PGTDIV",
                                                "REEMB","RESSUS","SERCOM","TAXA"))

#### ANALYSIS IN DATA ####

sum(base_unimed$Valor)


base_unimed$diaEvxPg <- difftime(base_unimed$`Data apresentação-prod med`,
                                 base_unimed$`Data de atendimento`,units = "days")

base_unimed$diaEvxPg <- as.numeric(base_unimed$diaEvxPg)

base_unimed$mesEvxPg <-floor((as.double(base_unimed$diaEvxPg)/365)*12)

base_unimed$flag12meses <- if_else(base_unimed$mesEvxPg > 11,"+","0")

base_unimed$flag4meses <- if_else(base_unimed$diaEvxPg > 120,"+","0")

base_unimed$flag6meses <- if_else(base_unimed$diaEvxPg > 180,"+","0")

table(base_unimed$flag4meses)

table(base_unimed$flag6meses)

base_unimed %>% filter(`Tipo da produção médica` != "RESSARC. SUS" & 
                         flag12meses == "+") %>% group_by(.) %>% summarise(sum(Valor))

base_unimed %>% filter(flag4meses == "+") %>% group_by(.) %>% summarise(sum(Valor))

base_unimed %>% filter(flag6meses == "+") %>% group_by(.) %>% summarise(sum(Valor))

analysis1 <- base_unimed %>% group_by(`Código do paciente`,`Nome do executante`
                                      ) %>% summarise(
                                        med_dias = mean(diaEvxPg),
                                        med_meses = mean(mesEvxPg),
                                        valor_sinistro = sum(Valor))

analysis2 <- base_unimed %>% group_by(`Nome do executante`,mesEvxPg) %>% summarise(
  valor_sinistro = sum(Valor))

analysis3 <- base_unimed %>% group_by(`Nome do executante`) %>% summarise(
  med_dias = mean(diaEvxPg),
  med_meses = mean(mesEvxPg),
  valor_sinistro = sum(Valor))

analysis4 <- base_unimed %>% group_by(`Código do serviço`,diaEvxPg) %>% summarise(
  cont_proc = sum(Quantidade)) 

analysis4$flag <- if_else(analysis4$diaEvxPg > 120, "+","0")

analysis5 <- analysis4 %>% filter(`Código do serviço` == "10101012" & 
                                      flag == "+") %>% group_by(
                                        `Código do serviço`) %>% summarise(
                                          qtde_proc = sum(cont_proc))

analysis6 <- analysis4 %>% filter(flag == "+") %>% group_by(
  `Código do serviço`) %>% summarise(qtde_proc = sum(cont_proc))

analysis7 <- base_unimed %>% group_by(`Código do serviço`,diaEvxPg) %>% summarise(
  cont_proc = n())

analysis7$flag <- if_else(analysis7$diaEvxPg > 120, "+","0")

analysis8 <- analysis7 %>% filter(`Código do serviço` == "10101012" & 
                                      flag == "+") %>% group_by(
                                        `Código do serviço`) %>% summarise(
                                          qtde_proc = sum(cont_proc))

analysis9 <- analysis7 %>% filter(flag == "+") %>% group_by(
  `Código do serviço`) %>% summarise(qtde_proc = sum(cont_proc))

analysis10 <- base_unimed %>% select(`Nome do executante`,
                                     diaEvxPg,mesEvxPg,
                                     `VALOR PAGO`) %>% filter(diaEvxPg > 120)

sum(analysis10$`VALOR PAGO`)

analysis11 <- base_unimed %>% select(`Nome do executante`,
                                     diaEvxPg,mesEvxPg,
                                     `VALOR PAGO`)

analysis12 <- teste %>% filter(`Data apresentação-prod med` > "01/05/18",
                               `Data apresentação-prod med` <= "31/06/18") %>% filter(
                                 `Data de atendimento` < "31/12/18")

sum(analysis12$Valor)

cost.p.person <- base_unimed %>% group_by(`Nome do paciente`) %>% summarise(
  valor = sum(Valor))

analysis13 <- base_unimed %>% filter(
  `Código do serviço` == "10101039") %>% group_by(
    `Nome do paciente`) %>% summarise(qt_cons = sum(Quantidade),
                                      valor = sum(Valor))

analysis14 <- base_unimed %>% filter(
  `Código do serviço` == "10101012") %>% group_by(
    `Nome do paciente`) %>% summarise(qt_cons = sum(Quantidade),
                                      valor = sum(Valor))

analysis15 <- base_unimed %>% filter(
  `Código do serviço` == "10106146") %>% group_by(
    `Nome do paciente`) %>% summarise(qt_cons = sum(Quantidade),
                                      valor = sum(Valor))

analysis16 <- base_unimed %>% group_by(Competencia,`Código do Contrato`,
                                       `Nome do titular`,
                                       `Nome do paciente`,
                                       `Tipo da produção médica`,
                                       `Código do serviço`) %>% summarise(
                                         valor_gasto = sum(Valor))

fwrite(analysis16, file = "D:/Users/sb046971/Documents/HighUsers_Unimed.csv",
       sep = ";", dec = ",")

fwrite(base_unimed, file = "D:/Users/sb046971/Documents/base_unimed.csv",
       sep = "|", dec = ",")

# lcapP <- base_unimed %>% filter(`Nome do titular` == "PAULO ISSAO SAITO")
# 
# fwrite(lcapP, file = "D:/Users/sb046971/Documents/lcap - P.csv", dec = ",", sep = "|")

# lcapMED <- base_unimed %>% filter(`Nome do paciente` == "MARIA EDUARDA DEO")
# 
# fwrite(lcapMED, file = "D:/Users/sb046971/Documents/lcap - MED.csv", dec = ",", sep = "|")
# 
# lcapMLO <- base_unimed %>% filter(`Nome do paciente` == "MIGUEL LIMA DE OLIVEIRA")
# 
# fwrite(lcapMLO, file = "D:/Users/sb046971/Documents/lcap - MLO.csv", dec = ",", sep = "|")

analysis17 <- base_unimed %>% filter(grepl('DOMICILIAR',`Descrição do serviço`))

analysis18 <- analysis17 %>% group_by(`Nome do titular`,
                                      `Nome do paciente`) %>% summarise(valor = sum(`Valor`))

fwrite(analysis17, file = "D:/Users/sb046971/Documents/HighUsers-HC.csv", sep = "|",dec = ",")

analysis19 <- base_unimed %>% filter(grepl('2019',
                                           Competencia)) %>% group_by(
                                             `Nome do titular`,
                                             `Nome do paciente`) %>% summarise(
                                               valor = sum(`Valor`))
