#### PACKAGES ####

require(dplyr)
require(data.table)
require(readr)

#### DATA JOIN ####

`2w` <- fread(file = "D:/Users/sb046971/Documents/Projeto - Trava - Carlos/2w.csv", 
              colClasses = c("CNPJ LIMPO" = "character"))

`2w`$Tipo <- "2w"

`4w` <- fread(file = "D:/Users/sb046971/Documents/Projeto - Trava - Carlos/4w.csv", 
              colClasses = c("CNPJ LIMPO" = "character"))

`4w`$Tipo <- "4w"

sap <- fread(file = "D:/Users/sb046971/Documents/Projeto - Trava - Carlos/SAP.csv", 
             colClasses = c("CNPJ LIMPO" = "character"))

finder2w <- left_join(`2w`,sap,by = "CNPJ LIMPO")

finder4w <- left_join(`4w`,sap, by = "CNPJ LIMPO")

fwrite(finder2w, file = "D:/Users/sb046971/Documents/Projeto - Trava - Carlos/finds2w.csv")

fwrite(finder4w, file = "D:/Users/sb046971/Documents/Projeto - Trava - Carlos/finds4w.csv")
