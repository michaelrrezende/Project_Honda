#### PACKAGES ####

require(dplyr)
require(data.table)

############# BIND ARQUIVOS DE SEGURO DE VIDA ######

setwd("D:/Users/sb046971/OneDrive - Honda/Documentos/Tokio Bind/Tábua biométrica 032021/")

base <- list.files(pattern = ".csv") %>%
  lapply(fread,dec = ",",colClasses = c("ESTITULANTE" = "character",
                                        "SUB_ESTITULANTE" = "character",
                                        "REGISTRO" = "character",
                                        "NOME_PESSOA" = "character",
                                        "DATA_NASC" = "character","CPF" = "character",
                                        "CAPITAL SEGURO" = "numeric","PREMIO" = "numeric",
                                        "DESPESAS MED E ODONTO" = "numeric",
                                        "PREMIO DESP MED E ODONTO" = "numeric",
                                        "DATA_ADMISSAO" = "character",
                                        "DATA_INI_VIGENCIA" = "character",
                                        "SEXO" = "character","ESTADO_CIVIL" = "character"),
         stringsAsFactors=F, encoding="UTF-8", sep = ";",
         select=c("ESTITULANTE","SUB_ESTITULANTE","REGISTRO","NOME_PESSOA","DATA_NASC",
                  "CPF","CAPITAL SEGURO","PREMIO","DESPESAS MED E ODONTO",
                  "PREMIO DESP MED E ODONTO","DATA_ADMISSAO","DATA_INI_VIGENCIA",
                  "SEXO","ESTADO_CIVIL"))  %>% bind_rows

fwrite(base,file = "d:/Users/sb046971/OneDrive - Honda/Documentos/Tokio Bind/TÁBUA BIOMÉTRICA 032021/BASEFINAL.CSV",
       sep = "|", dec = ",")

base$DATA_ADMISSAO <- as.Date(base$DATA_ADMISSAO, "%Y%m%d")
base$DATA_NASC <- as.Date(base$DATA_NASC, "%Y%m%d")
base$DATA_INI_VIGENCIA <- as.Date(base$DATA_INI_VIGENCIA, "%Y%m%d")

base$NIVEIS <- paste(base$ESTITULANTE,base$SUB_ESTITULANTE)
base$NIVEIS

base$NIVEIS <- NULL

plan1 <- base %>% filter(ESTITULANTE == "11477" & SUB_ESTITULANTE == "0000")
plan2 <- base %>% filter(ESTITULANTE == "11477" & SUB_ESTITULANTE == "0001")
plan3 <- base %>% filter(ESTITULANTE == "11477" & SUB_ESTITULANTE == "0002")
plan4 <- base %>% filter(ESTITULANTE == "11477" & SUB_ESTITULANTE == "0003")
plan5 <- base %>% filter(ESTITULANTE == "11477" & SUB_ESTITULANTE == "0004")
plan6 <- base %>% filter(ESTITULANTE == "11477" & SUB_ESTITULANTE == "0005")
plan7 <- base %>% filter(ESTITULANTE == "11477" & SUB_ESTITULANTE == "0006")
plan8 <- base %>% filter(ESTITULANTE == "11477" & SUB_ESTITULANTE == "0008")
plan9 <- base %>% filter(ESTITULANTE == "11477" & SUB_ESTITULANTE == "0010")
plan10 <- base %>% filter(ESTITULANTE == "11477" & SUB_ESTITULANTE == "0027")
plan11 <- base %>% filter(ESTITULANTE == "11477" & SUB_ESTITULANTE == "0032")
plan12 <- base %>% filter(ESTITULANTE == "11477" & SUB_ESTITULANTE == "0033")
plan13 <- base %>% filter(ESTITULANTE == "11477" & SUB_ESTITULANTE == "0034")
plan14 <- base %>% filter(ESTITULANTE == "11477" & SUB_ESTITULANTE == "0037")
plan15 <- base %>% filter(ESTITULANTE == "11477" & SUB_ESTITULANTE == "0038")
plan16 <- base %>% filter(ESTITULANTE == "11477" & SUB_ESTITULANTE == "0039")
plan31 <- base %>% filter(ESTITULANTE == "11477" & SUB_ESTITULANTE == "0044")
plan17 <- base %>% filter(ESTITULANTE == "11477" & SUB_ESTITULANTE == "0045")
plan18 <- base %>% filter(ESTITULANTE == "11477" & SUB_ESTITULANTE == "0046")
plan19 <- base %>% filter(ESTITULANTE == "11477" & SUB_ESTITULANTE == "0047")
plan20 <- base %>% filter(ESTITULANTE == "11477" & SUB_ESTITULANTE == "0048")
plan21 <- base %>% filter(ESTITULANTE == "11477" & SUB_ESTITULANTE == "0049")
plan22 <- base %>% filter(ESTITULANTE == "11502" & SUB_ESTITULANTE == "0000")
plan23 <- base %>% filter(ESTITULANTE == "11502" & SUB_ESTITULANTE == "0001")
plan24 <- base %>% filter(ESTITULANTE == "11502" & SUB_ESTITULANTE == "0002")
plan25 <- base %>% filter(ESTITULANTE == "11502" & SUB_ESTITULANTE == "0003")
plan26 <- base %>% filter(ESTITULANTE == "15336" & SUB_ESTITULANTE == "0000")
plan27 <- base %>% filter(ESTITULANTE == "15336" & SUB_ESTITULANTE == "0002")
plan28 <- base %>% filter(ESTITULANTE == "15336" & SUB_ESTITULANTE == "0003")
plan29 <- base %>% filter(ESTITULANTE == "15336" & SUB_ESTITULANTE == "0006")
plan30 <- base %>% filter(ESTITULANTE == "15346" & SUB_ESTITULANTE == "0000")

# install.packages("D:/Users/sb046971/Downloads/rJava_0.9-11.zip", repos = NULL)
# 
# Sys.setenv(JAVA_HOME='C:\\Arquivos de Programas\\Java\\jre1.8.0_201')

require(xlsx)

# Write the first data set in a new workbook
write.xlsx(plan1, file = "Base-Segmentada.xlsx",
           sheetName = "11477 0000", append = FALSE, row.names = F)

gc()
gc()

# Add a second data set in a new worksheet
write.xlsx(plan2, file = "Base-Segmentada.xlsx", 
           sheetName="11477 0001", append=TRUE, row.names = F)

gc()
gc()

# Add a third data set
write.xlsx(plan3, file = "Base-Segmentada.xlsx",
           sheetName="11477 0002", append=TRUE, row.names = F)

gc()
gc()

write.xlsx(plan4, file = "Base-Segmentada.xlsx",
           sheetName="11477 0003", append=TRUE, row.names = F)

gc()
gc()

write.xlsx(plan31, file = "Base-Segmentada.xlsx",
           sheetName="11477 0044", append=TRUE, row.names = F)

gc()
gc()

write.xlsx(plan6, file = "Base-Segmentada.xlsx",
           sheetName="11477 0005", append=TRUE, row.names = F)

gc()
gc()

# write.xlsx(plan7, file = "Base-Segmentada.xlsx",
#            sheetName="11477 0006", append=TRUE, row.names = F)

gc()
gc()

# write.xlsx(plan8, file = "Base-Segmentada.xlsx",
#            sheetName="11477 0008", append=TRUE, row.names = F)

write.xlsx(plan9, file = "Base-Segmentada.xlsx",
           sheetName="11477 0010", append=TRUE, row.names = F)

gc()
gc()

write.xlsx(plan10, file = "Base-Segmentada.xlsx",
           sheetName="11477 0027", append=TRUE, row.names = F)

gc()
gc()

write.xlsx(plan11, file = "Base-Segmentada.xlsx",
           sheetName="11477 0032", append=TRUE, row.names = F)

gc()
gc()

write.xlsx(plan12, file = "Base-Segmentada.xlsx",
           sheetName="11477 0033", append=TRUE, row.names = F)

gc()
gc()

write.xlsx(plan13, file = "Base-Segmentada.xlsx",
           sheetName="11477 0034", append=TRUE, row.names = F)

gc()
gc()

write.xlsx(plan14, file = "Base-Segmentada.xlsx",
           sheetName="11477 0037", append=TRUE, row.names = F)

gc()
gc()

write.xlsx(plan15, file = "Base-Segmentada.xlsx",
           sheetName="11477 0038", append=TRUE, row.names = F)

gc()
gc()

write.xlsx(plan16, file = "Base-Segmentada.xlsx",
           sheetName="11477 0039", append=TRUE, row.names = F)

gc()
gc()

write.xlsx(plan17, file = "Base-Segmentada.xlsx",
           sheetName="11477 0045", append=TRUE, row.names = F)

gc()
gc()

write.xlsx(plan18, file = "Base-Segmentada.xlsx",
           sheetName="11477 0046", append=TRUE, row.names = F)

gc()
gc()

write.xlsx(plan19, file = "Base-Segmentada.xlsx",
           sheetName="11477 0047", append=TRUE, row.names = F)

gc()
gc()

write.xlsx(plan20, file = "Base-Segmentada.xlsx",
           sheetName="11477 0048", append=TRUE, row.names = F)

gc()
gc()

write.xlsx(plan21, file = "Base-Segmentada.xlsx",
           sheetName="11477 0049", append=TRUE, row.names = F)

gc()
gc()

write.xlsx(plan22, file = "Base-Segmentada.xlsx",
           sheetName="11502 0000", append=TRUE, row.names = F)

gc()
gc()

write.xlsx(plan23, file = "Base-Segmentada.xlsx",
           sheetName="11502 0001", append=TRUE, row.names = F)

gc()
gc()

write.xlsx(plan24, file = "Base-Segmentada.xlsx",
           sheetName="11502 0002", append=TRUE, row.names = F)

gc()
gc()

write.xlsx(plan25, file = "Base-Segmentada.xlsx",
           sheetName="11502 0003", append=TRUE, row.names = F)

gc()
gc()

write.xlsx(plan26, file = "Base-Segmentada.xlsx",
           sheetName="15336 0000", append=TRUE, row.names = F)

gc()
gc()

write.xlsx(plan27, file = "Base-Segmentada.xlsx",
           sheetName="15336 0002", append=TRUE, row.names = F)

gc()
gc()

write.xlsx(plan28, file = "Base-Segmentada.xlsx",
           sheetName="15336 0003", append=TRUE, row.names = F)

gc()
gc()

write.xlsx(plan29, file = "Base-Segmentada.xlsx",
           sheetName="15336 0006", append=TRUE, row.names = F)

gc()
gc()

# write.xlsx(plan30, file = "Base-Segmentada.xlsx",
#            sheetName="15346 0000", append=TRUE, row.names = F)

gc()
gc()

