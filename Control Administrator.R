#### PACKAGES ####

require(dplyr)
require(data.table)
require(readr)
require(stringr)
require(chron)
require(readxl)
require(tidyr)
require(readxl)
require(xlsx)

#### DROP WARNINGS ####

assign("last.warning", NULL, envir = baseenv())

#### CHANGE CUT CENTURY FOR AGE // BECAUSE THE DEFAULT IS 1970 - 2070 ####

options(chron.year.expand =
          function (y, cut.off = 22, century = c(1900, 2000), ...) {
            chron:::year.expand(y, cut.off = cut.off, century = century, ...)
          }
)

#### populis ####

setwd("D:/Users/sb046971/OneDrive - Honda/Documentos/Controles Administrativos/Populis/")

populis <- read_xlsx("dados1603.xlsx")

populis2 <- populis %>% select(CPF,CCusto,Empresa)

# populis3 <- populis %>% select(Matrícula,Empresa)

populis <- populis %>% select(Matrícula,CCusto,Empresa)

populis$Matrícula <- as.numeric(populis$Matrícula)

demitidos <- read_xlsx("Ativos x Demitidos.xlsx", sheet = "Demitidos")

demitidos2 <- demitidos %>% select(CPF,CENTRO_CUSTO,NOME_EMPRESA_ABREVIADO)

demitidos <- demitidos %>% select(REGISTRO,CENTRO_CUSTO,NOME_EMPRESA_ABREVIADO)

demitidos17 <- read_xlsx("demitidos17.xlsx")
demitidos18 <- read_xlsx("demitidos18.xlsx")
demitidos19 <- read_xlsx("demitidos19.xlsx")
demitidos20 <- read_xlsx("demitidos20.xlsx")

demitidos3 <- bind_rows(demitidos17,demitidos18,demitidos19,demitidos20)

demitidos3 <- demitidos3 %>% select(Nome,CCusto,Empresa)

demitidos4 <- bind_rows(demitidos17,demitidos18,demitidos19,demitidos20)

demitidos4 <- demitidos4 %>% select(Matrícula,CCusto,Empresa)

demitidos4$Matrícula <- as.numeric(demitidos4$Matrícula)

names(demitidos3)[names(demitidos3) == "Nome"] <- "NOME SEGURADO/DEPENDENTE"

names(demitidos)[names(demitidos) == "REGISTRO"] <- "Matrícula"

names(demitidos)[names(demitidos) == "CENTRO_CUSTO"] <- "CCusto"

names(demitidos)[names(demitidos) == "NOME_EMPRESA_ABREVIADO"] <- "Empresa"

names(demitidos2)[names(demitidos2) == "NOME_EMPRESA_ABREVIADO"] <- "Empresa"

names(demitidos2)[names(demitidos2) == "CPF"] <- "NUMERO DO CPF"

names(demitidos2)[names(demitidos2) == "CENTRO_CUSTO"] <- "CCusto"

names(populis2)[names(populis2) == "CPF"] <- "NUMERO DO CPF"

demitidos$Matrícula <- as.numeric(demitidos$Matrícula)

relat.populis <- bind_rows(populis,demitidos)

relat.populis2 <- bind_rows(populis2,demitidos2)

#### bradesco ####

setwd("D:/Users/sb046971/OneDrive - Honda/Documentos/Controles Administrativos/Bradesco/")

posicao_bradesco <- read_xlsx("POSIÇÃO CADASTRAL 04.2021.xlsx" ,
                               sheet = "POS. CADASTRAL (TITULAR)", skip = 2)

posicao_bradesco <- posicao_bradesco %>% select(`NUMERO DO CERTIFICADO`,
                                                `NUMERO DO CPF`)

posicao_bradesco$`NUMERO DO CERTIFICADO` <- as.numeric(
  posicao_bradesco$`NUMERO DO CERTIFICADO`)

posicao_bradesco$`NUMERO DO CPF` <- as.numeric(
  posicao_bradesco$`NUMERO DO CPF`)

widths = c(1,4,7,2,35,4,8,1,1,1,4,8,2,6,15,15,2,20,12,32)

names =  c("TIPO DO REGISTRO","NUMERO DA SUBFATURA","NUMERO DO CERTIFICADO",
           "COMPLEMENTO DO CERTIFICADO","NOME SEGURADO/DEPENDENTE",
           "INDIC. SUBF. ANTER/ATUAL","DATA DE NASCIMENTO",
           "CODIGO DO SEXO","ESTADO CIVIL","COD. GRAU PARENT.DEP.",
           "CODIGO DO PLANO","DATA INICIO VIGENCIA","TIPO DE LANÇAMENTO",
           "DATA DE LANCAMENTO","VALOR DO LANCAMENTO","PARTE DO SEGURADO",
           "CODIGO DO LANCAMENTO","CARGO / OCUPACAO","Matrícula","FILLER")

faturamento.bradesco <- readr:: read_fwf(
  file ="D:/Users/sb046971/OneDrive - Honda/Documentos/Controles Administrativos/Bradesco/FATURA TÉCNICA 04.2021.TXT",
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

# setwd("D:/Users/sb046971/Documents/Controles Administrativos/Bradesco/")
# 
# faturamento.bradesco <- read_xlsx("FATURA TECNICA 05.2020.xlsx")
# 
# faturamento.bradesco$`NUMERO DO CERTIFICADO` <- as.numeric(
#faturamento.bradesco$`NUMERO DO CERTIFICADO`)

faturamento.bradesco$`VALOR DO LANCAMENTO` <- faturamento.bradesco$`VALOR DO LANCAMENTO`/100

faturamento.bradesco$`VALOR DO LANCAMENTO` <- ifelse(
  faturamento.bradesco$`CODIGO DO LANCAMENTO` >49,
  faturamento.bradesco$`VALOR DO LANCAMENTO`*(-1),
  faturamento.bradesco$`VALOR DO LANCAMENTO`)

faturamento.bradesco <- faturamento.bradesco %>% group_by(
  `TIPO DO REGISTRO`,`NUMERO DA SUBFATURA`,`NUMERO DO CERTIFICADO`,
  `COMPLEMENTO DO CERTIFICADO`,`NOME SEGURADO/DEPENDENTE`,`CODIGO DO SEXO`,
  `ESTADO CIVIL`,`COD. GRAU PARENT.DEP.`,`CODIGO DO PLANO`,
  `DATA INICIO VIGENCIA`,`TIPO DE LANÇAMENTO`,`PARTE DO SEGURADO`,
  `CARGO / OCUPACAO`,Matrícula) %>% summarise(Valor = sum(
    `VALOR DO LANCAMENTO`))

faturamento.bradesco$Matrícula <- as.numeric(faturamento.bradesco$Matrícula)

relat.bradesco <- left_join(faturamento.bradesco,posicao_bradesco, 
                            by = "NUMERO DO CERTIFICADO")

relat.bradesco <- left_join(relat.bradesco,relat.populis2,
                            by = "NUMERO DO CPF")

relat.bradesco <- left_join(relat.bradesco,demitidos3, 
                            by = "NOME SEGURADO/DEPENDENTE")

relat.bradesco <- relat.bradesco[order(relat.bradesco$CCusto.x,
                                       relat.bradesco$`NUMERO DO CPF`),]

relat.bradesco$CCusto <- ifelse(is.na(relat.bradesco$CCusto.x),
                                relat.bradesco$CCusto.y,
                                relat.bradesco$CCusto.x)

relat.bradesco$CCusto.x <- NULL
relat.bradesco$CCusto.y <- NULL

relat.bradesco <- relat.bradesco[order(relat.bradesco$Empresa.x,
                                       relat.bradesco$`NUMERO DO CPF`),]

relat.bradesco$Empresa <- ifelse(is.na(relat.bradesco$Empresa.x),
                                relat.bradesco$Empresa.y,
                                relat.bradesco$Empresa.x)

relat.bradesco$Empresa.x <- NULL
relat.bradesco$Empresa.y <- NULL

relat.bradesco <- left_join(relat.bradesco,relat.populis, by = "Matrícula")

relat.bradesco$CCusto <- ifelse(is.na(relat.bradesco$CCusto.x),
                                relat.bradesco$CCusto.y,
                                relat.bradesco$CCusto.x)

relat.bradesco$CCusto.x <- NULL
relat.bradesco$CCusto.y <- NULL

relat.bradesco <- relat.bradesco %>% group_by(
  `NUMERO DO CERTIFICADO`) %>% mutate(CCusto = case_when(is.na(CCusto) |
                                                         CCusto != first(
                                                           CCusto) ~ first(
                                                             CCusto),
                                                       TRUE ~ CCusto))

relat.bradesco$Empresa <- ifelse(is.na(relat.bradesco$Empresa.x),
                                relat.bradesco$Empresa.y,
                                relat.bradesco$Empresa.x)

relat.bradesco$Empresa.x <- NULL
relat.bradesco$Empresa.y <- NULL

relat.bradesco <- relat.bradesco %>% group_by(
  `NUMERO DO CERTIFICADO`) %>% mutate(Empresa = case_when(is.na(Empresa) |
                                                         Empresa != first(
                                                           Empresa) ~ first(
                                                             Empresa),
                                                       TRUE ~ Empresa))

relat.bradesco$elegibilidade <- ifelse(
  relat.bradesco$`COD. GRAU PARENT.DEP.` == 0, "TITULAR", "DEPENDENTE")

relat.bradesco$`NUMERO DA SUBFATURA` <- as.numeric(relat.bradesco$`NUMERO DA SUBFATURA`)

relat.bradesco <- relat.bradesco %>% unique(.)

# fwrite(relat.bradesco,
#        file = "z:/1.Saúde Assistencial/1.Medicina/BRADESCO/1.Faturamento/Honda/2021/04.ABRIL/faturatec0421.csv",dec = ",", sep = "|")

# relat.bradesco <- relat.bradesco %>% filter(`NUMERO DA SUBFATURA` %in% c(1,20,31))

pdca.bradesco <- relat.bradesco %>% mutate(
  tipo_valor = if_else(grepl('RETRO',
                             `NOME SEGURADO/DEPENDENTE`),
                       "RETROATIVO",
                       ifelse(grepl('APORTE',
                                    `NOME SEGURADO/DEPENDENTE`),
                              "APORTE","VALOR"))) %>% group_by(CCusto,
                                                               `NUMERO DA SUBFATURA`,
                                                               Empresa,
                                         `CODIGO DO PLANO`,tipo_valor,
                                         elegibilidade) %>%
  mutate(qtde.titulares = n()) %>% summarise(
    valor = sum(Valor),
    n.titulares = n())

pdca.bradesco$id <- seq.int(nrow(pdca.bradesco))

pdca.bradesco <- pdca.bradesco %>% spread(elegibilidade,
                                          n.titulares, fill = 0)

pdca.bradesco <- pdca.bradesco %>% spread(tipo_valor,valor, fill=0)

pdca.bradesco$APORTE <- 0 ## if don't exist apport

pdca.bradesco$RETROATIVO <- 0 ## if don't exist retroative

pdca.bradesco$OUTROS <- 0 ## if don't exist other values

pdca.brad <- pdca.bradesco %>% group_by(CCusto,Empresa,
                                        `CODIGO DO PLANO`) %>% summarise(
                                        tit = sum(TITULAR), 
                                        dep = sum(DEPENDENTE),
                                        valor.fim = sum(VALOR), 
                                        plano = "bradesco", 
                                        retroativo = 0 + sum(RETROATIVO), 
                                        aporte = sum(APORTE), 
                                        outros.valores = sum(OUTROS),
                                        custo.p.vida = round(valor.fim/tit,2),
                                        total.outros = sum(outros.valores) + 
                                          sum(retroativo) + sum(APORTE))

## if exist devolution in Bradesco

pdca.brad$CCusto <- if_else(is.na(pdca.brad$CCusto), "dev.brad", pdca.brad$CCusto)

dev.brad <- pdca.brad %>% filter(CCusto == "dev.brad")

pdca.brad <- pdca.brad %>% filter(!CCusto == "dev.brad")

pdca.brad$conta <- pdca.brad$tit/sum(pdca.brad$tit)*dev.brad$valor.fim

pdca.brad$valor.fim <- pdca.brad$valor.fim + pdca.brad$conta

pdca.brad$conta <- NULL

pdca.brad$custo.p.vida <- pdca.brad$valor.fim/pdca.brad$tit

# names(pdca.brad)[names(pdca.brad) == "novo"] <- "CCusto"
# names(pdca.brad)[names(pdca.brad) == "new.empresa"] <- "Empresa"

#### rateio bradesco ####
#### HAB ####

teste <- relat.bradesco %>% group_by(`TIPO DO REGISTRO`, `NUMERO DA SUBFATURA`, 
                            `NUMERO DO CERTIFICADO`, `COMPLEMENTO DO CERTIFICADO`, 
                            `NOME SEGURADO/DEPENDENTE`, `CODIGO DO SEXO`, `ESTADO CIVIL`,
                            `COD. GRAU PARENT.DEP.`, `CODIGO DO PLANO`, 
                            `DATA INICIO VIGENCIA`, `PARTE DO SEGURADO`, 
                            `CARGO / OCUPACAO`, `Matrícula`, `NUMERO DO CPF`,
                            `CCusto`, `Empresa`,
                            elegibilidade) %>% summarise(Valor = sum(Valor))

rateio <- teste %>% group_by(`NUMERO DA SUBFATURA`,CCusto,
                                       elegibilidade) %>% 
  mutate(qtde.titulares = n()) %>% summarise(
    valor = sum(Valor), 
    n.titulares = n())

rateio$id <- seq.int(nrow(rateio))

rateio.hab <- rateio %>% spread(elegibilidade,
                                n.titulares, 
                                fill = 0) %>% filter(
                                  `NUMERO DA SUBFATURA` %in% c(17,
                                                               28,
                                                               29)) %>% group_by(
                                                                 `NUMERO DA SUBFATURA`,
                                                       CCusto) %>% summarise(
                                                         tit = sum(TITULAR),
                                                         dep = sum(DEPENDENTE),
                                                         valor.fim = sum(valor),
                                                         total.eleg = tit + dep)

desc.rateio <- teste %>% filter(grepl("DEV",`NOME SEGURADO/DEPENDENTE`) & 
                                  is.na(CCusto)) %>% group_by(
                                    `NUMERO DA SUBFATURA`,CCusto) %>% summarise(
                                      valor.fim = sum(Valor))

desc.rateio$valor.fim <- desc.rateio$valor.fim*-1

desc.sub17 <- desc.rateio %>% filter(`NUMERO DA SUBFATURA` == 17)
desc.sub28 <- desc.rateio %>% filter(`NUMERO DA SUBFATURA` == 28)
desc.sub29 <- desc.rateio %>% filter(`NUMERO DA SUBFATURA` == 29)

rateio.hab <- rateio.hab %>% filter(!is.na(CCusto))

sub17 <- rateio.hab %>% filter(`NUMERO DA SUBFATURA` == 17)
sub28 <- rateio.hab %>% filter(`NUMERO DA SUBFATURA` == 28)
sub29 <- rateio.hab %>% filter(`NUMERO DA SUBFATURA` == 29)

sub17$perc.eleg <- sub17$total.eleg/sum(sub17$total.eleg)
sub28$perc.eleg <- sub28$total.eleg/sum(sub28$total.eleg)
sub29$perc.eleg <- sub29$total.eleg/sum(sub29$total.eleg)

sub17$valor.desc <- sub17$perc.eleg*desc.sub17$valor.fim
sub28$valor.desc <- sub28$perc.eleg*desc.sub28$valor.fim
sub29$valor.desc <- sub29$perc.eleg*desc.sub29$valor.fim

rateio.hab <- bind_rows(sub17,sub28,sub29)

rateio.hab$novo.valor <- round(rateio.hab$valor.fim-rateio.hab$valor.desc,4)

rateio.hab <- rateio.hab %>% group_by(CCusto) %>% summarise(tit = sum(tit), 
                                                            valor = round(sum(novo.valor),4))

iof <- sum(rateio.hab$valor)*0.0238

rateio.hab$tit.eleg <- rateio.hab$tit/sum(rateio.hab$tit)

rateio.hab$valor.iof <- rateio.hab$tit.eleg*iof

rateio.hab$valor.final <- round(rateio.hab$valor+rateio.hab$valor.iof,2)

rateio.hab.final <- rateio.hab %>% select(CCusto,tit,valor.final)

fwrite(rateio.hab.final, 
file = "z:/1.Saúde Assistencial/1.Medicina/BRADESCO/1.Faturamento/Honda/2021/04.ABRIL/rateio-hab.csv",dec = ",", sep = "|")

#### MAO ####

rateio.mao <- rateio %>% spread(elegibilidade,
                                n.titulares, 
                                fill = 0) %>% filter(
                                  `NUMERO DA SUBFATURA` %in% c(1,
                                                               20,
                                                               31)) %>% group_by(
                                                                 `NUMERO DA SUBFATURA`,
                                                                 CCusto,) %>% summarise(
                                                                   tit = sum(TITULAR),
                                                                   dep = sum(DEPENDENTE),
                                                                   valor.fim = sum(valor),
                                                                   total.eleg = tit + dep)

desc.rateio <- rateio.mao %>% filter(is.na(CCusto))

desc.rateio$valor.fim <- desc.rateio$valor.fim*-1

desc.sub1 <- desc.rateio %>% filter(`NUMERO DA SUBFATURA` == 1)
desc.sub20 <- desc.rateio %>% filter(`NUMERO DA SUBFATURA` == 20)
desc.sub31 <- desc.rateio %>% filter(`NUMERO DA SUBFATURA` == 31)

rateio.mao <- rateio.mao %>% filter(!is.na(CCusto))

sub1 <- rateio.mao %>% filter(`NUMERO DA SUBFATURA` == 1)
sub20 <- rateio.mao %>% filter(`NUMERO DA SUBFATURA` == 20)
sub31 <- rateio.mao %>% filter(`NUMERO DA SUBFATURA` == 31)

sub1$perc.eleg <- sub1$total.eleg/sum(sub1$total.eleg)
sub20$perc.eleg <- sub20$total.eleg/sum(sub20$total.eleg)
sub31$perc.eleg <- sub31$total.eleg/sum(sub31$total.eleg)

sub1$valor.desc <- sub1$perc.eleg*desc.sub1$valor.fim
sub20$valor.desc <- sub20$perc.eleg*desc.sub20$valor.fim
sub31$valor.desc <- sub31$perc.eleg*desc.sub31$valor.fim

rateio.mao <- bind_rows(sub1,sub20,sub31)

rateio.mao$novo.valor <- round(rateio.mao$valor.fim-rateio.mao$valor.desc,4)

rateio.mao <- rateio.mao %>% group_by(`NUMERO DA SUBFATURA`,
                                      CCusto) %>% summarise(tit = sum(tit), 
                                                            valor = round(sum(novo.valor),4))

rateio.mao$tit.eleg <- rateio.mao$tit/sum(rateio.mao$tit)

iof <- sum(rateio.mao$valor)*0.0238

rateio.mao$valor.iof <- rateio.mao$tit.eleg*iof

rateio.mao$valor.final <- round(rateio.mao$valor+rateio.mao$valor.iof,2)

rateio.mao.final <- rateio.mao %>% select(CCusto,tit,valor.final)

fwrite(rateio.mao.final, 
       file = "z:/1.Saúde Assistencial/1.Medicina/BRADESCO/1.Faturamento/Honda/2020/11.Novembro/rateio-mao.csv",dec = ",", sep = "")

#### SAO ####

rateio <- relat.bradesco  %>% group_by(`NUMERO DA SUBFATURA`,
                                       Empresa,CCusto,
                                       elegibilidade) %>% 
  mutate(qtde.titulares = n()) %>% summarise(
    valor = sum(Valor), 
    n.titulares = n())

rateio$id <- seq.int(nrow(rateio))

rateio.sao <- rateio %>% spread(
  elegibilidade,n.titulares,fill = 0) %>% filter(
    Empresa %in% c("SAO-BHB-MORUMBI",
                   "SAO-CNH S. C. SUL",
                   "SAO-HSF-MORUMBI")) %>% group_by(Empresa,
                     CCusto) %>% summarise(
                       tit = sum(TITULAR),
                       dep = sum(DEPENDENTE),
                       valor.fim = sum(valor),
                       total.eleg = tit + dep)

desc.rateio <- rateio.sao %>% filter(is.na(CCusto))

desc.rateio$valor.fim <- desc.rateio$valor.fim*-1

desc.sub1 <- desc.rateio %>% filter(`NUMERO DA SUBFATURA` == 1)

rateio.sao <- rateio.sao %>% filter(!is.na(CCusto))

#### unimed ####

setwd("D:/Users/sb046971/OneDrive - Honda/Documentos/Controles Administrativos/Unimed/02.2021/")

faturamento.unimed <- list.files(pattern = ".CSV") %>%
  lapply(fread,dec = ",",colClasses = c("Cod Matricula Usuario" = "character",
                                        "Nome do Titular" = "character",
                                        "Nome Usuario" = "character",
                                        "Centro de Custo" = "character",
                                       "Descricao Tipo Usuario" = "character",
                                       "Contigo Antigo Usuario" = "character",
                                       "Codigo Modulo" = "character",
                                        "Valor Mod Coletivo" = "numeric",
                                        "Valor Mod Privativo" = "numeric",
                                        "Valor Beneficio Familia" = "numeric",
                                        "Valor Help" = "numeric",
                                     "Valor Manutencao Contrato" = "numeric",
                                        "Valor Fator Moderador" = "numeric",
                                        "Valor Receita Usuario" = "numeric",
                                     "NUMERO DA SUBFATURA" = "numeric"),
         stringsAsFactors=F, encoding="UTF-8", sep = ";",
         select=c("Cod Matricula Usuario","Nome do Titular","Nome Usuario",
                  "Centro de Custo","Descricao Tipo Usuario",
               "Contigo Antigo Usuario","Codigo Modulo","Valor Mod Coletivo",
                  "Valor Mod Privativo","Valor Beneficio Familia",
                  "Valor Help","Valor Manutencao Contrato",
      "Valor Fator Moderador","Valor Receita Usuario",
      "NUMERO DA SUBFATURA"))  %>% bind_rows

faturamento.unimed$`Cod Matricula Usuario` <- str_replace_all(
  faturamento.unimed$`Cod Matricula Usuario`, "'", "")

names(faturamento.unimed)[names(
  faturamento.unimed) == "Cod Matricula Usuario"] <- "Matrícula"

faturamento.unimed$Matrícula <- as.numeric(faturamento.unimed$Matrícula)

relat.unimed <- left_join(faturamento.unimed,relat.populis, by = "Matrícula")

relat.unimed <- left_join(relat.unimed,demitidos4, by = "Matrícula")

relat.unimed$CCusto <- ifelse(is.na(relat.unimed$CCusto.x),
                                    relat.unimed$CCusto.y,
                                    relat.unimed$CCusto.x)

relat.unimed$CCusto.x <- NULL
relat.unimed$CCusto.y <- NULL

relat.unimed$Empresa <- ifelse(is.na(relat.unimed$Empresa.x),
                              relat.unimed$Empresa.y,
                              relat.unimed$Empresa.x)

relat.unimed$Empresa.x <- NULL
relat.unimed$Empresa.y <- NULL

relat.unimed <- relat.unimed %>% unique(.)

pdca.unimed <- relat.unimed %>% group_by(CCusto,Empresa,
                                        `Codigo Modulo`,
                                        `Descricao Tipo Usuario`) %>% 
  mutate(qtde.titulares = n(), 
         valor.agregados = if_else(`Descricao Tipo Usuario` == "AGREGADO",
                                   `Valor Receita Usuario`,0)) %>% summarise(
    valor = sum(`Valor Receita Usuario`), 
    n.titulares = n(),
    outros.valores = sum(`Valor Beneficio Familia`),
    retroativo = 0, aporte = 0, valor.agreg = sum(valor.agregados))

pdca.unimed$valor <- if_else(
  pdca.unimed$`Descricao Tipo Usuario` != "AGREGADO",pdca.unimed$valor,0)

pdca.unimed$id <- seq.int(nrow(pdca.unimed))

pdca.unimed <- pdca.unimed %>% spread(`Descricao Tipo Usuario`, 
                                      n.titulares, fill = 0)

pdca.unimed <- pdca.unimed %>% group_by(CCusto,Empresa,
                                      `Codigo Modulo`) %>% summarise(
                                        agreg = sum(AGREGADO),
                                        tit = sum(TITULAR), 
                                        dep = sum(DEPENDENTE),
                                        valor.fim = sum(valor), 
                                        plano = "unimed", 
                                        retroativo = sum(retroativo), 
                                        aporte = sum(aporte), 
                                        outros.valores = sum(outros.valores),
                                        custo.p.vida = round(valor.fim/tit,2),
                                        total.outros = sum(outros.valores) + 
                                          sum(retroativo) + sum(aporte),
                                        vlr.agreg = sum(valor.agreg))

# retro.hda <- pdca.unimed %>% filter(grepl('HDA',Empresa))
# 
# retro.hda$retroativo <- round(retro.hda$tit/sum(retro.hda$tit)*13868.92,2)
# 
# retro.hab <- pdca.unimed %>% filter(grepl('HAB',Empresa))
# 
# retro.hab$retroativo <- round(retro.hab$tit/sum(retro.hab$tit)*163695.4,2)
# 
# retro.htb <- pdca.unimed %>% filter(grepl('HTB',Empresa))
# 
# retro.htb$retroativo <- round(retro.htb$tit/sum(retro.htb$tit)*2599.55,2)
# 
# retro.hen <- pdca.unimed %>% filter(grepl('HEN',Empresa))
# 
# retro.hen$retroativo <- round(retro.hen$tit/sum(retro.hen$tit)*324.49,2)
# 
# retro.previhonda <- pdca.unimed %>% filter(grepl('PREVIHONDA',Empresa))
# 
# retro.previhonda$retroativo <- round(retro.previhonda$tit/sum(retro.previhonda$tit)*54.94,2)
# 
# pdca.unimed <- bind_rows(retro.hab,retro.hda,retro.hen,retro.htb,retro.previhonda)

#### samel ####

setwd("D:/Users/sb046971/OneDrive - Honda/Documentos/Controles Administrativos/Samel/02.2021")

hca <- read_xlsx("1271 - HONDA COMPONENTES.xlsx")

hda <- read_xlsx("1270 - MOTO HONDA.xlsx")

# hca$CPF <- as.numeric(hca$CPF)
# hca$RG <- as.numeric(hca$RG)
# # 
# hca$PISPASEP <- as.character(hca$PISPASEP)
# # hca$MATRICULAFUNCIONAL <- as.numeric(hca$MATRICULAFUNCIONAL)

faturamento.samel <- bind_rows(hda,hca)

faturamento.samel$NOVO_TIPO <- if_else(grepl("Agregado",
                                             faturamento.samel$GRAU),
                                       "AGREGADO",
                                       if_else(grepl("Dependente",
                                                     faturamento.samel$GRAU),
                                               "DEPENDENTE",
                                               "TITULAR"))

names(faturamento.samel)[names(
  faturamento.samel) == "MATRICULAFUNCIONAL"] <- "Matrícula"

relat.samel <- left_join(faturamento.samel,relat.populis, by = "Matrícula")

relat.samel <- left_join(relat.samel,demitidos4, by = "Matrícula")

relat.samel$CCusto <- ifelse(is.na(relat.samel$CCusto.x),
                                relat.samel$CCusto.y,
                                relat.samel$CCusto.x)

relat.samel$CCusto.x <- NULL
relat.samel$CCusto.y <- NULL

relat.samel$Empresa <- ifelse(is.na(relat.samel$Empresa.x),
                             relat.samel$Empresa.y,
                             relat.samel$Empresa.x)

relat.samel$Empresa.x <- NULL
relat.samel$Empresa.y <- NULL

relat.samel$VALOR_BENEFICIARIO <- str_replace_all(
  relat.samel$VALOR_BENEFICIARIO, ",", ".")

relat.samel$VALOR_BENEFICIARIO <- as.numeric(relat.samel$VALOR_BENEFICIARIO)

relat.samel$elegibilidade <- "ENFERMARIA"

pdca.samel <- relat.samel %>% group_by(CCusto,Empresa,
                                         elegibilidade,
                                         NOVO_TIPO) %>% 
  mutate(qtde.titulares = n(),
         valor.agregados = if_else(NOVO_TIPO == "AGREGADO",
                                   VALOR_BENEFICIARIO,0)) %>% summarise(
    valor = sum(VALOR_BENEFICIARIO), 
    n.titulares = n(),
    retroativo = 0, aporte = 0,
    outros.valores = 0,
    valor.agreg = sum(valor.agregados))

pdca.samel$valor <- if_else(
  pdca.samel$NOVO_TIPO != "AGREGADO",pdca.samel$valor,0)

pdca.samel$id <- seq.int(nrow(pdca.samel))

pdca.samel <- pdca.samel %>% spread(NOVO_TIPO, n.titulares, fill = 0)

pdca.samel <- pdca.samel %>% group_by(CCusto,Empresa,
                                      elegibilidade) %>% summarise(
                                          agreg = sum(AGREGADO),
                                          tit = sum(TITULAR), 
                                          dep = sum(DEPENDENTE),
                                          valor.fim = sum(valor), 
                                          plano = "samel", 
                                          retroativo = sum(retroativo), 
                                          aporte = sum(aporte), 
                                          outros.valores = sum(outros.valores),
                                          custo.p.vida = round(valor.fim/tit,2),
                                          total.outros = sum(outros.valores) + 
                                            sum(retroativo) + sum(aporte),
                                          vlr.agreg = sum(valor.agreg))

#### PDCA CONSOLIDATE ####

names(pdca.brad)[names(pdca.brad) == "CODIGO DO PLANO"] <- "Tipo/Cod Plano"
names(pdca.samel)[names(pdca.samel) == "elegibilidade"] <- "Tipo/Cod Plano"
names(pdca.unimed)[names(pdca.unimed) == "Codigo Modulo"] <- "Tipo/Cod Plano"

pdca.saude <- bind_rows(pdca.brad,pdca.unimed,pdca.samel)

pdca.saude <- pdca.saude %>% mutate_if(is.numeric,funs(ifelse(is.na(.),0,.)))

pdca.saude[is.na(pdca.saude)] <- "NÃO SE APLICA"

pdca.saude$`Tipo/Cod Plano` <- if_else(pdca.saude$`Tipo/Cod Plano` == 0, 
                                       "NÃO SE APLICA", 
                                       pdca.saude$`Tipo/Cod Plano`)

names(pdca.saude)[names(pdca.saude) == "CCusto"] <- "Centro de Custo"
names(pdca.saude)[names(pdca.saude) == "tit"] <- "Titulares"
names(pdca.saude)[names(pdca.saude) == "dep"] <- "Dependentes"
names(pdca.saude)[names(pdca.saude) == "valor.fim"] <- "Valor"
names(pdca.saude)[names(pdca.saude) == "plano"] <- "Plano"
names(pdca.saude)[names(pdca.saude) == "retroativo"] <- "Retroativo"
names(pdca.saude)[names(pdca.saude) == "aporte"] <- "Aporte"
names(pdca.saude)[names(pdca.saude) == "outros.valores"] <- "Outros Vlrs"
names(pdca.saude)[names(pdca.saude) == "custo.p.vida"] <- "Custo por Vida"
names(pdca.saude)[names(pdca.saude) == "total.outros"] <- "Total Outros"
names(pdca.saude)[names(pdca.saude) == "agreg"] <- "Agregados"
names(pdca.saude)[names(pdca.saude) == "vlr.agreg"] <- "Valor Agregados"

pdca.saude$Valor <- round(pdca.saude$Valor,2)
pdca.saude$`Custo por Vida` <- round(pdca.saude$`Custo por Vida`,2)

# fwrite(pdca.saude, file = "D:/Users/sb046971/OneDrive - Honda/Documentos/pdca_saude.csv", 
#        sep = ";", dec = ",")

setwd("D:/Users/sb046971/OneDrive - Honda/Documentos/Controles Administrativos/")

pdca.saude <- as.data.frame(pdca.saude)

pdca.saude$`Centro de Custo` <- if_else(grepl('CNH',pdca.saude$Empresa), 
                             paste("1",pdca.saude$`Centro de Custo`,sep = ""), 
                             if_else(grepl('BHB',pdca.saude$Empresa),
                                     paste("2",pdca.saude$`Centro de Custo`,sep = ""),
                                     if_else(grepl('HSF',pdca.saude$Empresa),
                                             paste("8",pdca.saude$`Centro de Custo`,sep = ""),
                                             pdca.saude$`Centro de Custo`)))

pdca.saude1 <- pdca.saude %>% group_by(`Centro de Custo`,
                                       Plano) %>% summarise(Titulares = sum(Titulares),
                                                            Dependentes = sum(Dependentes), 
                                                            Valor = sum(Valor),
                                                            Retroativo = sum(Retroativo),
                                                            Aporte = sum(Aporte), 
                                                            `Outros Vlrs` = sum(`Outros Vlrs`), 
                                                            `Total Outros` = sum(`Total Outros`),
                                                            Agregados = sum(Agregados), 
                                                            `Valor Agregados` = sum(`Valor Agregados`), 
                                                            `Custo por Vida` = round(sum(Valor)/sum(Titulares),2))

pdca.saude1 <- as.data.frame(pdca.saude1)

write.xlsx(pdca.saude1, file = "PDCA - Saude.xlsx",
           sheetName="02.2021", append = T, row.names = F)

gc()
gc()

# Add a second data set in a new worksheet
write.xlsx(pdca.saude, file = "PDCA - Saude-.xlsx", 
           sheetName="06.2020", append=T, row.names = F)

gc()
gc()

write.xlsx(pdca.saude, file = "PDCA - Saude-.xlsx", 
           sheetName="05.2020", append=T, row.names = F)

gc()
gc()

write.xlsx(pdca.saude, file = "PDCA - Saude-.xlsx", 
           sheetName="04.2020", append=TRUE, row.names = F)

gc()
gc()


#### acompanhamento mensal ####

# bradesco #

widths = c(1,4,7,2,35,4,8,1,1,1,4,8,2,6,15,15,2,20,12,32)

names =  c("TIPO DO REGISTRO","NUMERO DA SUBFATURA","NUMERO DO CERTIFICADO",
           "COMPLEMENTO DO CERTIFICADO","NOME SEGURADO/DEPENDENTE",
           "INDIC. SUBF. ANTER/ATUAL","DATA DE NASCIMENTO",
           "CODIGO DO SEXO","ESTADO CIVIL","COD. GRAU PARENT.DEP.",
           "CODIGO DO PLANO","DATA INICIO VIGENCIA","TIPO DE LANÇAMENTO",
           "DATA DE LANCAMENTO","VALOR DO LANCAMENTO","PARTE DO SEGURADO",
           "CODIGO DO LANCAMENTO","CARGO / OCUPACAO","Matrícula","FILLER")

fat.tec04 <- readr:: read_fwf(
  file ="D:/Users/sb046971/Documents/Controles Administrativos/Bradesco/FATURA TÉCNICA 04.2020.TXT",
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

fat.tec04$`VALOR DO LANCAMENTO` <- fat.tec04$`VALOR DO LANCAMENTO`/100

fat.tec04$`VALOR DO LANCAMENTO` <- ifelse(
  fat.tec04$`CODIGO DO LANCAMENTO` >49,
  fat.tec04$`VALOR DO LANCAMENTO`*(-1),
  fat.tec04$`VALOR DO LANCAMENTO`)

fat.tec04 <- fat.tec04 %>% group_by(
  `TIPO DO REGISTRO`,`NUMERO DA SUBFATURA`,`NUMERO DO CERTIFICADO`,
  `COMPLEMENTO DO CERTIFICADO`,`NOME SEGURADO/DEPENDENTE`,`CODIGO DO SEXO`,
  `ESTADO CIVIL`,`COD. GRAU PARENT.DEP.`,`CODIGO DO PLANO`,
  `DATA INICIO VIGENCIA`,`TIPO DE LANÇAMENTO`,`PARTE DO SEGURADO`,
  `CARGO / OCUPACAO`,Matrícula) %>% summarise(Valor = sum(
    `VALOR DO LANCAMENTO`))

fat.tec04$Matrícula <- as.numeric(fat.tec04$Matrícula)

fat.tec05 <- read_xlsx(
path = "D:/Users/sb046971/Documents/Controles Administrativos/Bradesco/FATURA TECNICA 05.2020.xlsx")

fat.tec05$`TIPO DO REGISTRO` <- as.numeric(fat.tec05$`TIPO DO REGISTRO`)
fat.tec05$`NUMERO DA SUBFATURA` <- as.numeric(fat.tec05$`NUMERO DA SUBFATURA`)
fat.tec05$`NUMERO DO CERTIFICADO` <- as.numeric(fat.tec05$`NUMERO DO CERTIFICADO`)
fat.tec05$`COMPLEMENTO DO CERTIFICADO` <- as.numeric(fat.tec05$`COMPLEMENTO DO CERTIFICADO`)
fat.tec05$`CODIGO DO SEXO`<- as.numeric(fat.tec05$`CODIGO DO SEXO`)
fat.tec05$`ESTADO CIVIL` <- as.numeric(fat.tec05$`ESTADO CIVIL`)
fat.tec05$`COD. GRAU PARENT.DEP.` <- as.numeric(fat.tec05$`COD. GRAU PARENT.DEP.`)
fat.tec05$`PARTE DO SEGURADO` <- as.numeric(fat.tec05$`PARTE DO SEGURADO`)
fat.tec05$Matrícula <- as.numeric(fat.tec05$Matrícula)

fat.tec05$`VALOR DO LANCAMENTO` <- ifelse(
  fat.tec05$`CODIGO DO LANCAMENTO` >49,
  fat.tec05$`VALOR DO LANCAMENTO`*(-1),
  fat.tec05$`VALOR DO LANCAMENTO`)

fat.tec05 <- fat.tec05 %>% group_by(
  `TIPO DO REGISTRO`,`NUMERO DA SUBFATURA`,`NUMERO DO CERTIFICADO`,
  `COMPLEMENTO DO CERTIFICADO`,`NOME SEGURADO/DEPENDENTE`,`CODIGO DO SEXO`,
  `ESTADO CIVIL`,`COD. GRAU PARENT.DEP.`,`CODIGO DO PLANO`,
  `DATA INICIO VIGENCIA`,`TIPO DE LANÇAMENTO`,`PARTE DO SEGURADO`,
  `CARGO / OCUPACAO`,Matrícula) %>% summarise(Valor = sum(
    `VALOR DO LANCAMENTO`))

fat.tec06 <- readr:: read_fwf(
  file ="D:/Users/sb046971/Documents/Controles Administrativos/Bradesco/FATURA TÉCNICA 06.2020.TXT",
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

fat.tec06$`VALOR DO LANCAMENTO` <- fat.tec06$`VALOR DO LANCAMENTO`/100

fat.tec06$`VALOR DO LANCAMENTO` <- ifelse(
  fat.tec06$`CODIGO DO LANCAMENTO` >49,
  fat.tec06$`VALOR DO LANCAMENTO`*(-1),
  fat.tec06$`VALOR DO LANCAMENTO`)

fat.tec06 <- fat.tec06 %>% group_by(
  `TIPO DO REGISTRO`,`NUMERO DA SUBFATURA`,`NUMERO DO CERTIFICADO`,
  `COMPLEMENTO DO CERTIFICADO`,`NOME SEGURADO/DEPENDENTE`,`CODIGO DO SEXO`,
  `ESTADO CIVIL`,`COD. GRAU PARENT.DEP.`,`CODIGO DO PLANO`,
  `DATA INICIO VIGENCIA`,`TIPO DE LANÇAMENTO`,`PARTE DO SEGURADO`,
  `CARGO / OCUPACAO`,Matrícula) %>% summarise(Valor = sum(
    `VALOR DO LANCAMENTO`))

fat.tec06$Matrícula <- as.numeric(fat.tec06$Matrícula)

fat.tec07 <- readr:: read_fwf(
  file ="D:/Users/sb046971/Documents/Controles Administrativos/Bradesco/FATURA TÉCNICA 07.2020.TXT",
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

fat.tec07$`VALOR DO LANCAMENTO` <- fat.tec07$`VALOR DO LANCAMENTO`/100

fat.tec07$`VALOR DO LANCAMENTO` <- ifelse(
  fat.tec07$`CODIGO DO LANCAMENTO` >49,
  fat.tec07$`VALOR DO LANCAMENTO`*(-1),
  fat.tec07$`VALOR DO LANCAMENTO`)

fat.tec07 <- fat.tec07 %>% group_by(
  `TIPO DO REGISTRO`,`NUMERO DA SUBFATURA`,`NUMERO DO CERTIFICADO`,
  `COMPLEMENTO DO CERTIFICADO`,`NOME SEGURADO/DEPENDENTE`,`CODIGO DO SEXO`,
  `ESTADO CIVIL`,`COD. GRAU PARENT.DEP.`,`CODIGO DO PLANO`,
  `DATA INICIO VIGENCIA`,`TIPO DE LANÇAMENTO`,`PARTE DO SEGURADO`,
  `CARGO / OCUPACAO`,Matrícula) %>% summarise(Valor = sum(
    `VALOR DO LANCAMENTO`))

fat.tec07$Matrícula <- as.numeric(fat.tec07$Matrícula)

fat.tec08 <- readr:: read_fwf(
  file ="D:/Users/sb046971/Documents/Controles Administrativos/Bradesco/FATURA TÉCNICA 08.2020.TXT",
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

fat.tec08$`VALOR DO LANCAMENTO` <- fat.tec08$`VALOR DO LANCAMENTO`/100

fat.tec08$`VALOR DO LANCAMENTO` <- ifelse(
  fat.tec08$`CODIGO DO LANCAMENTO` >49,
  fat.tec08$`VALOR DO LANCAMENTO`*(-1),
  fat.tec08$`VALOR DO LANCAMENTO`)

fat.tec08 <- fat.tec08 %>% group_by(
  `TIPO DO REGISTRO`,`NUMERO DA SUBFATURA`,`NUMERO DO CERTIFICADO`,
  `COMPLEMENTO DO CERTIFICADO`,`NOME SEGURADO/DEPENDENTE`,`CODIGO DO SEXO`,
  `ESTADO CIVIL`,`COD. GRAU PARENT.DEP.`,`CODIGO DO PLANO`,
  `DATA INICIO VIGENCIA`,`TIPO DE LANÇAMENTO`,`PARTE DO SEGURADO`,
  `CARGO / OCUPACAO`,Matrícula) %>% summarise(Valor = sum(
    `VALOR DO LANCAMENTO`))

fat.tec08$Matrícula <- as.numeric(fat.tec08$Matrícula)

fat.tec09 <- readr:: read_fwf(
  file ="D:/Users/sb046971/Documents/Controles Administrativos/Bradesco/FATURA TÉCNICA 09.2020.TXT",
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

fat.tec09$`VALOR DO LANCAMENTO` <- fat.tec09$`VALOR DO LANCAMENTO`/100

fat.tec09$`VALOR DO LANCAMENTO` <- ifelse(
  fat.tec09$`CODIGO DO LANCAMENTO` >49,
  fat.tec09$`VALOR DO LANCAMENTO`*(-1),
  fat.tec09$`VALOR DO LANCAMENTO`)

fat.tec09 <- fat.tec09 %>% group_by(
  `TIPO DO REGISTRO`,`NUMERO DA SUBFATURA`,`NUMERO DO CERTIFICADO`,
  `COMPLEMENTO DO CERTIFICADO`,`NOME SEGURADO/DEPENDENTE`,`CODIGO DO SEXO`,
  `ESTADO CIVIL`,`COD. GRAU PARENT.DEP.`,`CODIGO DO PLANO`,
  `DATA INICIO VIGENCIA`,`TIPO DE LANÇAMENTO`,`PARTE DO SEGURADO`,
  `CARGO / OCUPACAO`,Matrícula) %>% summarise(Valor = sum(
    `VALOR DO LANCAMENTO`))

fat.tec09$Matrícula <- as.numeric(fat.tec09$Matrícula)

fat.tec10 <- readr:: read_fwf(
  file ="D:/Users/sb046971/Documents/Controles Administrativos/Bradesco/FATURA TÉCNICA 10.2020.TXT",
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

fat.tec10$`VALOR DO LANCAMENTO` <- fat.tec10$`VALOR DO LANCAMENTO`/100

fat.tec10$`VALOR DO LANCAMENTO` <- ifelse(
  fat.tec10$`CODIGO DO LANCAMENTO` >49,
  fat.tec10$`VALOR DO LANCAMENTO`*(-1),
  fat.tec10$`VALOR DO LANCAMENTO`)

fat.tec10 <- fat.tec10 %>% group_by(
  `TIPO DO REGISTRO`,`NUMERO DA SUBFATURA`,`NUMERO DO CERTIFICADO`,
  `COMPLEMENTO DO CERTIFICADO`,`NOME SEGURADO/DEPENDENTE`,`CODIGO DO SEXO`,
  `ESTADO CIVIL`,`COD. GRAU PARENT.DEP.`,`CODIGO DO PLANO`,
  `DATA INICIO VIGENCIA`,`TIPO DE LANÇAMENTO`,`PARTE DO SEGURADO`,
  `CARGO / OCUPACAO`,Matrícula) %>% summarise(Valor = sum(
    `VALOR DO LANCAMENTO`))

fat.tec10$Matrícula <- as.numeric(fat.tec10$Matrícula)

fat.tec11 <- readr:: read_fwf(
  file ="D:/Users/sb046971/Documents/Controles Administrativos/Bradesco/FATURA TÉCNICA 11.2020.TXT",
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

fat.tec11$`VALOR DO LANCAMENTO` <- fat.tec11$`VALOR DO LANCAMENTO`/100

fat.tec11$`VALOR DO LANCAMENTO` <- ifelse(
  fat.tec11$`CODIGO DO LANCAMENTO` >49,
  fat.tec11$`VALOR DO LANCAMENTO`*(-1),
  fat.tec11$`VALOR DO LANCAMENTO`)

fat.tec11 <- fat.tec11 %>% group_by(
  `TIPO DO REGISTRO`,`NUMERO DA SUBFATURA`,`NUMERO DO CERTIFICADO`,
  `COMPLEMENTO DO CERTIFICADO`,`NOME SEGURADO/DEPENDENTE`,`CODIGO DO SEXO`,
  `ESTADO CIVIL`,`COD. GRAU PARENT.DEP.`,`CODIGO DO PLANO`,
  `DATA INICIO VIGENCIA`,`TIPO DE LANÇAMENTO`,`PARTE DO SEGURADO`,
  `CARGO / OCUPACAO`,Matrícula) %>% summarise(Valor = sum(
    `VALOR DO LANCAMENTO`))

fat.tec11$Matrícula <- as.numeric(fat.tec11$Matrícula)

fat.tec12 <- readr:: read_fwf(
  file ="D:/Users/sb046971/Documents/Controles Administrativos/Bradesco/FATURA TÉCNICA 12.2020.TXT",
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

fat.tec12$`VALOR DO LANCAMENTO` <- fat.tec12$`VALOR DO LANCAMENTO`/100

fat.tec12$`VALOR DO LANCAMENTO` <- ifelse(
  fat.tec12$`CODIGO DO LANCAMENTO` >49,
  fat.tec12$`VALOR DO LANCAMENTO`*(-1),
  fat.tec12$`VALOR DO LANCAMENTO`)

fat.tec12 <- fat.tec12 %>% group_by(
  `TIPO DO REGISTRO`,`NUMERO DA SUBFATURA`,`NUMERO DO CERTIFICADO`,
  `COMPLEMENTO DO CERTIFICADO`,`NOME SEGURADO/DEPENDENTE`,`CODIGO DO SEXO`,
  `ESTADO CIVIL`,`COD. GRAU PARENT.DEP.`,`CODIGO DO PLANO`,
  `DATA INICIO VIGENCIA`,`TIPO DE LANÇAMENTO`,`PARTE DO SEGURADO`,
  `CARGO / OCUPACAO`,Matrícula) %>% summarise(Valor = sum(
    `VALOR DO LANCAMENTO`))

fat.tec12$Matrícula <- as.numeric(fat.tec12$Matrícula)

fat.tec04$mes <- "04/2020"
fat.tec05$mes <- "05/2020"
fat.tec06$mes <- "06/2020"
fat.tec07$mes <- "07/2020"
fat.tec08$mes <- "08/2020"
fat.tec09$mes <- "09/2020"
fat.tec10$mes <- "10/2020"
fat.tec11$mes <- "11/2020"
fat.tec12$mes <- "12/2020"

fat.tec <- bind_rows(fat.tec04,fat.tec05,fat.tec06,fat.tec07,
                     fat.tec08,fat.tec09,fat.tec10,fat.tec11,
                     fat.tec12)

fat.tec$elegibilidade <- ifelse(fat.tec$`NUMERO DO CERTIFICADO` == 0, 
                                fat.tec$`COD. GRAU PARENT.DEP.` == 9,
                                ifelse(fat.tec$`COD. GRAU PARENT.DEP.` == 0, 
                                "TITULAR", "DEPENDENTE"))

qtd.eleg <- fat.tec %>% group_by(`NUMERO DA SUBFATURA`,
                                 mes,elegibilidade) %>% mutate(
                                   n.pessoas = n()) %>% summarise(
                                     n.pessoas) %>% distinct() %>% filter(
                                       elegibilidade == "TITULAR") 

values <- fat.tec %>% group_by(`NUMERO DA SUBFATURA`,mes) %>% summarise(sum(Valor))

real.orc <- left_join(values,qtd.eleg)

real.orc <- real.orc %>% select(-elegibilidade)

real.orc$per.capita <- round(real.orc$`sum(Valor)`/real.orc$n.pessoas,2)

# tst <- real.orc %>% spread(mes,`sum(Valor)`)

fwrite(real.orc, file = "D:/Users/sb046971/Documents/realxorc.csv", dec = ",", sep = ";")

# unimed #

setwd("D:/Users/sb046971/Documents/Controles Administrativos/Unimed/04.2020/")

fat.uni04 <- list.files(pattern = ".CSV") %>%
  lapply(fread,dec = ",",colClasses = c("Cod Matricula Usuario" = "character",
                                        "Nome do Titular" = "character",
                                        "Nome Usuario" = "character",
                                        "Centro de Custo" = "character",
                                        "Descricao Tipo Usuario" = "character",
                                        "Contigo Antigo Usuario" = "character",
                                        "Codigo Modulo" = "character",
                                        "Valor Mod Coletivo" = "numeric",
                                        "Valor Mod Privativo" = "numeric",
                                        "Valor Beneficio Familia" = "numeric",
                                        "Valor Help" = "numeric",
                                        "Valor Manutencao Contrato" = "numeric",
                                        "Valor Fator Moderador" = "numeric",
                                        "Valor Receita Usuario" = "numeric",
                                        "NUMERO DA SUBFATURA" = "numeric"),
         stringsAsFactors=F, encoding="UTF-8", sep = ";",
         select=c("Cod Matricula Usuario","Nome do Titular","Nome Usuario",
                  "Centro de Custo","Descricao Tipo Usuario",
                  "Contigo Antigo Usuario","Codigo Modulo","Valor Mod Coletivo",
                  "Valor Mod Privativo","Valor Beneficio Familia",
                  "Valor Help","Valor Manutencao Contrato",
                  "Valor Fator Moderador","Valor Receita Usuario",
                  "NUMERO DA SUBFATURA"))  %>% bind_rows

fat.uni04$`Cod Matricula Usuario` <- str_replace_all(
  fat.uni04$`Cod Matricula Usuario`, "'", "")

names(fat.uni04)[names(
  fat.uni04) == "Cod Matricula Usuario"] <- "Matrícula"

fat.uni04$Matrícula <- as.numeric(fat.uni04$Matrícula)

setwd("D:/Users/sb046971/Documents/Controles Administrativos/Unimed/05.2020/")

fat.uni05 <- list.files(pattern = ".CSV") %>%
  lapply(fread,dec = ",",colClasses = c("Cod Matricula Usuario" = "character",
                                        "Nome do Titular" = "character",
                                        "Nome Usuario" = "character",
                                        "Centro de Custo" = "character",
                                        "Descricao Tipo Usuario" = "character",
                                        "Contigo Antigo Usuario" = "character",
                                        "Codigo Modulo" = "character",
                                        "Valor Mod Coletivo" = "numeric",
                                        "Valor Mod Privativo" = "numeric",
                                        "Valor Beneficio Familia" = "numeric",
                                        "Valor Help" = "numeric",
                                        "Valor Manutencao Contrato" = "numeric",
                                        "Valor Fator Moderador" = "numeric",
                                        "Valor Receita Usuario" = "numeric",
                                        "NUMERO DA SUBFATURA" = "numeric"),
         stringsAsFactors=F, encoding="UTF-8", sep = ";",
         select=c("Cod Matricula Usuario","Nome do Titular","Nome Usuario",
                  "Centro de Custo","Descricao Tipo Usuario",
                  "Contigo Antigo Usuario","Codigo Modulo","Valor Mod Coletivo",
                  "Valor Mod Privativo","Valor Beneficio Familia",
                  "Valor Help","Valor Manutencao Contrato",
                  "Valor Fator Moderador","Valor Receita Usuario",
                  "NUMERO DA SUBFATURA"))  %>% bind_rows

fat.uni05$`Cod Matricula Usuario` <- str_replace_all(
  fat.uni05$`Cod Matricula Usuario`, "'", "")

names(fat.uni05)[names(
  fat.uni05) == "Cod Matricula Usuario"] <- "Matrícula"

fat.uni05$Matrícula <- as.numeric(fat.uni05$Matrícula)

setwd("D:/Users/sb046971/Documents/Controles Administrativos/Unimed/06.2020/")

fat.uni06 <- list.files(pattern = ".CSV") %>%
  lapply(fread,dec = ",",colClasses = c("Cod Matricula Usuario" = "character",
                                        "Nome do Titular" = "character",
                                        "Nome Usuario" = "character",
                                        "Centro de Custo" = "character",
                                        "Descricao Tipo Usuario" = "character",
                                        "Contigo Antigo Usuario" = "character",
                                        "Codigo Modulo" = "character",
                                        "Valor Mod Coletivo" = "numeric",
                                        "Valor Mod Privativo" = "numeric",
                                        "Valor Beneficio Familia" = "numeric",
                                        "Valor Help" = "numeric",
                                        "Valor Manutencao Contrato" = "numeric",
                                        "Valor Fator Moderador" = "numeric",
                                        "Valor Receita Usuario" = "numeric",
                                        "NUMERO DA SUBFATURA" = "numeric"),
         stringsAsFactors=F, encoding="UTF-8", sep = ";",
         select=c("Cod Matricula Usuario","Nome do Titular","Nome Usuario",
                  "Centro de Custo","Descricao Tipo Usuario",
                  "Contigo Antigo Usuario","Codigo Modulo","Valor Mod Coletivo",
                  "Valor Mod Privativo","Valor Beneficio Familia",
                  "Valor Help","Valor Manutencao Contrato",
                  "Valor Fator Moderador","Valor Receita Usuario",
                  "NUMERO DA SUBFATURA"))  %>% bind_rows

fat.uni06$`Cod Matricula Usuario` <- str_replace_all(
  fat.uni06$`Cod Matricula Usuario`, "'", "")

names(fat.uni06)[names(
  fat.uni06) == "Cod Matricula Usuario"] <- "Matrícula"

fat.uni06$Matrícula <- as.numeric(fat.uni06$Matrícula)

setwd("D:/Users/sb046971/Documents/Controles Administrativos/Unimed/07.2020/")

fat.uni07 <- list.files(pattern = ".CSV") %>%
  lapply(fread,dec = ",",colClasses = c("Cod Matricula Usuario" = "character",
                                        "Nome do Titular" = "character",
                                        "Nome Usuario" = "character",
                                        "Centro de Custo" = "character",
                                        "Descricao Tipo Usuario" = "character",
                                        "Contigo Antigo Usuario" = "character",
                                        "Codigo Modulo" = "character",
                                        "Valor Mod Coletivo" = "numeric",
                                        "Valor Mod Privativo" = "numeric",
                                        "Valor Beneficio Familia" = "numeric",
                                        "Valor Help" = "numeric",
                                        "Valor Manutencao Contrato" = "numeric",
                                        "Valor Fator Moderador" = "numeric",
                                        "Valor Receita Usuario" = "numeric",
                                        "NUMERO DA SUBFATURA" = "numeric"),
         stringsAsFactors=F, encoding="UTF-8", sep = ";",
         select=c("Cod Matricula Usuario","Nome do Titular","Nome Usuario",
                  "Centro de Custo","Descricao Tipo Usuario",
                  "Contigo Antigo Usuario","Codigo Modulo","Valor Mod Coletivo",
                  "Valor Mod Privativo","Valor Beneficio Familia",
                  "Valor Help","Valor Manutencao Contrato",
                  "Valor Fator Moderador","Valor Receita Usuario",
                  "NUMERO DA SUBFATURA"))  %>% bind_rows

fat.uni07$`Cod Matricula Usuario` <- str_replace_all(
  fat.uni07$`Cod Matricula Usuario`, "'", "")

names(fat.uni07)[names(
  fat.uni07) == "Cod Matricula Usuario"] <- "Matrícula"

fat.uni07$Matrícula <- as.numeric(fat.uni07$Matrícula)

setwd("D:/Users/sb046971/Documents/Controles Administrativos/Unimed/08.2020/")

fat.uni08 <- list.files(pattern = ".CSV") %>%
  lapply(fread,dec = ",",colClasses = c("Cod Matricula Usuario" = "character",
                                        "Nome do Titular" = "character",
                                        "Nome Usuario" = "character",
                                        "Centro de Custo" = "character",
                                        "Descricao Tipo Usuario" = "character",
                                        "Contigo Antigo Usuario" = "character",
                                        "Codigo Modulo" = "character",
                                        "Valor Mod Coletivo" = "numeric",
                                        "Valor Mod Privativo" = "numeric",
                                        "Valor Beneficio Familia" = "numeric",
                                        "Valor Help" = "numeric",
                                        "Valor Manutencao Contrato" = "numeric",
                                        "Valor Fator Moderador" = "numeric",
                                        "Valor Receita Usuario" = "numeric",
                                        "NUMERO DA SUBFATURA" = "numeric"),
         stringsAsFactors=F, encoding="UTF-8", sep = ";",
         select=c("Cod Matricula Usuario","Nome do Titular","Nome Usuario",
                  "Centro de Custo","Descricao Tipo Usuario",
                  "Contigo Antigo Usuario","Codigo Modulo","Valor Mod Coletivo",
                  "Valor Mod Privativo","Valor Beneficio Familia",
                  "Valor Help","Valor Manutencao Contrato",
                  "Valor Fator Moderador","Valor Receita Usuario",
                  "NUMERO DA SUBFATURA"))  %>% bind_rows

fat.uni08$`Cod Matricula Usuario` <- str_replace_all(
  fat.uni08$`Cod Matricula Usuario`, "'", "")

names(fat.uni08)[names(
  fat.uni08) == "Cod Matricula Usuario"] <- "Matrícula"

fat.uni08$Matrícula <- as.numeric(fat.uni08$Matrícula)

setwd("D:/Users/sb046971/Documents/Controles Administrativos/Unimed/09.2020/")

fat.uni09 <- list.files(pattern = ".CSV") %>%
  lapply(fread,dec = ",",colClasses = c("Cod Matricula Usuario" = "character",
                                        "Nome do Titular" = "character",
                                        "Nome Usuario" = "character",
                                        "Centro de Custo" = "character",
                                        "Descricao Tipo Usuario" = "character",
                                        "Contigo Antigo Usuario" = "character",
                                        "Codigo Modulo" = "character",
                                        "Valor Mod Coletivo" = "numeric",
                                        "Valor Mod Privativo" = "numeric",
                                        "Valor Beneficio Familia" = "numeric",
                                        "Valor Help" = "numeric",
                                        "Valor Manutencao Contrato" = "numeric",
                                        "Valor Fator Moderador" = "numeric",
                                        "Valor Receita Usuario" = "numeric",
                                        "NUMERO DA SUBFATURA" = "numeric"),
         stringsAsFactors=F, encoding="UTF-8", sep = ";",
         select=c("Cod Matricula Usuario","Nome do Titular","Nome Usuario",
                  "Centro de Custo","Descricao Tipo Usuario",
                  "Contigo Antigo Usuario","Codigo Modulo","Valor Mod Coletivo",
                  "Valor Mod Privativo","Valor Beneficio Familia",
                  "Valor Help","Valor Manutencao Contrato",
                  "Valor Fator Moderador","Valor Receita Usuario",
                  "NUMERO DA SUBFATURA"))  %>% bind_rows

fat.uni09$`Cod Matricula Usuario` <- str_replace_all(
  fat.uni09$`Cod Matricula Usuario`, "'", "")

names(fat.uni09)[names(
  fat.uni09) == "Cod Matricula Usuario"] <- "Matrícula"

fat.uni09$Matrícula <- as.numeric(fat.uni09$Matrícula)

setwd("D:/Users/sb046971/Documents/Controles Administrativos/Unimed/10.2020/")

fat.uni10 <- list.files(pattern = ".CSV") %>%
  lapply(fread,dec = ",",colClasses = c("Cod Matricula Usuario" = "character",
                                        "Nome do Titular" = "character",
                                        "Nome Usuario" = "character",
                                        "Centro de Custo" = "character",
                                        "Descricao Tipo Usuario" = "character",
                                        "Contigo Antigo Usuario" = "character",
                                        "Codigo Modulo" = "character",
                                        "Valor Mod Coletivo" = "numeric",
                                        "Valor Mod Privativo" = "numeric",
                                        "Valor Beneficio Familia" = "numeric",
                                        "Valor Help" = "numeric",
                                        "Valor Manutencao Contrato" = "numeric",
                                        "Valor Fator Moderador" = "numeric",
                                        "Valor Receita Usuario" = "numeric",
                                        "NUMERO DA SUBFATURA" = "numeric"),
         stringsAsFactors=F, encoding="UTF-8", sep = ";",
         select=c("Cod Matricula Usuario","Nome do Titular","Nome Usuario",
                  "Centro de Custo","Descricao Tipo Usuario",
                  "Contigo Antigo Usuario","Codigo Modulo","Valor Mod Coletivo",
                  "Valor Mod Privativo","Valor Beneficio Familia",
                  "Valor Help","Valor Manutencao Contrato",
                  "Valor Fator Moderador","Valor Receita Usuario",
                  "NUMERO DA SUBFATURA"))  %>% bind_rows

fat.uni10$`Cod Matricula Usuario` <- str_replace_all(
  fat.uni10$`Cod Matricula Usuario`, "'", "")

names(fat.uni10)[names(
  fat.uni10) == "Cod Matricula Usuario"] <- "Matrícula"

fat.uni10$Matrícula <- as.numeric(fat.uni10$Matrícula)

setwd("D:/Users/sb046971/Documents/Controles Administrativos/Unimed/11.2020/")

fat.uni11 <- list.files(pattern = ".CSV") %>%
  lapply(fread,dec = ",",colClasses = c("Cod Matricula Usuario" = "character",
                                        "Nome do Titular" = "character",
                                        "Nome Usuario" = "character",
                                        "Centro de Custo" = "character",
                                        "Descricao Tipo Usuario" = "character",
                                        "Contigo Antigo Usuario" = "character",
                                        "Codigo Modulo" = "character",
                                        "Valor Mod Coletivo" = "numeric",
                                        "Valor Mod Privativo" = "numeric",
                                        "Valor Beneficio Familia" = "numeric",
                                        "Valor Help" = "numeric",
                                        "Valor Manutencao Contrato" = "numeric",
                                        "Valor Fator Moderador" = "numeric",
                                        "Valor Receita Usuario" = "numeric",
                                        "NUMERO DA SUBFATURA" = "numeric"),
         stringsAsFactors=F, encoding="UTF-8", sep = ";",
         select=c("Cod Matricula Usuario","Nome do Titular","Nome Usuario",
                  "Centro de Custo","Descricao Tipo Usuario",
                  "Contigo Antigo Usuario","Codigo Modulo","Valor Mod Coletivo",
                  "Valor Mod Privativo","Valor Beneficio Familia",
                  "Valor Help","Valor Manutencao Contrato",
                  "Valor Fator Moderador","Valor Receita Usuario",
                  "NUMERO DA SUBFATURA"))  %>% bind_rows

fat.uni11$`Cod Matricula Usuario` <- str_replace_all(
  fat.uni11$`Cod Matricula Usuario`, "'", "")

names(fat.uni11)[names(
  fat.uni11) == "Cod Matricula Usuario"] <- "Matrícula"

fat.uni11$Matrícula <- as.numeric(fat.uni11$Matrícula)

setwd("D:/Users/sb046971/Documents/Controles Administrativos/Unimed/12.2020/")

fat.uni12 <- list.files(pattern = ".CSV") %>%
  lapply(fread,dec = ",",colClasses = c("Cod Matricula Usuario" = "character",
                                        "Nome do Titular" = "character",
                                        "Nome Usuario" = "character",
                                        "Centro de Custo" = "character",
                                        "Descricao Tipo Usuario" = "character",
                                        "Contigo Antigo Usuario" = "character",
                                        "Codigo Modulo" = "character",
                                        "Valor Mod Coletivo" = "numeric",
                                        "Valor Mod Privativo" = "numeric",
                                        "Valor Beneficio Familia" = "numeric",
                                        "Valor Help" = "numeric",
                                        "Valor Manutencao Contrato" = "numeric",
                                        "Valor Fator Moderador" = "numeric",
                                        "Valor Receita Usuario" = "numeric",
                                        "NUMERO DA SUBFATURA" = "numeric"),
         stringsAsFactors=F, encoding="UTF-8", sep = ";",
         select=c("Cod Matricula Usuario","Nome do Titular","Nome Usuario",
                  "Centro de Custo","Descricao Tipo Usuario",
                  "Contigo Antigo Usuario","Codigo Modulo","Valor Mod Coletivo",
                  "Valor Mod Privativo","Valor Beneficio Familia",
                  "Valor Help","Valor Manutencao Contrato",
                  "Valor Fator Moderador","Valor Receita Usuario",
                  "NUMERO DA SUBFATURA"))  %>% bind_rows

fat.uni12$`Cod Matricula Usuario` <- str_replace_all(
  fat.uni12$`Cod Matricula Usuario`, "'", "")

names(fat.uni12)[names(
  fat.uni12) == "Cod Matricula Usuario"] <- "Matrícula"

fat.uni12$Matrícula <- as.numeric(fat.uni12$Matrícula)

fat.uni04$mes <- "04/2020"
fat.uni05$mes <- "05/2020"
fat.uni06$mes <- "06/2020"
fat.uni07$mes <- "07/2020"
fat.uni08$mes <- "08/2020"
fat.uni09$mes <- "09/2020"
fat.uni10$mes <- "10/2020"
fat.uni11$mes <- "11/2020"
fat.uni12$mes <- "12/2020"

fat.uni <- bind_rows(fat.uni05,fat.uni06,fat.uni07,fat.uni08,
                     fat.uni09,fat.uni10,fat.uni11)#,fat.uni12)

fat.uni.full <- left_join(fat.uni,relat.populis, by = "Matrícula")

fat.uni.full <- left_join(fat.uni.full,demitidos4, by = "Matrícula")

fat.uni.full$CCusto <- ifelse(is.na(fat.uni.full$CCusto.x),
                              fat.uni.full$CCusto.y,
                              fat.uni.full$CCusto.x)

fat.uni.full$CCusto.x <- NULL
fat.uni.full$CCusto.y <- NULL

fat.uni.full$Empresa <- ifelse(is.na(fat.uni.full$Empresa.x),
                               fat.uni.full$Empresa.y,
                               fat.uni.full$Empresa.x)

fat.uni.full$Empresa.x <- NULL
fat.uni.full$Empresa.y <- NULL


qtd.eleg <- fat.uni.full %>% group_by(`NUMERO DA SUBFATURA`,
                                 mes,`Descricao Tipo Usuario`) %>% mutate(
                                   n.pessoas = n()) %>% summarise(
                                     n.pessoas) %>% distinct() %>% filter(
                                       `Descricao Tipo Usuario` == "TITULAR") 

values <- fat.uni.full %>% group_by(`NUMERO DA SUBFATURA`,mes) %>% summarise(sum(`Valor Receita Usuario`))

real.orc <- left_join(values,qtd.eleg)

real.orc <- real.orc %>% select(-`Descricao Tipo Usuario`)

real.orc$per.capita <- round(real.orc$"sum(`Valor Receita Usuario`)"/real.orc$n.pessoas,2)

 tst <- real.orc %>% spread(mes,"sum(`Valor Receita Usuario`)")

fwrite(real.orc, file = "D:/Users/sb046971/Documents/realxorc2.csv", dec = ",", sep = ";")


# samel #

setwd("D:/Users/sb046971/Documents/Controles Administrativos/Samel/04.2020")

hca <- read_xlsx("1271 - HONDA COMPONENTES.xlsx")

hda <- read_xlsx("1270 - MOTO HONDA.xlsx", sheet = 1)

hca$CPF <- as.numeric(hca$CPF)
hca$RG <- as.numeric(hca$RG)
hda$PISPASEP <- as.character(hda$PISPASEP)
# # hca$MATRICULAFUNCIONAL <- as.numeric(hca$MATRICULAFUNCIONAL)

faturamento.samel <- bind_rows(hda,hca)

faturamento.samel$NOVO_TIPO <- if_else(grepl("Agregado",
                                             faturamento.samel$GRAU),
                                       "AGREGADO",
                                       if_else(grepl("Dependente",
                                                     faturamento.samel$GRAU),
                                               "DEPENDENTE",
                                               "TITULAR"))

names(faturamento.samel)[names(
  faturamento.samel) == "MATRICULAFUNCIONAL"] <- "Matrícula"

relat.samel04 <- left_join(faturamento.samel,relat.populis, by = "Matrícula")

relat.samel04 <- left_join(relat.samel04,demitidos4, by = "Matrícula")

relat.samel04$CCusto <- ifelse(is.na(relat.samel04$CCusto.x),
                             relat.samel04$CCusto.y,
                             relat.samel04$CCusto.x)

relat.samel04$CCusto.x <- NULL
relat.samel04$CCusto.y <- NULL

relat.samel04$Empresa <- ifelse(is.na(relat.samel04$Empresa.x),
                              relat.samel04$Empresa.y,
                              relat.samel04$Empresa.x)

relat.samel04$Empresa.x <- NULL
relat.samel04$Empresa.y <- NULL

relat.samel04$VALOR_BENEFICIARIO <- str_replace_all(
  relat.samel04$VALOR_BENEFICIARIO, ",", ".")

relat.samel04$VALOR_BENEFICIARIO <- as.numeric(relat.samel04$VALOR_BENEFICIARIO)

relat.samel04$elegibilidade <- "ENFERMARIA"

names(relat.samel04)[names(relat.samel04) == "CONTRATANTE"] <- "NUMERO DA SUBFATURA"

relat.samel04$mes <- "04/2020"

setwd("D:/Users/sb046971/Documents/Controles Administrativos/Samel/05.2020")

hca <- read_xlsx("1271 - HONDA COMPONENTES.xlsx")

hda <- read_xlsx("1270 - MOTO HONDA.xlsx")

# hca$CPF <- as.numeric(hca$CPF)
# hca$RG <- as.numeric(hca$RG)
# # 
hca$PISPASEP <- as.character(hca$PISPASEP)
# # hca$MATRICULAFUNCIONAL <- as.numeric(hca$MATRICULAFUNCIONAL)

faturamento.samel <- bind_rows(hda,hca)

faturamento.samel$NOVO_TIPO <- if_else(grepl("Agregado",
                                             faturamento.samel$GRAU),
                                       "AGREGADO",
                                       if_else(grepl("Dependente",
                                                     faturamento.samel$GRAU),
                                               "DEPENDENTE",
                                               "TITULAR"))

names(faturamento.samel)[names(
  faturamento.samel) == "MATRICULAFUNCIONAL"] <- "Matrícula"

relat.samel05 <- left_join(faturamento.samel,relat.populis, by = "Matrícula")

relat.samel05 <- left_join(relat.samel05,demitidos4, by = "Matrícula")

relat.samel05$CCusto <- ifelse(is.na(relat.samel05$CCusto.x),
                              relat.samel05$CCusto.y,
                              relat.samel05$CCusto.x)

relat.samel05$CCusto.x <- NULL
relat.samel05$CCusto.y <- NULL

relat.samel05$Empresa <- ifelse(is.na(relat.samel05$Empresa.x),
                                relat.samel05$Empresa.y,
                                relat.samel05$Empresa.x)

relat.samel05$Empresa.x <- NULL
relat.samel05$Empresa.y <- NULL

relat.samel05$VALOR_BENEFICIARIO <- str_replace_all(
  relat.samel05$VALOR_BENEFICIARIO, ",", ".")

relat.samel05$VALOR_BENEFICIARIO <- as.numeric(relat.samel05$VALOR_BENEFICIARIO)

relat.samel05$elegibilidade <- "ENFERMARIA"

names(relat.samel05)[names(relat.samel05) == "CONTRATANTE"] <- "NUMERO DA SUBFATURA"

relat.samel05$mes <- "05/2020"

setwd("D:/Users/sb046971/Documents/Controles Administrativos/Samel/06.2020")

hca <- read_xlsx("1271 - HONDA COMPONENTES.xlsx")

hda <- read_xlsx("1270 - MOTO HONDA.xlsx")

# hca$CPF <- as.numeric(hca$CPF)
# hca$RG <- as.numeric(hca$RG)
# # 
hda$PISPASEP <- as.character(hda$PISPASEP)
hca$PISPASEP <- as.character(hca$PISPASEP)
# # hca$MATRICULAFUNCIONAL <- as.numeric(hca$MATRICULAFUNCIONAL)

faturamento.samel <- bind_rows(hda,hca)

faturamento.samel$NOVO_TIPO <- if_else(grepl("Agregado",
                                             faturamento.samel$GRAU),
                                       "AGREGADO",
                                       if_else(grepl("Dependente",
                                                     faturamento.samel$GRAU),
                                               "DEPENDENTE",
                                               "TITULAR"))

names(faturamento.samel)[names(
  faturamento.samel) == "MATRICULAFUNCIONAL"] <- "Matrícula"

relat.samel06 <- left_join(faturamento.samel,relat.populis, by = "Matrícula")

relat.samel06 <- left_join(relat.samel06,demitidos4, by = "Matrícula")

relat.samel06$CCusto <- ifelse(is.na(relat.samel06$CCusto.x),
                              relat.samel06$CCusto.y,
                              relat.samel06$CCusto.x)

relat.samel06$CCusto.x <- NULL
relat.samel06$CCusto.y <- NULL

relat.samel06$Empresa <- ifelse(is.na(relat.samel06$Empresa.x),
                                relat.samel06$Empresa.y,
                                relat.samel06$Empresa.x)

relat.samel06$Empresa.x <- NULL
relat.samel06$Empresa.y <- NULL

relat.samel06$VALOR_BENEFICIARIO <- str_replace_all(
  relat.samel06$VALOR_BENEFICIARIO, ",", ".")

relat.samel06$VALOR_BENEFICIARIO <- as.numeric(relat.samel06$VALOR_BENEFICIARIO)

relat.samel06$elegibilidade <- "ENFERMARIA"

names(relat.samel06)[names(relat.samel06) == "CONTRATANTE"] <- "NUMERO DA SUBFATURA"

relat.samel06$mes <- "06/2020"


setwd("D:/Users/sb046971/Documents/Controles Administrativos/Samel/07.2020")

hca <- read_xlsx("1271 - HONDA COMPONENTES.xlsx")

hda <- read_xlsx("1270 - MOTO HONDA.xlsx")

# hca$CPF <- as.numeric(hca$CPF)
# hca$RG <- as.numeric(hca$RG)
# # 

hda$PISPASEP <- as.character(hda$PISPASEP)
hca$PISPASEP <- as.character(hca$PISPASEP)
# hda$PISPASEP <- as.character(hda$PISPASEP)
# # hca$MATRICULAFUNCIONAL <- as.numeric(hca$MATRICULAFUNCIONAL)

faturamento.samel <- bind_rows(hda,hca)

faturamento.samel$NOVO_TIPO <- if_else(grepl("Agregado",
                                             faturamento.samel$GRAU),
                                       "AGREGADO",
                                       if_else(grepl("Dependente",
                                                     faturamento.samel$GRAU),
                                               "DEPENDENTE",
                                               "TITULAR"))

names(faturamento.samel)[names(
  faturamento.samel) == "MATRICULA"] <- "Matrícula"

relat.samel07 <- left_join(faturamento.samel,relat.populis, by = "Matrícula")

relat.samel07 <- left_join(relat.samel07,demitidos4, by = "Matrícula")

relat.samel07$CCusto <- ifelse(is.na(relat.samel07$CCusto.x),
                               relat.samel07$CCusto.y,
                               relat.samel07$CCusto.x)

relat.samel07$CCusto.x <- NULL
relat.samel07$CCusto.y <- NULL

relat.samel07$Empresa <- ifelse(is.na(relat.samel07$Empresa.x),
                                relat.samel07$Empresa.y,
                                relat.samel07$Empresa.x)

relat.samel07$Empresa.x <- NULL
relat.samel07$Empresa.y <- NULL

relat.samel07$VALOR_BENEFICIARIO <- str_replace_all(
  relat.samel07$VALOR_BENEFICIARIO, ",", ".")

relat.samel07$VALOR_BENEFICIARIO <- as.numeric(relat.samel07$VALOR_BENEFICIARIO)

relat.samel07$elegibilidade <- "ENFERMARIA"

names(relat.samel07)[names(relat.samel07) == "CONTRATANTE"] <- "NUMERO DA SUBFATURA"

relat.samel07$mes <- "07/2020"


setwd("D:/Users/sb046971/Documents/Controles Administrativos/Samel/08.2020")

hca <- read_xlsx("1271 - HONDA COMPONENTES.xlsx")

hda <- read_xlsx("1270 - MOTO HONDA.xlsx")

# hca$CPF <- as.numeric(hca$CPF)
# hca$RG <- as.numeric(hca$RG)
hda$PISPASEP <- as.character(hda$PISPASEP)
hca$PISPASEP <- as.character(hca$PISPASEP)
# # hca$MATRICULAFUNCIONAL <- as.numeric(hca$MATRICULAFUNCIONAL)

faturamento.samel <- bind_rows(hda,hca)

faturamento.samel$NOVO_TIPO <- if_else(grepl("Agregado",
                                             faturamento.samel$GRAU),
                                       "AGREGADO",
                                       if_else(grepl("Dependente",
                                                     faturamento.samel$GRAU),
                                               "DEPENDENTE",
                                               "TITULAR"))

names(faturamento.samel)[names(
  faturamento.samel) == "MATRICULA"] <- "Matrícula"

relat.samel08 <- left_join(faturamento.samel,relat.populis, by = "Matrícula")

relat.samel08 <- left_join(relat.samel08,demitidos4, by = "Matrícula")

relat.samel08$CCusto <- ifelse(is.na(relat.samel08$CCusto.x),
                               relat.samel08$CCusto.y,
                               relat.samel08$CCusto.x)

relat.samel08$CCusto.x <- NULL
relat.samel08$CCusto.y <- NULL

relat.samel08$Empresa <- ifelse(is.na(relat.samel08$Empresa.x),
                                relat.samel08$Empresa.y,
                                relat.samel08$Empresa.x)

relat.samel08$Empresa.x <- NULL
relat.samel08$Empresa.y <- NULL

relat.samel08$VALOR_BENEFICIARIO <- str_replace_all(
  relat.samel08$VALOR_BENEFICIARIO, ",", ".")

relat.samel08$VALOR_BENEFICIARIO <- as.numeric(relat.samel08$VALOR_BENEFICIARIO)

relat.samel08$elegibilidade <- "ENFERMARIA"

names(relat.samel08)[names(relat.samel08) == "CONTRATANTE"] <- "NUMERO DA SUBFATURA"

relat.samel08$mes <- "08/2020"



setwd("D:/Users/sb046971/Documents/Controles Administrativos/Samel/09.2020")

hca <- read_xlsx("1271 - HONDA COMPONENTES.xlsx")

hda <- read_xlsx("1270 - MOTO HONDA.xlsx")

# hca$CPF <- as.numeric(hca$CPF)
# hca$RG <- as.numeric(hca$RG)
# # 
hda$PISPASEP <- as.character(hda$PISPASEP)
hca$PISPASEP <- as.character(hca$PISPASEP)
# hda$PISPASEP <- as.character(hda$PISPASEP)
# # hca$MATRICULAFUNCIONAL <- as.numeric(hca$MATRICULAFUNCIONAL)

faturamento.samel <- bind_rows(hda,hca)

faturamento.samel$NOVO_TIPO <- if_else(grepl("Agregado",
                                             faturamento.samel$GRAU),
                                       "AGREGADO",
                                       if_else(grepl("Dependente",
                                                     faturamento.samel$GRAU),
                                               "DEPENDENTE",
                                               "TITULAR"))

names(faturamento.samel)[names(
  faturamento.samel) == "MATRICULAFUNCIONAL"] <- "Matrícula"

relat.samel09 <- left_join(faturamento.samel,relat.populis, by = "Matrícula")

relat.samel09 <- left_join(relat.samel09,demitidos4, by = "Matrícula")

relat.samel09$CCusto <- ifelse(is.na(relat.samel09$CCusto.x),
                               relat.samel09$CCusto.y,
                               relat.samel09$CCusto.x)

relat.samel09$CCusto.x <- NULL
relat.samel09$CCusto.y <- NULL

relat.samel09$Empresa <- ifelse(is.na(relat.samel09$Empresa.x),
                                relat.samel09$Empresa.y,
                                relat.samel09$Empresa.x)

relat.samel09$Empresa.x <- NULL
relat.samel09$Empresa.y <- NULL

relat.samel09$VALOR_BENEFICIARIO <- str_replace_all(
  relat.samel09$VALOR_BENEFICIARIO, ",", ".")

relat.samel09$VALOR_BENEFICIARIO <- as.numeric(relat.samel09$VALOR_BENEFICIARIO)

relat.samel09$elegibilidade <- "ENFERMARIA"

names(relat.samel09)[names(relat.samel09) == "CONTRATANTE"] <- "NUMERO DA SUBFATURA"

relat.samel09$mes <- "09/2020"


setwd("D:/Users/sb046971/Documents/Controles Administrativos/Samel/10.2020")

hca <- read_xlsx("1271 - HONDA COMPONENTES.xlsx")

hda <- read_xlsx("1270 - MOTO HONDA.xlsx")

# hca$CPF <- as.numeric(hca$CPF)
# hca$RG <- as.numeric(hca$RG)
# # 
hda$PISPASEP <- as.character(hda$PISPASEP)
hca$PISPASEP <- as.character(hca$PISPASEP)
# hda$PISPASEP <- as.character(hda$PISPASEP)
# # hca$MATRICULAFUNCIONAL <- as.numeric(hca$MATRICULAFUNCIONAL)

faturamento.samel <- bind_rows(hda,hca)

faturamento.samel$NOVO_TIPO <- if_else(grepl("Agregado",
                                             faturamento.samel$GRAU),
                                       "AGREGADO",
                                       if_else(grepl("Dependente",
                                                     faturamento.samel$GRAU),
                                               "DEPENDENTE",
                                               "TITULAR"))

names(faturamento.samel)[names(
  faturamento.samel) == "MATRICULAFUNCIONAL"] <- "Matrícula"

relat.samel10 <- left_join(faturamento.samel,relat.populis, by = "Matrícula")

relat.samel10 <- left_join(relat.samel10,demitidos4, by = "Matrícula")

relat.samel10$CCusto <- ifelse(is.na(relat.samel10$CCusto.x),
                               relat.samel10$CCusto.y,
                               relat.samel10$CCusto.x)

relat.samel10$CCusto.x <- NULL
relat.samel10$CCusto.y <- NULL

relat.samel10$Empresa <- ifelse(is.na(relat.samel10$Empresa.x),
                                relat.samel10$Empresa.y,
                                relat.samel10$Empresa.x)

relat.samel10$Empresa.x <- NULL
relat.samel10$Empresa.y <- NULL

relat.samel10$VALOR_BENEFICIARIO <- str_replace_all(
  relat.samel10$VALOR_BENEFICIARIO, ",", ".")

relat.samel10$VALOR_BENEFICIARIO <- as.numeric(relat.samel10$VALOR_BENEFICIARIO)

relat.samel10$elegibilidade <- "ENFERMARIA"

names(relat.samel10)[names(relat.samel10) == "CONTRATANTE"] <- "NUMERO DA SUBFATURA"

relat.samel10$mes <- "10/2020"



setwd("D:/Users/sb046971/Documents/Controles Administrativos/Samel/11.2020")

hca <- read_xlsx("1271 - HONDA COMPONENTES.xlsx")

hda <- read_xlsx("1270 - MOTO HONDA.xlsx")

# hca$CPF <- as.numeric(hca$CPF)
# hca$RG <- as.numeric(hca$RG)
# # 
hda$PISPASEP <- as.character(hda$PISPASEP)
hca$PISPASEP <- as.character(hca$PISPASEP)
# hda$PISPASEP <- as.character(hda$PISPASEP)
# # hca$MATRICULAFUNCIONAL <- as.numeric(hca$MATRICULAFUNCIONAL)

faturamento.samel <- bind_rows(hda,hca)

faturamento.samel$NOVO_TIPO <- if_else(grepl("Agregado",
                                             faturamento.samel$GRAU),
                                       "AGREGADO",
                                       if_else(grepl("Dependente",
                                                     faturamento.samel$GRAU),
                                               "DEPENDENTE",
                                               "TITULAR"))

names(faturamento.samel)[names(
  faturamento.samel) == "MATRICULAFUNCIONAL...6"] <- "Matrícula"

relat.samel11 <- left_join(faturamento.samel,relat.populis, by = "Matrícula")

relat.samel11 <- left_join(relat.samel11,demitidos4, by = "Matrícula")

relat.samel11$CCusto <- ifelse(is.na(relat.samel11$CCusto.x),
                               relat.samel11$CCusto.y,
                               relat.samel11$CCusto.x)

relat.samel11$CCusto.x <- NULL
relat.samel11$CCusto.y <- NULL

relat.samel11$Empresa <- ifelse(is.na(relat.samel11$Empresa.x),
                                relat.samel11$Empresa.y,
                                relat.samel11$Empresa.x)

relat.samel11$Empresa.x <- NULL
relat.samel11$Empresa.y <- NULL

relat.samel11$VALOR_BENEFICIARIO <- str_replace_all(
  relat.samel11$VALOR_BENEFICIARIO, ",", ".")

relat.samel11$VALOR_BENEFICIARIO <- as.numeric(relat.samel11$VALOR_BENEFICIARIO)

relat.samel11$elegibilidade <- "ENFERMARIA"

names(relat.samel11)[names(relat.samel11) == "CONTRATANTE"] <- "NUMERO DA SUBFATURA"

relat.samel11$mes <- "11/2020"

setwd("D:/Users/sb046971/Documents/Controles Administrativos/Samel/12.2020")

hca <- read_xlsx("1271 - HONDA COMPONENTES.xlsx")

hda <- read_xlsx("1270 - MOTO HONDA.xlsx")

# hca$CPF <- as.numeric(hca$CPF)
# hca$RG <- as.numeric(hca$RG)
# # 
hda$PISPASEP <- as.character(hda$PISPASEP)
hca$PISPASEP <- as.character(hca$PISPASEP)
# hda$PISPASEP <- as.character(hda$PISPASEP)
# # hca$MATRICULAFUNCIONAL <- as.numeric(hca$MATRICULAFUNCIONAL)

faturamento.samel <- bind_rows(hda,hca)

faturamento.samel$NOVO_TIPO <- if_else(grepl("Agregado",
                                             faturamento.samel$GRAU),
                                       "AGREGADO",
                                       if_else(grepl("Dependente",
                                                     faturamento.samel$GRAU),
                                               "DEPENDENTE",
                                               "TITULAR"))

names(faturamento.samel)[names(
  faturamento.samel) == "MATRICULAFUNCIONAL"] <- "Matrícula"

relat.samel12 <- left_join(faturamento.samel,relat.populis, by = "Matrícula")

relat.samel12 <- left_join(relat.samel12,demitidos4, by = "Matrícula")

relat.samel12$CCusto <- ifelse(is.na(relat.samel12$CCusto.x),
                               relat.samel12$CCusto.y,
                               relat.samel12$CCusto.x)

relat.samel12$CCusto.x <- NULL
relat.samel12$CCusto.y <- NULL

relat.samel12$Empresa <- ifelse(is.na(relat.samel12$Empresa.x),
                                relat.samel12$Empresa.y,
                                relat.samel12$Empresa.x)

relat.samel12$Empresa.x <- NULL
relat.samel12$Empresa.y <- NULL

relat.samel12$VALOR_BENEFICIARIO <- str_replace_all(
  relat.samel12$VALOR_BENEFICIARIO, ",", ".")

relat.samel12$VALOR_BENEFICIARIO <- as.numeric(relat.samel12$VALOR_BENEFICIARIO)

relat.samel12$elegibilidade <- "ENFERMARIA"

names(relat.samel12)[names(relat.samel12) == "CONTRATANTE"] <- "NUMERO DA SUBFATURA"

relat.samel12$mes <- "12/2020"

fat.samel <- bind_rows(relat.samel04,relat.samel05,relat.samel06,relat.samel07,
                       relat.samel08,relat.samel09,relat.samel10,relat.samel11,
                       relat.samel12)


qtd.eleg <- fat.samel %>% group_by(`NUMERO DA SUBFATURA`,
                                      mes,NOVO_TIPO) %>% mutate(
                                        n.pessoas = n()) %>% summarise(
                                          n.pessoas) %>% distinct() %>% filter(
                                            `NOVO_TIPO` == "TITULAR") 

values <- fat.samel %>% group_by(`NUMERO DA SUBFATURA`,mes) %>% summarise(sum(VALOR_BENEFICIARIO))

real.orc <- left_join(values,qtd.eleg)

real.orc <- real.orc %>% select(-NOVO_TIPO)

real.orc$per.capita <- round(real.orc$`sum(VALOR_BENEFICIARIO)`/real.orc$n.pessoas,2)

# tst <- real.orc %>% spread(mes,`sum(Valor)`)

fwrite(real.orc, file = "D:/Users/sb046971/Documents/realxorc3.csv", dec = ",", sep = ";")

#### comparativo real ####

hda3q <- read_excel("D:/Users/sb046971/Documents/Controles Administrativos/Cargas 3QFCST/05. HDA_Carga_Premissas_Unidadde_CC_Conta_Padrao (002).xlsx")

real09 <- read_excel("D:/Users/sb046971/Documents/Controles Administrativos/PDCA - Saude.xlsx",sheet = 4)
real10 <- read_excel("D:/Users/sb046971/Documents/Controles Administrativos/PDCA - Saude.xlsx",sheet = 5)
real11 <- read_excel("D:/Users/sb046971/Documents/Controles Administrativos/PDCA - Saude.xlsx",sheet = 6)

real09$Plano <- ifelse(real09$Plano == "samel", "UNIMED",real09$Plano)
real09$Plano <- ifelse(real09$Plano == "bradesco", "BRADESCO",real09$Plano)

x6 <- real09 %>% filter(unid %in% c("0202M","0202S",
                                    "0228S","0208S")) %>% group_by(
                                      `Centro de Custo`,
                                      unid) %>% summarise(
                                        Tit = sum(Titulares),
                                        Vlr = sum(Valor),
                                        `Custo por Vida` = round(
                                          Vlr/Tit,2)) %>% select(-Tit,-Vlr)

x6$Plano <- "BRADESCO"

real09 <- real09 %>% filter(unid != "0202M") %>% filter(
  unid != "0202S")%>% filter(unid !="0228S")%>% filter(unid !="0208S")

real09 <- bind_rows(real09,x6)

real09 <- real09 %>% select(`Centro de Custo`,unid,Plano,`Custo por Vida`)

names(real09)[names(real09) == "Centro de Custo"] <- "CC"
names(real09)[names(real09) == "unid"] <- "Unidade"
names(real09)[names(real09) == "Custo por Vida"] <- "mes09"
names(real09)[names(real09) == "Plano"] <- "Metrica"

real09$CC <- as.numeric(real09$CC)

hda3q <- left_join(hda3q,real09, by = c("CC","Unidade","Metrica"))

### mes 10

real10$Plano <- ifelse(real10$Plano == "samel", "UNIMED",real10$Plano)
real10$Plano <- ifelse(real10$Plano == "bradesco", "BRADESCO",real10$Plano)

x6 <- real10 %>% filter(unid %in% c("0202M","0202S",
                                    "0228S","0208S")) %>% group_by(
                                      `Centro de Custo`,unid) %>% summarise(
                                        Tit = sum(Titulares),
                                        Vlr = sum(Valor),
                                        `Custo por Vida` = round(
                                          Vlr/Tit,2)) %>% select(-Tit,-Vlr)
x6$Plano <- "BRADESCO"

real10 <- real10 %>% filter(unid != "0202M") %>% filter(
  unid != "0202S")%>% filter(unid !="0228S")%>% filter(unid !="0208S")

real10 <- bind_rows(real10,x6)

real10 <- real10 %>% select(`Centro de Custo`,unid,Plano,`Custo por Vida`)

names(real10)[names(real10) == "Centro de Custo"] <- "CC"
names(real10)[names(real10) == "unid"] <- "Unidade"
names(real10)[names(real10) == "Custo por Vida"] <- "mes10"
names(real10)[names(real10) == "Plano"] <- "Metrica"

real10$CC <- as.numeric(real10$CC)

hda3q <- left_join(hda3q,real10, by = c("CC","Unidade","Metrica"))

### mes 11

real11$Plano <- ifelse(real11$Plano == "samel", "UNIMED",real11$Plano)
real11$Plano <- ifelse(real11$Plano == "bradesco", "BRADESCO",real11$Plano)

x6 <- real11 %>% filter(unid %in% c("0202M","0202S",
                                    "0228S","0208S")) %>% group_by(
                                      `Centro de Custo`,unid) %>% summarise(
                                        Tit = sum(Titulares),
                                        Vlr = sum(Valor),
                                        `Custo por Vida` = round(
                                          Vlr/Tit,2)) %>% select(-Tit,-Vlr)
x6$Plano <- "BRADESCO"

real11 <- real11 %>% filter(unid != "0202M") %>% filter(
  unid != "0202S")%>% filter(unid !="0228S")%>% filter(unid !="0208S")

real11 <- bind_rows(real11,x6)

real11 <- real11 %>% select(`Centro de Custo`,unid,Plano,`Custo por Vida`)

names(real11)[names(real11) == "Centro de Custo"] <- "CC"
names(real11)[names(real11) == "unid"] <- "Unidade"
names(real11)[names(real11) == "Custo por Vida"] <- "mes11"
names(real11)[names(real11) == "Plano"] <- "Metrica"

real11$CC <- as.numeric(real11$CC)

hda3q <- left_join(hda3q,real11, by = c("CC","Unidade","Metrica"))

hda3q$mes09 <- ifelse(is.na(hda3q$mes09),hda3q$Set,hda3q$mes09)
hda3q$mes10 <- ifelse(is.na(hda3q$mes10),hda3q$Out,hda3q$mes10)
hda3q$mes11 <- ifelse(is.na(hda3q$mes11),hda3q$Nov,hda3q$mes11)

fwrite(hda3q,file = "D:/Users/sb046971/Documents/Controles Administrativos/Novas Cargas/hda3q.csv", 
       sep = "|", dec = ",")

hab3q <- read_excel("D:/Users/sb046971/Documents/Controles Administrativos/Cargas 3QFCST/05. HAB_Carga_Premissas_Unidadde_CC_Conta_Padrao (002).xlsx")

real09 <- read_excel("D:/Users/sb046971/Documents/Controles Administrativos/PDCA - Saude.xlsx",sheet = 4)
real10 <- read_excel("D:/Users/sb046971/Documents/Controles Administrativos/PDCA - Saude.xlsx",sheet = 5)
real11 <- read_excel("D:/Users/sb046971/Documents/Controles Administrativos/PDCA - Saude.xlsx",sheet = 6)

real09$Plano <- ifelse(real09$Plano == "unimed", "UNIMED",real09$Plano)
real09$Plano <- ifelse(real09$Plano == "bradesco", "BRADESCO",real09$Plano)

x7 <- real09 %>% filter(unid %in% c("514","512S")) %>% group_by(
                                      `Centro de Custo`,
                                      unid) %>% summarise(
                                        Tit = sum(Titulares),
                                        Vlr = sum(Valor),
                                        `Custo por Vida` = round(
                                          Vlr/Tit,2)) %>% select(-Tit,-Vlr)

x7$Plano <- "BRADESCO"

real09 <- real09 %>% filter(unid != "514") %>% filter(unid != "512S")

real09 <- bind_rows(real09,x7)

real09 <- real09 %>% select(`Centro de Custo`,unid,Plano,`Custo por Vida`)

names(real09)[names(real09) == "Centro de Custo"] <- "CC"
names(real09)[names(real09) == "unid"] <- "Unidade"
names(real09)[names(real09) == "Custo por Vida"] <- "mes09"
names(real09)[names(real09) == "Plano"] <- "Metrica"

real09$CC <- as.numeric(real09$CC)

hab3q <- left_join(hab3q,real09, by = c("CC","Unidade","Metrica"))

### mes 10

real10$Plano <- ifelse(real10$Plano == "unimed", "UNIMED",real10$Plano)
real10$Plano <- ifelse(real10$Plano == "bradesco", "BRADESCO",real10$Plano)

x7 <- real10 %>% filter(unid %in% c("514","512S")) %>% group_by(
  `Centro de Custo`,
  unid) %>% summarise(
    Tit = sum(Titulares),
    Vlr = sum(Valor),
    `Custo por Vida` = round(
      Vlr/Tit,2)) %>% select(-Tit,-Vlr)

x7$Plano <- "BRADESCO"

real10 <- real10 %>% filter(unid != "514") %>% filter(unid != "512S")

real10 <- bind_rows(real10,x7)

real10 <- real10 %>% select(`Centro de Custo`,unid,Plano,`Custo por Vida`)

names(real10)[names(real10) == "Centro de Custo"] <- "CC"
names(real10)[names(real10) == "unid"] <- "Unidade"
names(real10)[names(real10) == "Custo por Vida"] <- "mes10"
names(real10)[names(real10) == "Plano"] <- "Metrica"

real10$CC <- as.numeric(real10$CC)

hab3q <- left_join(hab3q,real10, by = c("CC","Unidade","Metrica"))

### mes 11

real11$Plano <- ifelse(real11$Plano == "unimed", "UNIMED",real11$Plano)
real11$Plano <- ifelse(real11$Plano == "bradesco", "BRADESCO",real11$Plano)

x7 <- real11 %>% filter(unid %in% c("514","512S")) %>% group_by(
  `Centro de Custo`,
  unid) %>% summarise(
    Tit = sum(Titulares),
    Vlr = sum(Valor),
    `Custo por Vida` = round(
      Vlr/Tit,2)) %>% select(-Tit,-Vlr)

x7$Plano <- "BRADESCO"

real11 <- real11 %>% filter(unid != "514") %>% filter(unid != "512S")

real11 <- bind_rows(real11,x7)

real11 <- real11 %>% select(`Centro de Custo`,unid,Plano,`Custo por Vida`)

names(real11)[names(real11) == "Centro de Custo"] <- "CC"
names(real11)[names(real11) == "unid"] <- "Unidade"
names(real11)[names(real11) == "Custo por Vida"] <- "mes11"
names(real11)[names(real11) == "Plano"] <- "Metrica"

real11$CC <- as.numeric(real11$CC)

hab3q <- left_join(hab3q,real11, by = c("CC","Unidade","Metrica"))

fwrite(hab3q,file = "D:/Users/sb046971/Documents/Controles Administrativos/Cargas 3QFCST/hab3q.csv", 
       sep = "|", dec = ",")
