#### PACKAGES ####

require(dplyr)
require(data.table)
require(ggplot2)
require(RColorBrewer)
require(readr)
require(stringr)
require(chron)
require(extrafont)

#### CHANGE CUT CENTURY FOR AGE // BECAUSE THE DEFAULT IS 1970 - 2070 ####

options(chron.year.expand =
          function (y, cut.off = 20, century = c(1900, 2000), ...) {
            chron:::year.expand(y, cut.off = cut.off, century = century, ...)
          }
)

#### WIDTHS AND NAMES LAYOUT SULAMERICA ####

widths = c(2,4,16,32,15,16,32,2,17,6,8,50,4,1,3,1,12,35,13,13,13,13,13,6,20,
            5,1,14,1,6,1,3,1,7,3,9,1,5,2,4,1,6,8,6,6,13,5)

names = c("CodigoRegistro","EmpresaPagadora","CodBenefTitular",
           "NomeBenefTitular","LocalTitular","CodUsuarioServico",
           "NomeUsuario","CategoriaPlano","NumeroConta","DataAtendimento",
           "CodigoServicoRealizado","DescricaoServico","GrupoEstatistico",
           "TipoInternacao","DiasInternados","Alta","CodPrestador",
           "NomePrestador","ValorApresentado","ValorPago",
           "ValorParteEmpresa","ValorParteEmpregado","ValorParteAntecipada",
           "DataPagamento","Espaço","CodigoPlano","Agregado","CGCCPF",
           "PrestadorTipo","DataRecebimentoConta","Posicao","QtdeServicos",
           "Espaço1","ValorUS","Idade","CodDiagnostico","CodAtendimento",
           "CodEspecPrincipal","CodDenteArea","CodFaces","PrefixoEmpresa",
           "CentroCusto","CodServicoPrincipal","InicioCobranca",
           "FimCobranca","Espaço2","Empresa")

#### READ ALL MONTHS AND TREATMENT DATES ####

sulamericatxt <- readr:: read_fwf(file = 
                "D:/Users/sb046971/Documents/Sinistro Sulamerica/66727.txt", 
                fwf_widths(widths, col_names = names), skip_empty_rows = T,skip = 1,
                col_types = cols("CodigoRegistro" = col_character(),
                                 "EmpresaPagadora" = col_character(),
                                 "CodBenefTitular" = col_character(),
                                 "NomeBenefTitular" = col_character(),
                                 "LocalTitular" = col_character(),
                                 "CodUsuarioServico" = col_character(),
                                 "NomeUsuario" = col_character(),
                                 "CategoriaPlano" = col_character(),
                                 "NumeroConta" = col_character(),
                                 "DataAtendimento" = col_character(),
                                 "CodigoServicoRealizado" = col_character(),
                                 "DescricaoServico" = col_character(),
                                 "GrupoEstatistico" = col_character(),
                                 "TipoInternacao" = col_character(),
                                 "DiasInternados" = col_integer(),
                                 "Alta" = col_character(),
                                 "CodPrestador" = col_character(),
                                 "NomePrestador" = col_character(),
                                 "ValorApresentado" = col_integer(),
                                 "ValorPago" = col_integer(),
                                 "ValorParteEmpresa" = col_integer(),
                                 "ValorParteEmpregado" = col_integer(),
                                 "ValorParteAntecipada" = col_integer(),
                                 "DataPagamento" = col_character(),
                                 "Espaço" = col_character(),
                                 "CodigoPlano" = col_character(),
                                 "Agregado" = col_character(),
                                 "CGCCPF" = col_character(),
                                 "PrestadorTipo" = col_character(),
                                 "DataRecebimentoConta" = col_character(),
                                 "Posicao" = col_character(),
                                 "QtdeServicos" = col_integer(),
                                 "Espaço1" = col_character(),
                                 "ValorUS" = col_integer(),
                                 "Idade" = col_integer(),
                                 "CodDiagnostico" = col_character(),
                                 "CodAtendimento" = col_character(),
                                 "CodEspecPrincipal" = col_character(),
                                 "CodDenteArea" = col_character(),
                                 "CodFaces" = col_character(),
                                 "PrefixoEmpresa" = col_character(),
                                 "CentroCusto" = col_character(),
                                 "CodServicoPrincipal" = col_character(),
                                 "InicioCobranca" = col_character(),
                                 "FimCobranca" = col_character(),
                                 "Espaço2" = col_character(),
                                 "Empresa" = col_character()))

sulamericatxt$ValorApresentado <- sulamericatxt$ValorApresentado/100
sulamericatxt$ValorPago <- sulamericatxt$ValorPago/100
sulamericatxt$ValorParteAntecipada <- sulamericatxt$ValorParteAntecipada/100
sulamericatxt$ValorParteEmpregado <- sulamericatxt$ValorParteEmpregado/100
sulamericatxt$ValorParteEmpresa <- sulamericatxt$ValorParteEmpresa/100

sulamericatxt$DataAtendimento <- as.Date(chron(format(as.Date(
  sulamericatxt$DataAtendimento, format = "%y%m%d"),"%m/%d/%y")))

sulamericatxt$DataPagamento <- as.Date(chron(format(as.Date(
  sulamericatxt$DataPagamento, format = "%y%m%d"),"%m/%d/%y")))

sulamericatxt$DataRecebimentoConta <- as.Date(chron(format(as.Date(
  sulamericatxt$DataRecebimentoConta, format = "%y%m%d"),"%m/%d/%y")))

#### SELECT DUPLICATED LINES TO DATA AND DROP ####

sulamericatxt$DUPLICADOS <- duplicated(sulamericatxt)

fwrite(sulamericatxt, 
       file = "d:/Users/sb046971/Documents/BaseSulamerica.csv", 
       sep = "|", dec = ",")

sulamerica_sd <- sulamericatxt  %>% select(-CodigoRegistro,-EmpresaPagadora,
                                           -CategoriaPlano,-Idade,
                                           -ValorParteEmpregado,
                                           -ValorParteAntecipada,-Espaço,-CodigoPlano,
                                           -Agregado,-Posicao,-Espaço1,-ValorUS,
                                           -CodDiagnostico,-CodDenteArea,-CodFaces,
                                           -PrefixoEmpresa,-CentroCusto,
                                           -CodServicoPrincipal,-InicioCobranca,
                                           -FimCobranca,-Espaço2)

sulamerica_sd$DUPLICADOS <- duplicated(sulamerica_sd)

sulamerica_sd %>% filter(DUPLICADOS == "TRUE") %>% group_by(.
                                                            ) %>% summarise(sum(ValorPago))

#### TREATMENT DATABASE ####

sulamerica_sd$Competencia <- substr(sulamerica_sd$DataPagamento, start = 1, stop = 7)

sulamerica_sd %>% group_by(Competencia) %>% summarise(sum(ValorPago))

sulamerica_sd$diaEvxPg <- difftime(sulamerica_sd$DataPagamento,
                                   sulamerica_sd$DataAtendimento,units = "days")

sulamerica_sd$diaEvxPg <- as.numeric(sulamerica_sd$diaEvxPg)

sulamerica_sd$mesEvxPg <-floor((as.double(sulamerica_sd$diaEvxPg)/365)*12)

sulamerica_sd$flag12meses <- if_else(sulamerica_sd$mesEvxPg > 11,"+","0")

sulamerica_sd$flag4meses <- if_else(sulamerica_sd$diaEvxPg > 120,"+","0")

sulamerica_sd$flag6meses <- if_else(sulamerica_sd$diaEvxPg > 180,"+","0")

#### ANALYSIS IN DATA ####

table(sulamerica_sd$flag12meses)

table(sulamerica_sd$flag4meses)

table(sulamerica_sd$flag6meses)

sulamerica_sd %>% filter(flag12meses == "+") %>% 
  group_by(.) %>% summarise(sum(ValorPago))

sulamerica_sd %>% filter(flag4meses == "+") %>% 
  group_by(.) %>% summarise(sum(ValorPago))

sulamerica_sd %>% filter(flag6meses == "+") %>% 
  group_by(.) %>% summarise(sum(ValorPago))


fwrite(sulamerica_sd, file = "D:/Users/sb046971/Documents/sulamericasd.csv",
       sep = "|", dec = ",")

analysis1 <- sulamerica_sd %>% group_by(CodBenefTitular,NomePrestador) %>% summarise(
                                        med_dias = mean(diaEvxPg),
                                        med_meses = mean(mesEvxPg),
                                        valor_sinistro = sum(ValorPago))

analysis2 <- sulamerica_sd %>% group_by(NomePrestador,mesEvxPg) %>% summarise(
  valor_sinistro = sum(ValorPago))

analysis3 <- sulamerica_sd %>% group_by(NomePrestador) %>% summarise(
  med_dias = mean(diaEvxPg),
  med_meses = mean(mesEvxPg),
  valor_sinistro = sum(ValorPago))

analysis4 <- sulamerica_sd %>% group_by(CodigoServicoRealizado,diaEvxPg) %>% summarise(
  cont_proc = sum(QtdeServicos)) 

analysis4$flag <- if_else(analysis4$diaEvxPg > 120, "+","0")

analysis5 <- analysis4 %>% filter(CodigoServicoRealizado == "10101012" & 
                                      flag == "+") %>% group_by(
                                        CodigoServicoRealizado) %>% summarise(
                                          qtde_proc = sum(cont_proc))

analysis6 <- analysis4 %>% filter(flag == "+") %>% group_by(
  CodigoServicoRealizado) %>% summarise(qtde_proc = sum(cont_proc))

analysis7 <- sulamerica_sd %>% group_by(CodigoServicoRealizado,diaEvxPg) %>% summarise(
  cont_proc = n())

analysis7$flag <- if_else(analysis7$diaEvxPg > 120, "+","0")

analysis8 <- analysis7 %>% filter(CodigoServicoRealizado == "10101012" & 
                                      flag == "+") %>% group_by(
                                        ) %>% summarise(
                                          qtde_proc = sum(cont_proc))

analysis9 <- analysis7 %>% filter(flag == "+") %>% group_by(
  CodigoServicoRealizado) %>% summarise(qtde_proc = sum(cont_proc))

analysis10 <- sulamerica_sd %>% select(NomePrestador,
                                     diaEvxPg,mesEvxPg,
                                     ValorPago) %>% filter(diaEvxPg > 120)

sum(analysis10$ValorPago) ## grafico

analysis11 <- sulamerica_sd %>% select(NomePrestador,diaEvxPg,
                                       mesEvxPg,ValorPago) %>% filter(diaEvxPg < 6000)

sum(analysis11$ValorPago)

analysis12 <- sulamerica_sd %>% filter(
  CodigoServicoRealizado == "10101039") %>% group_by(NomeUsuario) %>% summarise(
    qt_cons = sum(QtdeServicos),valor = sum(ValorPago))

analysis13 <- sulamerica_sd %>% filter(
  CodigoServicoRealizado == "10101012") %>% group_by(NomeUsuario) %>% summarise(
    qt_cons = sum(QtdeServicos),valor = sum(ValorPago))

analysis14 <- sulamerica_sd %>% filter(
  CodigoServicoRealizado == "10106146") %>% group_by(NomeUsuario) %>% summarise(
    qt_cons = sum(QtdeServicos),valor = sum(ValorPago))

analysis15 <- sulamerica_sd %>% group_by(NomeUsuario) %>% summarise(sum(ValorPago))

# high_users <- sulamerica_sd %>% filter(NomeUsuario == "ADEMILSON APARECIDO SANTI" |
# NomeUsuario == "SIMONE BASTOS REZENDE MORE" | 
# NomeUsuario == "JOCIANE DA SILVA DE AGUIAR" |
#   NomeUsuario == "TAKASHI KIYONAGA" | NomeUsuario == "ARMANDO ERIK DOMINGOS DE C" | 
#   NomeUsuario == "KELLY MOREIRA MALAVASI" | NomeUsuario == "EDSON ASSIS DA COSTA" | 
#   NomeUsuario == "ARAO COSTA FIALHO" | NomeUsuario == "ALECSANDRE FLORES MIRANDA" | 
#   NomeUsuario == "MAIKON SANTOS BEZERRA") 

# fwrite(high_users, 
#        file = "D:/Users/sb046971/Documents/Sinistro Sulamerica/high_users.csv",
#        sep = "|", dec = ",")

#### GRAPH DAYS AND VALUES ####

ggplot(analysis11,aes(diaEvxPg,ValorPago)) + 
  geom_point(mapping = aes(analysis11$diaEvxPg,analysis11$ValorPago)) + 
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
cost.p.person <- bradesco_sd %>% group_by(NomeUsuario) %>% summarise(
  valor = sum(`VALOR DO SINISTRO`))
