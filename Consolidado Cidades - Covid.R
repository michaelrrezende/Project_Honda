#### PACKAGES ####

require(dplyr)
require(data.table)
require(readxl)

#### juncao ####

setwd("D:/Users/sb046971/OneDrive - Honda/Desktop/")
  
options(scipen = 100, digits = 4)

sao.paulo <- read_excel("Dashboard Covid-19.xlsx", 
                        sheet = "São Paulo")

sao.caetano <- read_excel("Dashboard Covid-19.xlsx", 
                          sheet = "São Caetano do Sul")

recife <- read_excel("Dashboard Covid-19.xlsx", 
                     sheet = "Recife")

indaiatuba <- read_excel("Dashboard Covid-19.xlsx", 
                         sheet = "Indaiatuba")

jaboatao <- read_excel("Dashboard Covid-19.xlsx", 
                       sheet = "Jaboatão dos Guararapes")

campinas <- read_excel("Dashboard Covid-19.xlsx", 
                       sheet = "Campinas")

sumare <- read_excel("Dashboard Covid-19.xlsx", 
                     sheet = "Sumaré")

sao.carlos <- read_excel("Dashboard Covid-19.xlsx", 
                         sheet = "São Carlos")

rio.claro <- read_excel("Dashboard Covid-19.xlsx", 
                        sheet = "Rio Claro")

itirapina <- read_excel("Dashboard Covid-19.xlsx",
                        sheet = "Itirapina")

consolidado.cidades <- bind_rows(sao.paulo,sao.caetano,recife,indaiatuba,
                                 jaboatao,campinas,sumare,sao.carlos,rio.claro,
                                 itirapina)

consolidado.cidades$`Contamination Rate` <- round(
  consolidado.cidades$`Contamination Rate`,4)
consolidado.cidades$`Death Rate (100.000 hab)` <- round(
  consolidado.cidades$`Death Rate (100.000 hab)`,4)

fwrite(consolidado.cidades, 
       file = "D:/Users/sb046971/OneDrive - Honda/Desktop/consolidado_cidades.csv", 
       sep = ";", dec = ",",scipen = 100, bom = T, dateTimeAs = "write.csv")

##### writer #R ####

require(EpiEstim)

## carregar dados sobre pandemia de gripe em uma escola em 

data("Flu2009")

## estimar o número de reprodução (método "non_parametric_si") 
## ao não especificar t_start e t_end na configuração, eles são definidos para estimar 
## o número de reprodução em janelas semanais deslizantes 


res <- estimate_R(incid = Flu2009$incidence, 
                  method = "non_parametric_si",
                  config = make_config(list(si_distr = Flu2009$si_distr)))
plot(res)

## a segunda parcela produzida mostra, a cada dia, 
## a estimativa do número de reprodução ao longo da janela de 7 dias 
## terminando nesse dia.

## para especificar t_start e t_end na configuração, por exemplo, 
#ter janelas deslizantes quinzenais

t_start <- seq(2, nrow(Flu2009$incidence)-13)   
t_end <- t_start + 13                 
res <- estimate_R(incid = Flu2009$incidence, 
                  method = "non_parametric_si",
                  config = make_config(list(
                    si_distr = Flu2009$si_distr, 
                    t_start = t_start, 
                    t_end = t_end)))
plot(res)

## a segunda parcela produzida mostra, a cada dia, 
## a estimativa do número de reprodução ao longo da janela de 14 dias 
## terminando nesse dia.