mes0119 <- fread(
  file ="D:/Users/sb046971/Documents/Sinistro Bradesco/SN1901_D071015.txt",
  col.names = names, dec = ",",
  colClasses = c("TIPO DA SUBFATURA" = "character",
                 "NÚMERO DA SUBFATURA" = "character",
                 "CPF/CGC DO REFERENCIADO" = "character",
                 "NÚMERO DO CERTIFICADO" = "character",
                 "MATRICULA" = "character",
                 "CÓDIGO DO PACIENTE" = "character",
                 "CÓDIGO DO PROCEDIMENTO" = "character",
                 "DATA DO EVENTO" = "character",
                 "NÚMERO DO CONTRATO" = "character",
                 "CÓDIGO DO REFERENCIADO" = "character",
                 "DATA DO PAGAMENTO" = "character",
                 "DATA DE ADMISSAO" = "character",
                 "DATA DE NASCIMENTO" = "character",
                 "DATA DE NASCIMENTO(Y2K)" = "character",
                 "DATA DO EVENTO(Y2K)" = "character",
                 "DATA DO PAGAMENTO(Y2K)" = "character",
                 "ESPECIALIDADE" = "character",
                 "TIPO DE EVENTO" = "character",
                 "CODIGO DE AUTORIZACAO" = "character",
                 "CDB" = "character")) %>% filter(
                   !str_detect(`TIPO DE REGISTRO`, "T"))

## AAMMDD

mes0119$`DATA DO PAGAMENTO` <- as.character(
  mes0119$`DATA DO PAGAMENTO`)

mes0119$`DATA DO PAGAMENTO` <- as.Date(chron(format(as.Date(
  mes0119$`DATA DO PAGAMENTO`, format = "%d%m%y"),"%m/%d/%y")))

mes0119$`DATA DE NASCIMENTO` <- as.character(
  mes0119$`DATA DE NASCIMENTO`)

mes0119$`DATA DE NASCIMENTO` <- as.Date(chron(format(as.Date(
  mes0119$`DATA DE NASCIMENTO`, format = "%d%m%y"),"%m/%d/%y")))

mes0119$`DATA DO EVENTO` <- as.character(
  mes0119$`DATA DO EVENTO`)

mes0119$`DATA DO EVENTO` <- as.Date(chron(format(as.Date(
  mes0119$`DATA DO EVENTO`, format = "%d%m%y"),"%m/%d/%y")))

#### AAAAMMDD

mes0119$`DATA DE ADMISSÃO` <- as.character(
  mes0119$`DATA DE ADMISSÃO`)

mes0119$`DATA DE ADMISSÃO` <- as.Date(chron(format(as.Date(
  mes0119$`DATA DE ADMISSÃO`, format = "%d%m%Y"),"%m/%d/%y")))

mes0119$`DATA DE NASCIMENTO(Y2K)` <- as.character(
  mes0119$`DATA DE NASCIMENTO(Y2K)`)

mes0119$`DATA DE NASCIMENTO(Y2K)` <- as.Date(chron(format(as.Date(
  mes0119$`DATA DE NASCIMENTO(Y2K)`, format = "%d%m%Y"),"%m/%d/%y")))

mes0119$`DATA DO EVENTO(Y2K)` <- as.character(
  mes0119$`DATA DO EVENTO(Y2K)`)

mes0119$`DATA DO EVENTO(Y2K)` <- as.Date(chron(format(as.Date(
  mes0119$`DATA DO EVENTO(Y2K)`, format = "%d%m%Y"),"%m/%d/%y")))

mes0119$`DATA DO PAGAMENTO(Y2K)` <- as.character(
  mes0119$`DATA DO PAGAMENTO(Y2K)`)

mes0119$`DATA DO PAGAMENTO(Y2K)` <- as.Date(chron(format(as.Date(
  mes0119$`DATA DO PAGAMENTO(Y2K)`, format = "%d%m%Y"),"%m/%d/%y")))

mes0219 <- fread(
  file ="D:/Users/sb046971/Documents/Sinistro Bradesco/SN1902_D071015.txt",
  col.names = names, dec = ",",
  colClasses = c("TIPO DA SUBFATURA" = "character",
                 "NÚMERO DA SUBFATURA" = "character",
                 "CPF/CGC DO REFERENCIADO" = "character",
                 "NÚMERO DO CERTIFICADO" = "character",
                 "MATRICULA" = "character",
                 "CÓDIGO DO PACIENTE" = "character",
                 "CÓDIGO DO PROCEDIMENTO" = "character",
                 "DATA DO EVENTO" = "character",
                 "NÚMERO DO CONTRATO" = "character",
                 "CÓDIGO DO REFERENCIADO" = "character",
                 "DATA DO PAGAMENTO" = "character",
                 "DATA DE NASCIMENTO" = "character",
                 "DATA DE ADMISSAO" = "character",
                 "DATA DE NASCIMENTO(Y2K)" = "character",
                 "DATA DO EVENTO(Y2K)" = "character",
                 "DATA DO PAGAMENTO(Y2K)" = "character",
                 "ESPECIALIDADE" = "character",
                 "TIPO DE EVENTO" = "character",
                 "CODIGO DE AUTORIZACAO" = "character",
                 "CDB" = "character")) %>% filter(
                   !str_detect(`TIPO DE REGISTRO`, "T"))

## AAMMDD

mes0219$`DATA DO PAGAMENTO` <- as.character(
  mes0219$`DATA DO PAGAMENTO`)

mes0219$`DATA DO PAGAMENTO` <- as.Date(chron(format(as.Date(
  mes0219$`DATA DO PAGAMENTO`, format = "%d%m%y"),"%m/%d/%y")))

mes0219$`DATA DE NASCIMENTO` <- as.character(
  mes0219$`DATA DE NASCIMENTO`)

mes0219$`DATA DE NASCIMENTO` <- as.Date(chron(format(as.Date(
  mes0219$`DATA DE NASCIMENTO`, format = "%d%m%y"),"%m/%d/%y")))

mes0219$`DATA DO EVENTO` <- as.character(
  mes0219$`DATA DO EVENTO`)

mes0219$`DATA DO EVENTO` <- as.Date(chron(format(as.Date(
  mes0219$`DATA DO EVENTO`, format = "%d%m%y"),"%m/%d/%y")))

#### AAAAMMDD

mes0219$`DATA DE ADMISSÃO` <- as.character(
  mes0219$`DATA DE ADMISSÃO`)

mes0219$`DATA DE ADMISSÃO` <- as.Date(chron(format(as.Date(
  mes0219$`DATA DE ADMISSÃO`, format = "%d%m%Y"),"%m/%d/%y")))

mes0219$`DATA DE NASCIMENTO(Y2K)` <- as.character(
  mes0219$`DATA DE NASCIMENTO(Y2K)`)

mes0219$`DATA DE NASCIMENTO(Y2K)` <- as.Date(chron(format(as.Date(
  mes0219$`DATA DE NASCIMENTO(Y2K)`, format = "%d%m%Y"),"%m/%d/%y")))

mes0219$`DATA DO EVENTO(Y2K)` <- as.character(
  mes0219$`DATA DO EVENTO(Y2K)`)

mes0219$`DATA DO EVENTO(Y2K)` <- as.Date(chron(format(as.Date(
  mes0219$`DATA DO EVENTO(Y2K)`, format = "%d%m%Y"),"%m/%d/%y")))

mes0219$`DATA DO PAGAMENTO(Y2K)` <- as.character(
  mes0219$`DATA DO PAGAMENTO(Y2K)`)

mes0219$`DATA DO PAGAMENTO(Y2K)` <- as.Date(chron(format(as.Date(
  mes0219$`DATA DO PAGAMENTO(Y2K)`, format = "%d%m%Y"),"%m/%d/%y")))

#mes 03/2019

mes0319 <- fread(
  file ="D:/Users/sb046971/Documents/Sinistro Bradesco/SN1903_D071015.txt",
  col.names = names, dec = ",",
  colClasses = c("TIPO DA SUBFATURA" = "character",
                 "NÚMERO DA SUBFATURA" = "character",
                 "CPF/CGC DO REFERENCIADO" = "character",
                 "NÚMERO DO CERTIFICADO" = "character",
                 "MATRICULA" = "character",
                 "CÓDIGO DO PACIENTE" = "character",
                 "CÓDIGO DO PROCEDIMENTO" = "character",
                 "DATA DO EVENTO" = "character",
                 "NÚMERO DO CONTRATO" = "character",
                 "CÓDIGO DO REFERENCIADO" = "character",
                 "DATA DO PAGAMENTO" = "character",
                 "DATA DE NASCIMENTO" = "character",
                 "DATA DE ADMISSAO" = "character",
                 "DATA DE NASCIMENTO(Y2K)" = "character",
                 "DATA DO EVENTO(Y2K)" = "character",
                 "DATA DO PAGAMENTO(Y2K)" = "character",
                 "ESPECIALIDADE" = "character",
                 "TIPO DE EVENTO" = "character",
                 "CODIGO DE AUTORIZACAO" = "character",
                 "CDB" = "character")) %>% filter(
                   !str_detect(`TIPO DE REGISTRO`, "T"))

## AAMMDD

mes0319$`DATA DO PAGAMENTO` <- as.character(
  mes0319$`DATA DO PAGAMENTO`)

mes0319$`DATA DO PAGAMENTO` <- as.Date(chron(format(as.Date(
  mes0319$`DATA DO PAGAMENTO`, format = "%d%m%y"),"%m/%d/%y")))

mes0319$`DATA DE NASCIMENTO` <- as.character(
  mes0319$`DATA DE NASCIMENTO`)

mes0319$`DATA DE NASCIMENTO` <- as.Date(chron(format(as.Date(
  mes0319$`DATA DE NASCIMENTO`, format = "%d%m%y"),"%m/%d/%y")))

mes0319$`DATA DO EVENTO` <- as.character(
  mes0319$`DATA DO EVENTO`)

mes0319$`DATA DO EVENTO` <- as.Date(chron(format(as.Date(
  mes0319$`DATA DO EVENTO`, format = "%d%m%y"),"%m/%d/%y")))

#### AAAAMMDD

mes0319$`DATA DE ADMISSÃO` <- as.character(
  mes0319$`DATA DE ADMISSÃO`)

mes0319$`DATA DE ADMISSÃO` <- as.Date(chron(format(as.Date(
  mes0319$`DATA DE ADMISSÃO`, format = "%d%m%Y"),"%m/%d/%y")))

mes0319$`DATA DE NASCIMENTO(Y2K)` <- as.character(
  mes0319$`DATA DE NASCIMENTO(Y2K)`)

mes0319$`DATA DE NASCIMENTO(Y2K)` <- as.Date(chron(format(as.Date(
  mes0319$`DATA DE NASCIMENTO(Y2K)`, format = "%d%m%Y"),"%m/%d/%y")))

mes0319$`DATA DO EVENTO(Y2K)` <- as.character(
  mes0319$`DATA DO EVENTO(Y2K)`)

mes0319$`DATA DO EVENTO(Y2K)` <- as.Date(chron(format(as.Date(
  mes0319$`DATA DO EVENTO(Y2K)`, format = "%d%m%Y"),"%m/%d/%y")))

mes0319$`DATA DO PAGAMENTO(Y2K)` <- as.character(
  mes0319$`DATA DO PAGAMENTO(Y2K)`)

mes0319$`DATA DO PAGAMENTO(Y2K)` <- as.Date(chron(format(as.Date(
  mes0319$`DATA DO PAGAMENTO(Y2K)`, format = "%d%m%Y"),"%m/%d/%y")))

#mes 04/2019

mes0419 <- fread(
  file ="D:/Users/sb046971/Documents/Sinistro Bradesco/SN1904_D071015.txt",
  col.names = names, dec = ",",
  colClasses = c("TIPO DA SUBFATURA" = "character",
                 "NÚMERO DA SUBFATURA" = "character",
                 "CPF/CGC DO REFERENCIADO" = "character",
                 "NÚMERO DO CERTIFICADO" = "character",
                 "MATRICULA" = "character",
                 "CÓDIGO DO PACIENTE" = "character",
                 "CÓDIGO DO PROCEDIMENTO" = "character",
                 "DATA DO EVENTO" = "character",
                 "NÚMERO DO CONTRATO" = "character",
                 "CÓDIGO DO REFERENCIADO" = "character",
                 "DATA DO PAGAMENTO" = "character",
                 "DATA DE NASCIMENTO" = "character",
                 "DATA DE ADMISSAO" = "character",
                 "DATA DE NASCIMENTO(Y2K)" = "character",
                 "DATA DO EVENTO(Y2K)" = "character",
                 "DATA DO PAGAMENTO(Y2K)" = "character",
                 "ESPECIALIDADE" = "character",
                 "TIPO DE EVENTO" = "character",
                 "CODIGO DE AUTORIZACAO" = "character",
                 "CDB" = "character")) %>% filter(
                   !str_detect(`TIPO DE REGISTRO`, "T"))

## AAMMDD

mes0419$`DATA DO PAGAMENTO` <- as.character(
  mes0419$`DATA DO PAGAMENTO`)

mes0419$`DATA DO PAGAMENTO` <- as.Date(chron(format(as.Date(
  mes0419$`DATA DO PAGAMENTO`, format = "%d%m%y"),"%m/%d/%y")))

mes0419$`DATA DE NASCIMENTO` <- as.character(
  mes0419$`DATA DE NASCIMENTO`)

mes0419$`DATA DE NASCIMENTO` <- as.Date(chron(format(as.Date(
  mes0419$`DATA DE NASCIMENTO`, format = "%d%m%y"),"%m/%d/%y")))


mes0419$`DATA DO EVENTO` <- as.character(
  mes0419$`DATA DO EVENTO`)

mes0419$`DATA DO EVENTO` <- as.Date(chron(format(as.Date(
  mes0419$`DATA DO EVENTO`, format = "%d%m%y"),"%m/%d/%y")))

#### AAAAMMDD

mes0419$`DATA DE ADMISSÃO` <- as.character(
  mes0419$`DATA DE ADMISSÃO`)

mes0419$`DATA DE ADMISSÃO` <- as.Date(chron(format(as.Date(
  mes0419$`DATA DE ADMISSÃO`, format = "%d%m%Y"),"%m/%d/%y")))

mes0419$`DATA DE NASCIMENTO(Y2K)` <- as.character(
  mes0419$`DATA DE NASCIMENTO(Y2K)`)

mes0419$`DATA DE NASCIMENTO(Y2K)` <- as.Date(chron(format(as.Date(
  mes0419$`DATA DE NASCIMENTO(Y2K)`, format = "%d%m%Y"),"%m/%d/%y")))

mes0419$`DATA DO EVENTO(Y2K)` <- as.character(
  mes0419$`DATA DO EVENTO(Y2K)`)

mes0419$`DATA DO EVENTO(Y2K)` <- as.Date(chron(format(as.Date(
  mes0419$`DATA DO EVENTO(Y2K)`, format = "%d%m%Y"),"%m/%d/%y")))

mes0419$`DATA DO PAGAMENTO(Y2K)` <- as.character(
  mes0419$`DATA DO PAGAMENTO(Y2K)`)

mes0419$`DATA DO PAGAMENTO(Y2K)` <- as.Date(chron(format(as.Date(
  mes0419$`DATA DO PAGAMENTO(Y2K)`, format = "%d%m%Y"),"%m/%d/%y")))