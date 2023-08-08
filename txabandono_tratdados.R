# install.packages("sf")
# install.packages("clipr")
# install.packages("foreign")
# install.packages("rgdal")
# install.packages("car")
# install.packages("sjPlot")
library(foreign)
library(sf)
library(tidyverse)
library(magrittr)
library(clipr)
library(rgdal)
library(tidyverse)
library(psych)
library(car)
library(sjPlot)
library(gridExtra)

  #Carrega layer das escolas com o código do setor censitário 
escolas <- st_read("Bases/Processadas/ESC13SC10/ESC13SC10.gpkg")

#Filtra as escolas do MSP, estaduais e privadas, que tenham ensino médio regular
#Assgin nos NAs para as variáveis ENEM2013 e AB1EM_10
escolas_f1 <- escolas %>%
  filter(CODMUN == 3550308 & COD_DEP %in% c(2, 4) & REG_MED == 1 & ENS_REG == 1) %>% 
  select(NOMEMUN, CODESC, NOMEESC, COD_DEP, SC_2010_majority, REG_MED, REG_FU_9A, REG_FU_8A, INSE_AB,
         IDEB11AF, ENEM2013, AB1EM_10) %>% 
  mutate(ENEM2013 = if_else(ENEM2013 == 9999.99, NA, ENEM2013)) %>% 
  mutate(AB1EM_10 = if_else(AB1EM_10 == 999.9, NA, AB1EM_10)) %>% 
  mutate(INSE_AB  = if_else(INSE_AB > 999.9998, NA, INSE_AB))

rm(escolas)

#Importa o layer de setores censitários apenas para o MSP
setoresc <- st_read("Bases/CEM/Censo/SC2010_CEM_RMSAO_V4/") %>% 
  filter(COD_MU == 3550308) %>% 
  mutate(SLRMINPC = BA_009/510) #Transforma a renda familiar per capita em pct do Salario Minimo em 2010

#Importa e trata as variáveis dos Setores Censitários
  #Variáveis sobre pessoas
setoresc.pessoas <- read.dbf("Bases/CEM/Censo/SC2010_CEM_RMSAO_V4/SC2010_CEM_RMSAO_p1.dbf")
setoresc.pessoas <- setoresc.pessoas %>% 
  select(CODSETOR, CODSETTX, P3_001,
         P3_002,
         P3_003,
         P3_005) %>% 
  mutate(PCT_NEGRAS = (P3_003 + P3_005)/P3_001)
  
  #Variáveis sobre rendimento
setoresc.rendimento <- read.dbf("Bases/CEM/Censo/SC2010_CEM_RMSAO_V4/SC2010_CEM_RMSAO_p3.dbf")
setoresc.rendimento <- setoresc.rendimento %>% 
  select(CODSETOR, CODSETTX, D3_016,
         D3_017)
#Join nos dfs de setores censitários
setoresc.join <- inner_join(setoresc, setoresc.pessoas, c("SC_2010" = "CODSETOR")) %>% 
  inner_join(setoresc.rendimento, c("SC_2010"= "CODSETOR")) %>% 
  st_drop_geometry()

#Join com o df de escolas filtrado
escolas_f1$SC_2010_majority <- as.character(escolas_f1$SC_2010_majority) #Resguardando casos com decimal
setoresc.join$SC_2010 <- as.character(setoresc.join$SC_2010) #Resguardando casos com decimal
ESC_SC <- left_join(escolas_f1, setoresc.join, c("SC_2010_majority" = "SC_2010"))

rm(escolas_f1, setoresc.join, setoresc, setoresc.pessoas, setoresc.rendimento)

#Importa o Censo Escolar apenas para o município de São Paulo
censo_escolar <- read_csv2("Bases/INEP/microdados_ed_basica_2013/dados/microdados_ed_basica_2013.csv",
                           locale =  locale(encoding = "ISO-8859-1")) %>% 
  filter(CO_MUNICIPIO == 3550308)

#Seleciona apenas as variáveis de qtde de matrículas, docentes e turmas
censo_escolar <- censo_escolar %>% 
  select(CO_ENTIDADE, NO_ENTIDADE, QT_DOC_MED, QT_MAT_MED, QT_TUR_MED, QT_COMP_ALUNO, CO_ORGAO_REGIONAL)

#Join com a camada de escolas
ESC_SC <- left_join(ESC_SC, censo_escolar, c("CODESC" = "CO_ENTIDADE"))  %>% 
  filter(CO_ORGAO_REGIONAL != 11000) #Exclui ETECs

rm(censo_escolar)

#Importa a base de taxa de rendimento apenas para o MSP
tx_redimento <- read_csv("Bases/INEP/tx_rendimento_2013_MSP.csv",
                         locale = locale(decimal_mark = ","),
                         col_types = c("dccdccd")) %>% 
  filter(CO_MUNICIPIO == 3550308) %>% 
  select(-CO_MUNICIPIO, -NO_MUNICIPIO, -LOCALIZACAO, -REDE)

#Join com a camada de escolas
ESC_SC <- left_join(ESC_SC, tx_redimento, c("CODESC" = "CO_ESCOLA")) 

rm(tx_redimento)

#Cria as variáveis de Aluno/turma e Aluno/professor
ESC_SC_ESTs <- ESC_SC %>% 
  filter(COD_DEP == 2) %>% 
  filter(QT_MAT_MED > 1) %>%
  filter(!is.na(INSE_AB)) %>% 
  mutate(RAZ_ALU_TUR = QT_MAT_MED / QT_TUR_MED) %>% 
  mutate(RAZ_ALU_PROF = QT_MAT_MED / QT_DOC_MED) %>% 
  mutate(RAZ_PC_ALU = if_else(QT_COMP_ALUNO > 0, QT_COMP_ALUNO / QT_MAT_MED,0))

rm(ESC_SC)

