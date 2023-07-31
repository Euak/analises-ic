install.packages("psych")

library(tidyverse)
library(psych)

censo_escolar <- read_csv2("Bases/INEP/microdados_ed_basica_2010/dados/microdados_ed_basica_2010.csv",
                           locale =  locale(encoding = "ISO-8859-1")) %>% 
  filter(CO_MUNICIPIO == 3550308)

censo_escolar <- censo_escolar %>% 
  select(CO_ENTIDADE, NO_ENTIDADE, QT_DOC_MED, QT_MAT_MED, QT_TUR_MED)

ESC_SC <- left_join(ESC_SC, censo_escolar, c("CODESC" = "CO_ENTIDADE"))  


tx_redimento <- read_csv("Bases/INEP/tx_rendimento_2010_MSP.csv",
                         locale = locale(decimal_mark = ","),
                         col_types = paste0(c("dccdcccdc"), strrep("d", 54))) %>% 
  filter(CO_MUNICIPIO == 3550308) %>% 
  select(-Ano, -REGIAO, -UF, -CO_MUNICIPIO, -NO_MUNICIPIO, -LOCALIZACAO, -REDE)

ESC_SC <- left_join(ESC_SC, tx_redimento, c("CODESC" = "CO_ESCOLA")) 

ESC_SC_ESTs <- ESC_SC_ESTs %>% 
  mutate(RAZ_ALU_TUR = QT_MAT_MED / QT_TUR_MED) %>% 
  mutate(RAZ_ALU_PROF = QT_MAT_MED / QT_DOC_MED)


#INSE
summary(ESC_SC_ESTs$INSE_AB)
var(ESC_SC_ESTs$INSE_AB)
sd(ESC_SC_ESTs$INSE_AB)

#ENEM
summary(ESC_SC_ESTs$ENEM2013)
var(ESC_SC_ESTs$ENEM2013, na.rm=T)
sd(ESC_SC_ESTs$ENEM2013, na.rm=T)

#Aluno/Turma
summary(ESC_SC_ESTs$RAZ_ALU_TUR)
var(ESC_SC_ESTs$RAZ_ALU_TUR, na.rm=T)
sd(ESC_SC_ESTs$RAZ_ALU_TUR, na.rm=T)

#Aluno/Professor
summary(ESC_SC_ESTs$RAZ_ALU_PROF)
var(ESC_SC_ESTs$RAZ_ALU_PROF, na.rm=T)
sd(ESC_SC_ESTs$RAZ_ALU_PROF, na.rm=T)
describe(ESC_SC_ESTs$RAZ_ALU_PROF)
