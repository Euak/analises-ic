install.packages("sf")
install.packages("clipr")
install.packages("foreign")
install.packages("rgdal")
library(foreign)
library(sf)
library(tidyverse)
library(magrittr)
library(clipr)
library(rgdal)

#Carrega layer das escolas com o código do setor censitário 
escolas <- st_read("Bases/Processadas/ESC13SC10/ESC13SC10.gpkg")

#Filtra as escolas do MSP, estaduais e privadas, que tenham ensino médio regular
#Assgin nos NAs para as variáveis ENEM2013 e AB1EM_10
escolas_f1 <- escolas %>%
  filter(CODMUN == 3550308 & COD_DEP %in% c(2, 4) & REG_MED == 1 & ENS_REG == 1) %>% 
  select(NOMEMUN, CODESC, NOMEESC, COD_DEP, SC_2010_majority, REG_MED, REG_FU_9A, REG_FU_8A, INSE_AB,
         IDEB11AF, ENEM2013, AB1EM_10) %>% 
  mutate(ENEM2013 = if_else(ENEM2013 == 9999.99, NA, ENEM2013)) %>% 
  mutate(AB1EM_10 = if_else(AB1EM_10 == 999.9, NA, AB1EM_10))

#Conta os NAs para AB1EM_10
escolas_f1%>% 
  group_by(COD_DEP) %>% 
  summarise(total_non_na = sum(!is.na(AB1EM_10)), total_na = sum(is.na(AB1EM_10))) %>% 
  write_clip()

#Sumários da variável AB1EM_10 por Dep. Adm.
escolas_f1 %>% 
  filter(COD_DEP == 2) %$% 
  summary(AB1EM_10)
escolas_f1 %>% 
  filter(COD_DEP == 4) %$% 
  summary(AB1EM_10)

#Histograma AB1EM_10 por Dep. Adm.
escolas_f1 %>% 
  ggplot(aes(AB1EM_10)) +
  geom_histogram() +
  labs(x = "Taxa de Abandono na 1ª série - 2010", y = "Contagem") +
  facet_wrap(vars(COD_DEP))

#Boxplot AB1EM_10 por Dep. Adm.
escolas_f1 %>% 
  ggplot(aes(y = AB1EM_10)) +
  geom_boxplot() +
  labs(y = "Taxa de Abandono na 1ª série - 2010") +
  facet_wrap(vars(COD_DEP))

#Importa o layer de setores censitários apenas para o MSP
setoresc <- st_read("Bases/CEM/Censo/SC2010_CEM_RMSAO_V4/") %>% 
  filter(COD_MU == 3550308)

#Importa as variáveis dos Setores Censitários
setoresc.pessoas <- read.dbf("Bases/CEM/Censo/SC2010_CEM_RMSAO_V4/SC2010_CEM_RMSAO_p1.dbf")
setoresc.pessoas <- setoresc.pessoas %>% 
  select(CODSETOR, CODSETTX, P3_001,
         P3_002,
         P3_003,
         P3_005)
setoresc.rendimento <- read.dbf("Bases/CEM/Censo/SC2010_CEM_RMSAO_V4/SC2010_CEM_RMSAO_p3.dbf")
setoresc.rendimento <- setoresc.rendimento %>% 
  select(CODSETOR, CODSETTX, D3_016,
         D3_017)
#Join nos dfs de setores censitários
setoresc.join <- inner_join(setoresc, setoresc.pessoas, c("SC_2010" = "CODSETOR")) %>% 
  inner_join(setoresc.rendimento, c("SC_2010"= "CODSETOR")) %>% 
  st_drop_geometry()

#Join com o df de escolas filtrado
ESC_SC <- left_join(escolas_f1, setoresc.join, c("SC_2010_majority" = "SC_2010"))

#st_write(ESC_SC, "Bases/Processadas/ESC13SC10_EM/ESC13SC10_EM.geojson")
#st_write(setoresc, "Bases/Processadas/ESC13SC10_EM/SC10_MSP.geojson")

#Mapa das escolas estaduais por AB1EM_10
ggplot() +
  geom_sf(data = setoresc) +
  geom_sf(data = ESC_SC_ESTs, aes(colour = AB1EM_10)) +
  binned_scale(aesthetics = "color",
               scale_name = "stepsn", 
               palette = function(x) c("#FFFFFF", "#fee0d2", "#fc9272", "#de2d26"),
               breaks = c(0.1, 3.85, 10.40, 35.80),
               show.limits = TRUE, 
               guide = "colorsteps"
  )

#Mapa das escolas estaduais por TX_ABND_TOTEM
ggplot() +
  geom_sf(data = setoresc) +
  geom_sf(data = ESC_SC_ESTs, aes(colour = TX_ABND_TOTEM)) +
  binned_scale(aesthetics = "color",
               scale_name = "stepsn", 
               palette = function(x) c("#FFFFFF", "#fee0d2", "#fc9272", "#de2d26"),
               breaks = c(0.1, 3.6, 9.3, 35.80),
               show.limits = TRUE, 
               guide = "colorsteps"
  )
