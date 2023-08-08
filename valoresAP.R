#Importa o layer de setores censitários apenas para o MSP
setoresc <- st_read("Bases/CEM/Censo/SC2010_CEM_RMSAO_V4/") %>% 
  st_drop_geometry() %>% 
  filter(COD_MU == 3550308)
#Importa e trata as variáveis dos Setores Censitários
#Variáveis sobre pessoas
setoresc.pessoas <- read.dbf("Bases/CEM/Censo/SC2010_CEM_RMSAO_V4/SC2010_CEM_RMSAO_p1.dbf")
#Variáveis sobre rendimento
setoresc.rendimento <- read.dbf("Bases/CEM/Censo/SC2010_CEM_RMSAO_V4/SC2010_CEM_RMSAO_p3.dbf")
#Join nos dfs de setores censitários
setoresc.join <- inner_join(setoresc, setoresc.pessoas, c("SC_2010" = "CODSETOR")) %>% 
  inner_join(setoresc.rendimento, c("SC_2010"= "CODSETOR"))
rm(setoresc, setoresc.pessoas, setoresc.rendimento)

#Cálculo da Renda em Salário Mínimo por Domicílio na Área de Ponderação (AP)
AP_SLRMINPD.df <- setoresc.join %>% 
  group_by(AP_2010) %>% 
  summarise(AP_SLRMINPD = (sum(D3_003, na.rm = T) / sum(D1_002, na.rm = T))/510)

#Cálculo da porcentagem de pessoas negras (pretas e pardas) na Área de Ponderação (AP)
AP_PCTNEGRAS.df <- setoresc.join %>% 
  group_by(AP_2010) %>% 
  summarise(AP_PCTNEGRAS = sum(P3_003 + P3_005, na.rm = T) / sum(P3_001, na.rm = T))

#Cálculo da Renda em Salário Mínimo per Capita na Área de Ponderação (AP)
AP_RENDAPC_SLRMIN.df <- setoresc.join %>% 
  group_by(AP_2010) %>% 
  summarise(AP_RENDAPC_SLRMIN = (sum(P14_088, na.rm = T) / sum(P13_002, na.rm = T)) / 510)

#Incorporação ao layer de Escolas 
ESC_SC_ESTs <- ESC_SC_ESTs %>% 
  left_join(AP_PCTNEGRAS.df, by = ("AP_2010" = "AP_2010")) %>% 
  left_join(AP_SLRMINPD.df, by = ("AP_2010" = "AP_2010")) %>% 
  left_join(AP_RENDAPC_SLRMIN.df, by = ("AP_2010" = "AP_2010"))

rm(AP_SLRMINPD.df, AP_PCTNEGRAS.df)
