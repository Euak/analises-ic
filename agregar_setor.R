#Importa o layer de setores censitários apenas para o MSP
setoresc <- st_read("Bases/CEM/Censo/SC2010_CEM_RMSAO_V4/") %>% 
  filter(COD_MU == 3550308)

#Importa o layer de áreas de ponderação do MSP
apMSP <- st_read("Bases/CEM/Censo/AP2010_CEM_MSP/")

#Separa os valores agregados dos SCs em um df separado
valores_agregados <- ESC_SC_ESTs %>% 
  st_drop_geometry() %>% 
  select(SC_2010_majority,
         TX_ABND_TOTEM_SC_MEAN, #SC
         SLRMINPC, 
         INSE_SC_MEAN,
         RAZ_ALU_TUR_SC_MEAN,
         RAZ_ALU_PROF_SC_MEAN,
         RAZ_PC_ALU_SC_MEAN,
         PCT_NEGRAS) %>% 
  distinct()

#Separa os valores agregados das APs em um df separado
valores_agregados_ap <- ESC_SC_ESTs %>% 
  st_drop_geometry() %>% 
  select(AP_2010, 
         TX_ABND_TOTEM_AP_MEAN, #AP
         AP_RENDAPC_SLRMIN, 
         INSE_AP_MEAN,
         AP_PCTNEGRAS,
         RAZ_ALU_TUR_AP_MEAN,
         RAZ_ALU_PROF_AP_MEAN,
         RAZ_PC_ALU_AP_MEAN) %>% 
  distinct()

#Escreve as df em arquivos de planilhas
write.csv(valores_agregados, "Bases/Processadas/valores_agregados_sc.csv")
write.csv(valores_agregados_ap, "Bases/Processadas/valores_agregados_ap.csv")

#Lê as df em arquivos de planilhas
valores_agregados_sc <- read_csv("Bases/Processadas/valores_agregados_sc.csv")
valores_agregados_ap <- read_csv("Bases/Processadas/valores_agregados_ap.csv")

setoresc$SC_2010 <- as.character(setoresc$SC_2010) #Converte em charcter para resguardo
#Join da df por SCs com a layer de setores completa do MSP
setoresc.vars <- setoresc %>% 
  left_join(valores_agregados, by = c("SC_2010" = "SC_2010_majority")) %>% 
  st_as_sf()

#Join da df por APs com a layer de APs completa do MSP
aps.vars <- apMSP %>% 
  left_join(valores_agregados_ap, by = c("AP_2010" = "AP_2010"),  keep = F,
            multiple = "first") %>% 
  st_as_sf()

#Grava as layers com os valores agregados dos modelos
st_write(setoresc.vars, "Bases/Processadas/ESC/ESC_SC_MODELVARS.geojson", delete_dsn = T)
st_write(aps.vars, "Bases/Processadas/ESC/ESC_AP_MODELVARS.geojson", delete_dsn = T)

#Lê as layers com os valores agregados dos modelos
ESC_AP_MODELVARS <- st_read("Bases/Processadas/ESC/ESC_AP_MODELVARS.geojson")
ESC_SC_MODELVARS <- st_read("Bases/Processadas/ESC/ESC_SC_MODELVARS.geojson")
