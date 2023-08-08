#Cálculo da Média das Variáveis Explicativas por Setor Censitário (SC)
TX_ABND_TOTEM_SC_MEAN.df <- ESC_SC_ESTs %>% 
  st_drop_geometry() %>% 
  group_by(SC_2010_majority) %>% 
  summarise(TX_ABND_TOTEM_SC_MEAN = mean(TX_ABND_TOTEM),
            INSE_SC_MEAN = mean(INSE_AB),
            RAZ_ALU_TUR_SC_MEAN = mean(RAZ_ALU_TUR),
            RAZ_ALU_PROF_SC_MEAN = mean(RAZ_ALU_PROF),
            RAZ_PC_ALU_SC_MEAN = mean(RAZ_PC_ALU))

#Cálculo da Média das Variáveis Explicativas por Área de Ponderação (AP)
TX_ABND_TOTEM_AP_MEAN.df <- ESC_SC_ESTs %>% 
  st_drop_geometry() %>% 
  group_by(AP_2010) %>% 
  summarise(TX_ABND_TOTEM_AP_MEAN = mean(TX_ABND_TOTEM),
            INSE_AP_MEAN = mean(INSE_AB),
            RAZ_ALU_TUR_AP_MEAN = mean(RAZ_ALU_TUR),
            RAZ_ALU_PROF_AP_MEAN = mean(RAZ_ALU_PROF),
            RAZ_PC_ALU_AP_MEAN = mean(RAZ_PC_ALU))

#Incorporação ao layer de Escolas
ESC_SC_ESTs <- ESC_SC_ESTs %>% 
  left_join(TX_ABND_TOTEM_AP_MEAN.df, by = c("AP_2010" = "AP_2010") )

#Incorporação ao layer de Escolas
ESC_SC_ESTs <- ESC_SC_ESTs %>% 
  left_join(TX_ABND_TOTEM_SC_MEAN.df, by = c("SC_2010_majority" = "SC_2010_majority") )

rm(TX_ABND_TOTEM_SC_MEAN.df, TX_ABND_TOTEM_AP_MEAN.df)

write.csv(ESC_SC_ESTs, "Bases/Processadas/ESC_SC_ESTs.csv")
