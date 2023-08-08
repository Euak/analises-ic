# biblioteca para o GWR
install.packages("spgwr")
library(spgwr)
library(scales)

##Load
ESC_AP_MODELVARS <- st_read("Bases/Processadas/ESC/ESC_AP_MODELVARS.geojson")
shp <- ESC_AP_MODELVARS %>%
  st_as_sf() %>% 
  filter(!is.na(TX_ABND_TOTEM_AP_MEAN) &
           !is.na(INSE_AP_MEAN)) %>% 
  select(ID, AP_2010,
         TX_ABND_TOTEM_AP_MEAN, AP_RENDAPC_SLRMIN, INSE_AP_MEAN, AP_PCTNEGRAS,
         RAZ_ALU_TUR_AP_MEAN, RAZ_ALU_PROF_AP_MEAN, RAZ_PC_ALU_AP_MEAN)
shp <- as_Spatial(shp)

#======================================================
# GWR
#======================================================
library(raster)
# Definindo a banda para a analise dos vizinhos
# Nao se esqueca de substituir as variaveis (varDependente,
# var1 e var2) pelas variaveis que estamos analisando
bw <- gwr.sel(TX_ABND_TOTEM_AP_MEAN ~ AP_RENDAPC_SLRMIN + INSE_AP_MEAN + AP_PCTNEGRAS +
              RAZ_ALU_TUR_AP_MEAN + RAZ_ALU_PROF_AP_MEAN + RAZ_PC_ALU_AP_MEAN, data = shp)

# Rodando o gwr
# Nao se esqueca de substituir as variaveis (varDependente,
# var1 e var2) pelas variaveis que estamos analisando
gwr.model <- gwr(TX_ABND_TOTEM_AP_MEAN ~ AP_RENDAPC_SLRMIN + INSE_AP_MEAN + AP_PCTNEGRAS +
                   RAZ_ALU_TUR_AP_MEAN + RAZ_ALU_PROF_AP_MEAN + RAZ_PC_ALU_AP_MEAN, data = shp, 
                 bandwidth = bw, se.fit = T, hatmatrix=T)
#======================================================
# Analise de resultados
#======================================================

gwr.model

# Relembrando os resultados da OLS
summary(modelo6)
AIC(modelo6)

# Pegando os valores dos coeficientes para cada zona
results <- as.data.frame(gwr.model$SDF)

# Adicionando alguns resultados para o shapefile
# Coeficiente da renda per capita
shp$coefRenda <- results$AP_RENDAPC_SLRMIN
# Coeficiente de INSE
shp$coefINSE <- results$INSE_AP_MEAN
# Coeficiente de PCT de Negros
shp$coefNegras <- results$AP_PCTNEGRAS


# R2 local
shp$localr2 <- results$localR2
# Erros padronizados
shp$predSE <- scale.default(results$pred.se) 

shp_sf <- st_as_sf(shp)

#======================================================
# Mapas de resultados
#======================================================

# Mapa do R2 local
ggplot() + 
  geom_sf(data=shp_sf, aes(fill=localr2), color="grey60") + 
  scale_fill_gradientn(colours = c("#F7FAFF", "#6BADD5", "#072F6B"),
                       values = c(0, 0.5, 1),
                       guide = "colorbar",
                       limits = c(0,1)) +
  theme_minimal()

# Mapa dos erros padronizados
ggplot() + 
  geom_sf(data=shp_sf, aes(fill=predSE), color="grey60") + 
  scale_fill_gradientn(colours = c("#67001F", "#D6604D", "#F7F7F7", 
                                   "#4393C3", "#053061"),
                       values = rescale(c(-4, 4)),
                       limits = c(-4, 4),
                       oob = squish,
                       guide = "colorbar") +
  theme_minimal()

# Mapa dos coeficientes da renda per capita
ggplot() + 
  geom_sf(data=shp_sf, aes(fill=coefRenda), color="grey60") + 
  scale_fill_distiller(palette = "YlOrRd") +
  theme_minimal()

# Mapa dos coeficientes dos INSE
ggplot() + 
  geom_sf(data=shp_sf, aes(fill=coefINSE), color="grey60") + 
  scale_fill_distiller(palette = "YlOrRd") +
  theme_minimal()

# Mapa dos coeficientes da PCT de Pessoas Negras
ggplot() + 
  geom_sf(data=shp_sf, aes(fill=coefNegras), color="grey60") + 
  scale_fill_distiller(palette = "YlOrRd") +
  theme_minimal()
