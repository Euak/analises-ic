install.packages("psych")

library(tidyverse)
library(psych)

ESC_SC_ESTs <- read_csv("Bases/Processadas/ESC_SC_ESTs.csv")

#TX_ABND_TOTEM
summary(ESC_SC_ESTs$TX_ABND_TOTEM)
var(ESC_SC_ESTs$TX_ABND_TOTEM, na.rm = T)
sd(ESC_SC_ESTs$TX_ABND_TOTEM, na.rm = T)
ESC_SC_ESTs %>% 
  ggplot(aes(TX_ABND_TOTEM)) +
  geom_histogram() +
  labs(x = "TX_ABND_TOTEM", y = "Contagem")
ESC_SC_ESTs %>% 
  ggplot(aes(y = TX_ABND_TOTEM)) +
  geom_boxplot()

#Aluno/Professor
summary(ESC_SC_ESTs$PCT_NEGRAS)
var(ESC_SC_ESTs$PCT_NEGRAS, na.rm=T)
sd(ESC_SC_ESTs$PCT_NEGRAS, na.rm=T)
describe(ESC_SC_ESTs$PCT_NEGRAS)
ESC_SC_ESTs %>% 
  ggplot(aes(PCT_NEGRAS)) +
  geom_histogram() +
  labs(x = "% de Pessoas Negras", y = "Contagem")
ESC_SC_ESTs %>% 
  ggplot(aes(y = PCT_NEGRAS)) +
  geom_boxplot()

#INSE
summary(ESC_SC_ESTs$INSE_AB)
var(ESC_SC_ESTs$INSE_AB)
sd(ESC_SC_ESTs$INSE_AB)
ESC_SC_ESTs %>% 
  ggplot(aes(INSE_AB)) +
  geom_histogram() +
  labs(x = "INSE_AB", y = "Contagem")
ESC_SC_ESTs %>% 
  ggplot(aes(y = INSE_AB)) +
  geom_boxplot()

#ENEM
summary(ESC_SC_ESTs$ENEM2013)
var(ESC_SC_ESTs$ENEM2013, na.rm=T)
sd(ESC_SC_ESTs$ENEM2013, na.rm=T)
#Conta os NAs
ESC_SC_ESTs%>% 
  group_by(COD_DEP) %>% 
  summarise(total_non_na = sum(!is.na(ENEM2013)), total_na = sum(is.na(ENEM2013)))
ESC_SC_ESTs %>% 
  ggplot(aes(ENEM2013)) +
  geom_histogram() +
  labs(x = "ENEM 2013", y = "Contagem")

#Aluno/Turma
summary(ESC_SC_ESTs$RAZ_ALU_TUR)
var(ESC_SC_ESTs$RAZ_ALU_TUR, na.rm=T)
sd(ESC_SC_ESTs$RAZ_ALU_TUR, na.rm=T)
ESC_SC_ESTs %>% 
  ggplot(aes(RAZ_ALU_TUR)) +
  geom_histogram() +
  labs(x = "Aluno por Turma", y = "Contagem")
ESC_SC_ESTs %>% 
  ggplot(aes(y = RAZ_ALU_TUR)) +
  geom_boxplot()

#Aluno/Professor
summary(ESC_SC_ESTs$RAZ_ALU_PROF)
var(ESC_SC_ESTs$RAZ_ALU_PROF, na.rm=T)
sd(ESC_SC_ESTs$RAZ_ALU_PROF, na.rm=T)
describe(ESC_SC_ESTs$RAZ_ALU_PROF)
ESC_SC_ESTs %>% 
  ggplot(aes(RAZ_ALU_PROF)) +
  geom_histogram() +
  labs(x = "Aluno por Professor", y = "Contagem")
ESC_SC_ESTs %>% 
  ggplot(aes(y = RAZ_ALU_PROF)) +
  geom_boxplot()

#AP Pessoas Negras
summary(ESC_SC_ESTs$AP_PCTNEGRAS)
var(ESC_SC_ESTs$AP_PCTNEGRAS, na.rm=T)
sd(ESC_SC_ESTs$AP_PCTNEGRAS, na.rm=T)
describe(ESC_SC_ESTs$AP_PCTNEGRAS)
ESC_SC_ESTs %>% 
  ggplot(aes(AP_PCTNEGRAS)) +
  geom_histogram() +
  labs(x = " Pessoas Negrasr", y = "Contagem")
ESC_SC_ESTs %>% 
  ggplot(aes(y = AP_PCTNEGRAS)) +
  geom_boxplot()

#AP Renda Familiar em Salario Minimo
summary(ESC_SC_ESTs$AP_SLRMINPD)
var(ESC_SC_ESTs$AP_SLRMINPD, na.rm=T)
sd(ESC_SC_ESTs$AP_SLRMINPD, na.rm=T)
describe(ESC_SC_ESTs$AP_SLRMINPD)
ESC_SC_ESTs %>% 
  ggplot(aes(AP_SLRMINPD)) +
  geom_histogram() +
  labs(x = "Renda Familiar em Salario Minimo", y = "Contagem")
ESC_SC_ESTs %>% 
  ggplot(aes(y = AP_SLRMINPD)) +
  geom_boxplot()

#SLRMINPC
summary(ESC_SC_ESTs$SLRMINPC)
var(ESC_SC_ESTs$SLRMINPC, na.rm = T)
sd(ESC_SC_ESTs$INSE_AB, na.rm = T)
ESC_SC_ESTs %>% 
  ggplot(aes(SLRMINPC)) +
  geom_histogram() +
  labs(x = "SLRMINPC", y = "Contagem")
ESC_SC_ESTs %>% 
  ggplot(aes(y = SLRMINPC)) +
  geom_boxplot()

#AP Renda Per Capita em Salario Minimo
summary(ESC_SC_ESTs$AP_RENDAPC_SLRMIN)
var(ESC_SC_ESTs$AP_RENDAPC_SLRMIN, na.rm=T)
sd(ESC_SC_ESTs$AP_RENDAPC_SLRMIN, na.rm=T)
describe(ESC_SC_ESTs$AP_RENDAPC_SLRMIN)
ESC_SC_ESTs %>% 
  ggplot(aes(AP_RENDAPC_SLRMIN)) +
  geom_histogram() +
  labs(x = "Per Capita em Salario Minimo", y = "Contagem")
ESC_SC_ESTs %>% 
  ggplot(aes(y = AP_RENDAPC_SLRMIN)) +
  geom_boxplot()

ESC_SC_ESTs %>% 
  st_drop_geometry() %>% 
  mutate(sep_APRENDA = case_when(1.5>=AP_RENDAPC_SLRMIN ~ "0.65-1.5",
                               3>=AP_RENDAPC_SLRMIN & AP_RENDAPC_SLRMIN > 1.5 ~ "1.5-3",
                               4.5>=AP_RENDAPC_SLRMIN & AP_RENDAPC_SLRMIN > 3 ~ "3-4.5",
                               6>=AP_RENDAPC_SLRMIN & AP_RENDAPC_SLRMIN > 4.5 ~ "4.5-6",
                               7.5>=AP_RENDAPC_SLRMIN & AP_RENDAPC_SLRMIN > 6 ~ "6-7.5",
                               9>=AP_RENDAPC_SLRMIN & AP_RENDAPC_SLRMIN > 7.5 ~ "7.5-9",
                               10.5>=AP_RENDAPC_SLRMIN & AP_RENDAPC_SLRMIN > 9 ~ "9-10.5"
                               )) %>% 
  ggplot(aes(x=AP_PCTNEGRAS,y=TX_ABND_TOTEM))+
  geom_point()+
  facet_wrap(~sep_APRENDA,nrow = 4)

hist1 <- ESC_SC_ESTs %>% 
  ggplot(aes(SLRMINPC)) +
  geom_histogram() +
  labs(x = "Por Setor Censitário", y = "Contagem")

hist2 <- ESC_SC_ESTs %>% 
  ggplot(aes(AP_RENDAPC_SLRMIN)) +
  geom_histogram() +
  labs(x = "Por Área de Ponderação", y = "Contagem")

grid.arrange(hist1, hist2, nrow = 1)

#Computador/Aluno
summary(ESC_SC_ESTs$RAZ_PC_ALU)
var(ESC_SC_ESTs$RAZ_PC_ALU, na.rm=T)
sd(ESC_SC_ESTs$RAZ_PC_ALU, na.rm=T)
describe(ESC_SC_ESTs$RAZ_PC_ALU)
ESC_SC_ESTs %>% 
  ggplot(aes(RAZ_PC_ALU)) +
  geom_histogram() +
  labs(x = "Computador/Aluno", y = "Contagem")
ESC_SC_ESTs %>% 
  ggplot(aes(y = RAZ_PC_ALU)) +
  geom_boxplot()

#Computador/Aluno
summary(ESC_SC_ESTs$RAZ_PC_ALU_AP_MEAN )
var(ESC_SC_ESTs$RAZ_PC_ALU_AP_MEAN , na.rm=T)
sd(ESC_SC_ESTs$RAZ_PC_ALU_AP_MEAN , na.rm=T)
describe(ESC_SC_ESTs$RAZ_PC_ALU_AP_MEAN )
ESC_SC_ESTs %>% 
  ggplot(aes(RAZ_PC_ALU_AP_MEAN )) +
  geom_histogram() +
  labs(x = "Computador/Aluno", y = "Contagem")
ESC_SC_ESTs %>% 
  ggplot(aes(y = RAZ_PC_ALU_AP_MEAN )) +
  geom_boxplot()


#Mapa das escolas estaduais por TX_ABND_TOTEM
ggplot() +
  # geom_sf(data = setoresc) +
  geom_sf(data = ESC_SC_ESTs, aes(colour = TX_ABND_TOTEM)) +
  binned_scale(aesthetics = "color",
               scale_name = "stepsn", 
               palette = function(x) c("#FFFFFF", "#fee0d2", "#fc9272", "#de2d26"),
               breaks = c(0.1, 3.6, 9.3, 35.80),
               show.limits = TRUE, 
               guide = "colorsteps"
  )

summary(TX_ABND_TOTEM_SC_MEAN.df$TX_ABND_TOTEM_SC_MEAN)
describe(TX_ABND_TOTEM_SC_MEAN.df$TX_ABND_TOTEM_SC_MEAN)

summary(TX_ABND_TOTEM_AP_MEAN.df$TX_ABND_TOTEM_AP_MEAN)
describe(TX_ABND_TOTEM_AP_MEAN.df$TX_ABND_TOTEM_AP_MEAN)

hist1 <- TX_ABND_TOTEM_SC_MEAN.df %>% 
  ggplot(aes(TX_ABND_TOTEM_SC_MEAN)) +
  geom_histogram() +
  labs(x = "Por Setor Censitário", y = "Contagem")

hist2 <- TX_ABND_TOTEM_AP_MEAN.df %>% 
  ggplot(aes(TX_ABND_TOTEM_AP_MEAN)) +
  geom_histogram() +
  labs(x = "Por Área de Ponderação", y = "Contagem")

grid.arrange(hist1, hist2, nrow = 1)

#Correlações
ESC_SC_ESTs %>% 
  ggplot(aes(x = PCT_NEGRAS, y = TX_ABND_TOTEM)) +
  geom_point()

ESC_SC_ESTs %>% 
  ggplot(aes(x = BA_009, y = TX_ABND_TOTEM)) +
  geom_point()

ESC_SC_ESTs %>% 
  ggplot(aes(x = INSE_AB, y = TX_ABND_TOTEM)) +
  geom_point()

ESC_SC_ESTs %>% 
  ggplot(aes(x = RAZ_ALU_TUR, y = TX_ABND_TOTEM)) +
  geom_point()

ESC_SC_ESTs %>% 
  ggplot(aes(x = RAZ_ALU_PROF, y = TX_ABND_TOTEM)) +
  geom_point()

ESC_SC_ESTs %>% 
  ggplot(aes(x = AP_SLRMINPD, y = TX_ABND_TOTEM)) +
  geom_point()

ESC_SC_ESTs %>% 
  ggplot(aes(x = AP_RENDAPC_SLRMIN, y = TX_ABND_TOTEM)) +
  geom_point()

ESC_SC_ESTs %>% 
  ggplot(aes(x = RAZ_ALU_PC, y = TX_ABND_TOTEM)) +
  geom_point()

ESC_SC_ESTs %>% 
  ggplot(aes(x = RAZ_ALU_PC, y = PCT_NEGRAS)) +
  geom_point()

ESC_SC_ESTs %>% 
  ggplot(aes(x = AP_RENDAPC_SLRMIN, y = INSE_AB)) +
  geom_point()

#Modelo 2
modelo2 <- lm(data = ESC_SC_ESTs,
              formula = TX_ABND_TOTEM_AP_MEAN ~ AP_RENDAPC_SLRMIN + INSE_AB +
                RAZ_ALU_TUR + RAZ_ALU_PROF + RAZ_PC_ALU)

summary(modelo2)
vif(modelo2)
tab_model(modelo2, show.se = TRUE, digits = 3)

#Modelo 4
modelo4 <- lm(data = ESC_SC_ESTs,
              formula = TX_ABND_TOTEM_AP_MEAN ~ AP_RENDAPC_SLRMIN + AP_PCTNEGRAS + INSE_AB +
                RAZ_ALU_TUR + RAZ_ALU_PROF + RAZ_PC_ALU)

summary(modelo4)
vif(modelo4)
tab_model(modelo4, show.se = TRUE, digits = 3)


#Modelo 2v2
modelo2v2 <- lm(data = ESC_SC_ESTs,
              formula = TX_ABND_TOTEM ~ AP_RENDAPC_SLRMIN + INSE_AB +
                RAZ_ALU_TUR + RAZ_ALU_PROF + RAZ_PC_ALU)

summary(modelo2v2)
vif(modelo2v2)
tab_model(modelo2v2, show.se = TRUE, digits = 3)

#Modelo 4v2
modelo4v2 <- lm(data = ESC_SC_ESTs,
                formula = TX_ABND_TOTEM ~ AP_RENDAPC_SLRMIN + AP_PCTNEGRAS + INSE_AB +
                  RAZ_ALU_TUR + RAZ_ALU_PROF + RAZ_PC_ALU)

summary(modelo4v2)
vif(modelo4v2)
tab_model(modelo4v2, show.se = TRUE, digits = 3)



#Modelo 4v3
modelo4v3 <- lm(data = ESC_SC_ESTs,
                formula = TX_ABND_TOTEM ~ AP_RENDAPC_SLRMIN + AP_PCTNEGRAS + INSE_AP_MEAN +
                  RAZ_ALU_TUR_AP_MEAN + RAZ_ALU_PROF_AP_MEAN +
                  RAZ_PC_ALU_AP_MEAN)

summary(modelo4v3)
vif(modelo4v3)
tab_model(modelo4v3, show.se = TRUE, digits = 3)

#Modelo 2v3
modelo2v3 <- lm(data = ESC_SC_ESTs,
                formula = TX_ABND_TOTEM ~ AP_RENDAPC_SLRMIN + INSE_AP_MEAN +
                  RAZ_ALU_TUR_AP_MEAN + RAZ_ALU_PROF_AP_MEAN +
                  RAZ_PC_ALU_AP_MEAN)

summary(modelo2v3)
vif(modelo2v3)
tab_model(modelo2v3, show.se = TRUE, digits = 3)



#Modelo 1v3
modelo1v3 <- lm(data = ESC_SC_ESTs,
                formula = TX_ABND_TOTEM_AP_MEAN ~ AP_RENDAPC_SLRMIN + INSE_AP_MEAN +
                  RAZ_ALU_TUR_AP_MEAN + RAZ_ALU_PROF_AP_MEAN +
                  RAZ_PC_ALU_AP_MEAN)

summary(modelo1v3)
vif(modelo1v3)
tab_model(modelo1v3, show.se = TRUE, digits = 3)

#Modelo 3v3
modelo3v3 <- lm(data = ESC_SC_ESTs,
                formula = TX_ABND_TOTEM_AP_MEAN ~ AP_RENDAPC_SLRMIN+ AP_PCTNEGRAS + INSE_AP_MEAN +
                  RAZ_ALU_TUR_AP_MEAN + RAZ_ALU_PROF_AP_MEAN +
                  RAZ_PC_ALU_AP_MEAN)

summary(modelo3v3)
vif(modelo3v3)
tab_model(modelo3v3, show.se = TRUE, digits = 3)
