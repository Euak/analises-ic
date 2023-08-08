#TX_ABND_TOTEM
summary(ESC_AP_MODELVARS$TX_ABND_TOTEM_AP_MEAN)
var(ESC_AP_MODELVARS$TX_ABND_TOTEM_AP_MEAN, na.rm = T)
sd(ESC_AP_MODELVARS$TX_ABND_TOTEM_AP_MEAN, na.rm = T)
ESC_AP_MODELVARS %>% 
  ggplot(aes(TX_ABND_TOTEM_AP_MEAN)) +
  geom_histogram() +
  labs(x = "TX_ABND_TOTEM", y = "Contagem")
ESC_AP_MODELVARS %>% 
  ggplot(aes(y = TX_ABND_TOTEM_AP_MEAN)) +
  geom_boxplot()

#AP_RENDAPC_SLRMIN
summary(ESC_AP_MODELVARS$AP_RENDAPC_SLRMIN)
var(ESC_AP_MODELVARS$AP_RENDAPC_SLRMIN, na.rm = T)
sd(ESC_AP_MODELVARS$AP_RENDAPC_SLRMIN, na.rm = T)
ESC_AP_MODELVARS %>% 
  ggplot(aes(AP_RENDAPC_SLRMIN)) +
  geom_histogram() +
  labs(x = "AP_RENDAPC_SLRMIN", y = "Contagem")
ESC_AP_MODELVARS %>% 
  ggplot(aes(y = AP_RENDAPC_SLRMIN)) +
  geom_boxplot()

#PCT Pessoas Negras
summary(ESC_AP_MODELVARS$AP_PCTNEGRAS)
var(ESC_AP_MODELVARS$AP_PCTNEGRAS, na.rm = T)
sd(ESC_AP_MODELVARS$AP_PCTNEGRAS, na.rm = T)
ESC_AP_MODELVARS %>% 
  ggplot(aes(AP_PCTNEGRAS)) +
  geom_histogram() +
  labs(x = "AP_PCTNEGRAS", y = "Contagem")
ESC_AP_MODELVARS %>% 
  ggplot(aes(y = AP_PCTNEGRAS)) +
  geom_boxplot()

#INSE
summary(ESC_AP_MODELVARS$INSE_AP_MEAN)
var(ESC_AP_MODELVARS$INSE_AP_MEAN, na.rm = T)
sd(ESC_AP_MODELVARS$INSE_AP_MEAN, na.rm = T)
ESC_AP_MODELVARS %>% 
  ggplot(aes(INSE_AP_MEAN)) +
  geom_histogram() +
  labs(x = "INSE_AP_MEAN", y = "Contagem")
ESC_AP_MODELVARS %>% 
  ggplot(aes(y = INSE_AP_MEAN)) +
  geom_boxplot()

#Aluno/Turma
summary(ESC_AP_MODELVARS$RAZ_ALU_TUR_AP_MEAN)
var(ESC_AP_MODELVARS$RAZ_ALU_TUR_AP_MEAN, na.rm = T)
sd(ESC_AP_MODELVARS$RAZ_ALU_TUR_AP_MEAN, na.rm = T)
ESC_AP_MODELVARS %>% 
  ggplot(aes(RAZ_ALU_TUR_AP_MEAN)) +
  geom_histogram() +
  labs(x = "RAZ_ALU_TUR_AP_MEAN", y = "Contagem")
ESC_AP_MODELVARS %>% 
  ggplot(aes(y = RAZ_ALU_TUR_AP_MEAN)) +
  geom_boxplot()

#Aluno/Professor
summary(ESC_AP_MODELVARS$RAZ_ALU_PROF_AP_MEAN)
var(ESC_AP_MODELVARS$RAZ_ALU_PROF_AP_MEAN, na.rm = T)
sd(ESC_AP_MODELVARS$RAZ_ALU_PROF_AP_MEAN, na.rm = T)
ESC_AP_MODELVARS %>% 
  ggplot(aes(RAZ_ALU_PROF_AP_MEAN)) +
  geom_histogram() +
  labs(x = "RAZ_ALU_PROF_AP_MEAN", y = "Contagem")
ESC_AP_MODELVARS %>% 
  ggplot(aes(y = RAZ_ALU_PROF_AP_MEAN)) +
  geom_boxplot()

#Computador/Aluno
summary(ESC_AP_MODELVARS$RAZ_PC_ALU_AP_MEAN)
var(ESC_AP_MODELVARS$RAZ_PC_ALU_AP_MEAN, na.rm = T)
sd(ESC_AP_MODELVARS$RAZ_PC_ALU_AP_MEAN, na.rm = T)
ESC_AP_MODELVARS %>% 
  ggplot(aes(RAZ_PC_ALU_AP_MEAN)) +
  geom_histogram() +
  labs(x = "RAZ_PC_ALU_AP_MEAN", y = "Contagem")
ESC_AP_MODELVARS %>% 
  ggplot(aes(y = RAZ_PC_ALU_AP_MEAN)) +
  geom_boxplot()

#Correlações
ESC_AP_MODELVARS %>% 
  ggplot(aes(x = AP_RENDAPC_SLRMIN, y = TX_ABND_TOTEM_AP_MEAN)) +
  geom_point()

#Correlações
ESC_AP_MODELVARS %>% 
  ggplot(aes(x = AP_PCTNEGRAS, y = TX_ABND_TOTEM_AP_MEAN)) +
  geom_point()

#Modelo 1
modelo1 <- lm(data = ESC_AP_MODELVARS,
                formula = TX_ABND_TOTEM_AP_MEAN ~ AP_RENDAPC_SLRMIN)

summary(modelo1)
tab_model(modelo1, show.se = TRUE, digits = 3)

#Modelo 2
modelo2 <- lm(data = ESC_AP_MODELVARS,
              formula = TX_ABND_TOTEM_AP_MEAN ~ AP_RENDAPC_SLRMIN + AP_PCTNEGRAS)

summary(modelo2)
vif(modelo2)
tab_model(modelo2, show.se = TRUE, digits = 3)

#Modelo 3
modelo3 <- lm(data = ESC_AP_MODELVARS,
              formula = TX_ABND_TOTEM_AP_MEAN ~ AP_RENDAPC_SLRMIN + AP_PCTNEGRAS +
                INSE_AP_MEAN)

summary(modelo3)
vif(modelo3)
tab_model(modelo3, show.se = TRUE, digits = 3)

#Modelo 4
modelo4 <- lm(data = ESC_AP_MODELVARS,
              formula = TX_ABND_TOTEM_AP_MEAN ~ AP_RENDAPC_SLRMIN +
                INSE_AP_MEAN + RAZ_ALU_TUR_AP_MEAN + RAZ_ALU_PROF_AP_MEAN +
                RAZ_PC_ALU_AP_MEAN)

summary(modelo4)
vif(modelo4)
tab_model(modelo4, show.se = TRUE, digits = 3)

#Modelo 5
modelo5 <- lm(data = ESC_AP_MODELVARS,
              formula = TX_ABND_TOTEM_AP_MEAN ~ AP_PCTNEGRAS +
                INSE_AP_MEAN + RAZ_ALU_TUR_AP_MEAN + RAZ_ALU_PROF_AP_MEAN +
                RAZ_PC_ALU_AP_MEAN)

summary(modelo5)
vif(modelo5)
tab_model(modelo5, show.se = TRUE, digits = 3)

#Modelo 6
modelo6 <- lm(data = ESC_AP_MODELVARS,
              formula = TX_ABND_TOTEM_AP_MEAN ~ AP_RENDAPC_SLRMIN + AP_PCTNEGRAS +
                INSE_AP_MEAN + RAZ_ALU_TUR_AP_MEAN + RAZ_ALU_PROF_AP_MEAN +
                RAZ_PC_ALU_AP_MEAN)

summary(modelo6)
vif(modelo6)
tab_model(modelo6, show.se = TRUE, digits = 3)
