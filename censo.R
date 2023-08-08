library(tidyverse)

#Importa base de dados do Censo 2010
#Donwload -> https://drive.google.com/file/d/1-AOY_BU7aZjlTKCWjpD21In2ypUvW2GR/view?usp=drive_link
df.censo <- read_csv("Bases/CEM/Censo/tPes202362916299713.csv.gz")

#Filtra para a Cidade de SP
df.censo <- df.censo %>% 
  subset(cem_harm_uf == 35) %>% 
  subset(V0002 == 50308)

#Renomeia as variáveis
df.censo <- df.censo %>% 
  rename('municipio' = V0002,
         'a_ponderacao' = V0011,
         'parentesco' = V0502,
         'sexo' = V0601,
         'raca' = V0606,
         'curso_mais_elevado' = V0633,
         'concluiu' = V0634,
         'idade_meses' = V6033,
         'idade' = V6036,
         'ocupacao' = V6461,
         'atividade' = V6471,
         'renda_fami_pcapita' = V6531)

#Tipifica as variáveis em fatores
df.censo$a_ponderacao <-  as.factor(df.censo$a_ponderacao)
df.censo$parentesco <-  as.factor(df.censo$parentesco)
df.censo$sexo <-  as.factor(df.censo$sexo)
df.censo$raca <-  as.factor(df.censo$raca)
df.censo$curso_mais_elevado <-  as.factor(df.censo$curso_mais_elevado)
df.censo$ocupacao <-  as.factor(df.censo$ocupacao)
df.censo$atividade <-  as.factor(df.censo$atividade)

#Cria tabela de responsáveis pelo domicílio
df.responsaveis <- df.censo %>% 
  select(cem_harm_iddomicilio, parentesco, curso_mais_elevado, concluiu,
         ocupacao, atividade, sexo, raca) %>% 
  filter(parentesco %in% c(1, 2, 3)) # Filtra repsonsáveis e cônjugues

#Ajusta tabela de responsáveis para ficar 1 domicílio por linha
df.responsaveis <- df.responsaveis %>% 
  mutate(responsavel = if_else(parentesco == 1, 'resp', 'conj')) %>% 
  pivot_wider(names_from = responsavel, values_from = c(parentesco, curso_mais_elevado, concluiu,
                                                       ocupacao, atividade, sexo, raca))

#Atribui as características dos responsáveis por domicílio no DF geral 
df.join <- left_join(df.censo, df.responsaveis, by = c("cem_harm_iddomicilio" = "cem_harm_iddomicilio"))


#Filtra os filtra os tutelados com idade maior de 15 e menor 19, que é fora da idade distorção idade-série do INEP
df.join <- df.join %>% 
  filter(parentesco %in% c(4, 5, 6, 10, 11)) %>% #filtra tutelados
  filter(idade > 18) %>% # Com idade de ter concluido o ensino medio
  filter(curso_mais_elevado %in% c(10, 11, 12, 13, 14)) #frequentou pelo menos o ensino medio

df.join <- df.join %>% 
  mutate(concluiu_medio = if_else(curso_mais_elevado == 10 & concluiu == 2, 0, 1))

df.join %>% 
  select(curso_mais_elevado, concluiu, concluiu_medio) %>% 
  View()

df.join %>% 
  group_by(concluiu_medio, a_ponderacao) %>% 
  summarise("qtde" = n()) %>% 
  View()

df.join %>% 
  group_by(concluiu_medio, a_ponderacao) %>% 
  summarise("qtde" = n()) %>% 
  filter(concluiu_medio == 0) %>% 
  ggplot(aes(qtde)) + 
  geom_histogram()

