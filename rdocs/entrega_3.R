source("rdocs/source/packages.R")

# ---------------------------------------------------------------------------- #

#        ______   _____  ________      ________ 
#      |  ____| / ____| |__   __| /\  |__   __|
#     | |__    | (___     | |   /  \    | |   
#    |  __|    \___ \    | |  / /\ \   | |   
#   | |____   ____) |   | |  /____ \  | |   
#  |______   |_____/   |_| /_/    \_\|_|   
#  
#         Consultoria estatística 
#

# ---------------------------------------------------------------------------- #
# ############################## README ###################################### #
# Consultor, favor utilizar este arquivo .R para realizar TODAS as análises
# alocadas a você neste projeto pelo gerente responsável, salvo instrução 
# explícita do gerente para mudança.
#
# Escreva seu código da forma mais clara e legível possível, eliminando códigos
# de teste depreciados, ou ao menos deixando como comentário. Dê preferência
# as funções dos pacotes contidos no Tidyverse para realizar suas análises.
# ---------------------------------------------------------------------------- #


library("tidyverse")

# Filtrando as lojas de Ambar Seco 
cidade2 = relatorio_old_town_road_6_ %>%
  filter(CityID == 2)
# Filtrando a identificação do cliente pelas lojas 
cliente = relatorio_old_town_road_4_ %>%
  filter(StoreID %in% c(2,6,8,9)) %>%
  select(StoreID, ClientID)
cliente = distinct(cliente)
names(cliente) [c(2)]= c("Cli3ntID")
names(cliente) [c(1)]= c("Stor3ID")
# Filtrando a idade dos clientes
idade = relatorio_old_town_road_5_ %>%
  select(Age, Cli3ntID)
idade = distinct(idade)
names(idade)[c(2)] = c("Cli3ntID")
# Juntando os dados 
idade_cliente = left_join(cliente, idade, by = "Cli3ntID")
idade_cliente = left_join(idade_cliente,cidade2 , by = "Stor3ID")
names(relatorio_old_town_road_5_)[c(1)] = c("Cli3ntID")
names(relatorio_old_town_road_6_)[c(1)] = c("Stor3ID")

# Grafico 
graf_idade_cliente = ggplot(idade_cliente) +
  aes(x = Age) +
  geom_histogram(colour = "white", fill = "#A11D21", binwidth = 7) +
  facet_grid(. ~ NameStore) +
  labs(x = "Idade dos clientes (anos)", y = "Frequência") +
  theme_estat(
    strip.text = element_text(size = 12),
    strip.background = element_rect(colour = "black", fill = "white")
  )

