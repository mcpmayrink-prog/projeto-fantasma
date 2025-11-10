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


# Transformando pounds em kilogramas

relatorio_old_town_road_5_$Weight_kls = relatorio_old_town_road_5_$Weight_lbs*0.454
relatorio_old_town_road_5_$Weight_lbs = NULL

# Transformando dm em cm 
relatorio_old_town_road_5_$Height_cm = relatorio_old_town_road_5_$Height_dm * 10
relatorio_old_town_road_5_$Height_dm = NULL


library(tidyverse)

# Fazendo o gráfico Peso/Altura
graf <- ggplot(relatorio_old_town_road_5_, aes(x = Height_cm, y = Weight_kls)) +
  geom_point(colour = "#A11D21",size = 2, alpha = 0.3) +
  labs(x = "Altura do Cliente (cm)",y = "Peso do Cliente (kg)") +
  theme_estat()



# Fazendo os quadros de medidas
relatorio_old_town_road_5_ %>%
  print_quadro_resumo(var_name = Weight_kls)
relatorio_old_town_road_5_ %>%
  print_quadro_resumo(var_name = Height_cm)

# Calculando a correlação de pearson
cor(relatorio_old_town_road_5_$Height_cm, relatorio_old_town_road_5_$Weight_kls)
