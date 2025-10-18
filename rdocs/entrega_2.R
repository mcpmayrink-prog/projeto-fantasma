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

relatorio$Weight_kls = relatorio$Weight_lbs * 0.453
relatorio$Weight_lbs = NULL

# Transformando dm em cm 
relatorio$Height_cm = relatorio$Height_dm * 10
relatorio$Height_dm = NULL



library(tidyverse)



graf = ggplot(relatorio) +
  aes(x = Weight_kls, y = Height_cm) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(x = "Peso do Cliente (quilogramas)",y = "Altura do Cliente (centímetros)") +
  theme_estat()

ggsave("disp_uni.pdf", width = 158, height = 93, units = "mm")
