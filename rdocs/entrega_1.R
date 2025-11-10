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

##Análise 1

##código pra limpar o banco



install.packages('tinytex')
library(tinytex)
install.packages("vctrs")
library("vctrs")
installed.packages('vctrs')

install.packages("tidyverse")
library(tidyverse)
library(dplyr)

library(readxl)
relatorio_old_town_road <- read_excel("C:/Users/maria clara mayrink/Downloads/relatorio_old_town_road.xlsx", 
                                      sheet = "infos_produtos")
View(relatorio_old_town_road)

library(readxl)
relatorio_old_town_road_1_ <- read_excel("C:/Users/maria clara mayrink/Downloads/relatorio_old_town_road (1).xlsx", 
                                         sheet = "infos_vendas")
View(relatorio_old_town_road_1_)

library(readxl)
relatorio_old_town_road_2_ <- read_excel("C:/Users/maria clara mayrink/Downloads/relatorio_old_town_road (2).xlsx", 
                                         sheet = "infos_funcionarios")
View(relatorio_old_town_road_2_)

library(readxl)
relatorio_old_town_road_3_ <- read_excel("C:/Users/maria clara mayrink/Downloads/relatorio_old_town_road (3).xlsx", 
                                         sheet = "infos_cidades")
View(relatorio_old_town_road_3_)

library(readxl)
relatorio_old_town_road_4_ <- read_excel("C:/Users/maria clara mayrink/Downloads/relatorio_old_town_road (4).xlsx", 
                                         sheet = "relatorio_vendas")
View(relatorio_old_town_road_4_)

library(readxl)
relatorio_old_town_road_5_ <- read_excel("C:/Users/maria clara mayrink/Downloads/relatorio_old_town_road (5).xlsx", 
                                         sheet = "infos_clientes")
View(relatorio_old_town_road_5_)

library(readxl)
relatorio_old_town_road_6_ <- read_excel("C:/Users/maria clara mayrink/Downloads/relatorio_old_town_road (6).xlsx", 
                                         sheet = "infos_lojas")
View(relatorio_old_town_road_6_)

names(relatorio_old_town_road)[c(1)] = c("ItemID")
names(relatorio_old_town_road_1_)[c(1)] = c("SaleID")
names(relatorio_old_town_road_2_)[c(1)] = c("EmployeeID")
names(relatorio_old_town_road_3_)[c(1)] = c ("CityID")
names(relatorio_old_town_road_5_)[c(1)] = c("ClientID")
names(relatorio_old_town_road_6_)[c(1)] = c("StoreID")

install.packages("stringr")
library(stringr)
library(dplyr)



rel_1 = left_join(relatorio_old_town_road, relatorio_old_town_road_1_, by = "ItemID")
rel_1 = left_join(rel_1, relatorio_old_town_road_4_, by = "SaleID")
rel_medias = rel_1 %>%
  select(Date, StoreID, UnityPrice, Quantity)%>%
  mutate(UnityPrice = UnityPrice*5.31, rec_ = UnityPrice*Quantity, Date = ymd(Date), Date = format(Date, "%Y"))%>%
  group_by(Date, StoreID) %>%
  summarise(rec_total = sum(rec_))%>%
  ungroup()%>%
  group_by(Date)%>%
  summarise(rec_media = mean(rec_total))
#Criando o gráfico 
graf_medias <- ggplot(rel_medias) +
  aes(Date,rec_media , group=1) +
  geom_line(size=1,colour="#A11D21") + geom_point(colour="#A11D21",
                                                  size=2) +
  labs(x="Ano", y="Médias dos anos") +
  theme_estat()


