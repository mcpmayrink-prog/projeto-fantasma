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
relatorio  = left_join(relatorio_old_town_road, relatorio_old_town_road_1_, by = "ItemID") 
relatorio  = left_join(relatorio, relatorio_old_town_road_4_, by = "SaleID")
relatorio  = left_join(relatorio, relatorio_old_town_road_5_, by = "ClientID")
relatorio  = left_join(relatorio, relatorio_old_town_road_6_, by = "StoreID")
relatorio  = left_join(relatorio, relatorio_old_town_road_3_, by = "CityID")
relatorio = cross_join(relatorio, relatorio_old_town_road_2_)
relatorio
install.packages("stringr")
library(stringr)
library(dplyr)

# Ano 1880
anos1880 = relatorio %>%
  filter(str_starts(Date,"1880")) %>%
  select(ItemID, UnityPrice,Quantity,StoreID )
media1880 = anos1880 %>%
  group_by(StoreID) %>%
  summarise(mean(UnityPrice*Quantity))

# Ano 1881
anos1881 = relatorio %>%
  filter(str_starts(Date,"1881")) %>%
  select(ItemID, UnityPrice,Quantity,StoreID )
media1881 = anos1881 %>%
  group_by(StoreID) %>%
  summarise(mean(UnityPrice*Quantity))

# Ano 1882
anos1882 = relatorio %>%
  filter(str_starts(Date,"1882")) %>%
  select(ItemID, UnityPrice,Quantity,StoreID )
media1882 = anos1882 %>%
  group_by(StoreID) %>%
  summarise(mean(UnityPrice*Quantity))

# Ano 1883
anos1883 = relatorio %>%
  filter(str_starts(Date,"1883")) %>%
  select(ItemID, UnityPrice,Quantity,StoreID)
media1883 = anos1883 %>%
  group_by(StoreID) %>%
  summarise(mean(UnityPrice*Quantity))
 
# Ano 1884
anos1884 = relatorio %>%
  filter(str_starts(Date,"1884")) %>%
  select(ItemID, UnityPrice,Quantity,StoreID )
media1884 = anos1884 %>%
  group_by(StoreID) %>%
  summarise(mean(UnityPrice*Quantity))

# Ano 1885
anos1885 = relatorio %>%
  filter(str_starts(Date,"1885")) %>%
  select(ItemID, UnityPrice,Quantity,StoreID)
media1885 = anos1885 %>%
  group_by(StoreID) %>%
  summarise(mean(UnityPrice*Quantity))

# Ano 1886
anos1886 = relatorio %>%
  filter(str_starts(Date,"1886")) %>%
  select(ItemID, UnityPrice,Quantity,StoreID )
media1886 = anos1886 %>%
  group_by(StoreID) %>%
  summarise(mean(UnityPrice*Quantity))

# Ano 1887
anos1887 = relatorio %>%
  filter(str_starts(Date,"1887")) %>%
  select(ItemID, UnityPrice,Quantity,StoreID )
media1887 = anos1887 %>%
  group_by(StoreID) %>%
  summarise(mean(UnityPrice*Quantity))

# Ano 1888
anos1888 = relatorio %>%
  filter(str_starts(Date,"1888")) %>%
  select(ItemID, UnityPrice,Quantity,StoreID )
media1888 = anos1888 %>%
  group_by(StoreID) %>%
  summarise(mean(UnityPrice*Quantity))

# Ano 1889
anos1889 = relatorio %>%
  filter(str_starts(Date,"1889")) %>%
  select(ItemID, UnityPrice,Quantity,StoreID )
media1889 = anos1889 %>%
  group_by(StoreID) %>%
  summarise(Receitas = mean(UnityPrice*Quantity))

# Juntando as médias 
medias = as.data.frame(c(media1880, media1881, media1882, media1883, media1884, media1885, media1886, media1887, media1888, media1889))
medias = medias %>%
  select(mean.UnityPrice...Quantity., mean.UnityPrice...Quantity..1, mean.UnityPrice...Quantity..2, mean.UnityPrice...Quantity..3, mean.UnityPrice...Quantity..4, mean.UnityPrice...Quantity..5, mean.UnityPrice...Quantity..6, mean.UnityPrice...Quantity..7, mean.UnityPrice...Quantity..8, Receitas)
names(medias)[c(1)] = c("media1880")
names(medias)[c(2)] = c("media1881")
names(medias)[c(3)] = c("media1882")
names(medias)[c(4)] = c("media1883")
names(medias)[c(5)] = c("media1884")
names(medias)[c(6)] = c("media1885")
names(medias)[c(7)] = c("media1886")
names(medias)[c(8)] = c("media1887")
names(medias)[c(9)] = c("media1888")
names(medias)[c(10)] = c("media1889")
medias = medias*5.31
medias = colSums(medias)
medias = as.data.frame(medias)
medias$Ano = c(1880:1889)
min(medias$medias)
max(medias$medias)
#Criando o gráfico 
graf_medias <- ggplot(medias) +
  aes(Ano, medias , group=1) +
  geom_line(size=1,colour="#A11D21") + geom_point(colour="#A11D21",
                                                  size=2) +
  labs(x="Ano", y="Médias dos anos") +
  theme_estat()


