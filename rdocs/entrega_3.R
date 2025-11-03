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
names(cliente) [c(2)]= c("ClientID")
names(cliente) [c(1)]= c("StoreID")
# Filtrando a idade dos clientes
idade = relatorio_old_town_road_5_ %>%
  select(Age, ClientID)
idade = distinct(idade)
names(idade)[c(2)] = c("ClientID")
# Juntando os dados 
idade_cliente = left_join(cliente, idade, by = "ClientID")
idade_cliente = left_join(idade_cliente,cidade2 , by = "StoreID")
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
idade_cliente %>%
  print_quadro_resumo(Age)



# Análise 4

top_3_receitas = as.data.frame(c(media1889)) ##feito na entrega_1.R
top_3_receitas = top_3_receitas %>%
  slice_max(top_3_receitas$Receitas, n = 3)
produtos = left_join(relatorio_old_town_road_1_, relatorio_old_town_road_4_, by = "SaleID") %>%
  select(ItemID,Quantity, StoreID) %>%
  filter(StoreID %in% c(5,14,17)) 
top_3_receitas$NomeLoja = c("Loja TendTudo", "Ferraria Martelo de Ferro", "Ferraria Apache")

top_3_produtos_5 = produtos %>%
  group_by(ItemID) %>%
  filter(StoreID == 5) %>%
  summarise(sum(Quantity)) 
names(top_3_produtos_5)[c(2)] = c("Quantidades")
top_3_produtos_5 = top_3_produtos_5 %>%
  slice_max(Quantidades, n = 3)
top_3_produtos_5$NomeProduto = c("Chapéu de Couro", "Colt .45", "Espingarda")

top_3_produtos_14 = produtos %>%
  group_by(ItemID) %>%
  filter(StoreID == 14) %>%
  summarise(sum(Quantity)) 
names(top_3_produtos_14)[c(2)] = c("Quantidades")
top_3_produtos_14 = top_3_produtos_14 %>%
  slice_max(Quantidades, n = 3)
top_3_produtos_14$NomeProduto = c("Chapéu de Couro","Espingarda", "Pá" )

top_3_produtos_17 = produtos %>%
  group_by(ItemID) %>%
  filter(StoreID == 17) %>%
  summarise(sum(Quantity)) 
names(top_3_produtos_17)[c(2)] = c("Quantidades")
top_3_produtos_17 = top_3_produtos_17 %>%
  slice_max(Quantidades, n = 3)
top_3_produtos_17$NomeProduto = c("Chapéu de Couro","Colt .45", "Sela" )

# Fazendo gráficos 

receitas <- top_3_receitas %>%
  filter(!is.na(Receitas)) %>%
  count(Receitas) %>%
  mutate(
    freq = n,
    relative_freq = round((freq / sum(freq)) * 100, 1),
    freq = gsub("\\.", ",", relative_freq) %>% paste("%", sep = ""),
    label = str_c(n, " (", freq, ")") %>% str_squish()
  )
graf_receitas = ggplot(receitas) +   
  aes(x = fct_reorder(receitas$Receitas, n, .desc=T), y = n, label = label) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, #hjust = .5,
    size = 3
  ) +
  labs(x = "Nome das lojas", y = "Frequência") +
  theme_estat()
