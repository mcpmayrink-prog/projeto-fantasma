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
relatorio_old_town_road <- read_excel("C:/Users/maria clara mayrink/Downloads/relatorio_old_town_road.xlsx", 
                                      sheet = "infos_produtos")

relatorio_old_town_road_1_ <- read_excel("C:/Users/maria clara mayrink/Downloads/relatorio_old_town_road (1).xlsx", 
                                         sheet = "infos_vendas")


relatorio_old_town_road_4_ <- read_excel("C:/Users/maria clara mayrink/Downloads/relatorio_old_town_road (4).xlsx", 
                                         sheet = "relatorio_vendas")

relatorio_old_town_road_6_ <- read_excel("C:/Users/maria clara mayrink/Downloads/relatorio_old_town_road (6).xlsx", 
                                         sheet = "infos_lojas")
relatorio4  = left_join(relatorio_old_town_road, relatorio_old_town_road_1_, by = "ItemID") 
relatorio4  = left_join(relatorio4, relatorio_old_town_road_4_, by = "SaleID")
relatorio4  = left_join(relatorio4, relatorio_old_town_road_6_, by = "StoreID")

top_3 = relatorio4 %>%
  select(NameStore, Quantity, NameProduct, StoreID,UnityPrice,Date)%>%
  group_by(StoreID, NameStore, NameProduct)%>%
  filter(str_starts(Date, "1889"))%>%
  mutate(UnityPrice = UnityPrice*5.31, Receita = UnityPrice*Quantity)%>%
  summarise(Receita = sum(Receita))
top_3 %>%
  ungroup()%>%
  group_by(StoreID)%>%
  summarise(rec = sum(Receita))%>%
  arrange(desc(rec))
top_3 = top_3 %>%
  
  ungroup()

rel <- relatorio4 %>%
  filter(StoreID %in% c(7,5,17))%>%
  group_by(NameStore, NameProduct) %>%
  summarise(freq = n()) %>%
  mutate(
    freq_relativa = round(freq / sum(freq) * 100,1)
  )%>%
  arrange(desc(freq))%>%
  slice_max(freq, n = 3)
porcentagens <- str_c(rel$freq_relativa, "%") %>% str_replace("\\.", ",")
legendas <- str_squish(str_c(rel$freq, " (", porcentagens, ")"))
graf_rel = ggplot(rel) +
  aes(
    x = fct_reorder(rel$NameStore, freq, .desc = T), y = freq,
    fill = NameProduct, label = legendas
  ) +
  geom_col(position = position_dodge2(preserve = "single", padding =
                                        0)) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, hjust = 0.5,
    size = 3) +
  labs(x = "Nome da Loja", y = "Frequência", fill = "Nome do Produto") +
  theme_estat()

