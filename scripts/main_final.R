#-------------------------------------------------------------------------------
# Adicionando municípios outliers ---------------------------------------------
library(tidyverse)
options(scipen = 999)

source('scripts/functions.R')

base_principal <- read.csv('output/base_com_outliers.csv')

base <- base_principal[-1441,]

base <- base %>% 
  mutate(cluster_c = replace_na(cluster_c, 0),
         c1 = (c1 - min(c1))/(max(c1) - min(c1)),
         c2 = (c2 - min(c2))/(max(c2) - min(c2)),
         c3 = (c3 - min(c3))/(max(c3) - min(c3)),
         media_c = (c1+c2+c3)/3)

indicadores_socioeconomicos <- base %>% 
  group_by(porte, cluster_c) %>% 
  summarise(motorizacao_media = mean(c1),
            pib_medio = mean(c2),
            idhm_medio = mean(c3)) %>% 
  mutate(media_indicadores = (motorizacao_media+pib_medio+ idhm_medio)/3) %>% 
  select(porte, cluster_c, media_indicadores)

# medias_indicadores <- indicadores_socioeconomicos %>% 
#   filter(cluster_c!=0) %>% 
#   group_by(porte) %>% 
#   summarise(motorizacao = mean(motorizacao_media),
#             pib = mean(pib_medio),
#             idhm = mean(idhm_medio))



outra <- base %>% 
  left_join(tabela_pivotada, by ="porte")

tabela_pivotada <- indicadores_socioeconomicos %>% 
  pivot_wider(
    names_from = cluster_c,
    values_from = media_indicadores
  ) %>%  select(-"0") %>% 
  rename(cluster_1 = "1" ,
         cluster_2 = "2",
         cluster_3 = "3")

base <- outra %>% 
  mutate(
    diff_1 = abs(media_c - cluster_1 ),
    diff_2 = abs(media_c - cluster_2),
    diff_3 = abs(media_c - cluster_3),
    decisao = pmin(diff_1, diff_2, diff_3), 
    decisao_index = case_when(
      decisao == diff_1 ~ "Cluster 1",
      decisao == diff_2 ~ "Cluster 2",
      decisao == diff_3 ~ "Cluster 3"
    )) %>% 
  select(-c(cluster_1, cluster_2, cluster_3)) %>% 
  mutate(cluster = case_when(
    cluster_c == 0 ~ decisao_index,
    cluster_c == 1 ~ "Cluster 1",
    cluster_c == 2 ~ "Cluster 2",
    cluster_c == 3 ~ "Cluster 3"
  )) %>% rename(uf = sigla, )

f <- erros %>% 
  filter(cluster_c != 0) %>% 
  mutate(verif = ifelse(decisao_index == cluster, 0, 1))


sum(f$verif)
erros %>% filter(cluster_c != 0) %>% nrow()

# Obtenção dos níveis mínimos---------------------------------------------------
lista_clusters <- c("Cluster 1", "Cluster 2", "Cluster 3")
lista_portes <- c("Menor porte", "Médio porte", "Maior porte")

combinacoes <- expand.grid(porte_param = lista_portes,
                           cluster_param = lista_clusters)

resultado_quartis <- combinacoes %>% 
  mutate(reusltados_aninhados = map2(
    .x = cluster_param,
    .y = porte_param,
    ~calcula_valores_ifs(cluster = .x, porte = .y)
  )) %>% unnest() %>% 
  mutate(porte_param = factor(porte_param, 
                              levels = c("Menor porte", "Médio porte", "Maior porte")))%>% 
  arrange(porte_param, cluster_param)

referencia_radares <- base %>% 
  left_join(resultado_quartis, by = c("porte" = "porte_param", "cluster" = "cluster_param")) %>% 
  mutate(
    valor_abs = (valor_q3 * frota_23)/10000
  )

write.csv(referencia_radares, "output/base_referencia_radares.csv")
write.csv(referencia_radares, "shiny/base_referencia_radares.csv")

# Municípios outliers e com 0 mortes--------------------------------------------
outliers <- read.csv("bases/outliers_table.csv")

