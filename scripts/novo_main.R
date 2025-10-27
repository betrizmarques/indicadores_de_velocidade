#-------------------------------------------------------------------------------
# Novo método para o estudo dos indicadores-------------------------------------
library(tidyverse)
options(scipen = 999)

source('scripts/functions.R')


base_principal <- read.csv('output/base_principal_indicadores.csv')

base_principal <- base_principal %>% 
  mutate(cluster_junto = ifelse(cluster_c == 2, "Cluster 2", "Clusters 1 e 3"))


lista_clusters <- c("Clusters 1 e 3", "Cluster 2")
lista_portes <- c("Menor porte", "Médio porte", "Maior porte")

combinacoes <- expand.grid(porte_param = lista_portes,
                           cluster_param = lista_clusters)

resultado_quartis <- combinacoes %>% 
  mutate(reusltados_aninhados = map2(
    .x = cluster_param,
    .y = porte_param,
    ~calcula_valores_ifs(cluster_junto = .x, porte = .y)
  )) %>% unnest() %>% 
  mutate(porte_param = factor(porte_param, 
                levels = c("Menor porte", "Médio porte", "Maior porte")))%>% 
  arrange(porte_param, cluster_param)



referencia_radares <- base_principal %>% 
  left_join(resultado_quartis, by = c("porte" = "porte_param", "cluster_junto" = "cluster_param")) %>% 
  mutate(
    valor_abs = (valor_q3 * frota_23)/10000
  )

View(referencia_radares)
write.csv(referencia_radares, "output/base_referencia_radares.csv")
write.csv(referencia_radares, "shiny/base_referencia_radares.csv")
