#-------------------------------------------------------------------------------
# Script principal para a análise da correlação entre radares e mortes
#-------------------------------------------------------------------------------

library(tidyverse)

source('scripts/functions.R')


base_principal <- read.csv('output/base_principal_indicadores.csv')


lista_correlacoes <- lapply(1:79, calcula_correlacao_clusters)
data_frame_correlacoes <- bind_rows(lista_correlacoes)

#-------------------------------------------------------------------------------
cluster_lista <- 1:3
porte_lista <- c("Menor porte", "Médio porte", "Maior porte")

# Criar um data.frame com todas as 9 combinações
combinacoes <- expand.grid(
  cluster_param = cluster_lista,
  porte_param = porte_lista
)

resultado_final_quartis_75 <- expand.grid(
  cluster_param = cluster_lista,
  porte_param = porte_lista
) %>%
  mutate(resultados_aninhados = map2(
    .x = cluster_param,
    .y = porte_param,
    ~ calculo_quartis_75(cluster = .x, porte = .y) 
  )) %>%
  unnest(resultados_aninhados) 

#-------------------------------------------------------------------------------

resultado_final_quartis <- combinacoes %>%
  mutate(resultados_aninhados = map2(
    .x = cluster_param,
    .y = porte_param,
    ~ calculo_quartis(cluster = .x, porte = .y) 
  )) %>%
  unnest(resultados_aninhados)

#-------------------------------------------------------------------------------
resultado_final_cluster_c <- combinacoes %>% 
  mutate(resultados_aninhados = map2(
    .x = cluster_param,
    .y = porte_param,
    ~calcula_correlacao_clusters_c(cluster = .x, porte = .y)
  )) %>%  unnest(resultados_aninhados)

#-------------------------------------------------------------------------------
base_limpa <- na.omit(base_principal[c("I1", "mortes_10mil_veiculos")])
correlacao <- cor.test(base_limpa$I1, base_limpa$mortes_10mil_veiculos, method = 'spearman')
g <- ggplot(base_limpa, aes(mortes_10mil_veiculos, I1))+
  geom_point(size = 0.7)+
  geom_smooth()+
  
  labs(
    title = paste( "p-valor:", round(correlacao$p.value, 4), "Rho:", round(correlacao$estimate, 4))
  )
