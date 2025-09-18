#-------------------------------------------------------------------------------
# Script principal para a análise da correlação entre radares e mortes
#-------------------------------------------------------------------------------

library(tidyverse)
options(scipen = 999)

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

#Calcular correlação por tamanho de município-----------------------------------

tamanhos <- data.frame(
  tamanho1 = c(50000, 100000, 250000, 500000, 100000000),
  tamanho2 = c(0, 50000, 100000, 250000, 500000)
)

resultado_correlacao_tamanhos <- tamanhos %>% 
  mutate(resultados_aninhados = map2(
    .x = tamanho1,
    .y = tamanho2,
    ~correlacao_por_tamanho(tamanho1 = .x, tamanho2 = .y)
  )) %>% 
  unnest(resultados_aninhados)

# Por estado -------------------------------------------------------------------

correlacao_por_estado <- base_principal %>% 
  group_by(uf) %>% 
  summarise(media_mortes = mean(mortes_10mil_veiculos, na.rm = T),
            media_radares = mean(radares_10mil_veiculos, na.rm = T))

ggplot(correlacao_por_estado, aes(media_mortes, media_radares))+
  geom_point()+
  geom_smooth()

correlacao_goias <- base_principal %>% 
  filter( uf == "Ceará" & cluster_c == 3)

cor.test(correlacao_goias$mortes_10mil_veiculos, correlacao_goias$radares_10mil_veiculos, method = "spearman")
ggplot(correlacao_goias, aes(mortes_10mil_veiculos, radares_10mil_veiculos))+
  geom_point()+
  geom_smooth()

#--------------------------------------------------------------------------------
resultado_correlacao_cluster_c <- combinacoes %>% 
  mutate(resultado = map2(
    .x = cluster_param,
    .y = porte_param,
    ~correlacao_cluster_porte(cluster = .x, porte = .y)
  )) %>% unnest()
#-------------------------------------------------------------------------------
cluster_lista <- 1:3
uf_lista <- unique(base_principal$uf)

combinacoes <- expand.grid(
  cluster_param = cluster_lista,
  uf_param = uf_lista)

resultado_correlacao_cluster_estado <- combinacoes %>% 
  mutate(resultados_aninhados = map2(
    .x = cluster_param,
    .y = uf_param,
    ~correlacao_cluster_estado(cluster = .x, uf = .y)
  )) %>% unnest()


#--------------------------------------------------------------------------------
results <- lapply(uf_lista, correlacao_estado)
results <- bind_rows(results)
