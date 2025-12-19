#-------------------------------------------------------------------------------
# Adicionando municípios outliers ---------------------------------------------
library(tidyverse)
library(magrittr)
library(janitor)

options(scipen = 999)

source('scripts/functions.R')

base <- read.csv('output/base_com_outliers.csv')


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


tabela_pivotada <- indicadores_socioeconomicos %>% 
  pivot_wider(
    names_from = cluster_c,
    values_from = media_indicadores
  ) %>%  select(-"0") %>% 
  rename(cluster_1 = "1" ,
         cluster_2 = "2",
         cluster_3 = "3")

outra <- base %>% 
  left_join(tabela_pivotada, by ="porte")

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
    ))  %>% 
  mutate(cluster = case_when(
    cluster_c == 0 ~ decisao_index,
    cluster_c == 1 ~ "Cluster 1",
    cluster_c == 2 ~ "Cluster 2",
    cluster_c == 3 ~ "Cluster 3"
  )) 


clusters_agrupados <- base %>% 
  group_by(porte, cluster) %>% 
  summarise(contagem_municipios = n())


f <- base %>% 
  filter(cluster_c != 0) %>% 
  mutate(verif = ifelse(decisao_index == cluster, 0, 1))


sum(f$verif)
base %>% filter(cluster_c != 0) %>% nrow()

# Obtenção dos níveis mínimos---------------------------------------------------
lista_clusters <- c("Cluster 1", "Cluster 2", "Cluster 3")
lista_portes <- c("Menor porte", "Médio porte", "Maior porte")

combinacoes <- expand.grid(porte_param = lista_portes,
                           cluster_param = lista_clusters)

est_descritivas <- combinacoes %>% 
  mutate(reusltados_aninhados = map2(
    .x = cluster_param,
    .y = porte_param,
    ~calcula_valores_ifs(cluster = .x, porte = .y)
  )) %>% unnest() %>% 
  mutate(porte_param = factor(porte_param, 
                              levels = c("Menor porte", "Médio porte", "Maior porte")))%>% 
  arrange(porte_param, cluster_param) 


est_descritivas %>% mutate(relatorio = round(valor_q3, 2))


referencia_radares <- base %>% 
  left_join(est_descritivas, by = c("porte" = "porte_param", "cluster" = "cluster_param")) %>% 
  mutate(
    valor_abs = round((valor_q3 * frota_23)/10000)
  )

base_dashboard <- referencia_radares %>% 
  select(nome, uf, populacao_23, frota_23, porte, total_radares, radares_10mil_veiculos,
        cluster, valor_q3, valor_abs) %>% 
  mutate(
    radares_10mil_veiculos = round(radares_10mil_veiculos, 2),
    valor_q3 = round(valor_q3, 2),
    valor_abs = round(valor_abs),
    cluster_porte = paste0(cluster, " - ", porte),
    porte_com_numeros = case_when(
      porte == "Menor porte" ~ "Menor porte (<20 mil)",
      porte == "Médio porte" ~ "Médio porte (>20 mil e <100 mil)",
      porte == "Maior porte" ~ "Maior porte (>100 mil)")) %>%
  filter(!is.na(valor_q3))
  
  
  
write_csv(est_descritivas, "output/est_descritivas.csv")
write.csv(referencia_radares, "output/base_referencia_radares.csv")
write.csv(base_dashboard, "shiny/base_dashboard.csv")

# Municípios outliers e com 0 mortes--------------------------------------------
outliers <- read.csv("bases/outliers_table.csv")

# Quantos municípios só têm câmeras de segurança nas rodovias federais?

rodovias_federais <- readxl::read_xlsx("bases/INDICADORES_RADARES_MUNICIPIOS.xlsx", sheet = 1) %>% 
  clean_names() %>% 
  filter(tipo == "Rodovia", str_detect(local_verificacao, "BR")) %>% 
  group_by(uf_municipio) %>% 
  summarise(n_radares_fed = n())
  

radares_geral <- readxl::read_xlsx("bases/INDICADORES_RADARES_MUNICIPIOS.xlsx", sheet = 4) %>% 
  clean_names() %>% 
  select(1:8) %>% left_join(rodovias_federais, by = c("uf_municipio_com_acento" = "uf_municipio")) %>% 
  mutate(total = aprovados + reparadados,
         n_radares_fed = replace_na(n_radares_fed, 0))

radares_geral %>% nrow()
radares_geral %>% filter(total == n_radares_fed) %>% nrow()
