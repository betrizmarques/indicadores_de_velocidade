#-------------------------------------------------------------------------------
# Script utilizado para testar os dados do RENAEST------------------------------
#-------------------------------------------------------------------------------

source('scripts/functions.R')
library(tidyverse)

base_localidade <- read.csv2('bases/renaest/Localidade_DadosAbertos_20250812.csv')
base_vitimas <- read.csv2('bases/renaest/Vitimas_DadosAbertos_20250812.csv') %>% 
 #filter(qtde_obitos > 0) %>% 
  left_join(base_localidade, by = 'chv_localidade')

agrupadas_por_municipio <- base_vitimas %>% 
  filter(ano_acidente == 2023 & qtde_feridosilesos > 0 ) %>% 
  group_by(uf_acidente, municipio) %>% 
  summarise(sinistros_renaest = n_distinct(num_acidente)) %>% 
  mutate(municipio = tolower(municipio))

dados_rodovias_federais <- read.csv2('bases/renaest/datatran2023.csv')

agrupados_rodovias_federais <- dados_rodovias_federais %>% 
  filter(classificacao_acidente == "Com V\xedtimas Feridas" | classificacao_acidente == "Com V\xedtimas Fatais") %>% 
  group_by(uf, municipio) %>%
  summarise(sinistros_federais = n()) %>%
  mutate(municipio = tolower(municipio))
  # Calcular a quantidade de sinsitros -------------------

base_principal <- read.csv('output/base_principal_indicadores.csv') %>% 
  mutate(municipio_sem_acento = iconv(nome, from = "UTF-8", to = 'ASCII//TRANSLIT')) %>% 
  left_join(agrupadas_por_municipio, by = c("municipio_sem_acento" = "municipio", 'sigla' = 'uf_acidente')) %>% 
  left_join(agrupados_rodovias_federais, by = c("municipio_sem_acento" = "municipio", "sigla" = "uf")) %>%  
  mutate(sinistros_renaest = replace_na(sinistros_renaest, 0),
         sinistros_federais = replace_na(sinistros_federais, 0),
          total_sinistros = sinistros_federais+sinistros_renaest,
         sinistros_10mil_veiculos = total_sinistros/frota_23*10000,
         sinistros_10mil_veiculos = replace_na(sinistros_10mil_veiculos, 0),
         radares_10mil_veiculos = total_radares/frota_23*10000) %>% 
  mutate(radares_10mil_veiculos = replace_na(radares_10mil_veiculos, 0))

lista_correlacoes_renaest <- lapply(1:79, calculo_correlacao_clusters_renaest)

lista_aninhada_renaest <- bind_rows(lista_correlacoes_renaest)

estados_com_baixo_cv <- base_principal %>% 
#  filter(total_radares > 0) %>% 
  filter(sigla %in% c("GO", "PR", "MG", "PA", "PB"))

cor.test(estados_com_baixo_cv$radares_10mil_veiculos, estados_com_baixo_cv$sinistros_10mil_veiculos, method = "spearman")

#-------------------------------------------------------------------------------
cluster_lista <- 1:3
porte_lista <- c("Menor porte", "Médio porte", "Maior porte")

combinacoes <- expand.grid(
  cluster_param = cluster_lista,
  porte_param = porte_lista
)

resultados_quartis_renaest <- combinacoes %>% 
  mutate(resultados_aninhados = map2(
    .x = cluster_param,
    .y = porte_param,
    ~calculo_quartis_renaest(cluster = .x, porte = .y)
  )) %>% unnest(resultados_aninhados)

calculo_quartis_renaest()


cor.test(estados_com_baixo_cv$radares_10mil_veiculos, estados_com_baixo_cv$soma_das_vitimas_10mil_veiculos, method = "spearman")

#--------------------------------------------------------------------------------
lista_ufs <- unique(base_principal$uf)
correlacao_estados_aninhados <- lapply(lista_ufs, correlacao_por_estado)  
correlacao_estados <- bind_rows(correlacao_estados_aninhados)  
