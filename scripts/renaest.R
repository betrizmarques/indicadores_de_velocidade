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
  filter(ano_acidente == 2023 ) %>% 
  group_by(uf_acidente, municipio) %>% 
  summarise(qtde_vitimas = n()) %>% 
  mutate(municipio = tolower(municipio))

dados_rodovias_federais <- read.csv2('bases/renaest/datatran2023.csv')

agrupados_rodovias_federais <- dados_rodovias_federais %>% 
  filter(mortos > 0 | feridos >  0) %>% 
  mutate(total_vitimas = feridos+mortos) %>% 
  group_by(uf, municipio) %>% 
  summarise(qtde_vitimas_federais = sum(total_vitimas)) %>% 
  mutate(municipio = tolower(municipio))

base_principal <- read.csv('output/base_principal_indicadores.csv') %>% 
  mutate(municipio_sem_acento = iconv(nome, from = "UTF-8", to = 'ASCII//TRANSLIT')) %>% 
  left_join(agrupadas_por_municipio, by = c("municipio_sem_acento" = "municipio", 'sigla' = 'uf_acidente')) %>% 
  left_join(agrupados_rodovias_federais, by = c("municipio_sem_acento" = "municipio", "sigla" = "uf")) %>%  
  mutate(vitimas_10mil_veiculos = qtde_vitimas/`Frota`*10000,
         qtde_vitimas_federais = replace_na(qtde_vitimas_federais, 0),
         qtde_vitimas = replace_na(qtde_vitimas, 0),
         soma_das_vitimas = qtde_vitimas_federais+qtde_vitimas,
         soma_das_vitimas_10mil_veiculos = soma_das_vitimas/frota_23*10000,
         radares_10mil_veiculos = total_radares/frota_23*10000) %>% 
  mutate(radares_10mil_veiculos = replace_na(radares_10mil_veiculos, 0))

lista_correlacoes_renaest <- lapply(1:79, calculo_correlacao_clusters_renaest)

lista_aninhada_renaest <- bind_rows(lista_correlacoes_renaest)

estados_com_baixo_cv <- base_principal %>% 
#  filter(total_radares > 0) %>% 
  filter(sigla %in% c("GO", "PR", "MG", "PA", "PB")) %>% 
  mutate(soma_das_vitimas_10mil_veiculos = soma_das_vitimas/frota_23*10000)

cor.test(estados_com_baixo_cv$radares_10mil_veiculos, estados_com_baixo_cv$soma_das_vitimas_10mil_veiculos, method = "spearman")

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
