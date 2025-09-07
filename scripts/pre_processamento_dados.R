# -----------------------------------------------------------------------------
# Este script faz o pré processamento dos dados, ele carrega os dados da pasta 
# bases/ e junta os dados em uma só base chamada "base_principal", durante esse 
# processo, ele faz o tratamento necessário dos dados para a análise.
# -----------------------------------------------------------------------------

library(tidyverse)

ufs <- tibble::tibble( 
  sigla = c("AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA",
            "MG", "MS", "MT", "PA", "PB", "PE", "PI", "PR", "RJ", "RN",
            "RO", "RR", "RS", "SC", "SE", "SP", "TO"),
  estado = c("Acre", "Alagoas", "Amazonas", "Amapá", "Bahia", "Ceará", "Distrito Federal",
             "Espírito Santo", "Goiás", "Maranhão", "Minas Gerais", "Mato Grosso do Sul",
             "Mato Grosso", "Pará", "Paraíba", "Pernambuco", "Piauí", "Paraná", "Rio de Janeiro",
             "Rio Grande do Norte", "Rondônia", "Roraima", "Rio Grande do Sul", "Santa Catarina",
             "Sergipe", "São Paulo", "Tocantins"))

base_metas_mortes <- read.csv('bases/base_principal.csv') %>% 
  select(uf, nome_do_municipio, n_mortes_23, frota_23, var_23, reducao, meta_atingida, populacao_23) %>% 
  rename(nome= nome_do_municipio) %>% 
  left_join(ufs, by = c('uf'='sigla'))

base_metas <- read.csv("bases/municipios_metas.csv") %>% 
  left_join(base_metas_mortes, by = c('uf'='estado', 'nome')) %>% 
  mutate(nome = tolower(nome))

base_indicadores <- readxl::read_xlsx("bases/INDICADORES_RADARES_MUNICIPIOS.xlsx", sheet = 4) %>%
  mutate(nome_municipios = tolower(`Municipio (COM ACENTO)`)) %>% 
  select(SiglaUf, nome_municipios, Frota, Aprovados, Reparadados, I1 )

lista_municipios <- read.csv2('bases/final_table.csv') %>% 
  mutate(nome_municipio = tolower(nome)) %>% 
  select(nome_municipio, uf, cluster_a, cluster_b, cluster_c)

base_principal <- base_metas %>% 
  left_join(base_indicadores, by = c('nome' = "nome_municipios", 'uf.y' = "SiglaUf")) %>% 
  mutate(total_radares = Aprovados + Reparadados,
         mortes_10_mil_veiculos = n_mortes_23/frota_23*10000,
         radares_10mil_veiculos = (Aprovados+Reparadados)/frota_23*10000) %>% 
  left_join(lista_municipios, by =c('uf', 'nome' = 'nome_municipio'))

