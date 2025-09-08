# ------------------------------------------------------------------------------
# Este script faz o pré processamento dos dados, ele carrega os dados da pasta 
# bases/ e junta os dados em uma só base chamada "base_principal", durante esse 
# processo, ele faz o tratamento necessário dos dados para a análise.
# ------------------------------------------------------------------------------
# Por Ana Beatriz Marques e Prof. Dr. Jorge Tiago Bastos 
# Observatório Nacional de Segurança Viária.
#-------------------------------------------------------------------------------

library(tidyverse)
library(roadtrafficdeaths)

# Cria um data.frame com as siglas e seus respectivos nomes---------------------
ufs <- tibble::tibble( 
  sigla = c("AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA",
            "MG", "MS", "MT", "PA", "PB", "PE", "PI", "PR", "RJ", "RN",
            "RO", "RR", "RS", "SC", "SE", "SP", "TO"),
  estado = c("Acre", "Alagoas", "Amazonas", "Amapá", "Bahia", "Ceará", "Distrito Federal",
             "Espírito Santo", "Goiás", "Maranhão", "Minas Gerais", "Mato Grosso do Sul",
             "Mato Grosso", "Pará", "Paraíba", "Pernambuco", "Piauí", "Paraná", "Rio de Janeiro",
             "Rio Grande do Norte", "Rondônia", "Roraima", "Rio Grande do Sul", "Santa Catarina",
             "Sergipe", "São Paulo", "Tocantins"))

# Cria um data.frame com os dados das mortes------------------------------------
mortes <- rtdeaths %>% 
  filter(ano_ocorrencia %in% c(2021, 2022, 2023)) %>% 
  mutate(nome_minusculo = tolower(nome_municipio_ocor)) %>% 
  group_by(nome_minusculo, nome_uf_ocor) %>% 
  summarise(mortes_anos = n()/3) %>% 
  select(nome_minusculo,nome_uf_ocor, mortes_anos)

# Lê a base de outro estudo e seleciona as variáveis de interesse---------------
base_metas_mortes <- read.csv('bases/base_principal.csv') %>% 
  select(uf, nome_do_municipio, n_mortes_23, frota_23, var_23, reducao, meta_atingida, populacao_23) %>% 
  rename(nome= nome_do_municipio) %>% 
  left_join(ufs, by = c('uf'='sigla'))

# Lê a base de metas e junta com a base acima-----------------------------------
base_metas <- read.csv("bases/municipios_metas.csv") %>% 
  left_join(base_metas_mortes, by = c('uf'='estado', 'nome')) %>% 
  mutate(nome = tolower(nome))

# Lê a base de radares e selciona as variáveis de interesse---------------------
base_indicadores <- readxl::read_xlsx("bases/INDICADORES_RADARES_MUNICIPIOS.xlsx", sheet = 4) %>%
  mutate(nome_municipios = tolower(`Municipio (COM ACENTO)`)) %>% 
  select(SiglaUf, nome_municipios, Frota, Aprovados, Reparadados, I1 )

# Lê a base de clusters e seleciona as variáveis de interesse-------------------
lista_municipios <- read.csv2('bases/final_table.csv') %>% 
  mutate(nome_municipio = tolower(nome)) %>% 
  select(nome_municipio, uf, cluster_a, cluster_b, cluster_c)

# Junta tudo em uma base principal e cria novas variáveis com alguns cálculos---
base_principal <- base_metas %>% 
  left_join(base_indicadores, by = c('nome' = "nome_municipios", 'uf.y' = "SiglaUf")) %>% 
  mutate(total_radares = Aprovados + Reparadados,
         mortes_10_mil_veiculos = n_mortes_23/frota_23*10000,
         radares_10mil_veiculos = (Aprovados+Reparadados)/frota_23*10000) %>% 
  left_join(lista_municipios, by =c('uf', 'nome' = 'nome_municipio')) %>% 
  left_join(mortes, by = c('uf' = 'nome_uf_ocor', 'nome' = 'nome_minusculo')) %>% 
  rename(sigla = uf.y) %>% 
  mutate(mortes_anos  = replace_na(mortes_anos,0))

# Salva o arquivo csv em uma pasta chamada output/------------------------------
write.csv(base_principal, file = "output/base_principal_indicadores.csv")
