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
library(stringi)
library(fleetbr)

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

lista_municipios <- read.csv2('bases/indicadores_municipais.csv', sep = ",") %>% 
  mutate(nome_minusculo = tolower(nome),
         sem_acento = tolower(stri_trans_general(nome, "Latin-ASCII"))) %>% 
  left_join(ufs, by = c("uf" = "estado")) %>% 
  select(nome, nome_minusculo, sem_acento, sigla, c1, c2, c3)


populacao_23 <- readxl::read_xls("bases/POP_TCU_2023_Municipios_POP2022_Malha2023.xls", skip = 1) %>% 
  rename(populacao_23 = `POPULAÇÃO APURADA IBGE \n- CENSO DEMOGRÁFICO 2022 E MALHA TERRITORIAL 2023 -`,
         nome_do_municipio = `NOME DO MUNICÍPIO`) %>% 
  mutate(populacao_23 = str_remove(populacao_23, "\\s*\\([^\\)]*\\)"),
         nome_do_municipio = tolower(nome_do_municipio)) %>% 
  select(-`COD. UF`,-`COD. MUNIC`)

frota_23 <- readxl::read_xlsx('bases/INDICADORES_RADARES_MUNICIPIOS.xlsx', sheet = 2, skip = 3) %>% 
  mutate(uf = UF,
         nome = tolower(MUNICIPIO),
         frota_23 = TOTAL) %>% 
  select(uf, nome, frota_23)


base_indicadores <- readxl::read_xlsx("bases/INDICADORES_RADARES_MUNICIPIOS.xlsx", sheet = 4) %>%
  mutate(nome_minusculo = tolower(`Municipio (COM ACENTO)`),
         nome = `Municipio (COM ACENTO)`) %>% 
  select(SiglaUf, nome_minusculo, Aprovados, Reparadados, I1 )

clusters <- read.csv2('bases/final_table.csv') %>% 
  mutate(nome_minusculo = tolower(nome)) %>% 
  left_join(ufs, by = c("uf" = "estado")) %>% 
  select(nome_minusculo, sigla, cluster_a, cluster_b, cluster_c)

outliers <-  read.csv("bases/outliers_table.csv")
sem_mortes <- read.csv("bases/sem_mortes_table.csv") %>% 
  rename(ibge_cod = codigo)

municipios_faltantes <- outliers %>% 
  rbind(sem_mortes) %>% 
  left_join(ufs, by = c("uf" = "estado")) %>% 
  mutate(nome_minusculo = tolower(nome)) %>% 
  select(sigla, nome_minusculo, porte)


porte <- read.csv2(file = "bases/final_table.csv") %>% 
  left_join(ufs, by = c("uf" = "estado")) %>% 
  mutate(nome_minusculo = tolower(nome)) %>% 
  select(sigla, nome_minusculo, porte) %>% 
  rbind(municipios_faltantes)


base_com_outliers <- lista_municipios %>% 
  left_join(populacao_23, by = c("nome_minusculo" = "nome_do_municipio", "sigla" = "UF")) %>% 
  left_join(frota_23, by = c("sem_acento" = "nome", "sigla" = "uf")) %>% 
  left_join(base_indicadores, by = c("nome_minusculo" = "nome_minusculo", "sigla" = "SiglaUf")) %>%
  left_join(clusters, by = c("nome_minusculo", "sigla" )) %>% 
  left_join(porte, by = c("nome_minusculo", "sigla" )) %>% 
  mutate(total_radares = ifelse(is.na(Aprovados + Reparadados), 0, Aprovados + Reparadados),
         radares_10mil_veiculos = ((Aprovados+Reparadados)/frota_23)*10000,
         radares_10mil_veiculos = replace_na(radares_10mil_veiculos, 0))


# Salva o arquivo csv em uma pasta chamada output/------------------------------
write.csv(base_com_outliers, file = "output/base_com_outliers.csv")
