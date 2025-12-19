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
         sem_acento = tolower(stri_trans_general(nome, "Latin-ASCII")),
         sem_acento = str_replace(sem_acento, "'", "")) %>% 
  left_join(ufs, by = c("uf" = "estado")) %>% 
  select(nome, nome_minusculo, sem_acento, uf, sigla, c1, c2, c3)


populacao_23 <- readxl::read_xls("bases/POP_TCU_2023_Municipios_POP2022_Malha2023.xls", skip = 1) %>% 
  rename(populacao_23 = `POPULAÇÃO APURADA IBGE \n- CENSO DEMOGRÁFICO 2022 E MALHA TERRITORIAL 2023 -`,
         nome_do_municipio = `NOME DO MUNICÍPIO`) %>% 
  mutate(populacao_23 = str_remove(populacao_23, "\\s*\\([^\\)]*\\)"),
         nome_do_municipio = tolower(nome_do_municipio)) %>% 
  select(-`COD. UF`,-`COD. MUNIC`)

frota_23 <- readxl::read_xlsx('bases/INDICADORES_RADARES_MUNICIPIOS.xlsx', sheet = 2, skip = 3) %>% 
  mutate(uf = UF,
         nome = tolower(MUNICIPIO),
         nome = str_replace(nome, "'", ""),
         frota_23 = TOTAL) %>% 
  select(uf, nome, frota_23)



# nome_municipios <- nas$sem_acento
# linhas <- c()
# 
# funcao <- function(linha, nome_corrigido){
#   frota_23 <- c(174, 2465,5474, 5559, 1271, 3156, 3697,3709, 3716 ) 
# }


municipios_alterados <- c("eldorado do carajas", "santa izabel do para", "santa isabel do rio negro",
                          "couto magalhaes", "sao valerio", "pindare-mirim", 
                          "sao francisco de assis do piaui", "acu", "augusto severo",
                          "cerro cora", "januario cicco", "joca claudino", "sao domingos",
                          "belem do sao francisco", "iguaracy", "lagoa de itaenga", 
                          "sao sebastiao", "gracho cardoso", "lajedo do tabocal",
                          "santa terezinha", "amparo do serra", "barao de monte alto",
                          "dona eusebia", "gouveia", "queluzito", "sao thome das letras",
                          "paraty", "trajano de moraes", "biritiba mirim", "mogi guacu",
                          "mogi mirim", "bela vista da caroba", "munhoz de melo",
                          "pinhal de sao bento", "santa cruz de monte castelo", 
                          "balneario picarras", "lajeado grande", "presidente castello branco",
                          "sao lourenco do oeste", "sao miguel do oeste", "entre-ijuis",
                          "vila bela da santissima trindade", "bom jesus de goias" )

linha <- c(2465, 2527, 174, 5474, 5559, 1271, 3156, 3697, 3709, 3716, 3702, 2736,
             2740, 2803, 2861, 2883, 116, 4735, 426, 547, 1373, 1405, 1602, 1659,
             1972, 2111, 3646, 3680, 4861, 5133, 5134, 3228, 3422, 3462, 3517, 4445,
             4559, 4625, 4669, 4672, 4064, 2417, 916)


alterar_nome_municipio <- function(linha, municipios_alterados){
  frota_23[linha, 2] <<- municipios_alterados
}

mapply(alterar_nome_municipio, linha, municipios_alterados)

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


#nas <- base_com_outliers %>% filter(is.na(frota_23))

# Salva o arquivo csv em uma pasta chamada output/------------------------------
write.csv(base_com_outliers, file = "output/base_com_outliers.csv")
