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

# Municípios outliers e com 0 mortes--------------------------------------------
outliers <- read.csv("bases/outliers_table.csv")

## 
frota_23[2465,2] <- "eldorado do carajas"
frota_23[2527,2] <- "santa izabel do para"
frota_23[174,2] <- "santa isabel do rio negro"
frota_23[5474,2] <- "couto magalhaes"
frota_23[5559,2] <- "sao valerio"
frota_23[1271,2] <- "pindare-mirim"
frota_23[3156,2] <- "sao francisco de assis do piaui"
frota_23[3697,2] <- "acu"
frota_23[3709,2] <- "augusto severo"
frota_23[3716,2] <- "cerro cora"
frota_23[3702,2] <- "januario cicco"
frota_23[2736,2] <- "joca claudino"
frota_23[2740,2] <- "sao domingos"
frota_23[2803,2] <- "belem do sao francisco"
frota_23[2861,2] <- "iguaracy"
frota_23[2883,2] <- "lagoa de itaenga"
frota_23[116,2] <- "sao sebastiao"
frota_23[4735,2] <- "gracho cardoso"
frota_23[426,2] <- "lajedo do tabocal"
frota_23[547,2] <- "santa terezinha"
frota_23[1373,2] <- "amparo do serra"
frota_23[1405,2] <- "barao de monte alto"
frota_23[1602,2] <- "dona eusebia"
frota_23[1659,2] <- "gouveia"
frota_23[1972,2] <- "queluzito"
frota_23[2111,2] <- "sao thome das letras"
frota_23[3646,2] <- "paraty"
frota_23[3680,2] <- "trajano de moraes"
frota_23[4861,2] <- "biritiba mirim"
frota_23[5133,2] <- "mogi guacu"
frota_23[5134,2] <- "mogi mirim"
frota_23[3228,2] <- "bela vista da caroba"
frota_23[3422,2] <- "munhoz de melo"
frota_23[3462,2] <- "pinhal de sao bento"
frota_23[3517,2] <- "santa cruz de monte castelo"
frota_23[4445,2] <- "balneario picarras"
frota_23[4559,2] <- "lajeado grande"
frota_23[4625,2] <- "presidente castello branco"
frota_23[4669,2] <- "sao lourenco do oeste"
frota_23[4672,2] <- "sao miguel do oeste"
frota_23[4064,2] <- "entre-ijuis"
frota_23[2417,2] <- "vila bela da santissima trindade"
frota_23[916,2] <- "bom jesus de goias"
