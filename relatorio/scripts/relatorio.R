# Para rodar este scrpit, primeiro rode o script "novo_main".
library(gt)
library(tidyverse)

base <- read_csv("output/est_descritivas.csv")

tabela_gt <- base %>% 
  gt() %>% 
  cols_label(
    porte_param = "Porte",
    cluster_param = "Cluster",
    valor_max = "Valor Máximo",
    valor_medio = "Média",
    valor_q2 = "Mediana (Q2)",
    valor_q3  = "Q3"
  ) %>% 
  cols_align(
    align = "center",
    columns = everything() 
  )

gtsave(tabela_gt, "relatorio/tabelas/tabela_relatorio_indicadores.png")


#-------------------------------------------------------------------------------

soma_atual <- sum(referencia_radares$total_radares, na.rm = T)
soma_ideal <- round(sum(referencia_radares$valor_abs))

soma_ideal-soma_atual

# quantd de municípios por combinação de cluster/porte--------------------------
referencia_radares %>% 
  group_by(porte, cluster_c) %>% 
  summarise(contagem_municipios = n())

referencia_radares %>% 
  filter(is.na(valor_q3))
