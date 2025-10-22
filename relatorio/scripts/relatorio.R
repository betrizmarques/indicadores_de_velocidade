library(gt)
library(dplyr)


tabela_gt <- resultado_quartis %>% 
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

gtsave(tabela_gt, "relatorio/tabelas/tabela_relatorio_inidcadores.png")


#-------------------------------------------------------------------------------

soma_pais_inteiro <- sum(referencia_radares$radares_10mil_veiculos, na.rm = T)
soma_pais_inteiro_atual <- sum(referencia_radares$valor_q3, na.rm = T)
