# ------------------------------------------------------------------------------
# Script para armazenar as funções que seão usadas no script principal.
# ------------------------------------------------------------------------------


# Função que calcula, para cada um dos 79 clusters do estudo, a correlação de 
# spearman entre os radares e as mortes a cada 10 mil veículos nos municípios 
# brasileiros e salva tudo em uma lista de data.frames. Além de salvar um grá-
# fico de dispersão com o rho e o p-valor na pasta plots/-----------------------


base_principal <- read.csv('output/base_principal_indicadores.csv')

calcula_correlacao_clusters <- lapply(1:79, function(cluster){
  base <-  base_principal %>% 
    filter(cluster == !!cluster)
  
  base_limpa <- na.omit(base[c("radares_10mil_veiculos", "mortes_10mil_veiculos")])
  
  if (nrow(base_limpa)< 3){
    return(data.frame(
      cluster = cluster,
      rho = NA,
      p_valor = NA
    ))
  }
  
  correlacao <- cor.test(base$radares_10mil_veiculos, base$mortes_10mil_veiculos, method = 'spearman')
  
  rho_valor <- correlacao$estimate
  p_valor <- correlacao$p.value
  
  g <- ggplot(base_limpa, aes(mortes_10mil_veiculos, radares_10mil_veiculos))+
    geom_point(size = 0.7)+
    geom_smooth()+
    
    labs(
      title = paste(cluster, "p-valor:", round(correlacao$p.value, 4), "Rho:", round(correlacao$estimate, 4))
    )
  
  nome_do_arquivo <- paste0('plots/correlacao_', cluster,".png")
  
  ggsave(filename = nome_do_arquivo,plot = g, width = 6, height = 4, units = "in"  )
  
  return(data.frame(
    cluster = cluster,
    rho = rho_valor,
    p_valor = p_valor
  ))
})


calculo_correlacao <- bind_rows(calcula_correlacao_clusters)
# Separa a base em quartis------------------------------------------------------

calculo_quartis <- function(){
  limites_quartis <- quantile(base_principal$radares_10mil_veiculos, na.rm = T, robs = c(0, 0.25, 0.5, 0.75, 1.0))
  
  base_principal$quartil_radares <<- cut(base_principal$radares_10mil_veiculos,
                                        breaks = limites_quartis,
                                        labels = c("Q1", "Q2", "Q3", "Q4"),
                                        include.lowest = T)
}


# Filtra por cluster e porte e coloca 75% dos dados na variável Q75 e 25% na
# variável Q4. Depois retorna um data.frame com as médias-----------------------

cluster_lista <- 1:3
porte_lista <- c("Menor porte", "Médio porte", "Maior porte")

# Criar um data.frame com todas as 9 combinações
combinacoes <- expand.grid(
  cluster_param = cluster_lista,
  porte_param = porte_lista
)

calculo_quartis <- function(cluster, porte){
  filtrada <- base_principal %>% 
    filter(cluster_c == {{cluster}}, porte == {{porte}} ) %>% 
    na.omit(radares_10mil_veiculos) %>% 
    arrange(radares_10mil_veiculos) %>% 
    mutate(indice = 1:n())
  
  corte <- ceiling(0.75*nrow(filtrada))
  
  resultado <- filtrada %>% 
    mutate(quartil_radares = ifelse(indice < corte, "Q75", "Q4"))
  
  resultado <- resultado %>% 
    group_by(quartil_radares) %>% 
    summarise(media_radares = mean(radares_10mil_veiculos, na.rm = T),
              media_mortes = mean(mortes_10mil_veiculos))
  
  return(data.frame(
    quartil_radares = resultado$quartil_radares,
    media_radares = resultado$media_radares,
    media_mortes = resultado$media_mortes
  ))
}

resultado_final <- expand.grid(
  cluster_param = cluster_lista,
  porte_param = porte_lista
) %>%
  mutate(resultados_aninhados = map2(
    .x = cluster_param,
    .y = porte_param,
    ~ calculo_quartis(cluster = .x, porte = .y) 
  )) %>%
  unnest(resultados_aninhados) 

# È a mesma função que a acima, mas calcula os 4 quartis certinhos--------------

calculo_quartis <- function(cluster, porte){
  filtrada <- base_principal %>% 
    filter(cluster_c == {{cluster}}, porte == {{porte}})
  
  limites_quartis <- quantile(filtrada$mortes_10mil_veiculos, na.rm = T, robs = c(0, 0.25, 0.5, 0.75, 1.0))
  
  filtrada <- filtrada %>% 
    mutate(quartil_radares = cut(filtrada$mortes_10mil_veiculos,
                                 breaks = limites_quartis,
                                 labels = c("Q1", "Q2", "Q3", "Q4"),
                                 include.lowest = T))
  
  resultado <- filtrada %>% 
    group_by(quartil_radares) %>% 
    summarise(media_radares = mean(radares_10mil_veiculos, na.rm = T),
              media_mortes = mean(mortes_10mil_veiculos))
  
  return(data.frame(
    quartil_radares = resultado$quartil_radares,
    media_radares = resultado$media_radares,
    media_mortes = resultado$media_mortes
  ))
  
}

resultado_final <- combinacoes %>%
  mutate(resultados_aninhados = map2(
    .x = cluster_param,
    .y = porte_param,
    ~ calculo_quartis(cluster = .x, porte = .y) 
  )) %>%
  unnest(resultados_aninhados)

# Cálcula em cada cluster a correlação e retorna em um data.frame--------------

