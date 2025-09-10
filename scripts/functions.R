# ------------------------------------------------------------------------------
# Script para armazenar as funções que seão usadas no script principal.
# ------------------------------------------------------------------------------


# Função que calcula, para cada um dos 79 clusters do estudo, a correlação de 
# spearman entre os radares e as mortes a cada 10 mil veículos nos municípios 
# brasileiros e salva tudo em uma lista de data.frames. Além de salvar um grá-
# fico de dispersão com o rho e o p-valor na pasta plots/-----------------------

calcula_correlacao_clusters <- function(cluster){
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
  
  correlacao <- cor.test(base_limpa$radares_10mil_veiculos, base_limpa$mortes_10mil_veiculos, method = 'spearman')
  
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
}

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

calculo_quartis_75 <- function(cluster, porte){
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


# Cálcula em cada cluster a correlação e retorna em um data.frame---------------
calcula_correlacao_clusters_c <- function(cluster, porte){
  base <- base_principal %>% 
    filter(cluster_c == {{cluster}}, porte == {{porte}}) 
  
  base_limpa <- na.omit(base[c('radares_10mil_veiculos', 'mortes_10mil_veiculos')])
  
  if (nrow(base_limpa) < 3){
    return(data.frame(
      cluster = {{cluster}},
      rho = NA,
      p_valor = NA
    ))
  }
  
  correlacao <-  cor.test(base_limpa$radares_10mil_veiculos, base_limpa$mortes_10mil_veiculos, method = 'spearman')
  
  rho <- correlacao$estimate
  p_valor <- correlacao$p.value
  
  return(data.frame(
    cluster = cluster,
    rho = rho,
    p_valor = p_valor
  ))
  
}

#Calcula a correlação conforma o tamanho da cidade------------------------------
correlacao_por_tamanho <- function(tamanho1, tamanho2){
  base_filtrada <- base_principal %>% 
    filter(media_pop < tamanho1 & media_pop > tamanho2)
  
  correlacao <- cor.test(base_filtrada$radares_10mil_veiculos, base_filtrada$mortes_10mil_veiculos, method = "spearman")
  
  rho <- correlacao$estimate
  p_valor <- correlacao$p.value
  
  return(data.frame(
    tamanho = paste('entre', tamanho2, 'e', tamanho1),
    rho = rho,
    p_valor = p_valor
  ))
  
}



