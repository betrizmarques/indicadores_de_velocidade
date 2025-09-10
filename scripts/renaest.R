base_vitimas <- read.csv2('bases/renaest/Vitimas_DadosAbertos_20250812.csv') %>% 
 filter(qtde_obitos > 0) %>% 
  left_join(base_localidade, by = 'chv_localidade')

base_localidade <- read.csv2('bases/renaest/Localidade_DadosAbertos_20250812.csv')


