library(tidyverse)

# Configurar rutas de archivos: ----
rutas <- config::get(file = "config.yml")
ruta_data_in <- rutas$ruta_data_in
ruta_data_intermedia <- rutas$ruta_data_intermedia

ensayos_santillana <- ruta_data_intermedia %>% 
  file.path('ensayo_simce', 'consolidado_ensayo_simce.parquet') %>% 
  read_parquet() %>% 
  mutate(
    nivel = case_when(str_detect(tolower(curso), '2..m') ~ '2m',
                      str_detect(tolower(curso), '4..b') ~ '4b'),
    area = case_when(str_detect(tolower(area), 'lenguaje') ~ 'lenguaje', 
                           str_detect(tolower(area), 'mate') ~ 'matematica'))

# Estadística descriptiva básica:
ensayos_santillana %>% count(agno)
ensayos_santillana %>% count(nivel)
ensayos_santillana %>% count(agno, nivel)
ensayos_santillana %>% count(area) 

# Resultados promedio: 
ensayos_santillana %>% 
  ggplot(aes(x = porcentaje_logro, fill = area)) +
  geom_density(alpha = .3) + 
  facet_wrap(~ nivel)

ensayos_santillana %>% 
  # group_by(colegio, agno, nivel, area) %>% 
  # summarise(puntaje_promedio = mean(porcentaje_logro, na.rm = TRUE)) %>% 
  ggplot(aes(x = porcentaje_logro, fill = agno)) +
  geom_density(alpha = .3) + 
  facet_wrap(nivel ~ area)

