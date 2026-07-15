library(tidyverse)
library(arrow)
library(readxl)

# Configurar rutas de archivos: ----
rutas <- config::get(file = "config.yml")
ruta_data_in <- rutas$ruta_data_in
ruta_data_intermedia <- rutas$ruta_data_intermedia

# Cargar datos ----
## ENSAYOS ----
ensayos_santillana0 <- ruta_data_intermedia %>% 
  file.path('ensayo_simce', 'consolidado_ensayo_simce.parquet') %>% 
  read_parquet()

diccionario_rbd <- ruta_data_intermedia %>% 
  file.path('ensayo_simce', 'diccionario_rbd_ensayo.xlsx') %>%
  read_excel() %>% 
  select(id_colegio, rbd_revisado)
  
ensayos_santillana <- ensayos_santillana0 %>% 
  left_join(diccionario_rbd, by = 'id_colegio') %>% 
  mutate(
    nivel = case_when(str_detect(tolower(curso), '2..m') ~ '2m',
                      str_detect(tolower(curso), '4..b') ~ '4b'),
    area = case_when(str_detect(tolower(area), 'lenguaje') ~ 'lenguaje', 
                     str_detect(tolower(area), 'mate') ~ 'matematica'),
    tipo_evaluacion = str_extract(str_squish(evaluacion), '\\w+ \\w+ \\d'),
    apellido_evaluacion = evaluacion %>% 
      str_remove('\\(202.\\)') %>% 
      str_remove('-') %>% 
      str_squish() %>% 
      str_remove(tipo_evaluacion) %>% 
      str_squish(),
    apellido_evaluacion = ifelse(apellido_evaluacion == '', NA, apellido_evaluacion)
    )

# nombres_unicos <- unique(ensayos_santillana$nombre %>% tolower() %>% str_squish())
# nombres_unicos_select <- nombres_unicos
# 
# dist_matrix <- stringdist::stringdistmatrix(nombres_unicos_select, nombres_unicos_select,
#                                             method = 'cosine')    
# diag(dist_matrix) <- Inf
# minimos <- max.col(-dist_matrix, ties.method = "first")
# dist_minimo <- matrixStats::rowMins(dist_matrix)
# 
# comparacion <- data.frame(nombre = nombres_unicos_select, 
#                           nombre_cercano = nombres_unicos_select[minimos],
#                           distancia = dist_minimo)

## SIMCES ----
simce0_rbd <- ruta_data_intermedia %>% 
  file.path('simce', 'consolidado_datos_simce_rbd.parquet') %>% 
  read_parquet()

simce_rbd <- simce0_rbd %>% 
  select(agno, grado, rbd, starts_with('prom')) %>% 
  pivot_longer(cols = starts_with('prom'),
               names_to = 'area',
               values_to = 'promedio_simce') %>% 
  mutate(area = ifelse(str_detect(area, 'lect'), 'lenguaje', 'matematica'))

simce_rbd %>% 
  write_csv('resultados_simce_rbd.csv')

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

