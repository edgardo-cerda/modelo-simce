library(tidyverse)
library(arrow)
library(readxl)

# Configurar rutas de archivos: ----
rutas <- config::get(file = "config.yml")
ruta_data_in <- rutas$ruta_data_in
ruta_data_intermedia <- rutas$ruta_data_intermedia

# Especificar dónde se van a guardar las salidas
dir_salida <- ruta_data_intermedia |> file.path('ensayo_simce')
dir_salida |> dir.create(showWarnings = FALSE)

# Listado de archivos brutos de pruebas diagnóstico: ----
ruta_archivos_ensayo_simce <- ruta_data_in |> 
  list.files(pattern = 'medicion_santillana', 
             full.names = TRUE) %>% 
  map(list.files, full.names = TRUE,
                pattern = 'Ensayo',
                recursive = TRUE) %>% 
  unlist()

# Cargar y leer los archivos, para después consolidarlos: ----

## SIMCE por alumno: ----
datos_ensayo_simce <- ruta_archivos_ensayo_simce |> 
  map(read_excel) 

datos_ensayo_simce_consolidado <- datos_ensayo_simce |> 
  map(~{
    # Homologar nombres y tipos de datos:
    .x |> 
      janitor::clean_names() |> 
      select(agno = ano_lectivo, pais, id_colegio, colegio, curso, area, 
             id_evaluacion, evaluacion, 
             id_usuario_curso, nombre = nombre_y_apellido,
             porcentaje_logro = porcentaje_de_logro) |> 
      mutate(porcentaje_logro = as.numeric(porcentaje_logro))
  }
  ) |> 
  list_rbind()

# Agregar RBD
codigos_plenos_rbd <- file.path(ruta_data_in, 'Datos Medición Nacional_RBD',
                                  'SIMCE 2024-2025 + Colegios Pleno.xlsx') %>% 
  read_excel() %>% 
  janitor::clean_names() %>% 
  select(rbd, nom_rbd, id_pleno, id_pleno_2)

conversion_id_colegio_rbd <- datos_ensayo_simce_consolidado %>% 
  distinct(id_colegio, colegio) %>% 
  left_join(codigos_plenos_rbd, by = c('id_colegio' = 'id_pleno')) %>% 
  left_join(codigos_plenos_rbd, by = c('id_colegio' = 'id_pleno_2'),
            suffix = c('', '_2')) %>% 
  mutate(rbd = ifelse(!is.na(rbd), rbd, rbd_2)) %>% 
  distinct(id_colegio, rbd)
  
cat("Hay", sum(is.na(conversion_id_colegio_rbd$rbd)), "colegios sin RBD")

datos_ensayo_simce_consolidado_rbd <- datos_ensayo_simce_consolidado %>% 
  left_join(conversion_id_colegio_rbd, by = 'id_colegio')

# Guardar resultados
datos_ensayo_simce_consolidado_rbd |> 
  write_parquet(file.path(dir_salida, 'consolidado_ensayo_simce.parquet'))
