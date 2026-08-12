library(tidyverse)
library(arrow)
library(readxl)

# Cargar funciones 
source("funciones/funciones_proyecto_simce.R")

# Configurar rutas de archivos: ----
usuario <- Sys.info()[["user"]]
rutas <- config::get(config = usuario, file = "config.yml")

ruta_data_in <- rutas$ruta_data_in
ruta_data_intermedia <- rutas$ruta_data_intermedia

# Especificar dónde se van a guardar las salidas
dir_salida <- ruta_data_intermedia |> file.path('ensayo_santillana')
dir_salida |> dir.create(showWarnings = FALSE)

# Listado de archivos brutos de pruebas diagnóstico: ----
ruta_archivos_ensayo_santillana <- ruta_data_in %>%  
  file.path('ensayos_santillana') |> 
  list.files(pattern = 'medicion_santillana', 
             full.names = TRUE) %>% 
  map(list.files, full.names = TRUE,
                pattern = 'Ensayo',
                recursive = TRUE) %>% 
  unlist()

# Cargar y leer los archivos, para después consolidarlos: ----

## SIMCE por alumno: ----
datos_ensayo_santillana <- ruta_archivos_ensayo_santillana |> 
  map(read_excel) 

datos_ensayo_santillana_consolidado <- datos_ensayo_santillana |> 
  map(~{
    # Homologar nombres y tipos de datos:
    .x |> 
      janitor::clean_names() |> 
      dplyr::select(agno = ano_lectivo, pais, id_colegio, colegio, curso, area, 
             id_evaluacion, evaluacion, 
             id_usuario_curso, nombre = nombre_y_apellido,
             porcentaje_logro = porcentaje_de_logro) |> 
      mutate(porcentaje_logro = as.numeric(porcentaje_logro)) 
  }
  ) |> 
  list_rbind()




# Agregar RBD ----

## Hay distintas versiones así que las junto todas para ver si son consistentes primero: ----
codigos_plenos_rbd_1 <- file.path(ruta_data_in, 'Datos Medición Nacional_RBD',
                                  'SIMCE 2024-2025 + Colegios Pleno.xlsx') %>% 
  read_excel() %>% 
  janitor::clean_names() %>% 
  dplyr::select(rbd, id_pleno_1 = id_pleno, id_pleno_2) |> 
  pivot_longer(cols = c(id_pleno_1, id_pleno_2),
               names_to = "version_id_pleno",
               values_to = "id_pleno") |> 
  mutate(version_id_pleno = paste0('SIMCE 2024-2025 + Colegios Pleno - ', version_id_pleno))

codigos_plenos_rbd_2 <- file.path(ruta_data_in, 'Datos Medición Nacional_RBD',
                                  'Listado de colegios Pleno Chile.xlsx') %>% 
  read_excel() |> 
  janitor::clean_names() %>% 
  dplyr::select(rbd, id_pleno) |> 
  mutate(version_id_pleno = "Listado de colegios Pleno Chile",
         rbd = as.integer(rbd)) |> 
  filter(!is.na(rbd))

codigos_plenos_rbd_3 <- file.path(ruta_data_in, 'ensayos_santillana',
                                  'colegios_sin_rbd_ agregados v1.xlsx') %>% 
  read_excel(sheet = 'Lista Colegios Pleno') |> 
  janitor::clean_names() |> 
  dplyr::select(rbd, id_pleno = id_colegio) |> 
  mutate(version_id_pleno = "colegios_sin_rbd_ agregados v1")

## Evaluar si RBD son consistentes y están completas: ----
conversion_id_colegio_rbd <- bind_rows(codigos_plenos_rbd_1,
                                codigos_plenos_rbd_2,
                                codigos_plenos_rbd_3) |> 
  distinct(id_pleno, rbd, .keep_all = TRUE) |> 
  filter(!is.na(id_pleno))

datos_ensayo_santillana_consolidado_rbd <- datos_ensayo_santillana_consolidado %>% 
  left_join(conversion_id_colegio_rbd, by = c('id_colegio' = 'id_pleno')) 

cat("Hay", sum(is.na(datos_ensayo_santillana_consolidado_rbd |> distinct(id_colegio, rbd) |>  pull(rbd))), "colegios sin RBD")

datos_ensayo_santillana_consolidado_final <- datos_ensayo_santillana_consolidado_rbd |> 
  mutate(
    agno = as.numeric(agno),
    grado = case_when(str_detect(tolower(curso), '2..m') ~ '2m',
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
    apellido_evaluacion = ifelse(apellido_evaluacion == '', NA, apellido_evaluacion),
    n_evaluacion = str_extract(tipo_evaluacion, '\\d+') %>% str_squish(),
    tipo_evaluacion = str_remove(tipo_evaluacion, '\\d+') %>% str_squish()
  ) 

# Agregar puntaje simce modelo 1
tabla_conversion <- read_excel(file.path(ruta_data_in, "Escala Simce Santillana 4basico_2025.xlsx"),
                               sheet = "tabla_conversion")

datos_ensayo_santillana_consolidado_final<-datos_ensayo_santillana_consolidado_final |> 
  convertir_logro_simce_modelo1(tabla_conversion)



# Guardar resultados
datos_ensayo_santillana_consolidado_final |> 
  write_parquet(file.path(dir_salida, 'consolidado_ensayo_santillana.parquet'))
