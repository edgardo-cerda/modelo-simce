library(tidyverse)
library(arrow)
library(readxl)

# Configurar rutas de archivos: ----
# usuario <- Sys.info()[["user"]]
rutas <- config::get(file = "config.yml")

ruta_data_in <- rutas$ruta_data_in
ruta_data_intermedia <- rutas$ruta_data_intermedia

# Especificar dónde se van a guardar las salidas
dir_salida <- ruta_data_intermedia |> file.path('ensayo_simce')
dir_salida |> dir.create(showWarnings = FALSE)

# Listado de archivos brutos de pruebas diagnóstico: ----
ruta_archivos_ensayo_simce <- ruta_data_in %>%  
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
  left_join(conversion_id_colegio_rbd, by = 'id_colegio') %>% 
  mutate(
    agno = as.numeric(agno),
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
    apellido_evaluacion = ifelse(apellido_evaluacion == '', NA, apellido_evaluacion),
    n_evaluacion = str_extract(tipo_evaluacion, '\\d+') %>% str_squish(),
    tipo_evaluacion = str_remove(tipo_evaluacion, '\\d+') %>% str_squish()
  ) 

# Cargar y agregar datos de RBD revisados, si es que existen:
ruta_diccionario_rbd <- ruta_data_intermedia %>% 
  file.path('ensayo_simce', 'diccionario_rbd_ensayo.xlsx')

if (file.exists(ruta_diccionario_rbd)) {
  cat("Se agrega el RBD revisado")
  diccionario_rbd <- ruta_diccionario_rbd |> 
    read_excel() %>% 
    select(id_colegio, rbd_revisado)
  
  datos_ensayo_simce_consolidado_rbd <- datos_ensayo_simce_consolidado_rbd %>% 
    left_join(diccionario_rbd, by = 'id_colegio')
  
}


# Guardar resultados
datos_ensayo_simce_consolidado_rbd |> 
  write_parquet(file.path(dir_salida, 'consolidado_ensayo_simce.parquet'))


# Colegios sin RBD ----

sin_rbd <- datos_ensayo_simce_consolidado_rbd %>% 
  filter(is.na(rbd))

# Rev duplicados en curso por colegio  ----
dup_alumnos<-datos_ensayo_simce_consolidado_rbd %>% 
  group_by(id_colegio,id_usuario_curso,nombre, evaluacion) %>% 
  mutate(dup = n()) %>%
  ungroup() %>% 
  filter(dup>1) %>% 
  select(
    id_colegio
    ,colegio
    ,id_usuario_curso
    ,nombre
    ,evaluacion
    ,dup
  ) %>% 
  arrange(
    id_colegio
    ,id_usuario_curso
    ,evaluacion
  )



# Taludado curso 
# Existe un curso que dice 2 medio (1 medio) - definir que curso es
tab_curso<-datos_ensayo_simce_consolidado_rbd %>%
  filter(id_colegio==2016672) %>% 
  count(curso,colegio,id_colegio) 

# Bases de datos Excel para enviar a Santillana ----
tab_curso %>% 
  writexl::write_xlsx(file.path(dir_salida, 'tabulado_curso.xlsx'))

sin_rbd %>% 
  writexl::write_xlsx(file.path(dir_salida, 'colegios_sin_rbd.xlsx'))


dup_alumnos %>% 
  writexl::write_xlsx(file.path(dir_salida, 'duplicado_alumnos_colegio_usuario_nombre_evaluacion.xlsx'))

# liberar espacio y eliminar objetos
# gc()
# rm(list=ls())





