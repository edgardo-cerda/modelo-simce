# Script para obtener rutas

library(tidyverse)
library(arrow)
library(readxl)

# Configurar rutas de archivos: ----
usuario <- Sys.info()[["user"]]
rutas <- config::get(config = usuario, file = "config.yml")

ruta_data_intermedia <- rutas$ruta_data_intermedia
dir_salida <- ruta_data_intermedia |> file.path('ensayo_santillana')

# Colegios sin RBD ----
datos_ensayo_santillana_consolidado_rbd <- dir_salida |> 
  file.path('consolidado_ensayo_santillana.parquet') |> 
  read_parquet()

sin_rbd <- datos_ensayo_santillana_consolidado_rbd %>% 
  filter(is.na(rbd))

# Rev duplicados en curso por colegio  ----
dup_alumnos<-datos_ensayo_santillana_consolidado_rbd %>% 
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
tab_curso<-datos_ensayo_santillana_consolidado_rbd %>%
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





