
## Funciones procesamiento ##


library(readxl)
library(writexl)
library(janitor)
library(purrr)
library(arrow)
library(tidyverse)
library(dplyr)


# Estructura de datos -----------------------------------------------------

archivos <- list.files(
  "../data_in/reporte_irt_2/cognita/",
  pattern = "\\.xlsx$",
  full.names = TRUE
)


procesar_archivo <- function(path_excel){
  message("\n-----------------------------------")
  message("Procesando archivo: ", basename(path_excel))
  message("-----------------------------------")


# Cargar datos ------------------------------------------------------------
  data <- read_excel(path_excel, sheet = "Datos")
  data_rev <-read_excel(path_excel, sheet = "Matriz")
  # limiar nombres de variables
  data<-data %>% clean_names()
  data_rev<-data_rev %>% clean_names()
  
  message("\n-----------------------------------")
  message("Se Cargan archivos")
  message("-----------------------------------")
  
    
# Revisión duplicados -----------------------------------------------------
# código se detiene si existen duplicados por id_usuario_curso o nombre_y_apellido

#ID
if(
  data %>%
  count(id_usuario_curso) %>% 
  filter(n>1) %>% 
  nrow() > 0  
  |  data %>%
  count(nombre_y_apellido) %>% 
  filter(n>1) %>% 
  nrow() > 0  
  
){
  nombre_salida <- paste0(
    "../data_in/reporte_irt_2/problemas_duplicados_individual/",
    tools::file_path_sans_ext(basename(path_excel)),
    "_procesada.xlsx"
  )
  write_xlsx(data,paste0(nombre_salida))

  message("\n-----------------------------------")
  message("Archivo con valores duplicados: ", basename(path_excel))
  message("-----------------------------------")
  
  # eliminar duplicados de archivo con probelmas para continuar con el procesamiento
  
  # data<-data %>% 
  #   group_by(id_usuario_curso) %>% 
  #   slice(1) %>% 
  #   ungroup()

}

# Revisión de duplicados luego de eliminación

#   stopifnot(
#     data %>%
#       count(id_usuario_curso) %>% 
#       filter(n>1) %>% 
#       nrow() == 0  
#     |  data %>%
#       count(nombre_y_apellido) %>% 
#       filter(n>1) %>% 
#       nrow() == 0  
# 
#   )
# message("\n-----------------------------------")
# message("No existen duplicados por id y nombre")
# message("-----------------------------------")


# Revisión porcentaje de logro por prueba ---------------------------------
# Selección de variable y dar formato largo  ------------------------------
# selección  variable de interés

message("\n-----------------------------------")
message("Revisión número de registros eliminados por porcentaje de logro")
print(data %>% filter(porcentaje_de_logro<20) %>% nrow()) 
message("-----------------------------------")


data<-data %>% 
  dplyr::select(
     id_proyecto
    ,id_colegio
    ,id_evaluacion
    ,area
    ,evaluacion
    ,id_usuario_curso
    ,curso
    ,nombre_y_apellido
    ,porcentaje_de_logro
    ,starts_with("item_")
  ) %>% 
  # se ealiza filtro por porcentaje de logro mayor o igual a 20
  filter(porcentaje_de_logro>=20) %>%
  mutate(
    id_usuario_curso = row_number()
    ,nombre_y_apellido = as.character(row_number())
  )

  
message("\n-----------------------------------")
message("Se seleccionana variables")
message("-----------------------------------")


# dar formato largo 

data<-data %>% 
  pivot_longer(
    # definir columnas que se desean pivotear
      cols = starts_with("item_"),     
      #nvalores de respuestas
      names_to = "item",
      # nombre de columna que se pivotea
      values_to = "respuesta"
  )

# Guardar valor de registros después de pivotear la base 
n_post_pivot <- data %>% nrow()

message("\n-----------------------------------")
message("Se pivotea la data")
message("-----------------------------------")



# separar número de ítem de ID para cruce con data de revisión
data <- data %>%
  mutate(
     # extraer número de ítem
     item_no = str_extract(item,"(?<=item_)\\d{1,2}")
     # extraer número id de item
    ,item_id =  as.integer(str_extract(item,"(?<=_id_)\\d+")) 
  )


# cruce para revisión de ítem por item_id
data<-data %>% 
  dplyr::left_join(
    data_rev %>%
      dplyr::select(
        item_id
        ,clave_correcta_s
      ),
    by = "item_id"
  )

message("\n-----------------------------------")
message("Se agregan clave correcta")
message("-----------------------------------")

# print(data %>% count(curso))
# print(data %>% filter(is.na(clave_correcta_s)) %>% view())
# print(data_rev %>% count(clave_correcta_s))
# no puede haber ítem sin clave correcta
stopifnot(
  data %>% 
    filter(is.na(clave_correcta_s)) %>% 
    nrow()==0
)



message("\n-----------------------------------")
message("Todos los ítems tienen una clave")
message("-----------------------------------")



# Generar variable correcta-incorrecta ------------------------------------
data<-data %>% 
  mutate(
    # Generar variable con alternativa correcta
     alternativa_correcta = if_else(respuesta==clave_correcta_s,1,0)
     # Generar variable con alternativa incorrecta
    ,alternativa_incorrecta = if_else(respuesta==clave_correcta_s,0,1)
    ,porcentaje_de_logro = as.integer(porcentaje_de_logro)
    ,item_no = as.integer(item_no)
  )

message("\n-----------------------------------")
message("Se crean variables correctas e incorrecta")
message("-----------------------------------")



# No pueden quedar valores 1 en una misma fila, es decir que sea correcta e incorrecta a la vez
stopifnot(
  data %>% 
    filter(
      alternativa_correcta==alternativa_incorrecta
    ) %>% 
    nrow()==0
)

message("\n-----------------------------------")
message("Se valida consistencia entre clave correcta e incorrecta")
message("-----------------------------------")



# Exportar formato de datos para procesamiento ----------------------------
nombre_salida <- paste0(
  "../data_out/reporte_irt_2/01_limpieza_bbdd/",
  tools::file_path_sans_ext(basename(path_excel)),
  "_procesada.parquet"
)
write_parquet(data,paste0(nombre_salida))

message("\n-----------------------------------")
message("Se escriben los datos")
message("-----------------------------------")


rm(list = ls())
gc()
gc()

}



  
walk(archivos, procesar_archivo)

# Revisar que se hayan exportado todos los archivos en carpta de salida 
archivos_final <- list.files(
  "../data_out/reporte_irt_2/01_limpieza_bbdd/",
  pattern = "\\.parquet$",
  full.names = TRUE
)

stopifnot(
  length(archivos)==length(archivos_final)
) 

message("\n Proceso completado sin errores.")


