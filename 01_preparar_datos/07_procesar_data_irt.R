


# Librerías ----
library(readxl)
library(writexl)
library(janitor)
library(purrr)
library(arrow)
library(tidyverse)
library(dplyr)
library(readxl)
library(writexl)
#Funciones ----
source("funciones/funciones_proyecto_simce.R")

# Generar rutas 
rutas <- generar_ruta()
ruta_data_in <- rutas$ruta_data_in
ruta_data_intermedia <- rutas$ruta_data_intermedia

# Cargar datos ----
## Parámetros


## generar ruta archivo de ensayos ----
ruta_archivos_ensayo_simce <- ruta_data_in %>%  
  list.files(pattern = 'ensayos_santillana', 
             full.names = TRUE) %>%
  list.files(pattern = 'medicion_santillana', 
             full.names = TRUE) %>% 
  map(list.files, full.names = TRUE,
      pattern = 'Ensayo',
      recursive = TRUE) %>% 
  unlist()

## Cargar archivo de datos y matriz de revisión

data <- map(ruta_archivos_ensayo_simce
            ,~read_excel(.x,sheet = "Datos") %>% 
              clean_names())

data_rev <- map(ruta_archivos_ensayo_simce
            ,~read_excel(.x,sheet = "Matriz") %>% 
              clean_names())

# Procesamiento información ----

## Eliminar porcentaje de logro no deseado ----
### eliminar menores a 20

data<-map(data
          ,~.x %>%
            mutate(porcentaje_de_logro=as.numeric(porcentaje_de_logro)) %>% 
            filter(porcentaje_de_logro>=20))

### eliminar mayores  a 100
data<-map(data
          ,~.x %>%
            mutate(porcentaje_de_logro=as.numeric(porcentaje_de_logro)) %>% 
            filter(porcentaje_de_logro<=100))

print("Eliminado porcentajes de logro menores a 20 y mayores a 100")



## Seleccionar variables de interés ----

data<-map(data
  ,~.x %>% 
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
    )
  
)

print("Seleccionadas variables de interés")
  
## Dar formato largo  ----



data<-map(data
          ,~.x %>% 
            pivot_longer(
              # definir columnas que se desean pivotear
              cols = starts_with("item_"),     
              #nvalores de respuestas
              names_to = "item",
              # nombre de columna que se pivotea
              values_to = "respuesta"
            )
)

## separar número de ítem de ID para cruce con data de revisión ----
data<-map(data,
          ~.x %>%
            mutate(
              # extraer número de ítem
              item_no = str_extract(item,"(?<=item_)\\d{1,2}")
              # extraer número id de item
              ,item_id =  as.integer(str_extract(item,"(?<=_id_)\\d+")) 
            )
)


# Generar revisión de respuestas  ----

## cruce para revisión de ítem por item_id ----

data_revisada <- map2(data
                      ,data_rev
                      ,~.x %>% 
                        dplyr::left_join(
                          .y %>%
                            dplyr::select(
                              item_id
                              ,clave_correcta_s
                            ),
                          by = "item_id"
                        )
                      )



### Revisión - ningún registro puede quedar con  clave correcta en NA ----
  rev_clave<-map(data_revisada
    ,~.x %>% 
      filter(is.na(clave_correcta_s)) %>% 
      nrow()==0   
  )

stopifnot(
  rev_clave==TRUE
)




## Generar variable correcta-incorrecta ------------------------------------

data_revisada<-map(data_revisada
                   ,~.x %>%
                     mutate(
                       # Generar variable con alternativa correcta
                       alternativa_correcta = if_else(respuesta==clave_correcta_s,1,0)
                       # Generar variable con alternativa incorrecta
                       ,alternativa_incorrecta = if_else(respuesta==clave_correcta_s,0,1)
                       ,porcentaje_de_logro = as.integer(porcentaje_de_logro)
                       ,item_no = as.integer(item_no)
                     )
                   )




### Revisar -  no puede haber alternativa correcta e incorrecta a la vez ----
rev_clave2<-map(data_revisada
               ,~.x %>% 
                 filter(
                   alternativa_correcta==alternativa_incorrecta
                 ) %>% 
                 nrow()==0
               )

stopifnot(
  rev_clave2==TRUE
)



# Exportar formato de datos para procesamiento ----------------------------
# Generar nombre de objetos


nombre_salida <- str_to_lower(paste0(
  str_extract(ruta_archivos_ensayo_simce, "LEN{1}|MAT{1}")
  ,"_"
  ,str_extract(ruta_archivos_ensayo_simce, "IIM{1}|\\dB")
  ,"_"
  ,str_extract(ruta_archivos_ensayo_simce, "Ensayo\\d")
)) %>% 
  str_replace("ii","II")


# nombfar objetos lista
names(data_revisada)=nombre_salida

write_rds(data_revisada,paste0(
  ruta_data_intermedia
  ,"datos_procesados_irt.rds"
))

print("Se escribe salida de datos en RDS")


