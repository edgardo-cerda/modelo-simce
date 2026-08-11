
# #Funciones ----
# source("funciones/funciones_proyecto_simce.R")
# rutas <- generar_ruta()
# ruta_data_in <- rutas$ruta_data_in
# ruta_data_intermedia <- rutas$ruta_data_intermedia
# 


generar_data_pre_irt <- function(ruta_data_in,ruta_data_intermedia){
  

# Librerías ----
require(readxl)
require(writexl)
require(janitor)
require(purrr)
require(arrow)
require(tidyverse)
require(dplyr)
require(readxl)
require(writexl)
require(readr)

# Cargar datos ----
## Parámetros
# Generar rutas 


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

# nota: el objeto 60 tiene problema de tener dos claves correctas
# Se elimina y se vuelve a correr validación
# data_rev[[60]]<-NULL
# data[[60]]<-NULL

ruta_archivos_ensayo_simce <- ruta_archivos_ensayo_simce[-60]



# Generar nombre de objetos
#  Elimianr los que tnegan 2025
ruta_archivos_ensayo_simce <- ruta_archivos_ensayo_simce[!str_detect(ruta_archivos_ensayo_simce, "2025")]


nombre_salida <- str_to_lower(paste0(
  str_extract(ruta_archivos_ensayo_simce, "LEN{1}|MAT{1}")
  ,"_"
  ,str_extract(ruta_archivos_ensayo_simce, "IIM{1}|\\dB")
  ,"_"
  ,str_extract(ruta_archivos_ensayo_simce, "Ensayo\\d")
)) %>% 
  str_replace("ii","II")




## Cargar archivo de datos y matriz de revisión

data <- map(ruta_archivos_ensayo_simce
            ,~read_excel(.x,sheet = "Datos") %>% 
              clean_names())

data_rev <- map(ruta_archivos_ensayo_simce
            ,~read_excel(.x,sheet = "Matriz") %>% 
              clean_names())

# Revisar - Los archivos de clave deben tener sólo 1 respuesta correcta -----


data_rev<-map(data_rev
              ,~.x |> 
                mutate(
                  rev_clave = str_count(clave_correcta_s,"[[:alpha:]]")
                )
              ) 

for (i in 1:length(data_rev)) {
  
  if(max(data_rev[[i]]$rev_clave)>1){
    print(i)
  }else{
    print(glue::glue("data{i}:tiene una respuesta correcta por item")) 
  }
  
}  

# Revisar - por nivel los datos deben tener la misma cantidad de ítems

rev_n_item_nivel<-map2(data_rev,ruta_archivos_ensayo_simce
                      ,~.x |> 
                        mutate(
                          archivo = .y
                        )
                       )



rev_n_item_nivel<-map2_dfr(rev_n_item_nivel,nombre_salida
                           ,~.x |> 
                             mutate(
                               numero_item = max(.x$item_no)
                               ,nombre_base := .y
                             ) |> 
                             dplyr::select(
                               numero_item
                               ,nombre_base
                               ,archivo
                             ) |> 
                             slice(1)
                           )
rev_n_item_nivel<-rev_n_item_nivel |>
  group_by(nombre_base) |> 
  mutate(
    max = max(numero_item)
    ,min = min(numero_item)
  ) |> 
  ungroup()

rev_n_item_nivel |> 
  filter(
    max!=min
  ) |> 
  arrange(nombre_base) |> 
  dplyr::select(numero_item, nombre_base,archivo)

# pendiente eliminar registros para ejecutar irt -----
# se eliminan archivos detectados en análisis

names(data)=nombre_salida
names(data_rev)=nombre_salida
# elimianr registros detecvtados
data[["mat_4b_ensayo2"]]<-NULL
data_rev[["mat_4b_ensayo2"]]<-NULL
data[["mat_IIm_ensayo1"]]<-NULL
data_rev[["mat_IIm_ensayo1"]]<-NULL

# Revisar si todo quedó ok luego de eliminar los registros ----
vector_nombres2 <- data_rev |> names()


rev_n_item_nivel2<-map2_dfr(data_rev,vector_nombres2
                           ,~.x |> 
                             mutate(
                               numero_item = max(.x$item_no)
                               ,nombre_base := .y
                             ) |> 
                             dplyr::select(
                               numero_item
                               ,nombre_base
                               #,archivo
                             ) |> 
                             slice(1)
)
rev_n_item_nivel2<-rev_n_item_nivel2 |>
  group_by(nombre_base) |> 
  mutate(
    max = max(numero_item)
    ,min = min(numero_item)
  ) |> 
  ungroup()

rev_n_item_nivel2 |> 
  filter(
    max!=min
  ) |> 
  arrange(nombre_base) |> 
  dplyr::select(numero_item, nombre_base)




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

saveRDS(data_revisada,paste0(
  ruta_data_intermedia
  ,"datos_procesados_irt.rds"
))


return(data_revisada)

print("Se escribe salida de datos en RDS")

}


