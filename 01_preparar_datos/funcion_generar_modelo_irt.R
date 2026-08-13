

# Cargar funciones -----
require(tidyverse)
require(dplyr)
require(arrow)
require(readr)

data <-read_rds("../modelo-simce-datos/data_intermedia/datos_procesados_irt.rds")

source("funciones/funciones_proyecto_simce.R")
# generar rutas
rutas<-generar_ruta()

# generar_tabulado_agregado_irt(
#   data = data_irt
#   ,ruta_data_in = rutas$ruta_data_in
#   ,ruta_data_intermedia = rutas$ruta_data_intermedia
# )


generar_tabulado_agregado_irt <- function(data,ruta_data_in, ruta_data_intermedia){


## Generar modelo IRT tres parámetros ##
# objetivo: a partir de los datos con formato se genera objeto irt por cada ensayo y asignatura

# librerias -----
require(readxl)
require(janitor)
require(MASS)
require(tidyverse)
require(dplyr)
require(tidyverse)
require(polycor)
require(ltm)
require(KernSmoothIRT)
require(psych)
require(arrow)

source("funciones/funciones_proyecto_simce.R")

# Parámetros
asignatura <- c("mat","len")
nivel <- c("4b","IIm")
vector_ensayos <-1:6


data_irt_4b<-map(asignatura
        ,~generar_data_pre_irt(
           data = data
          ,n_ensayos = 1:6
          ,asignatura = .x
          ,nivel = nivel[[1]])
        )


data_irt_2m<-map(asignatura
                 ,~generar_data_pre_irt(
                   data = data
                   ,n_ensayos = 1:6
                   ,asignatura = .x
                   ,nivel = nivel[[2]])
)


data_irt_mate <- list(
  data_irt_4b[[1]]
  ,data_irt_2m[[1]]
)

data_irt_leng <- list(
  data_irt_4b[[2]]
  ,data_irt_2m[[2]]
)



print(" se ejcutto generar_data_pre_irt")  

# generar modelo y salida en en carpeta intermedia

# Matemática
tab_resumen_mate_4b<-map_dfr(vector_ensayos
                          ,~  generar_modelo_irt(data_input = data_irt_mate
                                                 ,asignatura = 1
                                                 ,ensayos = .x )
                        )

tab_resumen_mate_2m<-map_dfr(vector_ensayos
                             ,~  generar_modelo_irt(data_input = data_irt_mate
                                                    ,asignatura = 2
                                                    ,ensayos = .x )
)




# Lenguaje
tab_resumen_leng_4b<-map_dfr(vector_ensayos
                          ,~  generar_modelo_irt(data_input = data_irt_leng
                                                 ,asignatura = 1
                                                 ,ensayos = .x )
)

tab_resumen_leng_2m<-map_dfr(vector_ensayos
                          ,~  generar_modelo_irt(data_input = data_irt_leng
                                                 ,asignatura = 2
                                                 ,ensayos = .x )
)

# unificar dastos por asignatura 
tab_resumen_leng <- bind_rows(
  tab_resumen_leng_4b |> 
    mutate(grado = "4b")
 ,tab_resumen_leng_2m |> 
    mutate(grado = "2m")
  
)

tab_resumen_mate <- bind_rows(
  tab_resumen_mate_4b |> 
    mutate(grado = "4b")
  ,tab_resumen_mate_2m |> 
    mutate(grado = "2m")
  
)




# Exportar resultados 
write_parquet(tab_resumen_leng
              ,paste0(
                ruta_data_intermedia
                ,"tab_resumen_irt_lenguaje.parquet"
              )
              )
write_parquet(tab_resumen_mate
              ,paste0(
                ruta_data_intermedia
                ,"tab_resumen_irt_matematica.parquet"
              )
)

}





# Gráfico información  ítem
# plot(pdt_irt3pl
#      ,type="IIC"
#      ,items=0
#      , xlab = "Habilidad"
#      , ylab="Información"
#      ,main = "Función de información de la prueba")



