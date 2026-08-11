

# Cargar funciones -----

# data <-read_rds("../modelo-simce-datos/data_intermedia/datos_procesados_irt.rds")
# 
# source("funciones/funciones_proyecto_simce.R")
# # generar rutas
# rutas<-generar_ruta()
# 
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


data_irt<-map2(asignatura,nivel
        ,~generar_data_pre_irt(
           data = data
          ,n_ensayos = 1:6
          ,asignatura = .x
          ,nivel = .y)
        )
print(" se ejcutto generar_data_pre_irt")  

# generar modelo y salida en en carpeta intermedia

# Matemática
tab_resumen_mate<-map_dfr(vector_ensayos
                          ,~  generar_modelo_irt(data_input = data_irt
                                                 ,asignatura = 1
                                                 ,ensayos = .x )
                        )





# Lenguaje
tab_resumen_leng<-map_dfr(vector_ensayos
                          ,~  generar_modelo_irt(data_input = data_irt
                                                 ,asignatura = 2
                                                 ,ensayos = .x )
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



