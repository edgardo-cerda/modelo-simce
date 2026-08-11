library(targets)

# Cargar funciones generales ----
source("funciones/funciones_proyecto_simce.R")

# Cargar funciones modelo irt
source("01_preparar_datos/funcion_generar_modelo_irt.R")
source("01_preparar_datos/funcion_procesar_data_irt.R")


# End this file with a list of target objects.
list(
  tar_target(rutas
             ,generar_ruta())
  ,tar_target(
    bbdd_pre_irt
    ,generar_data_pre_irt(
      ruta_data_in = rutas$ruta_data_in
      ,ruta_data_intermedia = rutas$ruta_data_intermedia
    )
  )
  ,tar_target(
    bbdd_irt_resumen
    ,generar_tabulado_agregado_irt(
      data = bbdd_pre_irt
      ,ruta_data_in = rutas$ruta_data_in
      ,ruta_data_intermedia = rutas$ruta_data_intermedia
    )
  )
  
)
