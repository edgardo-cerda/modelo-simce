## Funciones proyecto ##
#nota: este script agrupa las funciones del proyecto modelo-simce y que se utilizan durante todo el proceso 



# Generales -----

## generar ruta ----
### Genera ruta según usuario que usa el proyecto 

generar_ruta <- function(){
  
  usuario <- Sys.info()[["user"]]
  rutas <- config::get(config = usuario, file = "config.yml")
  
  ruta_data_in <- rutas$ruta_data_in
  ruta_data_intermedia <- rutas$ruta_data_intermedia
  
  return(
    list(
      ruta_data_in = rutas$ruta_data_in
      ,ruta_data_intermedia = rutas$ruta_data_intermedia
      ,ruta_outputs = rutas$ruta_outputs
      )
  )
  
}



