library(tidyverse)
library(arrow)
library(readxl)

# Configurar rutas de archivos: ----
rutas <- config::get(file = "config.yml")
ruta_data_in <- rutas$ruta_data_in
ruta_data_intermedia <- rutas$ruta_data_intermedia

# Especificar dónde se van a guardar las salidas
dir_salida <- ruta_data_intermedia |> file.path('prueba_diagnostico')
dir_salida |> dir.create(showWarnings = FALSE)

# Listado de archivos brutos de pruebas diagnóstico: ----
ruta_archivos_prueba_diagnostico <- ruta_data_in |> 
  file.path('reporte_irt', 'original') |> 
  list.files(pattern = 'reporte_p', 
             full.names = TRUE)  

# Cargar y leer los archivos, para después consolidarlos: ----

## SIMCE por alumno: ----
datos_prueba_diagnostico <- ruta_archivos_prueba_diagnostico |> 
  map(read_excel) 

datos_prueba_diagnostico_consolidado <- datos_prueba_diagnostico |> 
  map(~{
    # Homologar nombres y tipos de datos:
    .x |> 
      janitor::clean_names() |> 
      select(agno = ano_lectivo, pais, id_colegio, colegio, curso, area, 
             id_evaluacion, evaluacion, id_usuario_curso, nombre = nombre_y_apellido,
             porcentaje_logro = porcentaje_de_logro) |> 
      mutate(porcentaje_logro = as.numeric(porcentaje_logro))
  }
  ) |> 
  list_rbind()
