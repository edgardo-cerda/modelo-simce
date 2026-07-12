# Script para cargar y consolidar los resultados de pruebas SIMCE ####
# A nivel de estudiante y de colegio

library(tidyverse)
library(arrow)

# Configurar rutas de archivos: ----
rutas <- config::get(file = "config.yml")
ruta_data_in <- rutas$ruta_data_in
ruta_data_intermedia <- rutas$ruta_data_intermedia

# Especificar dónde se van a guardar las salidas
dir_salida <- ruta_data_intermedia |> file.path('simce')
dir_salida |> dir.create(showWarnings = FALSE)

# Listado de archivos brutos SIMCE: ----
ruta_archivos_brutos_simce <- ruta_data_in |> 
  file.path('simce_agencia_calidad_educacion') |> 
  list.files(pattern = 'Simce.*.zip', 
             full.names = TRUE)  


# Cargar y leer los archivos, para después consolidarlos: ----
leer_simce <- function(archivo_zip, nombre_zip) {
  
  # Identificar archivos dentro del zip según patrón del nombre:
  archivos_en_zip <- unzip(archivo_zip, list = TRUE)
  archivo_a_cargar <- archivos_en_zip |> 
    filter(str_detect(tolower(Name), 'csv'), # archivos csv 
           str_detect(Name, nombre_zip)) |> 
    slice(1) |> 
    pull(Name)
  
  data <- tryCatch({    
    # Leer con read_delim
    archivo <- read_csv(unz(description = archivo_zip, filename = archivo_a_cargar), 
                        locale = locale(encoding = "Latin1"))
    if (ncol(archivo) == 1) archivo <- read_csv2(unz(description = archivo_zip, filename = archivo_a_cargar), 
                                                 locale = locale(encoding = "Latin1")) 
    if (ncol(archivo) == 1) archivo <- read_delim(unz(description = archivo_zip, filename = archivo_a_cargar), 
                                                  delim = '|', locale = locale(encoding = "Latin1")) 
    if (ncol(archivo) == 1) errorCondition()
    
    return(archivo)
    
    }, # Hay un archivo que tiene un problema en los nombres de variables, así que hay que hacer todo este webeo para leerlo:
    error = function(e) {
      
      nombres_variables_limpias <- readLines(unz(description = archivo_zip,
                                                 filename = archivo_a_cargar),
                                             n = 1) |>
        str_remove_all('\\"') |>
        str_split_1('\\|')
      
      archivo <- read_delim(unz(description = archivo_zip,
                                filename = archivo_a_cargar),
                            delim = '|',
                            skip = 1, 
                            locale = locale(encoding = "Latin1"))
      names(archivo) <- nombres_variables_limpias
      
      archivo
        # archivo |> janitor::clean_names()
    }
  )
  
  return(data)
}

## SIMCE por alumno: ----
datos_simce_alu_mrun <- ruta_archivos_brutos_simce |> 
  map(leer_simce, nombre_zip = 'alu_mrun')

datos_simce_alu_mrun_consolidado <- datos_simce_alu_mrun |> 
  map(~{
    # Homologar nombres y tipos de datos:
    data_vars_seleccionadas <- .x |> 
        select(agno, grado, idalumno, mrun, rbd, dvrbd, cod_curso,
               starts_with(c('ptje_mate', 'ptje_lect', 'eem_mate', 'eem_lect', 'eda_mate', 'eda_lect')))
    names(data_vars_seleccionadas) <- str_remove_all(names(data_vars_seleccionadas),
                                                     '(4b|8b|2m|6b)_alu')
    data_homologada <- data_vars_seleccionadas |> 
      mutate(across(contains(c('ptje', 'eem', 'eda')), as.numeric))
    return(data_homologada)
    }
    ) |> 
  list_rbind()
  
datos_simce_alu_mrun_consolidado |> 
  write_parquet(file.path(dir_salida, 'consolidado_datos_simce_alu.parquet'))

## SIMCE por colegio: ----
datos_simce_rbd <- ruta_archivos_brutos_simce |> 
  map(leer_simce, nombre_zip = '_rbd')

datos_simce_rbd_consolidado <- datos_simce_rbd |> 
  map(~{
    # Homologar nombres y tipos de datos:
    data_vars_seleccionadas <- .x |>
        select(agno, grado, rbd, dvrbd, nom_rbd, cod_com_rbd, nom_com_rbd,
               cod_depe1, cod_depe2, cod_grupo, cod_rural_rbd,
               starts_with(c('nalu_lect', 'nalu_mate',
                             'prom_lect', 'prom_mate',
                             'palu_eda_ins_lect', 'palu_eda_ele_lect', 'palu_eda_ade_lect',
                             'palu_eda_ins_mate', 'palu_eda_ele_mate', 'palu_eda_ade_mate'
                             )))
    names(data_vars_seleccionadas) <- str_remove_all(names(data_vars_seleccionadas),
                                                     '(4b|8b|2m|6b)_rbd')
    data_homologada <- data_vars_seleccionadas |>
      mutate(across(contains(c('nalu', 'prom', 'palu')), as.numeric))
    return(data_homologada)
    }
    ) |> 
  list_rbind()
  
datos_simce_rbd_consolidado |> 
  write_parquet(file.path(dir_salida, 'consolidado_datos_simce_rbd.parquet'))
