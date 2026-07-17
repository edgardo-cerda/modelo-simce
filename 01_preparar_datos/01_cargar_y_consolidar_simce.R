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
  list.files(pattern = 'Simce.*.zip', 
             full.names = TRUE)  

# Quitar duplicados:
ruta_archivos_brutos_simce_desduplicado <- 
  data.frame(base = ruta_archivos_brutos_simce) |>
  mutate(
    nivel = str_extract(base, '(cuarto|segundo|sexto|octavo)'),
    año = str_extract(base, '(202.) -')) |> 
  group_by(nivel, año) |> 
  distinct(nivel, año,  .keep_all = TRUE) |> 
  pull(base)

# Los datos usan separadores inconsistentes, así que agrego una función para detectarlo automático y cargar los datos:
identificar_separadores <- function(ruta_archivo) {
  # 1. Leer una muestra de las primeras 100 líneas como texto plano
  lineas <- read_lines(ruta_archivo, n_max = 100)
  texto_completo <- paste(lineas, collapse = "\n")
  
  # 2. Identificar el separador de columnas (delimitador)
  # Contamos cuántas veces aparece cada uno en la muestra
  conteos_delim <- c(
    "," = str_count(texto_completo, ","),
    ";" = str_count(texto_completo, ";"),
    "|" = str_count(texto_completo, "\\|")
  )
  
  # Seleccionamos el que tenga mayor presencia
  delimitador_predilecto <- names(which.max(conteos_delim))
  
  # Si no hay ninguno de los tres, por defecto asumimos coma
  if (max(conteos_delim) == 0) {
    delimitador_predilecto <- ","
  }
  
  # 3. Identificar el separador decimal
  # Buscamos patrones numéricos explícitos: número-punto-número vs número-coma-número
  con_punto <- str_detect(texto_completo, "[0-9]+\\.[0-9]+")
  con_coma  <- str_detect(texto_completo, "[0-9]+,[0-9]+")
  
  if (con_punto && !con_coma) {
    decimal_predilecto <- "."
  } else if (con_coma && !con_punto) {
    decimal_predilecto <- ","
  } else if (con_punto && con_coma) {
    # Conflicto: si aparecen ambos, descartamos el que esté actuando como delimitador de columnas
    if (delimitador_predilecto == ",") {
      decimal_predilecto <- "."
    } else {
      # Si el delimitador es ";" o "|", el que tenga decimales suele ser la coma en formato europeo/latino
      decimal_predilecto <- "," 
    }
  } else {
    # Si no se detectan números con decimales, usamos el estándar según el delimitador
    decimal_predilecto <- ifelse(delimitador_predilecto == ",", ".", ",")
  }
  
  # Retornar los resultados en una lista estructurada
  return(list(
    separador_columnas = delimitador_predilecto,
    separador_decimal  = decimal_predilecto
  ))
}

# Cargar y leer los archivos, para después consolidarlos: ----
leer_simce <- function(archivo_zip, nombre_zip) {
  
  # Identificar archivos dentro del zip según patrón del nombre:
  archivos_en_zip <- unzip(archivo_zip, list = TRUE)
  archivo_a_cargar <- archivos_en_zip |> 
    filter(str_detect(tolower(Name), 'csv'), # archivos csv 
           str_detect(Name, nombre_zip)) |> 
    slice(1) |> 
    pull(Name)
  
  # Para identificar distintos tipos de separadores (decimal y del csv)
  separadores <- identificar_separadores(unz(description = archivo_zip, 
                                             filename = archivo_a_cargar))
  
  # Hay archivos con nombres mal guardados, así que guardo los nombres por separado, corrijo, y después las pego
  nombres_variables_limpias <- readLines(unz(description = archivo_zip,
                                             filename = archivo_a_cargar),
                                         n = 1) |>
    str_remove_all('\\"') |>
    str_split_1(paste0('\\', separadores$separador_columnas))
  
  archivo <- read_delim(unz(description = archivo_zip, filename = archivo_a_cargar),
                        locale = locale(encoding = "Latin1",
                                        decimal_mark = separadores$separador_decimal),
                        delim = separadores$separador_columnas,
                        skip = 1)
  
  n_dif_names <- ncol(archivo) - length(nombres_variables_limpias)
  if (n_dif_names > 0) {
    nombres_nuevos <- paste0('var_', 1:n_dif_names)
    nombres_variables_limpias <- c(nombres_variables_limpias, nombres_nuevos)
  }
  names(archivo) <- nombres_variables_limpias
  
  return(archivo)
}

## SIMCE por alumno: ----
datos_simce_alu_mrun <- ruta_archivos_brutos_simce_desduplicado |> 
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
  list_rbind() |> 
  # Corregir casos que por alguna razón sigue saliendo con error:
  mutate(
    ptje_mate = ifelse(agno == 2024 & grado == '2m', ptje_mate/100, ptje_mate),
    ptje_lect = ifelse(agno == 2024 & grado == '2m', ptje_lect/100, ptje_lect)
  )
  
datos_simce_alu_mrun_consolidado |> 
  write_parquet(file.path(dir_salida, 'consolidado_datos_simce_alu.parquet'))

## SIMCE por colegio: ----
datos_simce_rbd <- ruta_archivos_brutos_simce_desduplicado |> 
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

# Pasar resultados por area a formato largo:

datos_simce_rbd_consolidado_long <- datos_simce_rbd_consolidado |> 
  select(-starts_with('palu')) |> 
  pivot_longer(
    cols = starts_with(c('nalu', 'prom')),
                       names_to = c(".value", "area"),
                       names_pattern = "(.*)_(.*)"
                       ) |> 
  mutate(area = ifelse(str_detect(area, 'lect'), 'lenguaje', 'matematica'),
         agno = as.numeric(agno)) |> 
  rename(promedio_simce = prom)


datos_simce_rbd_consolidado_long |> 
  write_parquet(file.path(dir_salida, 'consolidado_datos_simce_rbd.parquet'))
