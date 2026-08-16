# Script para cargar y consolidar los resultados de pruebas SIMCE ####
# A nivel de estudiante y de colegio

library(tidyverse)
library(arrow)

# Configurar rutas de archivos: ----
usuario <- Sys.info()[["user"]]
rutas <- config::get(config = usuario, file = "config.yml")

ruta_data_in <- rutas$ruta_data_in
ruta_data_intermedia <- rutas$ruta_data_intermedia

# Especificar dónde se van a guardar las salidas
dir_salida <- ruta_data_intermedia |> file.path('simce')
dir_salida |> dir.create(showWarnings = FALSE)

# Listado de archivos brutos SIMCE: ----
ruta_archivos_brutos_simce <- ruta_data_in |> 
  file.path('resultados_simce') |> 
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
#
# La corrección: ampliar la muestra usada para detectar el separador decimal
# (para tener más chances de encontrar celdas con puntaje) y, si aun así no
# se encuentra ningún patrón decimal, no asumir "," en base al delimitador.
identificar_separadores <- function(ruta_archivo, n_muestra_decimal = 5000) {
  lineas <- read_lines(ruta_archivo, n_max = n_muestra_decimal)
  
  # 1. Para el delimitador basta con las primeras líneas (aparece en TODAS
  #    las filas, tengan o no puntaje), así que usamos solo ese subconjunto:
  texto_delim <- paste(head(lineas, 100), collapse = "\n")
  
  # 2. Identificar el separador de columnas (delimitador)
  # Contamos cuántas veces aparece cada uno en la muestra
  conteos_delim <- c(
    "," = str_count(texto_delim, ","),
    ";" = str_count(texto_delim, ";"),
    "|" = str_count(texto_delim, "\\|")
  )
  
  # Seleccionamos el que tenga mayor presencia
  delimitador_predilecto <- names(which.max(conteos_delim))
  
  # Si no hay ninguno de los tres, por defecto asumimos coma
  if (max(conteos_delim) == 0) {
    delimitador_predilecto <- ","
  }
  
  # 3. Identificar el separador decimal.
  texto_decimal <- paste(lineas, collapse = "\n")
  
  # Buscamos patrones numéricos explícitos: número-punto-número vs número-coma-número
  con_punto <- str_detect(texto_decimal, "[0-9]+\\.[0-9]+")
  con_coma  <- str_detect(texto_decimal, "[0-9]+,[0-9]+")
  
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
    # No se encontró NINGÚN número con decimales, ni con "." ni con ",", ni
    # siquiera en la muestra ampliada. En vez de adivinar en base al
    # delimitador (supuesto que resultó ser incorrecto para esta fuente:
    # los archivos SIMCE usan ";" + "." y no ";" + ","), avisamos y usamos
    # "." como valor por defecto, que es el estándar observado en todos los
    # archivos de esta fuente revisados hasta ahora.
    warning(paste0("No se detectaron decimales en la muestra de ", ruta_archivo,
                   " (ni con '.' ni con ','). Se usará '.' por defecto: ",
                   "verifica manualmente este archivo."))
    decimal_predilecto <- "."
  }
  
  # Retornar los resultados en una lista estructurada
  return(list(
    separador_columnas = delimitador_predilecto,
    separador_decimal  = decimal_predilecto
  ))
}

# Chequeo de sanidad: los puntajes SIMCE (por alumno) siempre caen dentro de
# un rango conocido. Si tras la carga aparecen valores fuera de ese rango,
# es señal de un problema de parseo (como el que motivó esta corrección) y
# no de un valor legítimo, así que lo dejamos como una alerta explícita en
# vez de fallar en silencio.
validar_rango_puntajes <- function(datos, columnas, rango = c(100, 500)) {
  for (col in columnas) {
    fuera_de_rango <- datos |>
      filter(!is.na(.data[[col]]) & (.data[[col]] < rango[1] | .data[[col]] > rango[2]))
    if (nrow(fuera_de_rango) > 0) {
      resumen <- fuera_de_rango |> count(agno, grado, name = "n_casos_fuera_de_rango")
      warning(paste0(nrow(fuera_de_rango), " valores de '", col,
                     "' fuera del rango plausible [", rango[1], ", ", rango[2],
                     "]. Revisa por agno/grado:\n",
                     paste(capture.output(print(resumen)), collapse = "\n")))
    }
  }
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

# Sexo del alumno, si la fuente lo trae. Hasta ahora el select() de más
# abajo conservaba sólo identificadores y puntajes, así que cualquier
# variable demográfica del archivo original se perdía en silencio.
#
# POR QUÉ IMPORTA: los ensayos Santillana traen `sexo` estimado a partir
# del nombre (ver 02_cargar_y_consolidar_ensayos.R). Sin la contraparte en
# el SIMCE no se puede medir la brecha de sexo en el resultado que se
# quiere predecir, y por lo tanto no se puede saber si el sexo aporta algo
# POR SOBRE lo que el ensayo ya mide. Con esta columna esa pregunta pasa a
# ser contestable con datos en vez de con un supuesto.
#
# El nombre exacto cambia entre versiones del archivo de la Agencia, así
# que se prueban varios candidatos y se avisa fuerte si no aparece
# ninguno: un any_of() que no encuentra nada no falla, y sin este aviso el
# problema volvería a pasar inadvertido.
VARS_SEXO_CANDIDATAS <- c('sexo', 'gen_alu')

sexo_detectado <- datos_simce_alu_mrun |>
  map(~ intersect(VARS_SEXO_CANDIDATAS, names(.x))) |>
  unlist() |> unique()

if (length(sexo_detectado) == 0) {
  candidatas_parecidas <- datos_simce_alu_mrun |>
    map(~ grep('gen|sex', names(.x), ignore.case = TRUE, value = TRUE)) |>
    unlist() |> unique()
  warning('No se encontró variable de sexo en los archivos alu_mrun. ',
          'Columnas con nombre parecido: ',
          if (length(candidatas_parecidas)) paste(candidatas_parecidas, collapse = ', ')
          else '(ninguna)',
          '. Si alguna corresponde, agregarla a VARS_SEXO_CANDIDATAS.')
} else {
  message('Variable(s) de sexo detectada(s) en el SIMCE por alumno: ',
          paste(sexo_detectado, collapse = ', '))
}

datos_simce_alu_mrun_consolidado <- datos_simce_alu_mrun |>
  map(~{
    # Homologar nombres y tipos de datos:
    data_vars_seleccionadas <- .x |>
      select(agno, grado, idalumno, mrun, rbd, dvrbd, cod_curso,
             any_of(VARS_SEXO_CANDIDATAS),
             starts_with(c('ptje_mate', 'ptje_lect', 'eem_mate', 'eem_lect', 'eda_mate', 'eda_lect')))
    names(data_vars_seleccionadas) <- str_remove_all(names(data_vars_seleccionadas),
                                                     '(4b|8b|2m|6b)_alu')
    data_homologada <- data_vars_seleccionadas |> 
      mutate(across(contains(c('ptje', 'eem', 'eda')), as.numeric))
    return(data_homologada)
  }
  ) |> 
  list_rbind() |> 
  mutate(sexo = ifelse(!is.na(gen_alu), gen_alu, sexo),
         sexo = ifelse(sexo %in% 1:2, sexo, NA))

# Chequeo de sanidad: alerta (no detiene la ejecución) si aparecen puntajes
# fuera de rango plausible, para detectar a tiempo problemas de parseo futuros:
validar_rango_puntajes(datos_simce_alu_mrun_consolidado, c('ptje_mate', 'ptje_lect'))

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

# Limpiar ambiente
gc()


