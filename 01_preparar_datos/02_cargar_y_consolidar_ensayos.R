library(tidyverse)
library(arrow)
library(readxl)

# Cargar funciones 
source("funciones/funciones_proyecto_simce.R")

# Configurar rutas de archivos: ----
usuario <- Sys.info()[["user"]]
rutas <- config::get(config = usuario, file = "config.yml")

ruta_data_in <- rutas$ruta_data_in
ruta_data_intermedia <- rutas$ruta_data_intermedia

# Especificar dónde se van a guardar las salidas
dir_salida <- ruta_data_intermedia |> file.path('ensayo_santillana')
dir_salida |> dir.create(showWarnings = FALSE)

# Listado de archivos brutos de pruebas diagnóstico: ----
ruta_archivos_ensayo_santillana <- ruta_data_in %>%  
  file.path('ensayos_santillana') |> 
  list.files(pattern = 'medicion_santillana', 
             full.names = TRUE) %>% 
  map(list.files, full.names = TRUE,
                pattern = 'Ensayo',
                recursive = TRUE) %>% 
  unlist()

# Cargar y leer los archivos, para después consolidarlos: ----

## SIMCE por alumno: ----
datos_ensayo_santillana <- ruta_archivos_ensayo_santillana |> 
  map(read_excel) 

datos_ensayo_santillana_consolidado <- datos_ensayo_santillana |> 
  map(~{
    # Homologar nombres y tipos de datos:
    .x |> 
      janitor::clean_names() |> 
      dplyr::select(agno = ano_lectivo, pais, id_colegio, colegio, curso, area, 
             id_evaluacion, evaluacion, 
             id_usuario_curso, nombre = nombre_y_apellido,
             porcentaje_logro = porcentaje_de_logro) |> 
      mutate(porcentaje_logro = as.numeric(porcentaje_logro)) 
  }
  ) |> 
  list_rbind()

# Obtener sexo a partir del nombre
nombres_guaguas <- guaguas::guaguas %>% 
  filter(sexo %in% c('M', 'F'), anio > 1990) %>% 
  count(nombre, sexo, wt = n) %>% 
  mutate(sexo = ifelse(sexo == 'M', 'hombre', 'mujer')) %>% 
  pivot_wider(names_from = sexo, 
              values_from = n) %>%
  mutate(across(where(is.numeric), ~replace_na(.x, 0)), 
         n_total = hombre + mujer,
         p_hombre = hombre / n_total,
         sexo = case_when(p_hombre > .75 ~ 'hombre',
                          p_hombre < .25 ~ 'mujer',
                          TRUE ~ 'indeterminado')) %>% 
  filter(n_total >= 5)

get_sexo_del_nombre <- function(x) {
  x <- tolower(str_squish(x))
  sexo <- nombres_guaguas$sexo[match(x, tolower(nombres_guaguas$nombre))]
  return(sexo)
}

nombres_con_sexo <- datos_ensayo_santillana_consolidado %>% 
  distinct(id_usuario_curso, nombre) %>% 
  mutate(
    nombre = tolower(str_squish(nombre)),
    nombre_corregido = str_replace_all(nombre, c('&aacute;' = 'á',
                                                 '&eacute;' = 'é',
                                                 '&iacute;' = 'í',
                                                 '&oacute;' = 'ó',
                                                 '&uacute;' = 'ú',
                                                 '&agrave;' = 'á',
                                                 '&egrave;' = 'é',
                                                 '&igrave;' = 'í',
                                                 '&ograve;' = 'ó',
                                                 '&ugrave;' = 'ú',
                                                 '&ntilde;' = 'ñ',
                                                 'de la ' = ' ',
                                                 'del ' = ' ',
                                                 'di ' = ' ',
                                                 'de ' = ' ',
                                                 '-' = '')
                                       ),
    cambio = nombre_corregido != nombre,
    primer_nombre = str_extract(nombre_corregido, '\\w+\\s\\w+\\s(\\w+)', group = 1),
    segundo_nombre = str_extract(nombre_corregido, '\\w+\\s\\w+\\s\\w+\\s(\\w+)', group = 1),
    ultimo_nombre = str_extract(nombre_corregido, '(\\w+)$', group = 1),
    ultimo_nombre = ifelse((ultimo_nombre == primer_nombre & !is.na(primer_nombre)) | (ultimo_nombre == segundo_nombre & !is.na(segundo_nombre)), NA, ultimo_nombre),
    penultimo_nombre = str_extract(nombre_corregido, '(\\w+) (\\w+)$', group = 1),
    penultimo_nombre = ifelse((penultimo_nombre == primer_nombre & !is.na(primer_nombre)) | (penultimo_nombre == segundo_nombre & !is.na(segundo_nombre)) | (penultimo_nombre == ultimo_nombre & !is.na(ultimo_nombre) ), NA, penultimo_nombre),
    sexo_primer_nombre = get_sexo_del_nombre(primer_nombre),
    sexo_segundo_nombre = get_sexo_del_nombre(segundo_nombre),
    sexo_ultimo_nombre = get_sexo_del_nombre(ultimo_nombre),
    sexo_penultimo_nombre = get_sexo_del_nombre(penultimo_nombre),
    sexo = case_when(
      !is.na(sexo_primer_nombre) ~ sexo_primer_nombre,
      !is.na(sexo_segundo_nombre) ~ sexo_segundo_nombre,
      !is.na(sexo_ultimo_nombre) ~ sexo_ultimo_nombre,
      !is.na(sexo_penultimo_nombre) ~ sexo_penultimo_nombre))

# Agregar RBD ----

## Hay distintas versiones así que las junto todas para ver si son consistentes primero: ----
codigos_plenos_rbd_1 <- file.path(ruta_data_in, 'Datos Medición Nacional_RBD',
                                  'SIMCE 2024-2025 + Colegios Pleno.xlsx') %>% 
  read_excel() %>% 
  janitor::clean_names() %>% 
  dplyr::select(rbd, id_pleno_1 = id_pleno, id_pleno_2) |> 
  pivot_longer(cols = c(id_pleno_1, id_pleno_2),
               names_to = "version_id_pleno",
               values_to = "id_pleno") |> 
  mutate(version_id_pleno = paste0('SIMCE 2024-2025 + Colegios Pleno - ', version_id_pleno))

codigos_plenos_rbd_2 <- file.path(ruta_data_in, 'Datos Medición Nacional_RBD',
                                  'Listado de colegios Pleno Chile.xlsx') %>% 
  read_excel() |> 
  janitor::clean_names() %>% 
  dplyr::select(rbd, id_pleno) |> 
  mutate(version_id_pleno = "Listado de colegios Pleno Chile",
         rbd = as.integer(rbd)) |> 
  filter(!is.na(rbd))

codigos_plenos_rbd_3 <- file.path(ruta_data_in, 'ensayos_santillana',
                                  'colegios_sin_rbd_ agregados v1.xlsx') %>% 
  read_excel(sheet = 'Lista Colegios Pleno') |> 
  janitor::clean_names() |> 
  dplyr::select(rbd, id_pleno = id_colegio) |> 
  mutate(version_id_pleno = "colegios_sin_rbd_ agregados v1")

<<<<<<< HEAD
codigos_plenos_rbd_4 <- file.path(ruta_data_in, 'Datos Medición Nacional_RBD',
                                  'Colegios_rbd_pendientes_de_revision PP_M.xlsx') %>% 
  read_excel() |>
  janitor::clean_names()

codigos_plenos_rbd_4<-codigos_plenos_rbd_4 |>
  mutate(version_id_pleno = "agregado_v4"
         ,rbd_identificado= as.numeric(rbd_identificado)) |> 
  select(
    "rbd"=rbd_identificado
    ,id_pleno
    ,version_id_pleno
  ) |> 
  filter(
    !is.na(rbd)
  )




## Evaluar si RBD son consistentes y están completas: ----
conversion_id_colegio_rbd <- bind_rows(codigos_plenos_rbd_1,
                                codigos_plenos_rbd_2,
                                codigos_plenos_rbd_3,
                                codigos_plenos_rbd_4) |> 
  distinct(id_pleno, rbd, .keep_all = TRUE) |> 
=======
codigos_plenos_rbd_4 <- file.path(ruta_data_in, 'ensayos_santillana',
                                  'Colegios_rbd_pendientes_de_revision_PP_M_05082026.xlsx') %>% 
  read_excel() |> 
  janitor::clean_names() %>% 
  dplyr::select(id_pleno, rbd = rbd_identificado) |> 
  mutate(rbd = as.numeric(rbd),
    version_id_pleno = "Colegios_rbd_pendientes_de_revision_PP_M_05082026") %>% 
  filter(!is.na(rbd))

## Evaluar si RBD son consistentes y están completas: ----
conversion_id_colegio_rbd <- bind_rows(codigos_plenos_rbd_4,
                                       codigos_plenos_rbd_3,
                                       codigos_plenos_rbd_2,
                                       codigos_plenos_rbd_1
                                ) |> 
  distinct(id_pleno, .keep_all = TRUE) |> 
  filter(!is.na(id_pleno))

datos_ensayo_santillana_consolidado_rbd <- datos_ensayo_santillana_consolidado %>% 
  left_join(conversion_id_colegio_rbd, by = c('id_colegio' = 'id_pleno')) 

cat("Hay", sum(is.na(datos_ensayo_santillana_consolidado_rbd |> distinct(id_colegio, rbd) |>  pull(rbd))), "colegios sin RBD")

datos_ensayo_santillana_consolidado_final0 <- datos_ensayo_santillana_consolidado_rbd |> 
  left_join(nombres_con_sexo %>% distinct(id_usuario_curso, sexo), 
            by = 'id_usuario_curso') %>% 
  mutate(
    agno = as.numeric(agno),
    grado = case_when(str_detect(tolower(curso), '2..m') ~ '2m',
                      str_detect(tolower(curso), '4..b') ~ '4b'),
    area = case_when(str_detect(tolower(area), 'lenguaje') ~ 'lenguaje', 
                     str_detect(tolower(area), 'mate') ~ 'matematica'),
    tipo_evaluacion = str_extract(str_squish(evaluacion), '\\w+ \\w+ \\d'),
    apellido_evaluacion = evaluacion %>% 
      str_remove('\\(202.\\)') %>% 
      str_remove('-') %>% 
      str_squish() %>% 
      str_remove(tipo_evaluacion) %>% 
      str_squish(),
    apellido_evaluacion = ifelse(apellido_evaluacion == '', NA, apellido_evaluacion),
    n_evaluacion = str_extract(tipo_evaluacion, '\\d+') %>% str_squish(),
    tipo_evaluacion = str_remove(tipo_evaluacion, '\\d+') %>% str_squish()
  )

# Agregar puntaje simce modelo 1
tabla_conversion_modelo1 <- read_excel(file.path(ruta_data_in, "Escala Simce Santillana 4basico_2025.xlsx"),
                               sheet = "tabla_conversion") %>% 
  rename(lenguaje = puntaje_lect4b,
         matematica = puntaje_mate4b) %>%  
  mutate(grado = '4b') %>%  # Modelo 1 solo cubre los puntajes de 4b, no 2m
  pivot_longer(cols = c(lenguaje, matematica),
               names_to = 'area',
               values_to = 'simce_estimado_modelo1')

datos_ensayo_santillana_consolidado_final <- datos_ensayo_santillana_consolidado_final0 |> 
  mutate(porcentaje_logro_sin_decimal = round(porcentaje_logro)) %>% 
  left_join(tabla_conversion_modelo1, by = c('porcentaje_logro_sin_decimal' = 'porc_lect4b', 'grado', 'area'))

# Guardar resultados
datos_ensayo_santillana_consolidado_final |> 
  write_parquet(file.path(dir_salida, 'consolidado_ensayo_santillana.parquet'))

# Limpiar ambiente
gc()
rm(list=ls())

