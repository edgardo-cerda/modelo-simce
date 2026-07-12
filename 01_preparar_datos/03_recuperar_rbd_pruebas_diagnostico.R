# =============================================================================
# Extracción y verificación de códigos RBD
# =============================================================================

library(readxl)
library(dplyr)
library(stringr)
library(stringi)
library(stringdist)
library(writexl)

# -----------------------------------------------------------------------------
# 0. RUTAS
# -----------------------------------------------------------------------------

# Configurar rutas de archivos: ----
rutas <- config::get(file = "config.yml")
ruta_data_in <- rutas$ruta_data_in
ruta_data_intermedia <- rutas$ruta_data_intermedia

archivo_diagnosticos <- ruta_data_intermedia |> file.path('prueba_diagnostico', 'consolidado_pruebas_diagnostico.parquet')
archivo_simces <- ruta_data_intermedia |> file.path('simce', 'consolidado_datos_simce_rbd.parquet')

archivo_salida <- ruta_data_intermedia |> file.path('prueba_diagnostico', 'diccionario_rbd_prueba_diagnostico.xlsx')

# -----------------------------------------------------------------------------
# 1. CARGA DE DATOS
# -----------------------------------------------------------------------------
pruebas_diagnostico <- read_parquet(archivo_diagnosticos) |> 
  select(id_colegio, colegio) %>%
  distinct(colegio, .keep_all = TRUE)

rbd_ref <- read_parquet(archivo_simces) |> 
  mutate(
    rbd         = as.integer(rbd),
    dvrbd       = as.character(dvrbd),
    nom_simce = stri_trans_general(toupper(nom_rbd), "Latin-ASCII"),
    nom_com_rbd = stri_trans_general(toupper(nom_com_rbd), "Latin-ASCII")
  ) |> 
  distinct(rbd, dvrbd, nom_simce, nom_com_rbd) |> 
  distinct(rbd, .keep_all = TRUE)

rbd_validos <- rbd_ref$rbd

# -----------------------------------------------------------------------------
# 2. DÍGITO VERIFICADOR DE RBD (mismo algoritmo módulo 11 que el RUT chileno)
# -----------------------------------------------------------------------------
dv_rbd <- function(rbd) {
  digitos  <- rev(as.integer(strsplit(as.character(rbd), "")[[1]]))
  factores <- rep(c(2, 3, 4, 5, 6, 7), length.out = length(digitos))
  resto    <- 11 - (sum(digitos * factores) %% 11)
  dplyr::case_when(resto == 11 ~ "0", resto == 10 ~ "K", TRUE ~ as.character(resto))
}

# -----------------------------------------------------------------------------
# 3. EXTRACCIÓN DE CANDIDATOS NUMÉRICOS DESDE EL NOMBRE DEL COLEGIO
#    Reglas:
#      - Se descarta el año inicial "(2025)".
#      - Se descartan números precedidos por "N°"/"Nº" (numeración interna
#        del establecimiento, ej. "Escuela Básica N° 2468"), que NO es RBD.
#      - Si el nombre trae el patrón "RBD-DV" (ej. "1393-5"), se interpreta
#        como RBD + dígito verificador y se valida con dv_rbd().
# -----------------------------------------------------------------------------
colegios_por_rbd <- pruebas_diagnostico |> 
  mutate(rbd_candidato = extraer_rbd_candidatos(colegio),
         rbd_candidato = as.integer(rbd_candidato)) |> 
  left_join(rbd_ref, by = c('rbd_candidato' = 'rbd')) |> 
  filter(!is.na(nom_simce)) |> 
  mutate(dist = stringdist(limpiar_nombre(colegio),
                           limpiar_nombre(nom_simce), method = "jw", p = 0.1))

extraer_candidatos <- function(nombre) {
  s <- str_remove(nombre, "^\\(\\s*20\\d{2}\\s*\\)")
  
  m <- gregexpr("\\d+", s)[[1]]
  if (m[1] == -1) return(character(0))
  largos <- attr(m, "match.length")
  
  candidatos <- character(0)
  for (i in seq_along(m)) {
    ini <- m[i]
    contexto_previo <- substr(s, max(1, ini - 3), ini - 1)
    if (str_detect(contexto_previo, "(?i)[n\u00f1][\u00b0\u00ba]\\s*$")) next
    candidatos <- c(candidatos, substr(s, ini, ini + largos[i] - 1))
  }
  candidatos
}

detectar_rbd_dv <- function(nombre) {
  s <- str_remove(nombre, "^\\(\\s*20\\d{2}\\s*\\)")
  m <- str_match(s, "(\\d{2,6})-(\\d|[Kk])(?!\\d)")
  if (is.na(m[1, 1])) return(NULL)
  list(rbd = as.integer(m[1, 2]), dv = toupper(m[1, 3]))
}

# -----------------------------------------------------------------------------
# 4. EXTRACCIÓN POR COINCIDENCIA APROXIMADA (fallback cuando el nombre no
#    trae ningún RBD). Se compara el nombre "limpio" del colegio contra
#    nom_rbd del listado de referencia, usando similitud Jaro-Winkler y,
#    si es posible, la comuna mencionada entre paréntesis como filtro.
#
#    OJO: simce2m2025_rbd_preliminar.xlsx es el listado de 2° medio 2025.
#    Muchos colegios del ensayo son de 4° básico y pueden no aparecer ahí
#    (ej. escuelas que no imparten enseñanza media). Por eso el umbral de
#    aceptación es exigente y todo lo que no lo supere queda para revisión
#    manual en vez de asignar un RBD incorrecto.
# -----------------------------------------------------------------------------
limpiar_nombre <- function(nombre) {
  s <- str_remove(nombre, "^\\(\\s*20\\d{2}\\s*\\)")
  s <- str_replace_all(s, "[\\(\\)\\-\u2013\\d\u00b0\u00ba\\.]", " ")
  s <- str_squish(s)
  toupper(stri_trans_general(s, "Latin-ASCII"))
}

extraer_comuna <- function(nombre) {
  grupos <- str_match_all(nombre, "\\(([^\\)]+)\\)")[[1]][, 2]
  grupos <- grupos[!str_detect(grupos, "\\d") & nchar(str_trim(grupos)) > 2]
  if (length(grupos) == 0) return(NA_character_)
  toupper(stri_trans_general(str_trim(tail(grupos, 1)), "Latin-ASCII"))
}

sim_token_sort <- function(a, b) {
  ord <- function(x) paste(sort(str_split(x, "\\s+")[[1]]), collapse = " ")
  1 - stringdist(ord(a), ord(b), method = "jw", p = 0.1)
}

buscar_por_nombre <- function(nombre) {
  nombre_limpio <- limpiar_nombre(nombre)
  comuna_hint   <- extraer_comuna(nombre)
  
  candidatos <- rbd_ref
  if (!is.na(comuna_hint)) {
    filtrado <- candidatos %>% filter(str_detect(nom_com_rbd, fixed(comuna_hint)))
    if (nrow(filtrado) > 0) candidatos <- filtrado
  }
  
  sims <- vapply(candidatos$nom_simce, sim_token_sort, numeric(1), b = nombre_limpio)
  best <- which.max(sims)
  
  list(
    rbd_sugerido  = candidatos$rbd[best],
    nombre_simce  = candidatos$nom_simce[best],
    comuna_simce  = candidatos$nom_com_rbd[best],
    similitud     = round(sims[best] * 100, 1)
  )
}

UMBRAL_ACEPTACION <- 85   # % de similitud mínimo para aceptar un match por nombre

# -----------------------------------------------------------------------------
# 5. PROCESAMIENTO PRINCIPAL
# -----------------------------------------------------------------------------
procesar_colegio <- function(nombre) {
  
  cat("Procesando", nombre, '\n')
  
  rbd_dv <- detectar_rbd_dv(nombre)
  if (!is.null(rbd_dv)) {
    dv_calculado <- dv_rbd(rbd_dv$rbd)
    ok <- dv_calculado == rbd_dv$dv
    return(tibble(
      RBD              = rbd_dv$rbd,
      Metodo           = "RBD-DV en nombre",
      Estado           = if (ok) "OK (DV validado)" else "REVISAR (DV no coincide)",
      Similitud_nombre = NA_real_,
      Nombre_SIMCE     = NA_character_
    ))
  }
  
  candidatos <- extraer_candidatos(nombre)
  
  if (length(candidatos) == 1) {
    rbd <- as.integer(candidatos[1])
    en_listado <- rbd %in% rbd_validos
    return(tibble(
      RBD    = rbd,
      Metodo = "Extraído del nombre",
      Estado = if (en_listado) "OK (verificado en listado simce)"
      else "OK (no verificable: colegio no est\u00e1 en listado simce)",
      Similitud_nombre = NA_real_,
      Nombre_SIMCE     = if (en_listado) rbd_ref$nom_simce[rbd_ref$rbd == rbd][1] else NA_character_
    ))
  }
  
  if (length(candidatos) >= 2) {
    return(tibble(
      RBD    = as.integer(candidatos[1]),
      Metodo = "Extraído del nombre (ambiguo)",
      Estado = paste0("AMBIGUO - candidatos: ", paste(candidatos, collapse = ", "),
                      " - revisar manualmente"),
      Similitud_nombre = NA_real_,
      Nombre_SIMCE     = NA_character_
    ))
  }
  
  # length(candidatos) == 0  ->  no hay número en el nombre: buscar por similitud
  m <- buscar_por_nombre(nombre)
  if (!is.na(m$similitud) && m$similitud >= UMBRAL_ACEPTACION) {
    tibble(
      RBD    = as.integer(m$rbd_sugerido),
      Metodo = "Coincidencia por nombre (listado simce)",
      Estado = paste0("REVISAR SUGERIDO (similitud ", m$similitud, "%)"),
      Similitud_nombre = m$similitud,
      Nombre_SIMCE     = m$nombre_simce
    )
  } else {
    tibble(
      RBD    = NA_integer_,
      Metodo = "Sin número en el nombre",
      Estado = paste0("NO ENCONTRADO - mejor coincidencia '", m$nombre_simce,
                      "' (", m$similitud, "%) - revisar manualmente"),
      Similitud_nombre = m$similitud,
      Nombre_SIMCE     = NA_character_
    )
  }
}

resultado <- pruebas_diagnostico %>% 
  rowwise() %>%
  mutate(procesar_colegio(colegio)) %>%
  ungroup()

# -----------------------------------------------------------------------------
# 6. RESUMEN Y EXPORTACIÓN
# -----------------------------------------------------------------------------
cat("Total colegios:", nrow(resultado), "\n")
cat("RBD extraído directo del nombre:",
    sum(resultado$Metodo == "Extraído del nombre"), "\n")
cat("RBD-DV validado en nombre:",
    sum(resultado$Metodo == "RBD-DV en nombre"), "\n")
cat("Casos ambiguos (2+ números):",
    sum(resultado$Metodo == "Extraído del nombre (ambiguo)"), "\n")
cat("Resueltos por coincidencia de nombre (>=", UMBRAL_ACEPTACION, "%):",
    sum(resultado$Metodo == "Coincidencia por nombre (listado simce)"), "\n")
cat("Sin resolver (requieren revisión manual):",
    sum(resultado$Metodo == "Sin número en el nombre"), "\n")

write_xlsx(resultado, archivo_salida)