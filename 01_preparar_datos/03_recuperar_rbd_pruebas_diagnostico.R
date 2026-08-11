# =============================================================================
# Extracción y verificación de códigos rbd_identificado
# =============================================================================

library(tidyverse)
library(arrow)
library(stringi)
library(stringdist)
library(writexl)
library(httr)

UMBRAL_ACEPTACION <- 90   # % de similitud mínimo para aceptar un match por nombre
UMBRAL_COMUNA     <- 85   # % de similitud mínimo para aceptar que dos comunas son la misma

# -----------------------------------------------------------------------------
# 0. RUTAS
# -----------------------------------------------------------------------------

# Configurar rutas de archivos: ----
usuario <- Sys.info()[["user"]]
rutas <- config::get(config = usuario, file = "config.yml")

ruta_data_in <- rutas$ruta_data_in
ruta_data_intermedia <- rutas$ruta_data_intermedia

archivo_ensayo <- ruta_data_intermedia |> file.path('ensayo_santillana', 'consolidado_ensayo_santillana.parquet')
archivo_simces <- ruta_data_intermedia |> file.path('simce', 'consolidado_datos_simce_rbd.parquet')

archivo_salida <- ruta_data_intermedia |>
  file.path('ensayo_santillana', 'diccionario_rbd_ensayo.xlsx')

# -----------------------------------------------------------------------------
# 1. CARGA DE DATOS
# -----------------------------------------------------------------------------

# Patrón de comuna entre paréntesis: letras (incl. tildes y ñ) y espacios.
# ANTES solo incluía [a-zñ ], por lo que comunas con tilde (p.ej. "Viña del
# Mar", "Valparaíso", "Quilpué", "La Unión") no se reconocían: str_extract
# devolvía NA y el "(comuna)" quedaba pegado al nombre del colegio en vez de
# limpiarse. Se agrega áéíóúü al set de caracteres permitido.
PATRON_COMUNA <- '\\(([a-zñáéíóúü ]+)\\)'

ensayo_santillana <- read_parquet(archivo_ensayo) |>
  select(id_colegio, colegio, rbd_santillana = rbd) %>%
  distinct(colegio, .keep_all = TRUE) |>
  mutate(colegio = str_replace_all(tolower(colegio),
                                   pattern = c('esc\\.' = 'escuela ',
                                               'bas\\.' = 'basica ',
                                               'col\\.|col ' = 'colegio ',
                                               '(part|partic)\\.' = 'paticular ',
                                               '(municip|munic)\\.' = 'municipal',
                                               'tecn\\.' = 'tecnico',
                                               'polit\\.' = 'politecnico',
                                               'fund\\. educ\\.' = 'fundacion educacional',
                                               'c\\. h\\.' = 'cientifico humanista',
                                               'biling.e$' = '')),
         # Quitar partes del nombre del colegio que no son realmente del colegio:
         # (piloto) / (impreso) son etiquetas administrativas, no una comuna.
         colegio = str_replace_all(colegio, c('\\(piloto\\)' = '', '- piloto -' = '',
                                              '\\(impreso\\)' = '')),
         # Comuna en el nombre (p.ej. "(quilpué)"). group = 1 captura solo el
         # contenido, ya sin paréntesis, por lo que no hace falta un
         # str_replace_all posterior para quitarlos.
         comuna = str_extract(colegio, PATRON_COMUNA, group = 1) |> str_squish(),
         # Versión normalizada (sin tildes) para comparar contra comuna_std,
         # que también viene normalizada. Esto es lo que soluciona los casos
         # donde la comuna está escrita distinto (con/sin tilde, con/sin ñ).
         comuna_norm = if_else(is.na(comuna), NA_character_,
                               str_squish(stri_trans_general(comuna, "Latin-ASCII"))),
         colegio = str_remove(colegio, PATRON_COMUNA),
         colegio = str_remove(colegio, '\\(202.\\)'), # Año 2024-2025 en el nombre
         colegio = str_replace_all(str_squish(colegio), c('^-' = '', '-$' = '')),
         colegio = str_squish(colegio))

rbd_referencia <- read_parquet(archivo_simces) |>
  mutate(
    rbd         = as.integer(rbd),
    dvrbd       = as.character(dvrbd),
    nom_simce = stri_trans_general(tolower(nom_rbd), "Latin-ASCII"),
    comuna_std = stri_trans_general(tolower(nom_com_rbd), "Latin-ASCII") |> str_squish()
  ) |>
  distinct(rbd, dvrbd, nom_simce, comuna_std) |>
  distinct(rbd, .keep_all = TRUE) |>
  mutate(nom_simce = tolower(nom_simce),
         nom_simce = str_replace_all(nom_simce,
                                     pattern = c('esc\\.' = 'escuela ',
                                                 'bas\\.' = 'basica ',
                                                 'col\\.' = 'colegio ',
                                                 '(part|partic)\\.' = 'paticular ',
                                                 '(municip|munic)\\.' = 'municipal',
                                                 'tecn\\.' = 'tecnico',
                                                 'polit\\.' = 'politecnico',
                                                 'fund\\. educ\\.' = 'fundacion educacional',
                                                 'c\\. h\\.' = 'cientifico humanista')),
         nom_simce = str_squish(nom_simce))

rbd_validos <- rbd_referencia$rbd

# -----------------------------------------------------------------------------
# 2. DÍGITO VERIFICADOR DE rbd_identificado (mismo algoritmo módulo 11 que el RUT chileno)
# -----------------------------------------------------------------------------
dv_rbd <- function(rbd) {
  digitos  <- rev(as.integer(strsplit(as.character(rbd), "")[[1]]))
  factores <- rep(c(2, 3, 4, 5, 6, 7), length.out = length(digitos))
  resto    <- 11 - (sum(digitos * factores) %% 11)
  # antes devolvía "K" en mayúscula, pero detectar_rbd_dv() compara contra
  # tolower(dv) => una rbd_identificado-DV real con "K" nunca calzaba. Se devuelve en minúscula.
  dplyr::case_when(resto == 11 ~ "0", resto == 10 ~ "k", TRUE ~ as.character(resto))
}

# -----------------------------------------------------------------------------
# 3. EXTRACCIÓN DE CANDIDATOS NUMÉRICOS DESDE EL NOMBRE DEL COLEGIO
#    Reglas:
#      - Se descarta el año inicial "(2025)".
#      - Se descartan números precedidos por "N°"/"Nº" (numeración interna
#        del establecimiento, ej. "Escuela Básica N° 2468"), que NO es rbd_identificado.
#      - Si el nombre trae el patrón "rbd_identificado-DV" (ej. "1393-5"), se interpreta
#        como rbd_identificado + dígito verificador y se valida con dv_rbd().
# -----------------------------------------------------------------------------
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
  list(rbd = as.integer(m[1, 2]), dv = tolower(m[1, 3]))
}

# -----------------------------------------------------------------------------
# 4. VALIDACIÓN DE COMUNA
#    Compara la comuna extraída del nombre del ensayo contra comuna_std del
#    listado SIMCE, tolerando diferencias de escritura (tildes, ñ/n, mayúsculas)
#    mediante similitud Jaro-Winkler. Ambos inputs deben venir ya normalizados
#    (minúscula, sin tilde) para que la comparación sea consistente.
#    Devuelve NA si no hay comuna con la cual comparar (no es un error, es que
#    no hay información suficiente para validar).
# -----------------------------------------------------------------------------
comuna_coincide <- function(hint, referencia, umbral_comuna = UMBRAL_COMUNA) {
  ifelse(
    is.na(hint) | is.na(referencia),
    NA,
    referencia == hint |
      (1 - stringdist(hint, referencia, method = "jw", p = 0.1)) * 100 >= umbral_comuna
  )
}

# -----------------------------------------------------------------------------
# 5. EXTRACCIÓN POR COINCIDENCIA APROXIMADA (fallback cuando el nombre no
#    trae ningún rbd_identificado). Se compara el nombre "limpio" del colegio contra
#    nom_simce del listado de referencia, usando similitud Jaro-Winkler y,
#    si está disponible, la comuna extraída en la sección 1 como filtro
#    (con comparación tolerante a diferencias de escritura, ver sección 4).
#
#    OJO: simce2m2025_rbd_preliminar.xlsx es el listado de 2° medio 2025.
#    Muchos colegios del ensayo son de 4° básico y pueden no aparecer ahí
#    (ej. escuelas que no imparten enseñanza media). Por eso el umbral de
#    aceptación es exigente y todo lo que no lo supere queda para revisión
#    manual en vez de asignar un rbd_identificado incorrecto.
# -----------------------------------------------------------------------------
limpiar_nombre <- function(nombre) {
  s <- str_remove(nombre, "^\\(\\s*20\\d{2}\\s*\\)")
  s <- str_replace_all(s, "[\\(\\)\\-\u2013\\d\u00b0\u00ba\\.]", " ")
  s <- str_squish(s)
  tolower(stri_trans_general(s, "Latin-ASCII"))
}

sim_token_sort <- function(a, b) {
  ord <- function(x) paste(sort(str_split(x, "\\s+")[[1]]), collapse = " ")
  1 - stringdist(ord(a), ord(b), method = "jw", p = 0.1)
}

# NOTA: antes esta función recibía `nombre` (el nombre del colegio) y trataba
# de extraer la comuna desde ahí con extraer_comuna(). Como en la sección 1 la
# comuna ya se extrae y se quita del nombre, ese paréntesis ya no existe en
# `nombre` y extraer_comuna() siempre devolvía NA: el filtro por comuna estaba
# efectivamente muerto. Ahora se recibe directamente `comuna_hint` (ya
# normalizada) como parámetro, calculada una sola vez en la sección 1.
buscar_por_nombre <- function(nombre, comuna_hint = NA_character_) {
  nombre_limpio <- limpiar_nombre(nombre)
  
  candidatos <- rbd_referencia
  if (!is.na(comuna_hint)) {
    filtrado <- candidatos %>% filter(comuna_coincide(comuna_hint, comuna_std))
    if (nrow(filtrado) > 0) candidatos <- filtrado
  }
  
  sims <- vapply(candidatos$nom_simce, sim_token_sort, numeric(1), b = nombre_limpio)
  best <- which.max(sims)
  
  list(
    rbd_sugerido  = candidatos$rbd[best],
    nombre_simce  = candidatos$nom_simce[best],
    comuna_simce  = candidatos$comuna_std[best],
    similitud     = round(sims[best] * 100, 1)
  )
}

# -----------------------------------------------------------------------------
# 6. PROCESAMIENTO PRINCIPAL
# -----------------------------------------------------------------------------
procesar_colegio <- function(nombre, comuna_hint = NA_character_) {
  
  cat("Procesando", nombre)
  
  rbd_dv <- detectar_rbd_dv(nombre)
  
  if (!is.null(rbd_dv)) {
    dv_calculado <- dv_rbd(rbd_dv$rbd)
    ok <- dv_calculado == rbd_dv$dv
    
    cat("- rbd_identificado-DV en nombre:", rbd_dv$rbd, '\n')
    return(tibble(
      rbd_identificado  = rbd_dv$rbd,
      Metodo            = "rbd_identificado-DV en nombre",
      Estado            = if (ok) "OK (DV validado)" else "REVISAR (DV no coincide)",
      Similitud_nombre  = NA_real_,
      Nombre_SIMCE      = NA_character_,
      Comuna_ensayo     = comuna_hint,
      Comuna_coincide   = NA
    ))
  }
  
  candidatos <- extraer_candidatos(nombre)
  
  if (length(candidatos) == 1) {
    rbd <- as.integer(candidatos[1])
    en_listado <- rbd %in% rbd_validos
    comuna_simce <- if (en_listado) rbd_referencia$comuna_std[rbd_referencia$rbd == rbd][1] else NA_character_
    comuna_ok <- comuna_coincide(comuna_hint, comuna_simce)
    
    estado <- case_when(
      !en_listado ~ "OK (no verificable: colegio no est\u00e1 en listado simce)",
      isTRUE(comuna_ok) ~ "OK (verificado en listado simce, comuna coincide)",
      isFALSE(comuna_ok) ~ "REVISAR (rbd_identificado en listado simce pero la comuna del nombre no coincide)",
      TRUE ~ "OK (verificado en listado simce)"
    )
    
    cat(" -> Extraído del nombre:", rbd, '\n')
    
    return(tibble(
      rbd_identificado  = rbd,
      Metodo            = "Extraído del nombre",
      Estado            = estado,
      Similitud_nombre  = NA_real_,
      Nombre_SIMCE      = if (en_listado) rbd_referencia$nom_simce[rbd_referencia$rbd == rbd][1] else NA_character_,
      Comuna_ensayo     = comuna_hint,
      Comuna_coincide   = comuna_ok
    ))
  }
  
  if (length(candidatos) >= 2) {
    cat(' -> Extraído del nombre (ambiguo). Candidatos:', paste(candidatos, collapse = ", "), '\n')
    
    # Antes: se elegía candidatos[1] a ciegas y quedaba todo como "revisar
    # manualmente". Ahora: si hay una comuna en el nombre, se usa para ver si
    # SOLO uno de los candidatos numéricos es un rbd_identificado válido cuya comuna
    # coincide -> en ese caso se resuelve automáticamente. Si no, sigue ambiguo.
    cand_int <- as.integer(candidatos)
    info <- tibble(rbd = cand_int) |>
      left_join(rbd_referencia |> select(rbd, nom_simce, comuna_std), by = "rbd") |>
      mutate(comuna_ok = comuna_coincide(comuna_hint, comuna_std))
    
    # filter() descarta NA y FALSE por igual, así que solo quedan candidatos
    # que están en el listado simce Y cuya comuna coincide con la del nombre.
    validos <- info |> filter(rbd %in% rbd_validos, comuna_ok)
    
    if (!is.na(comuna_hint) && nrow(validos) == 1) {
      cat("    -> Resuelto por comuna:", validos$rbd[1], '\n')
      return(tibble(
        rbd_identificado  = validos$rbd[1],
        Metodo            = "Extraído del nombre (ambiguo, resuelto por comuna)",
        Estado            = paste0("OK (comuna '", comuna_hint, "' coincide solo con rbd_identificado ", validos$rbd[1], ")"),
        Similitud_nombre  = NA_real_,
        Nombre_SIMCE      = validos$nom_simce[1],
        Comuna_ensayo     = comuna_hint,
        Comuna_coincide   = TRUE
      ))
    }
    
    return(tibble(
      rbd_identificado  = as.integer(candidatos[1]),
      Metodo            = "Extraído del nombre (ambiguo)",
      Estado            = paste0("AMBIGUO - candidatos: ", paste(candidatos, collapse = ", "),
                                 if (!is.na(comuna_hint)) paste0(" - comuna en nombre: '", comuna_hint, "'") else "",
                                 " - revisar manualmente"),
      Similitud_nombre  = NA_real_,
      Nombre_SIMCE      = NA_character_,
      Comuna_ensayo     = comuna_hint,
      Comuna_coincide   = NA
    ))
  }
  
  # length(candidatos) == 0  ->  no hay número en el nombre: buscar por similitud
  m <- buscar_por_nombre(nombre, comuna_hint)
  comuna_ok <- comuna_coincide(comuna_hint, m$comuna_simce)
  
  if (!is.na(m$similitud) && m$similitud >= UMBRAL_ACEPTACION) {
    cat(" -> Coincidencia por nombre", m$rbd_sugerido,  "(similitud", m$similitud, "%)", '\n')
    
    tibble(
      rbd_identificado  = as.integer(m$rbd_sugerido),
      Metodo            = "Coincidencia por nombre (listado simce)",
      Estado            = paste0("REVISAR SUGERIDO (similitud ", m$similitud, "%)"),
      Similitud_nombre  = m$similitud,
      Nombre_SIMCE      = m$nombre_simce,
      Comuna_ensayo     = comuna_hint,
      Comuna_coincide   = comuna_ok
    )
  } else {
    cat(" -> NO ENCONTRADO - mejor coincidencia'", m$nombre_simce,"'(", m$similitud, "%) - revisar manualmente", '\n')
    
    tibble(
      rbd_identificado  = NA_integer_,
      Metodo            = "Sin número en el nombre",
      Estado            = paste0("NO ENCONTRADO - mejor coincidencia '", m$nombre_simce,
                                 "' (", m$similitud, "%) - revisar manualmente"),
      Similitud_nombre  = m$similitud,
      Nombre_SIMCE      = NA_character_,
      Comuna_ensayo     = comuna_hint,
      Comuna_coincide   = comuna_ok
    )
  }
}

resultado <- ensayo_santillana %>%
  rowwise() %>%
  mutate(procesar_colegio(colegio, comuna_norm)) %>%
  ungroup()

resultados_clasificados <- resultado %>%
  mutate(
    rbd_revisado = case_when(
      rbd_santillana == rbd_identificado ~ rbd_santillana,
      Estado == 'OK (verificado en listado simce)' ~ rbd_identificado,
      Estado == 'OK (verificado en listado simce, comuna coincide)' ~ rbd_identificado,
      is.na(rbd_santillana) & !is.na(rbd_identificado)  ~ rbd_identificado,
      rbd_santillana != rbd_identificado & !is.na(rbd_santillana) & !is.na(rbd_identificado) ~ rbd_santillana,
      is.na(rbd_santillana) & is.na(rbd_identificado) ~ NA_integer_,
      !is.na(rbd_santillana) & is.na(rbd_identificado) ~ rbd_santillana),
    clasificacion = case_when(
      rbd_santillana == rbd_identificado ~ 'OK',
      Estado == 'OK (verificado en listado simce)' ~ 'OK',
      Estado == 'OK (verificado en listado simce, comuna coincide)' ~ 'OK',
      is.na(rbd_santillana) & !is.na(rbd_identificado) & Similitud_nombre > 91 ~ 'OK',
      is.na(rbd_santillana) & !is.na(rbd_identificado)  ~ 'Revisar',
      rbd_santillana != rbd_identificado & !is.na(rbd_santillana) & !is.na(rbd_identificado) ~ 'Revisar',
      is.na(rbd_santillana) & is.na(rbd_identificado) ~ 'Sin rbd_identificado',
      !is.na(rbd_santillana) & is.na(rbd_identificado) ~ 'Revisar')
  )

# Resumen y comparacion con rbd de santillana:
resultados_clasificados %>%
  count(clasificacion)

# -----------------------------------------------------------------------------
# 7. RESUMEN Y EXPORTACIÓN
# -----------------------------------------------------------------------------
cat("Total colegios:", nrow(resultado), "\n")
cat("rbd_identificado extraído directo del nombre:",
    sum(resultado$Metodo == "Extraído del nombre"), "\n")
cat("rbd_identificado-DV validado en nombre:",
    sum(resultado$Metodo == "rbd_identificado-DV en nombre"), "\n")
cat("Casos ambiguos resueltos por comuna:",
    sum(resultado$Metodo == "Extraído del nombre (ambiguo, resuelto por comuna)"), "\n")
cat("Casos ambiguos sin resolver (2+ números):",
    sum(resultado$Metodo == "Extraído del nombre (ambiguo)"), "\n")
cat("Resueltos por coincidencia de nombre (>=", UMBRAL_ACEPTACION, "%):",
    sum(resultado$Metodo == "Coincidencia por nombre (listado simce)"), "\n")
cat("Sin resolver (requieren revisión manual):",
    sum(resultado$Metodo == "Sin número en el nombre"), "\n")

write_xlsx(resultados_clasificados, archivo_salida)

# resultados_clasificados |>
#   filter(is.na(rbd_santillana)) |>
#   arrange(clasificacion) |>
#   select(colegio, Nombre_SIMCE, Estado, Similitud_nombre, clasificacion) |>
#   view()
