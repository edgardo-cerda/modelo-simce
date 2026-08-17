# =============================================================
# 00_irt_calibracion.R
# -------------------------------------------------------------
# Calibración IRT concurrente de los ensayos Santillana.
#
# CORRE ANTES QUE 01. Lee los Excel crudos ítem por ítem y produce,
# para cada estudiante, una habilidad estimada (theta) que SÍ es
# comparable entre ensayos del mismo año, grado y área.
#
# -------------------------------------------------------------
# POR QUÉ HACE FALTA
# -------------------------------------------------------------
# Los ensayos de un mismo año no tienen la misma dificultad. En 4b
# matemática 2025 el logro medio por ensayo va 61.0, 62.2, 68.2, 60.1,
# 70.4, 72.8: entre el más difícil y el más fácil hay 12.6 puntos de
# porcentaje de logro. Como cada colegio aplica un subconjunto distinto
# de los seis ensayos, parte de lo que hoy parece "este colegio rinde
# más" es en realidad "este colegio aplicó los ensayos fáciles". Medido
# sobre 2025, la dificultad media de la batería que aplicó cada colegio
# explica entre 2.4% y 13.4% de la varianza de `mean_logro` ENTRE
# colegios, que es el predictor principal del modelo de 02.
#
# -------------------------------------------------------------
# POR QUÉ CONCURRENTE Y NO UN MODELO POR ENSAYO
# -------------------------------------------------------------
# Un 3PL ajustado por separado en cada ensayo —que es lo que hace el
# pipeline de `_targets.R`— NO sirve para este problema. Cada ajuste
# estandariza theta a media 0 y varianza 1 DENTRO de su propia forma, así
# que la diferencia de dificultad entre el ensayo 3 y el 5 queda
# absorbida en la normalización en vez de medida. Los `mean_dffclt` de
# `tab_resumen_irt_*.parquet` están cada uno en su propia escala y no son
# comparables entre sí.
#
# Para ponerlos en una escala común hace falta un enlace (linking). Se
# revisaron los `item_id` de las 66 pruebas y el resultado es:
#
#   - NO hay ítems ancla. El solapamiento de ítems entre ensayos del
#     mismo año, grado y área es exactamente CERO en las 12
#     combinaciones (141 pares). De 2.335 ítems distintos, 2.260
#     aparecen en una sola prueba. Tampoco comparten ítems las formas
#     "Extenso" y "Priorizado" de matemática 2023.
#   - SÍ hay personas comunes, y muchas. Cada par de ensayos comparte
#     entre 1.380 y 5.890 estudiantes en 2025 (mínimo 496 en 2023;
#     mínimo 248 en 2024 salvo el ensayo 3 de 4b matemática, que tiene
#     54 casos en total y queda enlazado por 41-50 estudiantes).
#
# Es decir: el diseño no es de ítems comunes sino de PERSONAS comunes
# (non-equivalent groups with common persons). Con ese diseño, una
# calibración CONCURRENTE —todos los ensayos del grupo en un solo
# modelo, con NA estructural en los ítems que cada estudiante no rindió—
# identifica parámetros de ítem y thetas en una escala única. Los
# estudiantes que rindieron varias formas hacen de puente.
#
# Este script verifica esa conectividad antes de calibrar y se detiene
# si algún grupo queda partido en componentes inconexas.
#
# -------------------------------------------------------------
# LO QUE ESTE SCRIPT *NO* RESUELVE
# -------------------------------------------------------------
# El enlace ENTRE AÑOS. No hay personas comunes entre años (los 4b de
# 2024 no son los 4b de 2025) y de ítems comunes sólo existe el Ensayo 1
# de lenguaje: 40 ítems repetidos en 2m los tres años (2023-2024-2025) y
# 35 en 4b entre 2023 y 2024. En matemática el solapamiento es CERO en
# los seis pares de años. Por lo tanto theta queda en una escala propia
# de cada año x grado x área: sirve para comparar estudiantes y colegios
# DENTRO de un año, no para comparar niveles entre años. La deriva de
# escala entre años que 02 marca como pendiente sigue abierta.
#
# SALVEDAD sobre ese cero, que conviene tener presente antes de concluir
# que las formas son enteramente nuevas cada año: puede ser un artefacto
# del identificador. Los `item_id` son códigos de banco (5.6M a 7.4M) y
# están fuertemente estratificados por año — 2023 ocupa 5.62-5.98M, 2024
# llega hasta 6.70M, 2025 hasta 7.39M — que es el patrón esperable si el
# ID se asigna al CARGAR el ítem y no a su contenido. Con ese esquema, un
# ítem reutilizado que se vuelve a subir entra como ítem nuevo y su ancla
# se pierde en el registro, no en el diseño.
#
# El caso del Ensayo 1 de lenguaje muestra que el sistema SÍ puede
# preservar el ID cuando se reutiliza la forma completa, así que las dos
# hipótesis siguen vivas y estos datos no las separan: haría falta el
# contenido del ítem o un identificador estable. La distinción importa
# porque cambia el costo de la solución — si hay solapamiento real
# oculto, enlazar los años es un problema de metadatos y no exige
# rediseñar los ensayos.
#
# -------------------------------------------------------------
# SALIDAS (en <ruta_outputs>/modelo_lme_alu_v2/)
#   - irt_theta.rds      : 1 fila por estudiante x año x grado x área.
#                          `theta` (EAP) y `se_theta`. Es el insumo que
#                          reemplaza a `porcentaje_logro` en 01.
#   - irt_items.rds      : parámetros a y b por ítem, con su forma.
#   - irt_formas.rds     : dificultad media de cada forma en escala
#                          theta, junto al logro medio observado. Es la
#                          tabla que muestra cuánto se corrige.
#   - irt_linking.rds    : personas comunes por par de formas y
#                          conectividad del diseño.
#   - irt_ajuste.csv     : resumen de la calibración por grupo.
#
# Nota: son 12 calibraciones sobre matrices de ~12.000 x 240; toma
# bastantes minutos.
# =============================================================

library(tidyverse)
library(readxl)
library(janitor)
library(mirt)

# ---- 0. Configuración --------------------------------------------
usuario <- Sys.info()[["user"]]
rutas <- config::get(config = usuario, file = "config.yml")
ruta_data_in <- rutas$ruta_data_in
ruta_outputs <- rutas$ruta_outputs

dir_salidas <- ruta_outputs %>% file.path('modelo_lme_alu_v2')
dir_salidas %>% dir.create(showWarnings = FALSE, recursive = TRUE)

# Parámetros -------------------------------------------------------

# "2PL" es el default a propósito. El 3PL agrega un parámetro de
# adivinación por ítem y con 240 ítems por grupo la estimación se vuelve
# inestable: el pipeline actual de `_targets.R` ya devuelve "Hessian
# matrix at convergence is not positive definite". Si se quiere 3PL,
# conviene fijar el parámetro c en un valor común (1/n_alternativas) en
# vez de estimarlo libre.
MODELO_IRT <- "2PL"          # "Rasch" | "2PL" | "3PL"

# Las respuestas vienen como A/B/C/D o "-" (omitida). En la fuente el
# porcentaje de logro cuenta la omisión como incorrecta, así que el
# default mantiene esa convención y theta queda comparable con el logro.
# "faltante" la trata como no respondida (se ignora en la verosimilitud),
# que es defendible si se cree que omitir es no llegar a la pregunta.
OMITIDAS_COMO <- "incorrecta"   # "incorrecta" | "faltante"

# Dos formas se consideran enlazadas si comparten al menos esta cantidad
# de estudiantes. Bajo este umbral el enlace existe pero es frágil.
MIN_PERSONAS_PAR <- 30

# Ítems con menos respuestas que esto se excluyen de la calibración: sus
# parámetros no serían estimables de forma útil y arrastran al resto.
MIN_RESP_ITEM <- 100

# Máximo de ciclos EM.
NCYCLES <- 2000

set.seed(20250813)

MARCA_OMITIDA <- "-"

# ---- 1. Inventario de archivos ------------------------------------
# El nombre del archivo trae toda la metadata: 4B_Ensayo1_2025_MAT.xlsx,
# IIM_Ensayo3BAS_2023_MAT.xlsx, etc.
carpeta_ensayos <- ruta_data_in %>% file.path("ensayos_santillana")

archivos <- carpeta_ensayos %>%
  list.files(pattern = "\\.xlsx$", full.names = TRUE, recursive = TRUE)
archivos <- archivos[str_detect(basename(archivos), "Ensayo")]

inventario <- tibble(ruta = archivos, arch = basename(archivos)) %>%
  mutate(
    agno   = as.integer(str_extract(arch, "20\\d\\d")),
    grado  = if_else(str_detect(arch, "^IIM"), "2m", "4b"),
    area   = if_else(str_detect(arch, "LEN"), "lenguaje", "matematica"),
    ensayo = as.integer(str_extract(arch, "(?<=Ensayo)\\d")),
    # 2023 matemática viene en dos formas con ítems distintos: "BAS"
    # (Priorizado/basal) y "EXT" (Extenso/bbcc). NO son dos versiones de
    # la misma prueba —comparten 0 ítems—, así que se tratan como formas
    # separadas. Sí comparten estudiantes (93 a 1.432 por par), que es lo
    # que permite calibrarlas juntas.
    forma_sufijo = case_when(str_detect(arch, "BAS") ~ "BAS",
                             str_detect(arch, "EXT") ~ "EXT",
                             TRUE ~ ""),
    forma = paste0("E", ensayo, forma_sufijo)
  )

stopifnot(
  "Hay archivos de ensayo sin año, grado, área o número reconocible" =
    !any(is.na(inventario$agno) | is.na(inventario$ensayo))
)

cat("Archivos de ensayo encontrados:", nrow(inventario), "\n")
print(inventario %>% count(agno, grado, area, name = "n_formas") %>%
        pivot_wider(names_from = agno, values_from = n_formas,
                    names_prefix = "a"))

# ---- 2. Lectura y puntuación de un archivo ------------------------
# Devuelve una fila por estudiante x ítem, con `correcto` en 0/1/NA.
# La llave de persona es `id_usuario_curso`, NUNCA el nombre: hay
# homónimos, y es justamente lo que rompe el pipeline IRT de `_targets.R`
# (el pivot_wider por nombre genera list-cols y el unlist posterior
# devuelve un vector más largo que la tabla).
# La clave se normaliza a las letras aceptadas, en mayúscula y sin
# separadores: "B, C" queda "BC". Un ítem con DOS alternativas correctas no
# es un error de la planilla: es la práctica estándar cuando, después de
# aplicar la prueba, se detecta que dos opciones eran defendibles y se
# aceptan ambas. En este corpus hay exactamente uno (ítem 31 de
# 4B_Ensayo6_2025_MAT, clave "B, C"). Se puntúa correcto si la respuesta
# está en el conjunto. Lo que sí es un error es una clave vacía.
normalizar_claves <- function(m) {
  m %>% clean_names() %>%
    transmute(item_id = as.numeric(item_id),
              item_no = as.integer(item_no),
              clave   = str_to_upper(str_remove_all(
                as.character(clave_correcta_s), "[^[:alpha:]]")))
}

# Devuelve un texto con el problema encontrado, o "" si el archivo sirve.
# Se usa como comprobación previa para no descubrir a los 20 minutos de
# lectura que una planilla estaba mal.
revisar_prueba <- function(ruta) {
  m <- tryCatch(read_excel(ruta, sheet = "Matriz"), error = function(e) e)
  if (inherits(m, "error")) return(paste("no se puede abrir:", conditionMessage(m)))
  cl <- tryCatch(normalizar_claves(m), error = function(e) e)
  if (inherits(cl, "error")) return("hoja Matriz sin las columnas esperadas")
  if (any(is.na(cl$item_id)) || anyDuplicated(cl$item_id) > 0) {
    return("hoja Matriz con item_id duplicado o ausente")
  }
  if (any(is.na(cl$clave) | cl$clave == "")) {
    return(paste("hoja Matriz con", sum(is.na(cl$clave) | cl$clave == ""),
                 "ítem(s) sin clave correcta"))
  }
  ""
}

leer_prueba <- function(ruta, reintentos = 2) {

  leer <- function() {
    claves <- read_excel(ruta, sheet = "Matriz") %>% normalizar_claves()

    if (any(is.na(claves$item_id)) || anyDuplicated(claves$item_id) > 0) {
      stop("hoja Matriz con item_id duplicado o ausente")
    }
    if (any(is.na(claves$clave) | claves$clave == "")) {
      stop("hoja Matriz con ítems sin clave correcta")
    }
    n_multi <- sum(nchar(claves$clave) > 1)
    if (n_multi > 0) {
      cat("    (", n_multi, "ítem(s) con más de una alternativa aceptada:",
          paste(claves$clave[nchar(claves$clave) > 1], collapse = ", "), ")\n")
    }

    # Se leen sólo las columnas necesarias: los ítems como texto y el id
    # como número. Leer las 76 columnas de cada archivo cuesta memoria sin
    # aportar nada.
    nombres <- read_excel(ruta, sheet = "Datos", n_max = 0) %>%
      clean_names() %>% names()
    cols_item <- grep("^item_.*_id_\\d+$", nombres, value = TRUE)

    if (length(cols_item) == 0) {
      stop("la hoja Datos no trae columnas con el patrón item_<n>_id_<id>")
    }

    tipos <- rep("skip", length(nombres))
    tipos[nombres %in% cols_item]     <- "text"
    tipos[nombres == "id_usuario_curso"] <- "numeric"
    tipos[nombres == "id_colegio"]       <- "numeric"

    datos <- read_excel(ruta, sheet = "Datos", col_types = tipos) %>%
      clean_names()

    largo <- datos %>%
      pivot_longer(all_of(cols_item), names_to = "col", values_to = "respuesta") %>%
      mutate(item_id = as.numeric(str_extract(col, "(?<=_id_)\\d+"))) %>%
      select(-col) %>%
      inner_join(claves %>% select(item_id, clave), by = "item_id") %>%
      mutate(
        respuesta = str_trim(toupper(as.character(respuesta))),
        omitida   = is.na(respuesta) | respuesta %in% c("", MARCA_OMITIDA),
        # `clave` puede traer más de una letra ("BC"): correcto si la
        # respuesta es cualquiera de ellas.
        correcto  = case_when(
          omitida & OMITIDAS_COMO == "faltante"        ~ NA_real_,
          omitida                                      ~ 0,
          str_detect(clave, fixed(coalesce(respuesta, ""))) ~ 1,
          TRUE                                         ~ 0
        )
      ) %>%
      select(id_usuario_curso, id_colegio, item_id, correcto)

    largo
  }

  for (i in seq_len(reintentos)) {
    res <- tryCatch(leer(), error = function(e) e)
    if (!inherits(res, "error")) return(res)
    # Los Excel viven en OneDrive y a veces quedan bloqueados a mitad de
    # sincronización; un reintento resuelve la mayoría de esos casos.
    Sys.sleep(2)
  }
  stop("No se pudo leer ", basename(ruta), ": ", conditionMessage(res))
}

# ---- 3. Conectividad del diseño -----------------------------------
# Dos formas quedan enlazadas si comparten estudiantes. El diseño sirve
# para calibración concurrente sólo si TODAS las formas del grupo quedan
# en una sola componente conexa; si no, las escalas de cada componente
# son independientes y no se pueden comparar entre sí.
componentes_conexas <- function(formas, pares_enlazados) {
  padre <- setNames(formas, formas)
  raiz <- function(x) { while (padre[[x]] != x) x <- padre[[x]]; x }
  for (k in seq_len(nrow(pares_enlazados))) {
    a <- raiz(pares_enlazados$f1[k]); b <- raiz(pares_enlazados$f2[k])
    # `<-` y no `<<-`: `padre` es local a esta función, y `<<-` se saltaría
    # el local para escribir en el global, dejando la unión sin efecto.
    if (a != b) padre[[a]] <- b
  }
  map_chr(formas, raiz)
}

# ---- 3b. Calibración incremental -----------------------------------
# Cada grupo (año x grado x área) se calibra por separado y es
# independiente de los demás, así que no tiene sentido rehacer los años
# viejos cada vez que llega una ronda nueva: son decenas de minutos para
# obtener exactamente el mismo resultado.
#
# QUÉ DISPARA UNA RECALIBRACIÓN. No basta con "ya está calculado": el caso
# habitual es que lleguen MÁS ensayos de un año que ya se calibró, y ahí
# reusar el resultado anterior daría una calibración obsoleta en silencio.
# Por eso se guarda una HUELLA de los archivos de entrada de cada grupo
# —nombre, tamaño y fecha de modificación— junto con los parámetros que
# afectan el ajuste. Si algo de eso cambió, el grupo se recalibra.
#
# PARA FORZAR UNA RECALIBRACIÓN COMPLETA: borrar irt_theta.rds. Al no
# encontrar la salida anterior, el script rehace todo.
ruta_theta   <- dir_salidas %>% file.path("irt_theta.rds")
ruta_items   <- dir_salidas %>% file.path("irt_items.rds")
ruta_huellas <- dir_salidas %>% file.path("irt_huellas.rds")

# Los parámetros entran en la huella: cambiar el tipo de modelo o cómo se
# tratan las omitidas invalida lo calculado aunque los Excel sean idénticos.
PARAMS_HUELLA <- paste(MODELO_IRT, OMITIDAS_COMO, MIN_RESP_ITEM,
                       MIN_PERSONAS_PAR, NCYCLES, sep = "/")

huella_grupo <- function(rutas_archivos) {
  rutas_archivos <- sort(rutas_archivos)
  inf <- file.info(rutas_archivos)
  paste(
    paste(basename(rutas_archivos), inf$size,
          format(inf$mtime, "%Y%m%d%H%M%S"),
          sep = ":", collapse = "|"),
    PARAMS_HUELLA, sep = "||"
  )
}

hay_previo <- all(file.exists(c(ruta_theta, ruta_items, ruta_huellas)))
huellas_previas <- if (hay_previo) readRDS(ruta_huellas) else list()

# Salidas anteriores, para conservar los grupos que no cambiaron.
leer_previo <- function(nombre) {
  r <- dir_salidas %>% file.path(nombre)
  if (hay_previo && file.exists(r)) readRDS(r) else NULL
}
previo <- list(
  theta   = leer_previo("irt_theta.rds"),
  items   = leer_previo("irt_items.rds"),
  formas  = leer_previo("irt_formas.rds"),
  linking = leer_previo("irt_linking.rds"),
  ajuste  = leer_previo("irt_ajuste_grupos.rds"),
  omitidos = leer_previo("irt_omitidos.rds")
)

if (hay_previo) {
  cat("\nSe encontró una calibración anterior:", length(huellas_previas),
      "grupo(s) registrado(s).\n")
  cat("Se recalibrarán sólo los grupos cuyos archivos o parámetros hayan cambiado.\n")
  cat("(Para rehacer todo, borre irt_theta.rds y vuelva a correr.)\n")
} else {
  cat("\nNo hay calibración anterior utilizable: se calibra todo desde cero.\n")
}

# ---- 4. Calibración por año x grado x área ------------------------
grupos <- inventario %>% distinct(agno, grado, area) %>% arrange(agno, grado, area)

theta_todo   <- list()
items_todo   <- list()
formas_todo  <- list()
linking_todo <- list()
ajuste_todo  <- list()
omitidos     <- list()

huellas_nuevas  <- list()   # se guarda al final, con lo reusado y lo nuevo
grupos_reusados <- character(0)

# Comprobación previa: ¿se pueden abrir y usar todos los archivos del grupo?
# Los Excel viven en OneDrive y a veces quedan tomados por Excel o por el
# propio sincronizador ("Permission denied"); y alguna planilla puede venir
# con la hoja Matriz mal formada. Conviene detectarlo ANTES de leer nada, por
# dos razones: una calibración concurrente a la que le falta una forma no es
# una calibración incompleta sino una MAL ESCALADA (la forma ausente deja de
# aportar su enlace), y una corrida de 12 grupos no debería perderse entera —
# ni descubrir el problema tras 20 minutos de lectura— por un archivo. Se
# omite el grupo completo, nunca una forma suelta.

for (i in seq_len(nrow(grupos))) {

  # Nombres largos a propósito: `coef()` de mirt devuelve un data.frame con
  # columnas llamadas `a`, `b`, `g` y `u` (discriminación, dificultad,
  # adivinación, asíntota superior). Si las variables del bucle se llamaran
  # `a` y `g`, el enmascaramiento de datos de dplyr haría ganar a esas
  # columnas dentro de mutate() y `grado` terminaría siendo el parámetro de
  # adivinación en vez del grado. Pasa en silencio.
  anio_i <- grupos$agno[i]; grado_i <- grupos$grado[i]; area_i <- grupos$area[i]
  clave_grupo <- paste(anio_i, grado_i, area_i, sep = "_")
  cat("\n===", clave_grupo, "===\n")

  files_grupo <- inventario %>%
    filter(agno == anio_i, grado == grado_i, area == area_i)

  # ¿Cambió algo desde la última corrida? Si no, se reusa lo calculado.
  huella_actual <- huella_grupo(files_grupo$ruta)
  huellas_nuevas[[clave_grupo]] <- huella_actual

  sin_cambios <- !is.null(huellas_previas[[clave_grupo]]) &&
                 identical(huellas_previas[[clave_grupo]], huella_actual) &&
                 !is.null(previo$theta) &&
                 any(previo$theta$agno == anio_i & previo$theta$grado == grado_i &
                     previo$theta$area == area_i)

  if (sin_cambios) {
    cat("  sin cambios en los", nrow(files_grupo),
        "archivo(s): se reusa la calibración anterior\n")
    grupos_reusados <- c(grupos_reusados, clave_grupo)
    next
  }

  problemas <- map_chr(files_grupo$ruta, revisar_prueba)
  if (any(problemas != "")) {
    malos <- paste0(files_grupo$arch[problemas != ""], " (",
                    problemas[problemas != ""], ")")
    warning("Grupo ", clave_grupo, ": ", sum(problemas != ""),
            " archivo(s) inutilizable(s): ", paste(malos, collapse = "; "),
            ". Se omite el grupo entero.")
    cat("  OMITIDO:", paste(malos, collapse = "; "), "\n")
    omitidos[[clave_grupo]] <- tibble(
      agno = anio_i, grado = grado_i, area = area_i,
      motivo = "archivo inutilizable",
      detalle = paste(malos, collapse = "; ")
    )
    next
  }

  respuestas <- map_dfr(seq_len(nrow(files_grupo)), function(j) {
    cat("  leyendo", files_grupo$arch[j], "...\n")
    leer_prueba(files_grupo$ruta[j]) %>% mutate(forma = files_grupo$forma[j])
  })

  # Un estudiante no debería responder dos veces el mismo ítem. Si pasa
  # (rindió la misma forma dos veces), se queda el promedio redondeado.
  dups <- sum(duplicated(respuestas[c("id_usuario_curso", "item_id")]))
  if (dups > 0) {
    cat("  ", dups, "respuestas duplicadas (mismo alumno y mismo ítem): se promedian\n")
    respuestas <- respuestas %>%
      group_by(id_usuario_curso, id_colegio, item_id, forma) %>%
      summarise(correcto = round(mean(correcto, na.rm = TRUE)), .groups = "drop") %>%
      mutate(correcto = if_else(is.nan(correcto), NA_real_, correcto))
  }

  # --- 4a. Diagnóstico de enlace entre formas ---
  pers_forma <- respuestas %>% distinct(forma, id_usuario_curso)
  formas <- sort(unique(pers_forma$forma))

  pares <- if (length(formas) < 2) tibble(f1 = character(), f2 = character(),
                                          n_comunes = integer()) else {
    cmb <- combn(formas, 2)
    map_dfr(seq_len(ncol(cmb)), function(k) {
      p1 <- pers_forma$id_usuario_curso[pers_forma$forma == cmb[1, k]]
      p2 <- pers_forma$id_usuario_curso[pers_forma$forma == cmb[2, k]]
      tibble(f1 = cmb[1, k], f2 = cmb[2, k], n_comunes = length(intersect(p1, p2)))
    })
  }

  comp <- componentes_conexas(formas, pares %>% filter(n_comunes >= MIN_PERSONAS_PAR))
  n_comp <- n_distinct(comp)

  cat("  formas:", length(formas), "| personas comunes por par: mín",
      if (nrow(pares)) min(pares$n_comunes) else NA,
      "/ mediana", if (nrow(pares)) median(pares$n_comunes) else NA,
      "| componentes conexas:", n_comp, "\n")

  linking_todo[[clave_grupo]] <- pares %>%
    mutate(agno = anio_i, grado = grado_i, area = area_i,
           enlazado = n_comunes >= MIN_PERSONAS_PAR)

  if (n_comp > 1) {
    # No se calibra: unir formas que no comparten estudiantes produciría
    # thetas en escalas arbitrarias entre sí, con apariencia de estar en
    # una sola. Es preferible detenerse y revisar.
    warning("Grupo ", clave_grupo, ": el diseño queda en ", n_comp,
            " componentes inconexas. Se omite la calibración concurrente.")
    omitidos[[clave_grupo]] <- tibble(
      agno = anio_i, grado = grado_i, area = area_i,
      motivo = "diseño inconexo",
      detalle = paste(n_comp, "componentes; formas:",
                      paste(formas, collapse = ", "))
    )
    next
  }

  # --- 4b. Matriz persona x ítem ---
  # NA = ítem no administrado a ese estudiante. mirt lo trata como
  # faltante en la verosimilitud, que es exactamente lo que corresponde
  # cuando la ausencia es por diseño.
  matriz <- respuestas %>%
    select(id_usuario_curso, item_id, correcto) %>%
    pivot_wider(names_from = item_id, values_from = correcto,
                names_prefix = "i") %>%
    arrange(id_usuario_curso)

  ids_persona <- matriz$id_usuario_curso
  resp_mat <- matriz %>% select(-id_usuario_curso) %>% as.data.frame()

  # Ítems inservibles para la calibración: pocos respondientes o sin
  # variación (todos lo aciertan o nadie lo acierta).
  n_resp  <- colSums(!is.na(resp_mat))
  p_corr  <- colMeans(resp_mat, na.rm = TRUE)
  malos   <- n_resp < MIN_RESP_ITEM | is.na(p_corr) | p_corr <= 0 | p_corr >= 1
  if (any(malos)) {
    cat("  ítems excluidos (pocas respuestas o sin variación):", sum(malos), "\n")
    resp_mat <- resp_mat[, !malos, drop = FALSE]
  }

  # Estudiantes sin ninguna respuesta utilizable.
  filas_ok <- rowSums(!is.na(resp_mat)) > 0
  resp_mat <- resp_mat[filas_ok, , drop = FALSE]
  ids_persona <- ids_persona[filas_ok]

  cat("  matriz:", nrow(resp_mat), "estudiantes x", ncol(resp_mat), "ítems |",
      sprintf("%.0f%% observada\n", 100 * mean(!is.na(resp_mat))))

  # --- 4c. Calibración concurrente ---
  t0 <- Sys.time()
  modelo <- mirt(resp_mat, model = 1, itemtype = MODELO_IRT, method = "EM",
                 technical = list(NCYCLES = NCYCLES), verbose = FALSE)
  segundos <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  cat(sprintf("  calibrado en %.0f s | converge: %s | ciclos: %d\n",
              segundos, extract.mirt(modelo, "converged"),
              extract.mirt(modelo, "iterations")))

  # --- 4d. Theta por estudiante (EAP) ---
  fs <- fscores(modelo, method = "EAP", full.scores = TRUE, full.scores.SE = TRUE)

  theta_grupo <- tibble(
    agno = anio_i, grado = grado_i, area = area_i,
    id_usuario_curso = ids_persona,
    theta    = as.numeric(fs[, 1]),
    se_theta = as.numeric(fs[, 2]),
    n_items_resp = rowSums(!is.na(resp_mat))
  ) %>%
    left_join(
      respuestas %>% group_by(id_usuario_curso) %>%
        summarise(k_formas = n_distinct(forma),
                  id_colegio = first(id_colegio), .groups = "drop"),
      by = "id_usuario_curso"
    )

  theta_todo[[clave_grupo]] <- theta_grupo

  # --- 4e. Parámetros de ítem ---
  pars <- coef(modelo, IRTpars = TRUE, simplify = TRUE)$items %>%
    as.data.frame() %>% rownames_to_column("col") %>%
    mutate(item_id = as.numeric(str_remove(col, "^i"))) %>%
    select(-col)

  items_grupo <- pars %>%
    left_join(respuestas %>% distinct(item_id, forma), by = "item_id") %>%
    left_join(
      respuestas %>% group_by(item_id) %>%
        summarise(n_resp = sum(!is.na(correcto)),
                  p_correcta = mean(correcto, na.rm = TRUE), .groups = "drop"),
      by = "item_id"
    ) %>%
    mutate(agno = anio_i, grado = grado_i, area = area_i)

  items_todo[[clave_grupo]] <- items_grupo

  # --- 4f. Dificultad de cada forma, ya en escala común ---
  # `b_medio` es la dificultad media de los ítems de la forma en la
  # escala theta del grupo. A diferencia de los `mean_dffclt` de
  # `tab_resumen_irt_*.parquet`, estos SÍ son comparables entre formas,
  # porque salen de una sola calibración.
  formas_todo[[clave_grupo]] <- items_grupo %>%
    group_by(agno, grado, area, forma) %>%
    summarise(n_items = n(), b_medio = mean(b, na.rm = TRUE),
              a_medio = mean(a, na.rm = TRUE),
              p_correcta_media = mean(p_correcta), .groups = "drop") %>%
    left_join(
      respuestas %>% group_by(forma) %>%
        summarise(logro_medio = 100 * mean(correcto, na.rm = TRUE),
                  n_estudiantes = n_distinct(id_usuario_curso), .groups = "drop"),
      by = "forma"
    ) %>%
    # theta medio de quienes rindieron cada forma: si difiere entre
    # formas, la comparación de logros crudos estaba además contaminada
    # por composición, no sólo por dificultad.
    left_join(
      respuestas %>% distinct(forma, id_usuario_curso) %>%
        inner_join(theta_grupo %>% select(id_usuario_curso, theta),
                   by = "id_usuario_curso") %>%
        group_by(forma) %>%
        summarise(theta_medio_rindieron = mean(theta), .groups = "drop"),
      by = "forma"
    )

  # Se extraen ANTES de armar el tibble: dentro de tibble() la columna
  # `tipo_modelo` se evalúa con enmascaramiento de datos, y si la columna
  # se llamara `modelo` taparía al objeto `modelo` en las expresiones
  # siguientes (extract.mirt recibiría el string "2PL").
  convergio <- extract.mirt(modelo, "converged")
  ciclos    <- extract.mirt(modelo, "iterations")

  ajuste_todo[[clave_grupo]] <- tibble(
    agno = anio_i, grado = grado_i, area = area_i,
    n_formas = length(formas), n_estudiantes = nrow(resp_mat),
    n_items = ncol(resp_mat), pct_observada = 100 * mean(!is.na(resp_mat)),
    min_personas_par = if (nrow(pares)) min(pares$n_comunes) else NA_integer_,
    componentes = n_comp,
    tipo_modelo = MODELO_IRT,
    convergio = convergio,
    ciclos = ciclos,
    segundos = segundos
  )

  rm(respuestas, matriz, resp_mat, modelo, fs); gc(verbose = FALSE)
}

# ---- 5. Consolidar -------------------------------------------------
# Lo recién calibrado, más lo que se conservó de la corrida anterior. Sólo
# se recuperan los grupos que siguen en el inventario: si se borraron los
# Excel de un año, sus resultados salen también.
recuperar <- function(prev) {
  if (is.null(prev) || length(grupos_reusados) == 0) return(NULL)
  prev %>% filter(paste(agno, grado, area, sep = "_") %in% grupos_reusados)
}

irt_theta    <- bind_rows(bind_rows(theta_todo),   recuperar(previo$theta)) %>%
  arrange(agno, grado, area, id_usuario_curso)
irt_items    <- bind_rows(bind_rows(items_todo),   recuperar(previo$items)) %>%
  arrange(agno, grado, area, item_id)
irt_formas   <- bind_rows(bind_rows(formas_todo),  recuperar(previo$formas)) %>%
  arrange(agno, grado, area, forma)
irt_linking  <- bind_rows(bind_rows(linking_todo), recuperar(previo$linking)) %>%
  arrange(agno, grado, area)
irt_ajuste   <- bind_rows(bind_rows(ajuste_todo),  recuperar(previo$ajuste)) %>%
  arrange(agno, grado, area)
irt_omitidos <- bind_rows(bind_rows(omitidos),     recuperar(previo$omitidos))

cat("\n\n=============================================================\n")
cat("RESUMEN DE LA CALIBRACIÓN\n")
cat("=============================================================\n")
cat("Grupos calibrados en esta corrida:", nrow(grupos) - length(grupos_reusados),
    "de", nrow(grupos), "\n")
if (length(grupos_reusados) > 0) {
  cat("Grupos reusados de la corrida anterior (sin cambios en sus archivos):\n  ",
      paste(grupos_reusados, collapse = ", "), "\n")
}
cat("\n")
print(irt_ajuste %>%
        mutate(across(where(is.numeric), ~round(.x, 1))) %>%
        as.data.frame())

if (nrow(irt_omitidos) > 0) {
  cat("\n*** GRUPOS SIN CALIBRAR (", nrow(irt_omitidos), "de",
      nrow(grupos), ") ***\n")
  print(as.data.frame(irt_omitidos))
  cat("\nEstos grupos NO tienen theta en irt_theta.rds. Si el motivo es un\n",
      "archivo no legible, casi siempre está abierto en Excel o tomado por\n",
      "OneDrive: cerrarlo y volver a correr sólo esos grupos.\n")
}

# ---- 6. Diagnósticos ------------------------------------------------
# (i) ¿Cuánto corrige? Dificultad de cada forma en escala theta contra el
#     logro crudo. Si el orden no coincide, es que el logro crudo estaba
#     mezclando dificultad con composición.
cat("\n\nDIFICULTAD DE CADA FORMA (escala theta común) vs LOGRO CRUDO\n")
cat("b_medio alto = forma más difícil. theta_medio_rindieron distinto de 0\n")
cat("indica que quienes la rindieron no eran un grupo promedio.\n\n")
print(
  irt_formas %>%
    filter(agno == max(agno)) %>%
    transmute(grado, area, forma, n_items,
              logro_medio = round(logro_medio, 1),
              b_medio = round(b_medio, 2),
              theta_rindieron = round(theta_medio_rindieron, 2)) %>%
    arrange(grado, area, forma) %>%
    as.data.frame()
)

# (ii) Precisión de theta. La confiabilidad marginal es la contraparte
#      directa de `conf_indice` de 01, así que se puede comparar cuánto
#      gana (o pierde) la medida individual al pasar por IRT.
comparacion <- irt_theta %>%
  group_by(agno, grado, area) %>%
  summarise(
    n = n(),
    theta_medio = mean(theta),
    theta_sd    = sd(theta),
    se_medio    = mean(se_theta),
    # confiabilidad marginal empírica: 1 - E[SE^2] / Var(theta observada)
    confiabilidad = 1 - mean(se_theta^2) / var(theta),
    .groups = "drop"
  )

cat("\n\nPRECISIÓN DE theta POR GRUPO\n")
cat("(confiabilidad marginal = 1 - E[SE^2]/Var(theta). Comparable con\n")
cat(" `conf_indice` de 01, que en la versión sin IRT va de 0.51 a 0.80.)\n\n")
print(comparacion %>% mutate(across(where(is.numeric), ~round(.x, 3))) %>%
        as.data.frame())

cat("\n\nPRECISIÓN SEGÚN CUÁNTAS FORMAS RINDIÓ EL ESTUDIANTE\n")
print(
  irt_theta %>% filter(agno == max(agno)) %>%
    group_by(grado, area, k_formas) %>%
    summarise(n = n(), se_medio = round(mean(se_theta), 3),
              .groups = "drop") %>%
    pivot_wider(names_from = k_formas, values_from = c(n, se_medio)) %>%
    as.data.frame()
)

# (iii) Enlace: el par más débil de cada grupo es el que sostiene la
#       escala común. Conviene mirarlo en cada corrida.
cat("\n\nENLACE MÁS DÉBIL DE CADA GRUPO (personas comunes)\n")
print(
  irt_linking %>% group_by(agno, grado, area) %>%
    slice_min(n_comunes, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    transmute(agno, grado, area, par = paste(f1, "-", f2), n_comunes) %>%
    arrange(n_comunes) %>%
    as.data.frame()
)

# ---- 7. Guardar -----------------------------------------------------
saveRDS(irt_theta,   dir_salidas %>% file.path("irt_theta.rds"))
saveRDS(irt_items,   dir_salidas %>% file.path("irt_items.rds"))
saveRDS(irt_formas,  dir_salidas %>% file.path("irt_formas.rds"))
saveRDS(irt_linking, dir_salidas %>% file.path("irt_linking.rds"))
# En RDS además del CSV: la calibración incremental lo lee de vuelta para
# conservar la fila de los grupos que no se recalculan.
saveRDS(irt_ajuste,  dir_salidas %>% file.path("irt_ajuste_grupos.rds"))
write_csv(irt_ajuste,  dir_salidas %>% file.path("irt_ajuste.csv"))
if (nrow(irt_omitidos) > 0) {
  saveRDS(irt_omitidos, dir_salidas %>% file.path("irt_omitidos.rds"))
  write_csv(irt_omitidos, dir_salidas %>% file.path("irt_omitidos.csv"))
} else if (file.exists(dir_salidas %>% file.path("irt_omitidos.rds"))) {
  # Ya no hay grupos omitidos: se borra el registro viejo para que no
  # quede un archivo que contradice a la corrida actual.
  file.remove(dir_salidas %>% file.path("irt_omitidos.rds"))
}
write_csv(irt_formas,  dir_salidas %>% file.path("irt_dificultad_formas.csv"))
write_csv(irt_linking, dir_salidas %>% file.path("irt_linking.csv"))

# Las huellas van al final, y sólo si todo lo anterior se guardó bien: si
# el script muriera antes, la próxima corrida recalibra en vez de dar por
# buena una salida a medio escribir.
saveRDS(huellas_nuevas, ruta_huellas)

cat("\n\nListo. Salidas en", dir_salidas, "\n")
cat("  - irt_theta.rds   : habilidad estimada por estudiante (insumo de 01)\n")
cat("  - irt_items.rds   : parámetros a y b por ítem\n")
cat("  - irt_formas.rds  : dificultad de cada forma en escala común\n")
cat("  - irt_linking.rds : personas comunes por par de formas\n")
cat("  - irt_huellas.rds : qué archivos generaron cada grupo (control de\n")
cat("                      la calibración incremental)\n")
cat("\nRECORDATORIO: theta es comparable DENTRO de cada año x grado x área,\n")
cat("no entre años. Ver la nota del encabezado sobre el enlace entre años.\n")
