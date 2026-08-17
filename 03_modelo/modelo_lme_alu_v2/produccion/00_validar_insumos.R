# =============================================================
# 00_validar_insumos.R
# -------------------------------------------------------------
# Chequeo previo. Corre ANTES de calcular nada y revisa que estén todas
# las condiciones para que el pipeline llegue hasta el final.
#
# Existe porque la mayoría de las fallas de una corrida no son del modelo
# sino de los insumos: un archivo mal nombrado, un Excel sin la hoja que
# corresponde, un archivo de OneDrive que figura pero no está descargado.
# Descubrir eso a los veinte minutos de cálculo es caro y el mensaje de
# error nativo no explica nada.
#
# Se puede correr solo (doble clic en VALIDAR.bat o Rscript sobre este
# archivo) o lo llama el driver antes de empezar.
# =============================================================

# La carpeta de este script. El driver la define antes de cargarlo; si se
# corre solo, se deduce del argumento --file de Rscript.
if (!exists("DIR_PRODUCCION")) {
  .args <- commandArgs(trailingOnly = FALSE)
  .f <- sub("^--file=", "", .args[grepl("^--file=", .args)])
  DIR_PRODUCCION <- if (length(.f)) dirname(normalizePath(.f)) else getwd()
}
dir_produccion <- DIR_PRODUCCION

source(file.path(dir_produccion, "comun.R"))

PAQUETES <- c("tidyverse", "arrow", "readxl", "janitor", "mirt", "lme4",
              "config", "broom")

cat("=============================================================\n")
cat("CHEQUEO DE INSUMOS\n")
cat("=============================================================\n\n")

registro <- nuevo_registro()
info <- list()

# ---- 1. Paquetes de R ------------------------------------------------
instalados <- rownames(installed.packages())
faltan_pq <- setdiff(PAQUETES, instalados)
registro <- if (length(faltan_pq) == 0) {
  anotar(registro, "OK", "Paquetes de R instalados")
} else {
  anotar(registro, "ERROR", "Faltan paquetes de R",
         paste0("Falta: ", paste(faltan_pq, collapse = ", "),
                "\nCorra una vez instalar_dependencias.R"))
}

# ---- 2. rutas.txt y carpetas ----------------------------------------
rutas <- tryCatch(leer_rutas(file.path(dir_produccion, "rutas.txt")),
                  error = function(e) e)

if (inherits(rutas, "error")) {
  registro <- anotar(registro, "ERROR", "rutas.txt no se puede leer",
                     conditionMessage(rutas))
  rutas <- NULL
} else {
  esperadas <- c("data_in", "data_intermedia", "data_out")
  faltan_r <- setdiff(esperadas, names(rutas))
  if (length(faltan_r)) {
    registro <- anotar(registro, "ERROR", "rutas.txt incompleto",
                       paste("Falta definir:", paste(faltan_r, collapse = ", ")))
  } else {
    for (r in esperadas) {
      registro <- if (dir.exists(rutas[[r]])) {
        anotar(registro, "OK", paste0("Carpeta ", r), rutas[[r]])
      } else {
        anotar(registro, "ERROR", paste0("La carpeta ", r, " no existe"),
               paste0(rutas[[r]], "\nRevise la ruta en rutas.txt."))
      }
    }
  }
}

# Sin rutas no se puede seguir chequeando nada más.
if (is.null(rutas) || !all(c("data_in", "data_intermedia", "data_out") %in% names(rutas))) {
  imprimir_registro(registro)
  cat("\nNO SE PUEDE CONTINUAR: arregle rutas.txt y vuelva a intentar.\n")
  validacion_ok <- FALSE
  if (!exists("EJECUTADO_POR_DRIVER")) quit(save = "no", status = 1)
}

dir_salidas <- file.path(rutas[["data_out"]], "modelo_lme_alu_v2")

# ---- 3. Archivos de ensayo -------------------------------------------
carpeta_ensayos <- file.path(rutas[["data_in"]], "ensayos_santillana")

if (!dir.exists(carpeta_ensayos)) {
  registro <- anotar(registro, "ERROR", "No existe la carpeta de ensayos",
                     paste0(carpeta_ensayos,
                            "\nDebe llamarse exactamente 'ensayos_santillana'",
                            " dentro de data_in."))
  archivos <- character(0)
} else {
  archivos <- list.files(carpeta_ensayos, pattern = "\\.xlsx$",
                         full.names = TRUE, recursive = TRUE)
  archivos <- archivos[grepl("Ensayo", basename(archivos))]
  registro <- if (length(archivos) > 0) {
    anotar(registro, "OK", paste("Archivos de ensayo encontrados:", length(archivos)))
  } else {
    anotar(registro, "ERROR", "No hay archivos de ensayo",
           paste0("Se buscaron .xlsx con 'Ensayo' en el nombre dentro de\n",
                  carpeta_ensayos))
  }
}

# ---- 4. Nombres de archivo -------------------------------------------
if (length(archivos) > 0) {
  meta <- parsear_nombre_ensayo(basename(archivos))
  malos <- meta[is.na(meta$agno) | is.na(meta$ensayo), , drop = FALSE]

  registro <- if (nrow(malos) == 0) {
    anotar(registro, "OK", "Todos los nombres de archivo se entienden")
  } else {
    anotar(registro, "ERROR",
           paste(nrow(malos), "archivo(s) con nombre que no se entiende"),
           paste0(paste(malos$archivo, collapse = "\n"),
                  "\nVer CONTRATO_DE_DATOS.md: el nombre debe traer el año",
                  " (20XX) y el número de ensayo (EnsayoN)."))
  }

  if (any(meta$ensayo_multidigito)) {
    registro <- anotar(registro, "ERROR", "Número de ensayo de dos dígitos",
                       paste0(paste(meta$archivo[meta$ensayo_multidigito],
                                    collapse = "\n"),
                              "\nEl pipeline lee un solo dígito: 'Ensayo10' se",
                              " leería como ensayo 1.\nHay que renombrar o",
                              " ajustar el patrón en 00_irt_calibracion.R."))
  }

  resumen_formas <- as.data.frame(
    table(meta$agno[!is.na(meta$agno)], meta$grado[!is.na(meta$agno)],
          meta$area[!is.na(meta$agno)])
  )
  names(resumen_formas) <- c("agno", "grado", "area", "n_formas")
  resumen_formas <- resumen_formas[resumen_formas$n_formas > 0, ]
  info$formas <- resumen_formas
  info$anio_objetivo <- suppressWarnings(max(meta$agno, na.rm = TRUE))

  cat("Formas detectadas por año, grado y área:\n")
  print(resumen_formas, row.names = FALSE)
  cat("\nAño más reciente con ensayos:", info$anio_objetivo, "\n\n")
}

# ---- 5. Disponibilidad real de los archivos (OneDrive) ---------------
if (length(archivos) > 0) {
  cat("Verificando que los archivos estén descargados...\n")
  no_disponibles <- archivos[!vapply(archivos, archivo_disponible, logical(1))]
  registro <- if (length(no_disponibles) == 0) {
    anotar(registro, "OK", "Todos los archivos de ensayo están descargados")
  } else {
    anotar(registro, "ERROR",
           paste(length(no_disponibles), "archivo(s) no están descargados"),
           paste0(paste(basename(no_disponibles), collapse = "\n"),
                  "\nSon archivos de OneDrive que figuran pero no están en el",
                  " disco.\nAbra la carpeta, haga clic derecho y elija",
                  " 'Conservar siempre en este dispositivo'."))
  }
}

# ---- 6. Contenido de los Excel ---------------------------------------
# Mismo chequeo que hace 00_irt_calibracion.R por dentro, pero antes de
# empezar a calcular.
revisar_excel <- function(ruta) {
  m <- tryCatch(suppressMessages(readxl::read_excel(ruta, sheet = "Matriz", n_max = 5)),
                error = function(e) e)
  if (inherits(m, "error")) return("no tiene la hoja 'Matriz' o no se puede abrir")
  # Los encabezados vienen con tildes y espacios ("Ítem ID"); se normalizan
  # con la misma función que usa 00_irt_calibracion.R.
  nm <- janitor::make_clean_names(names(m))
  if (!any(nm == "item_id"))    return("la hoja 'Matriz' no tiene la columna 'Ítem ID'")
  if (!any(grepl("clave", nm))) return("la hoja 'Matriz' no tiene la columna 'Clave correcta(s)'")
  d <- tryCatch(readxl::read_excel(ruta, sheet = "Datos", n_max = 0),
                error = function(e) e)
  if (inherits(d, "error")) return("no tiene la hoja 'Datos'")
  nd <- janitor::make_clean_names(names(d))
  if (!any(grepl("^item_.*_id_\\d+$", nd))) {
    return("la hoja 'Datos' no trae columnas item_<n>_id_<id>")
  }
  if (!any(nd == "id_usuario_curso")) {
    return("la hoja 'Datos' no trae la columna id_usuario_curso")
  }
  ""
}

if (length(archivos) > 0 && length(faltan_pq) == 0) {
  cat("Revisando el contenido de", length(archivos), "archivos...\n")
  problemas <- vapply(archivos, revisar_excel, character(1))
  con_problema <- which(nzchar(problemas))
  registro <- if (length(con_problema) == 0) {
    anotar(registro, "OK", "Todos los Excel tienen las hojas y columnas esperadas")
  } else {
    anotar(registro, "ERROR",
           paste(length(con_problema), "Excel con problemas de formato"),
           paste0(basename(archivos[con_problema]), ": ",
                  problemas[con_problema], collapse = "\n"))
  }
}

# ---- 7. Archivos intermedios -----------------------------------------
intermedios <- c(
  file.path(rutas[["data_intermedia"]], "ensayo_santillana",
            "ensayos_santillana_corregido.parquet"),
  file.path(rutas[["data_intermedia"]], "simce",
            "resultados_simce_rbd_corregido.parquet")
)
falta_int <- intermedios[!vapply(intermedios, archivo_disponible, logical(1))]
registro <- if (length(falta_int) == 0) {
  anotar(registro, "OK", "Archivos intermedios disponibles")
} else {
  anotar(registro, "ERROR", "Faltan archivos intermedios",
         paste0(paste(falta_int, collapse = "\n"),
                "\nLos generan los scripts de 01_preparar_datos/.",
                "\nSi existen pero figuran como no disponibles, es OneDrive:",
                " descárguelos."))
}

# ---- 8. Salidas de 01a (insumos de SIMCE) ----------------------------
de_01a <- c("simce_dist.rds", "simce_colegio.rds", "contexto_colegio.rds",
            "nivel_historico.rds", "sd_historica.rds", "respaldo_historico.rds",
            "conf_simce.rds", "descriptivos_simce.rds", "forma_z.rds",
            "cortes_tercil.rds", "limites_simce.rds", "anios_horizonte.rds")
falta_01a <- de_01a[!file.exists(file.path(dir_salidas, de_01a))]

registro <- if (length(falta_01a) == 0) {
  anotar(registro, "OK", "Insumos de SIMCE presentes (salidas de 01a)")
} else {
  anotar(registro, "ERROR", "Faltan insumos de SIMCE",
         paste0("Falta: ", paste(falta_01a, collapse = ", "),
                "\nHay que correr una vez 01a_insumos_simce.R.",
                "\nSólo hace falta repetirlo cuando llega un SIMCE nuevo."))
}

# ---- 9. El horizonte cubre el año objetivo ---------------------------
if (length(falta_01a) == 0 && !is.null(info$anio_objetivo) &&
    is.finite(info$anio_objetivo)) {
  horizonte <- readRDS(file.path(dir_salidas, "anios_horizonte.rds"))
  registro <- if (info$anio_objetivo %in% horizonte) {
    anotar(registro, "OK",
           paste0("El año ", info$anio_objetivo, " está dentro del horizonte de 01a"),
           paste0("Horizonte precalculado: ", min(horizonte), " a ", max(horizonte)))
  } else {
    anotar(registro, "ERROR",
           paste0("El año ", info$anio_objetivo, " está fuera del horizonte de 01a"),
           paste0("01a precalculó ", min(horizonte), " a ", max(horizonte),
                  ".\nHay que volver a correr 01a_insumos_simce.R subiendo",
                  " HORIZONTE_ANIOS."))
  }
}

# ---- 10. Modelos entrenados ------------------------------------------
modelos_req <- c("modelos_escolares_produccion.rds",
                 "modelos_dispersion_produccion.rds",
                 "limites_dispersion_produccion.rds",
                 "anios_cerrados.rds")
falta_mod <- modelos_req[!file.exists(file.path(dir_salidas, modelos_req))]

registro <- if (length(falta_mod) == 0) {
  anotar(registro, "OK", "Modelos entrenados presentes")
} else {
  anotar(registro, "ERROR", "Faltan modelos entrenados",
         paste0("Falta: ", paste(falta_mod, collapse = ", "),
                "\nLos generan 02_modelo_escolar.R y 02b_modelo_dispersion.R.",
                "\nSólo hace falta repetirlo cuando llega un SIMCE nuevo."))
}

# ---- 11. ¿Hay SIMCE nuevo sin procesar? ------------------------------
# No bloquea la corrida: se puede predecir igual. Pero si llegó un SIMCE
# que no está en los modelos, conviene re-estimar antes.
if (length(falta_mod) == 0 && length(falta_int) == 0) {
  anios_cerrados <- readRDS(file.path(dir_salidas, "anios_cerrados.rds"))
  anios_simce <- tryCatch({
    d <- arrow::open_dataset(intermedios[2])
    sort(unique(as.numeric(dplyr::pull(dplyr::collect(dplyr::distinct(d, agno)), agno))))
  }, error = function(e) NULL)

  if (!is.null(anios_simce)) {
    # Sólo cuentan los años POSTERIORES al último que entrenó el modelo. Los
    # anteriores sin ensayo (2022, por ejemplo) no son filas de
    # entrenamiento posibles: se usan igual como historia del colegio.
    nuevos <- anios_simce[anios_simce > max(anios_cerrados)]
    if (length(nuevos) > 0) {
      registro <- anotar(registro, "AVISO", "Hay SIMCE que los modelos no vieron",
                         paste0("Años de SIMCE sin usar: ",
                                paste(nuevos, collapse = ", "),
                                "\nSe puede predecir igual, pero conviene correr",
                                " la ruta de re-estimación\n(01a, 01b, 02, 02b)",
                                " para aprovecharlos. Ver COMO_CORRER.md."))
    }
  }
}

# ---- Veredicto -------------------------------------------------------
cat("\n-------------------------------------------------------------\n")
cat("RESULTADO DEL CHEQUEO\n")
cat("-------------------------------------------------------------\n")
imprimir_registro(registro)

n_error <- sum(registro$estado == "ERROR")
n_aviso <- sum(registro$estado == "AVISO")
validacion_ok <- n_error == 0

cat("\n")
if (validacion_ok && n_aviso == 0) {
  cat("TODO EN ORDEN. Se puede continuar.\n")
} else if (validacion_ok) {
  cat("SE PUEDE CONTINUAR, con", n_aviso, "aviso(s) que conviene leer.\n")
} else {
  cat("NO SE PUEDE CONTINUAR:", n_error, "problema(s) que hay que arreglar.\n")
  cat("Cada uno dice arriba qué hacer.\n")
}
cat("-------------------------------------------------------------\n")

if (!exists("EJECUTADO_POR_DRIVER")) {
  quit(save = "no", status = if (validacion_ok) 0 else 1)
}
