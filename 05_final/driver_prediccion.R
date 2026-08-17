# =============================================================
# driver_prediccion.R
# -------------------------------------------------------------
# Corre la ruta de PREDICCIÓN de punta a punta:
#
#     chequeo de insumos -> 00 (IRT) -> 01b (insumos) -> 03 (predicción)
#                        -> informe de calidad -> carpeta de entrega
#
# No re-estima el modelo: usa los coeficientes ya ajustados. Si llegó un
# SIMCE nuevo hay que correr antes la ruta de re-estimación, que está
# explicada en COMO_CORRER.md.
#
# Normalmente no se ejecuta a mano: lo llama PREDECIR.bat.
# =============================================================

if (!exists("DIR_PRODUCCION")) {
  .args <- commandArgs(trailingOnly = FALSE)
  .f <- sub("^--file=", "", .args[grepl("^--file=", .args)])
  DIR_PRODUCCION <- if (length(.f)) dirname(normalizePath(.f)) else getwd()
}
dir_produccion <- DIR_PRODUCCION

source(file.path(dir_produccion, "comun.R"))

dir_raiz    <- raiz_proyecto(dir_produccion)
dir_modelo  <- file.path(dir_raiz, "03_modelo", "modelo_lme_alu_v2")
rscript     <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")

t_inicio <- Sys.time()

cat("=============================================================\n")
cat("PREDICCIÓN DE RESULTADOS SIMCE\n")
cat("=============================================================\n")
cat("Inicio:", format(t_inicio, "%Y-%m-%d %H:%M:%S"), "\n")
cat("Proyecto:", dir_raiz, "\n\n")

# ---- 1. Configuración -----------------------------------------------
# Los scripts del modelo leen sus rutas de config.yml (indexado por
# usuario de Windows) y lo hacen con ruta RELATIVA, así que hay que
# pararse en la raíz del proyecto. El driver sincroniza ese archivo desde
# rutas.txt para que nadie tenga que editar un YAML.
rutas <- leer_rutas(file.path(dir_produccion, "rutas.txt"))
usuario <- Sys.info()[["user"]]

estado_cfg <- sincronizar_config(file.path(dir_raiz, "config.yml"), usuario, rutas)
cat("config.yml para el usuario '", usuario, "': ", estado_cfg, "\n\n", sep = "")

setwd(dir_raiz)

dir_salidas <- file.path(rutas[["data_out"]], "modelo_lme_alu_v2")

# ---- 2. Chequeo de insumos ------------------------------------------
EJECUTADO_POR_DRIVER <- TRUE
source(file.path(dir_produccion, "00_validar_insumos.R"), local = FALSE)

if (!validacion_ok) {
  cat("\nSe detiene acá. No se calculó nada.\n")
  quit(save = "no", status = 1)
}

anio_objetivo <- info$anio_objetivo

# ---- 3. Cadena de cálculo -------------------------------------------
dir_entrega <- file.path(dir_salidas, "entregas", anio_objetivo)
dir.create(dir_entrega, recursive = TRUE, showWarnings = FALSE)
archivo_log <- file.path(dir_entrega, "log_corrida.txt")

pasos <- data.frame(
  script = c("00_irt_calibracion.R", "01b_insumos_ensayo.R",
             "03_prediccion_nueva_ronda.R"),
  titulo = c("Calibración IRT de los ensayos",
             "Construcción de insumos del ensayo",
             "Predicción por colegio y por estudiante"),
  stringsAsFactors = FALSE
)

cat("\n=============================================================\n")
cat("CÁLCULO — ", nrow(pasos), " pasos. Puede tomar varios minutos.\n", sep = "")
cat("No cierre esta ventana.\n")
cat("=============================================================\n")

tiempos <- numeric(nrow(pasos))
cat("", file = archivo_log)   # log en blanco

for (i in seq_len(nrow(pasos))) {

  cat(sprintf("\n[%d/%d] %s\n", i, nrow(pasos), pasos$titulo[i]))
  cat(sprintf("       (%s)\n", pasos$script[i]))
  t0 <- Sys.time()

  salida <- suppressWarnings(system2(
    rscript, args = shQuote(file.path(dir_modelo, pasos$script[i])),
    stdout = TRUE, stderr = TRUE
  ))
  codigo <- attr(salida, "status")
  tiempos[i] <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

  cat(paste0("\n########## ", pasos$script[i], " ##########\n"),
      file = archivo_log, append = TRUE)
  cat(salida, sep = "\n", file = archivo_log, append = TRUE)

  if (!is.null(codigo) && codigo != 0) {
    cat("\n-------------------------------------------------------------\n")
    cat("FALLÓ EL PASO", i, ":", pasos$script[i], "\n")
    cat("-------------------------------------------------------------\n")
    cat("Últimas líneas del error:\n\n")
    cat(tail(salida, 25), sep = "\n")
    cat("\n\nEl detalle completo quedó en:\n  ", archivo_log, "\n")
    quit(save = "no", status = 1)
  }

  cat("       listo en",
      if (tiempos[i] < 60) sprintf("%.0f s\n", tiempos[i])
      else sprintf("%.1f min\n", tiempos[i] / 60))
}

# ---- 4. Informe de calidad ------------------------------------------
cat("\n[informe] Generando el informe de calidad...\n")
INFORME_DIR_SALIDAS <- dir_salidas
INFORME_ANIO        <- anio_objetivo
INFORME_DESTINO     <- file.path(dir_entrega, "informe_calidad.html")
INFORME_TIEMPOS     <- setNames(tiempos, pasos$script)
source(file.path(dir_produccion, "99_informe_calidad.R"), local = FALSE)

# ---- 5. Carpeta de entrega ------------------------------------------
entregables <- c("predicciones_colegio.csv", "predicciones_individual.csv")
copiados <- file.copy(file.path(dir_salidas, entregables),
                      file.path(dir_entrega, entregables), overwrite = TRUE)

t_total <- as.numeric(difftime(Sys.time(), t_inicio, units = "mins"))

cat("\n=============================================================\n")
cat("LISTO. Tiempo total:", sprintf("%.1f minutos", t_total), "\n")
cat("=============================================================\n\n")
cat("La entrega quedó en:\n  ", dir_entrega, "\n\n")
cat("  informe_calidad.html        <- ABRA ESTO PRIMERO\n")
cat("  predicciones_colegio.csv     una fila por colegio\n")
cat("  predicciones_individual.csv  una fila por estudiante\n")
cat("  log_corrida.txt              detalle técnico de la corrida\n\n")

if (!all(copiados)) {
  cat("AVISO: no se pudieron copiar todos los archivos a la carpeta de\n")
  cat("entrega. Están igual en", dir_salidas, "\n\n")
}

# Abrir el informe en el navegador.
tryCatch(utils::browseURL(INFORME_DESTINO), error = function(e) {
  cat("No se pudo abrir el informe solo. Ábralo a mano desde la ruta de arriba.\n")
})
