# =============================================================
# instalar_dependencias.R
# -------------------------------------------------------------
# Se corre UNA SOLA VEZ, la primera, en un computador nuevo.
#
# En Windows apunta al repositorio de Posit, que sirve los paquetes ya
# compilados. Sin eso, R intenta compilar `arrow`, `mirt` y `lme4` desde
# el código fuente, que necesita herramientas de desarrollo instaladas y
# suele fallar.
#
# Para correrlo: abra este archivo en RStudio y presione "Source", o
# desde una consola:
#     Rscript instalar_dependencias.R
# =============================================================

PAQUETES <- c("tidyverse", "arrow", "readxl", "janitor", "mirt", "lme4",
              "config", "broom", "yaml")

if (.Platform$OS.type == "windows") {
  options(repos = c(CRAN = "https://packagemanager.posit.co/cran/latest"))
  cat("Usando el repositorio de Posit (paquetes precompilados para Windows).\n\n")
} else {
  options(repos = c(CRAN = "https://cloud.r-project.org"))
}

instalados <- rownames(installed.packages())
faltan <- setdiff(PAQUETES, instalados)

cat("Version de R:", R.version.string, "\n")
cat("Biblioteca:  ", .libPaths()[1], "\n\n")

if (length(faltan) == 0) {
  cat("Ya estan todos los paquetes instalados. No hay nada que hacer.\n")
} else {
  cat("Faltan", length(faltan), "paquete(s):", paste(faltan, collapse = ", "), "\n")
  cat("Instalando. Esto puede tomar varios minutos la primera vez.\n\n")
  install.packages(faltan)
}

# Comprobacion final: que cada uno se pueda cargar de verdad, no solo que
# figure instalado.
cat("\n-------------------------------------------------------------\n")
cat("COMPROBACION\n")
cat("-------------------------------------------------------------\n")

problemas <- character(0)
for (p in PAQUETES) {
  ok <- suppressWarnings(suppressMessages(
    requireNamespace(p, quietly = TRUE)
  ))
  cat(sprintf("  %-12s %s\n", p, if (ok) "ok" else "NO SE PUDO CARGAR"))
  if (!ok) problemas <- c(problemas, p)
}

cat("\n")
if (length(problemas) == 0) {
  cat("Todo listo. Ya puede usar PREDECIR.bat\n")
} else {
  cat("Quedaron paquetes sin instalar:", paste(problemas, collapse = ", "), "\n")
  cat("Copie el mensaje de error de mas arriba y pidalo revisar.\n")
}
