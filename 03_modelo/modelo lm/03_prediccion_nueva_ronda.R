# =============================================================
# 03_prediccion_nueva_ronda.R
# -------------------------------------------------------------
# Aplica los modelos ajustados en 02_modelo_escolar.R para
# predecir SIMCE en la "próxima ronda":
#
#   a) A nivel de colegio: con las features agregadas del colegio.
#   b) A nivel de estudiante: con el MISMO modelo (mismos
#      coeficientes), reemplazando las features agregadas del
#      colegio por las features propias de cada estudiante
#      (mean_logro, last_logro, slope_logro y su propio n_evals).
#
# Supuesto clave de la predicción individual: como el modelo es
# lineal en variables que significan lo mismo a ambos niveles
# (promedio de logro, último logro, pendiente), se asume que la
# relación ensayo -> SIMCE es aproximadamente la misma dentro de
# un colegio que entre colegios. Es un punto de partida razonable
# mientras no exista un SIMCE individual real para calibrar
# directamente un modelo a nivel de estudiante (el SIMCE oficial
# sólo se reporta agregado por colegio). Ver README.md, sección
# "Limitaciones y siguientes pasos".
#
# "Próxima ronda" = por defecto, el año más reciente presente en
# los ensayos (hoy 2025, que además ya tiene SIMCE real para
# comparar -> sirve como demo/validación adicional). Cuando
# lleguen los ensayos de un año nuevo (2026), basta con:
#   1) agregar/actualizar data/ensayos_santiyana.csv
#   2) volver a correr 01_preparar_datos.R
#   3) volver a correr este script
# y se obtendrán predicciones genuinas "hacia adelante", incluso
# de forma progresiva a medida que los colegios van rindiendo los
# ensayos 1, 2, 3... durante el año (no hace falta esperar los 6).
# =============================================================

library(tidyverse)

rutas <- config::get(file = "config.yml")
ruta_outputs <- rutas$ruta_outputs
dir_salidas <- ruta_outputs %>% file.path('modelo_lm')

ind_features    <- readRDS(file.path(dir_salidas, "ind_features.rds"))
school_features <- readRDS(file.path(dir_salidas, "school_features.rds"))
modelos         <- readRDS(file.path(dir_salidas, "modelos_escolares.rds"))

anio_prediccion <- max(school_features$agno)
cat("Prediciendo ronda:", anio_prediccion, "\n\n")

predecir_grupo <- function(datos, clave) {
  if (!clave %in% names(modelos)) {
    datos$pred_simce <- NA_real_
    return(datos)
  }
  datos$pred_simce <- predict(modelos[[clave]], newdata = datos)
  datos
}

# ---- a) Predicción a nivel de colegio ------------------------------------
pred_colegio <- school_features %>%
  filter(agno == anio_prediccion) %>%
  group_split(grado, area) %>%
  map_dfr(~ predecir_grupo(.x, paste(.x$grado[1], .x$area[1], sep = "_")))

write_csv(
  pred_colegio %>%
    select(agno, grado, area, rbd_revisado, n_estudiantes, mean_logro, last_logro, pred_simce),
  file.path(dir_salidas, "predicciones_colegio.csv"))
)

# ---- b) Predicción a nivel de estudiante ---------------------------------
ind_para_predecir <- ind_features %>%
  filter(agno == anio_prediccion) %>%
  rename(n_evals_prom = n_evals)   # mismo nombre de columna que usa el modelo

pred_individual <- ind_para_predecir %>%
  group_split(grado, area) %>%
  map_dfr(~ predecir_grupo(.x, paste(.x$grado[1], .x$area[1], sep = "_")))

write_csv(
  pred_individual %>%
    select(agno, grado, area, rbd_revisado, id_usuario_curso, mean_logro, last_logro, pred_simce),
  file.path(dir_salidas, "predicciones_individual.csv")
)

# ---- Chequeo de coherencia entre ambos niveles ---------------------------
# El promedio de las predicciones individuales de un colegio debería
# acercarse a la predicción hecha directamente a nivel de colegio (no son
# idénticas porque el colegio no es exactamente el promedio simple de sus
# estudiantes en todas las variables, p.ej. n_evals_prom).
chequeo <- pred_individual %>%
  group_by(agno, grado, area, rbd_revisado) %>%
  summarise(pred_individual_prom = mean(pred_simce, na.rm = TRUE), .groups = "drop") %>%
  inner_join(
    pred_colegio %>% select(agno, grado, area, rbd_revisado, pred_colegio = pred_simce),
    by = c("agno", "grado", "area", "rbd_revisado")
  ) %>%
  mutate(diferencia = pred_individual_prom - pred_colegio)

cat("Diferencia entre predicción de colegio y promedio de predicciones individuales:\n")
print(chequeo %>% summarise(
  diferencia_media     = mean(diferencia, na.rm = TRUE),
  diferencia_abs_media = mean(abs(diferencia), na.rm = TRUE)
))

write_csv(chequeo, file.path(dir_salidas, "chequeo_coherencia.csv"))

cat("\nListo. Archivos generados en", dir_salidas, ":\n",
    " - predicciones_colegio.csv\n",
    " - predicciones_individual.csv\n",
    " - chequeo_coherencia.csv\n")
