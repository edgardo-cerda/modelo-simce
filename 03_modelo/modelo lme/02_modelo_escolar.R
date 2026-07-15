# =============================================================
# 02_modelo_escolar.R
# -------------------------------------------------------------
# Primer modelo: regresión lineal múltiple que estima el
# promedio SIMCE de un colegio a partir del desempeño agregado
# de sus estudiantes en los ensayos Santillana del mismo año,
# grado y área, más el efecto histórico del colegio:
#
#   promedio_simce ~ mean_logro + pred_final_logro + slope_logro +
#                     n_evals_prom + colegio_efecto_historico
#
# mean_logro / n_evals_prom: como en la v1 (promedio de logro y
#   cantidad de ensayos rendidos).
# pred_final_logro / slope_logro: sustituyen a "last_logro" y a la
#   pendiente calculada a mano de la v1. Vienen de un modelo de
#   crecimiento mixto (lme4) por estudiante, ajustado en
#   01_preparar_datos.R, que "encoge" (shrinkage) la trayectoria de
#   cada estudiante hacia el promedio del grupo según cuánta
#   evidencia real tenga.
# colegio_efecto_historico: efecto persistente del colegio estimado
#   con SIMCE de años ANTERIORES (incluye años sin ensayo, ej. 2022),
#   también calculado en 01_preparar_datos.R con ventana expansiva
#   para no filtrar información del futuro.
#
# Se ajusta un modelo separado por combinación (grado x área)
# porque la escala SIMCE y la dificultad de los ensayos difieren
# entre 4°básico/2°medio y lenguaje/matemática.
#
# ¿Por qué partir con regresión lineal y no un modelo más complejo
# (mixtos, random forest, boosting)?
#   - Es interpretable: cada coeficiente se explica fácilmente a
#     un equipo directivo o docente ("por cada punto adicional de
#     logro promedio en los ensayos, el SIMCE esperado sube X").
#   - Con ~100-220 colegios por grupo (grado x área) el riesgo de
#     sobreajuste de un modelo más flexible es alto; conviene
#     partir simple y comparar contra esta base en una 2a iteración.
#   - Es el modelo que se puede "transferir" de forma más directa
#     a nivel individual (ver 03_prediccion_nueva_ronda.R).
#
# Validación: se usa el año más reciente disponible como conjunto
# de prueba ("out-of-time") y los años anteriores como
# entrenamiento. Es el escenario real de uso: predecir un año que
# aún no se conoce, usando datos ya cerrados.
# =============================================================

library(tidyverse)
library(broom)

# ---- 0. Configuración --------------------------------------------
# Configurar rutas de archivos: ----
rutas <- config::get(file = "config.yml")
ruta_data_in <- rutas$ruta_data_in
ruta_data_intermedia <- rutas$ruta_data_intermedia

dir_salidas <- ruta_data_intermedia %>% file.path('modelo_lme')
dir_salidas %>% dir.create(showWarnings = FALSE)

school_model_data <- dir_salidas %>% file.path('school_model_data.rds') %>% readRDS()

anios     <- sort(unique(school_model_data$agno))
anio_test <- max(anios)
cat("Años disponibles para entrenar/validar:", paste(anios, collapse = ", "), "\n")
cat("Año usado como prueba (out-of-time):", anio_test, "\n\n")

formula_modelo <- promedio_simce ~ mean_logro + pred_final_logro + slope_logro +
  n_evals_prom + colegio_efecto_historico

grupos <- school_model_data %>% distinct(grado, area)

modelos    <- list()
resultados <- list()

for (i in seq_len(nrow(grupos))) {

  g <- grupos$grado[i]; a <- grupos$area[i]
  clave <- paste(g, a, sep = "_")

  datos_grupo <- school_model_data %>% filter(grado == g, area == a)

  train <- datos_grupo %>% filter(agno != anio_test)
  test  <- datos_grupo %>% filter(agno == anio_test)

  if (nrow(train) < 20 || nrow(test) < 10) {
    cat("Grupo", clave, ": muy pocos datos, se omite.\n\n")
    next
  }

  modelo <- lm(formula_modelo, data = train)
  modelos[[clave]] <- modelo

  pred <- predict(modelo, newdata = test)
  obs  <- test$promedio_simce

  mae_modelo   <- mean(abs(pred - obs))
  rmse_modelo  <- sqrt(mean((pred - obs)^2))
  # Base de comparación: "predecir siempre el promedio histórico del
  # grupo" (equivalente a no usar los ensayos en absoluto).
  mae_baseline <- mean(abs(mean(train$promedio_simce) - obs))
  r2_test      <- 1 - sum((obs - pred)^2) / sum((obs - mean(obs))^2)

  resultados[[clave]] <- tibble(
    grado = g, area = a,
    n_train = nrow(train), n_test = nrow(test),
    mae = mae_modelo, rmse = rmse_modelo, mae_baseline = mae_baseline,
    mejora_vs_baseline_pct = 100 * (mae_baseline - mae_modelo) / mae_baseline,
    r2_test = r2_test
  )

  cat("---", clave, "---\n")
  print(tidy(modelo))
  cat(sprintf(
    "MAE modelo: %.1f | MAE baseline (promedio histórico): %.1f | Mejora: %.0f%% | R2 (test %s): %.2f\n\n",
    mae_modelo, mae_baseline, resultados[[clave]]$mejora_vs_baseline_pct, anio_test, r2_test
  ))
}

tabla_resultados <- bind_rows(resultados)
cat("Resumen de validación (todos los grupos):\n")
print(tabla_resultados)

# ---- Gráfico de diagnóstico: observado vs. predicho en el año de prueba ----
diag_plot_data <- map_dfr(names(modelos), function(clave) {
  partes <- str_split(clave, "_", n = 2)[[1]]
  g <- partes[1]; a <- partes[2]
  datos_grupo <- school_model_data %>% filter(grado == g, area == a, agno == anio_test)
  tibble(
    grado = g, area = a,
    observado = datos_grupo$promedio_simce,
    predicho  = predict(modelos[[clave]], newdata = datos_grupo)
  )
})

p <- ggplot(diag_plot_data, aes(observado, predicho)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  facet_wrap(~ grado + area, scales = "free") +
  labs(
    title = paste("Colegios: SIMCE observado vs. predicho -", anio_test),
    x = "SIMCE observado", y = "SIMCE predicho"
  ) +
  theme_minimal()

ggsave(dir_salidas %>% file.path("diagnostico_observado_vs_predicho.png"), p, width = 8, height = 6)

# Diferencias promedio entre observado y predicho:
diag_plot_data %>% 
  mutate(diferencia = predicho - observado) %>%
  group_by(grado, area) %>% 
  summarise(dif_media = mean(diferencia),
            dif_abs_media = mean(abs(diferencia)))

diag_plot_data %>% 
  mutate(diferencia = predicho - observado) %>% 
  ggplot(aes(x = diferencia)) +
  facet_wrap(area ~ fct_rev(grado)) +
  geom_density()

# ---- Guardar modelos y métricas ----------------------------------------
saveRDS(modelos, dir_salidas %>% file.path("modelos_escolares.rds"))
write_csv(tabla_resultados, dir_salidas %>% file.path("metricas_validacion.csv"))

cat("\nListo. Modelos guardados en output/modelos_escolares.rds\n")
cat("Métricas en output/metricas_validacion.csv\n")
cat("Gráfico en output/diagnostico_observado_vs_predicho.png\n")
