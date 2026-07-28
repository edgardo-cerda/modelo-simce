# =============================================================
# 02_modelo_escolar.R  (v3)
# -------------------------------------------------------------
# Modelo del NIVEL (promedio) del colegio. Sin cambios de diseño
# respecto a la v2: regresión lineal múltiple, una por combinación
# grado x área, validada out-of-time contra el año más reciente.
#
#   promedio_simce ~ mean_logro + pred_final_logro + slope_logro +
#                     n_evals_prom + colegio_efecto_historico
#
# Lo único que cambia es el rol que cumple dentro del pipeline:
# ahora este modelo entrega la MEDIA de la distribución que se le
# va a asignar a cada colegio, y 02b_modelo_dispersion.R entrega
# su ANCHO. Las predicciones individuales de 03 se construyen
# sobre ambos (media + dispersión), en vez de aplicarle los
# coeficientes de colegio a cada estudiante como en la v2.
#
# Por eso importa que este script quede tal cual está: es el
# ancla de todo lo demás, y es el único componente validado
# contra verdad observada directa (el SIMCE promedio publicado).
# =============================================================

library(tidyverse)
library(broom)

# ---- 0. Configuración --------------------------------------------
usuario <- Sys.info()[["user"]]
rutas <- config::get(config = usuario, file = "config.yml")
ruta_outputs <- rutas$ruta_outputs
dir_salidas <- ruta_outputs %>% file.path('modelo_lme_alu')

school_model_data <- dir_salidas %>% file.path('school_model_data.rds') %>% readRDS()

anios     <- sort(unique(school_model_data$agno))
anio_test <- max(anios)
cat("Años disponibles para entrenar/validar:", paste(anios, collapse = ", "), "\n")
cat("Año usado como prueba (out-of-time):", anio_test, "\n\n")

# `colegio_efecto_historico` ahora viene con prior contextual desde 01:
# es la suma de la expectativa según GSE / dependencia / ruralidad y el
# desvío propio del colegio. La fórmula no cambia, pero la variable
# significa algo mejor, sobre todo en colegios con poca o nula historia.
#
# Se puede además incluir `contexto_nivel` como término aparte, para que
# el modelo le dé a la expectativa contextual un peso distinto que al
# desvío del colegio. Al probarlo out-of-time no aportó (MAE entre -0.2
# y +0.3 puntos, peor en un grupo): con ~150-220 filas por grupo es un
# grado de libertad que no se paga. Queda en FALSE por defecto y el
# script imprime igual la comparación para que se pueda auditar en cada
# corrida sobre los datos del momento.
INCLUIR_CONTEXTO_APARTE <- FALSE

formula_base <- promedio_simce ~ mean_logro + pred_final_logro + slope_logro +
  n_evals_prom + colegio_efecto_historico
formula_ctx  <- update(formula_base, . ~ . + contexto_nivel)

formula_modelo <- if (INCLUIR_CONTEXTO_APARTE) formula_ctx else formula_base

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

  # Auditoría: el mismo modelo con el contexto como término separado.
  # No se usa para elegir (eso sería seleccionar sobre el año de prueba),
  # se imprime para poder revisar la decisión con datos frescos.
  mae_alternativa <- tryCatch({
    alt <- lm(if (INCLUIR_CONTEXTO_APARTE) formula_base else formula_ctx, data = train)
    mean(abs(predict(alt, newdata = test) - obs))
  }, error = function(e) NA_real_)

  mae_modelo   <- mean(abs(pred - obs))
  rmse_modelo  <- sqrt(mean((pred - obs)^2))
  mae_baseline <- mean(abs(mean(train$promedio_simce) - obs))
  r2_test      <- 1 - sum((obs - pred)^2) / sum((obs - mean(obs))^2)

  # ¿Cómo le va a los colegios con poca o nula historia propia? Es donde
  # el prior contextual debería notarse.
  sin_hist <- test$n_anios_hist <= 1
  mae_sin_historia <- if (any(sin_hist)) mean(abs(pred[sin_hist] - obs[sin_hist])) else NA_real_
  mae_con_historia <- if (any(!sin_hist)) mean(abs(pred[!sin_hist] - obs[!sin_hist])) else NA_real_

  resultados[[clave]] <- tibble(
    grado = g, area = a,
    n_train = nrow(train), n_test = nrow(test),
    mae = mae_modelo, rmse = rmse_modelo, mae_baseline = mae_baseline,
    mejora_vs_baseline_pct = 100 * (mae_baseline - mae_modelo) / mae_baseline,
    r2_test = r2_test,
    mae_alternativa_contexto = mae_alternativa,
    n_test_sin_historia = sum(sin_hist),
    mae_sin_historia = mae_sin_historia,
    mae_con_historia = mae_con_historia
  )

  cat("---", clave, "---\n")
  print(tidy(modelo))
  cat(sprintf(
    "MAE modelo: %.1f | MAE baseline (promedio histórico): %.1f | Mejora: %.0f%% | R2 (test %s): %.2f\n",
    mae_modelo, mae_baseline, resultados[[clave]]$mejora_vs_baseline_pct, anio_test, r2_test
  ))
  cat(sprintf(
    "  MAE con contexto como término aparte: %.1f (no se usa; sólo auditoría)\n",
    mae_alternativa
  ))
  cat(sprintf(
    "  MAE en colegios con <=1 año de historia (n=%d): %.1f | con historia: %.1f\n\n",
    sum(sin_hist), mae_sin_historia, mae_con_historia
  ))
}

tabla_resultados <- bind_rows(resultados)
cat("Resumen de validación (todos los grupos):\n")
print(tabla_resultados)

# ---- Gráfico de diagnóstico: observado vs. predicho en el año de prueba ----
# Se guardan también el rbd y el contexto de cada colegio: 05 los usa
# para los tooltips del gráfico interactivo de la presentación (poder
# pasar el mouse sobre un punto y ver de qué colegio se trata es lo que
# hace útil ese gráfico en una reunión).
diag_plot_data <- map_dfr(names(modelos), function(clave) {
  partes <- str_split(clave, "_", n = 2)[[1]]
  g <- partes[1]; a <- partes[2]
  datos_grupo <- school_model_data %>% filter(grado == g, area == a, agno == anio_test)
  tibble(
    grado = g, area = a,
    rbd_revisado  = datos_grupo$rbd_revisado,
    gse_etiqueta  = datos_grupo$gse_etiqueta,
    depe2_etiqueta = datos_grupo$depe2_etiqueta,
    n_anios_hist  = datos_grupo$n_anios_hist,
    n_estudiantes = datos_grupo$n_estudiantes,
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
print(
  diag_plot_data %>%
    mutate(diferencia = predicho - observado) %>%
    group_by(grado, area) %>%
    summarise(dif_media = mean(diferencia),
              dif_abs_media = mean(abs(diferencia)), .groups = "drop")
)

# ---- Guardar modelos y métricas ----------------------------------------
saveRDS(modelos, dir_salidas %>% file.path("modelos_escolares.rds"))
saveRDS(diag_plot_data, dir_salidas %>% file.path("diag_nivel.rds"))  # lo usa 05
saveRDS(anio_test, dir_salidas %>% file.path("anio_test.rds"))  # lo reusa 04
write_csv(tabla_resultados, dir_salidas %>% file.path("metricas_validacion.csv"))

cat("\nListo. Modelos de NIVEL guardados en output/modelo_lme/modelos_escolares.rds\n")
cat("Siguiente paso: 02b_modelo_dispersion.R (modelo del ANCHO de la distribución)\n")
