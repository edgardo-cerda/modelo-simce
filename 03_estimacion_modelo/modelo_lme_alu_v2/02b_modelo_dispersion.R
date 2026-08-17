# =============================================================
# 02b_modelo_dispersion.R
# -------------------------------------------------------------
# Segundo modelo del colegio: en vez del PROMEDIO del SIMCE
# (02_modelo_escolar.R), predice el ANCHO de su distribución interna, es
# decir la desviación estándar de los puntajes de sus estudiantes:
#
#   sd_simce ~ sd_entre_estud + iqr_logro_ensayo + sd_hist_colegio +
#              mean_logro
#
# Cuatro términos lineales: un coeficiente por predictor, todos legibles.
#
# PARA QUÉ SIRVE. Sin esta pieza las predicciones individuales quedan
# apretadas en torno al promedio del colegio: ordenan bien a los
# estudiantes pero subestiman gravemente cuán distintos son entre sí. El
# archivo de alumnos permite modelar esa dispersión directamente.
#
# LOS PREDICTORES, en orden de importancia observada:
#
#   sd_hist_colegio    qué tan heterogéneo ha sido este colegio en años
#                      anteriores (ventana expansiva). El más informativo:
#                      la heterogeneidad de un colegio es bastante
#                      persistente. Incluye prior contextual de GSE,
#                      dependencia y ruralidad, que acá funciona más como
#                      valor por defecto que como señal (R² ~0.01 en el
#                      universo nacional) — ver 01, sección 6b.
#   iqr_logro_ensayo   cuán dispersos están los resultados de sus
#   sd_entre_estud     estudiantes en los ensayos de ESTE año. Es la señal
#                      fresca, la que detecta cambios que la historia no
#                      ve. El IQR resulta algo más robusto que la sd
#                      frente a colegios con outliers.
#   mean_logro         nivel del colegio. Entra porque la dispersión
#                      depende del nivel: los colegios muy arriba o muy
#                      abajo tienen menos recorrido (efecto piso/techo).
#
# OJO CON EL NOMBRE: `sd_hist_colegio` es el histórico de la DISPERSIÓN de
# este colegio; `nivel_hist_colegio`, que aparece en 02, es el de su
# NIVEL. Son cosas distintas y es fácil confundirlas.
#
# PONDERACIÓN. La sd muestral de un colegio de 20 alumnos es mucho más
# ruidosa que la de uno de 200 (error relativo ~1/sqrt(2(n-1))). Se pondera
# por número de alumnos para que los colegios chicos, que son casi ruido
# puro, no dominen el ajuste.
#
# DOS JUEGOS DE MODELOS, igual que en 02: los de validación excluyen el
# último año con SIMCE y producen las métricas; los de producción usan
# todos los años cerrados y sirven para predecir una ronda nueva. 03 elige
# el que no vio el año objetivo.
# =============================================================

library(tidyverse)
library(broom)

# ---- 0. Configuración --------------------------------------------
usuario <- Sys.info()[["user"]]
rutas <- config::get(config = usuario, file = "config.yml")
ruta_outputs <- rutas$ruta_outputs
dir_salidas <- ruta_outputs %>% file.path('modelo_lme_alu_v2')

school_model_data <- dir_salidas %>% file.path('school_model_data.rds') %>% readRDS()

# Sólo sirven los colegios con sd interna observada y estable.
datos_sd <- school_model_data %>%
  filter(!is.na(sd_simce), !is.na(sd_hist_colegio), !is.na(sd_entre_estud))

anio_test <- max(school_model_data$agno)
cat("Año usado como prueba (out-of-time):", anio_test, "\n")
cat("Colegios con sd interna observada:", nrow(datos_sd), "de", nrow(school_model_data), "\n\n")

formula_sd <- sd_simce ~ sd_entre_estud + iqr_logro_ensayo +
  sd_hist_colegio + mean_logro

cat("Fórmula:\n  ")
print(formula_sd)
cat("\n")

grupos <- datos_sd %>% distinct(grado, area)

modelos_sd  <- list()
limites_sd  <- list()
resultados  <- list()

for (i in seq_len(nrow(grupos))) {

  g <- grupos$grado[i]; a <- grupos$area[i]
  clave <- paste(g, a, sep = "_")

  datos_grupo <- datos_sd %>% filter(grado == g, area == a)
  train <- datos_grupo %>% filter(agno != anio_test)
  test  <- datos_grupo %>% filter(agno == anio_test)

  if (nrow(train) < 30 || nrow(test) < 10) {
    cat("Grupo", clave, ": muy pocos datos para el modelo de dispersión, se omite.\n",
        "  (03 usará la dispersión histórica del colegio como respaldo.)\n\n")
    next
  }

  modelo <- lm(formula_sd, data = train, weights = n_alu_simce)
  modelos_sd[[clave]] <- modelo

  # Rango plausible: una sd predicha nunca debería salirse mucho del
  # rango observado en entrenamiento (protege contra extrapolaciones
  # absurdas en colegios con features atípicas).
  lim <- quantile(train$sd_simce, c(0.02, 0.98), names = FALSE)
  limites_sd[[clave]] <- lim

  pred <- pmin(pmax(predict(modelo, newdata = test), lim[1]), lim[2])
  obs  <- test$sd_simce

  mae_modelo    <- mean(abs(pred - obs))
  mae_baseline  <- mean(abs(mean(train$sd_simce) - obs))          # "todos igual de dispersos"
  mae_historico <- mean(abs(test$sd_hist_colegio - obs))          # "igual que su propia historia"
  r2_test       <- 1 - sum((obs - pred)^2) / sum((obs - mean(obs))^2)

  resultados[[clave]] <- tibble(
    grado = g, area = a,
    n_train = nrow(train), n_test = nrow(test),
    sd_media_observada = mean(obs),
    sd_media_predicha  = mean(pred),
    mae = mae_modelo,
    mae_baseline_constante = mae_baseline,
    mae_baseline_historico = mae_historico,
    mejora_vs_constante_pct = 100 * (mae_baseline - mae_modelo) / mae_baseline,
    mejora_vs_historico_pct = 100 * (mae_historico - mae_modelo) / mae_historico,
    r2_test = r2_test
  )

  cat("---", clave, "---\n")
  print(tidy(modelo))
  cat(sprintf(
    "MAE: %.2f | baseline constante: %.2f | baseline histórico: %.2f | R2 (test %s): %.2f\n\n",
    mae_modelo, mae_baseline, mae_historico, anio_test, r2_test
  ))
}

tabla_sd <- bind_rows(resultados)
cat("Resumen de validación del modelo de dispersión:\n")
print(tabla_sd)

# ---- 2. Modelos de producción ----------------------------------------
# Mismo criterio que en 02: los de arriba dejan fuera el último año para
# poder medir; para predecir una ronda nueva se usan todos los años
# cerrados. 03 elige según el año que le toque predecir. Los límites de
# recorte se recalculan sobre el mismo entrenamiento ampliado, porque son
# los cuantiles 2 y 98 de la sd observada y tienen que corresponder a los
# datos con que se ajustó el modelo.
modelos_sd_produccion <- list()
limites_sd_produccion <- list()

for (i in seq_len(nrow(grupos))) {

  g <- grupos$grado[i]; a <- grupos$area[i]
  clave <- paste(g, a, sep = "_")

  datos_grupo <- datos_sd %>% filter(grado == g, area == a)
  if (nrow(datos_grupo) < 30) {
    cat("Grupo", clave, ": muy pocos datos para el modelo de dispersión de producción.\n")
    next
  }

  modelos_sd_produccion[[clave]] <- lm(formula_sd, data = datos_grupo,
                                       weights = n_alu_simce)
  limites_sd_produccion[[clave]] <- quantile(datos_grupo$sd_simce,
                                             c(0.02, 0.98), names = FALSE)
}

cat("\nModelos de dispersión de producción (todos los años cerrados):",
    length(modelos_sd_produccion), "grupos\n")

# ---- Diagnóstico: sd observada vs. predicha --------------------------
diag_sd <- map_dfr(names(modelos_sd), function(clave) {
  partes <- str_split(clave, "_", n = 2)[[1]]
  g <- partes[1]; a <- partes[2]
  dg <- datos_sd %>% filter(grado == g, area == a, agno == anio_test)
  lim <- limites_sd[[clave]]
  tibble(
    grado = g, area = a,
    rbd_revisado = dg$rbd_revisado,
    gse_etiqueta = dg$gse_etiqueta,
    n_alu_simce  = dg$n_alu_simce,
    sd_observada = dg$sd_simce,
    sd_predicha  = pmin(pmax(predict(modelos_sd[[clave]], newdata = dg), lim[1]), lim[2]),
    sd_historica = dg$sd_hist_colegio
  )
})

p_sd <- ggplot(diag_sd, aes(sd_observada, sd_predicha)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  facet_wrap(~ grado + area, scales = "free") +
  labs(
    title = paste("Colegios: dispersión interna observada vs. predicha -", anio_test),
    subtitle = "sd de los puntajes SIMCE individuales dentro de cada colegio",
    x = "sd observada", y = "sd predicha"
  ) +
  theme_minimal()

ggsave(dir_salidas %>% file.path("diagnostico_dispersion.png"), p_sd, width = 8, height = 6)

# ---- Guardar ----------------------------------------------------------
saveRDS(modelos_sd, dir_salidas %>% file.path("modelos_dispersion.rds"))
saveRDS(diag_sd,    dir_salidas %>% file.path("diag_dispersion.rds"))  # lo usa 05
saveRDS(limites_sd, dir_salidas %>% file.path("limites_dispersion.rds"))
saveRDS(modelos_sd_produccion,
        dir_salidas %>% file.path("modelos_dispersion_produccion.rds"))
saveRDS(limites_sd_produccion,
        dir_salidas %>% file.path("limites_dispersion_produccion.rds"))
write_csv(tabla_sd, dir_salidas %>% file.path("metricas_dispersion.csv"))

cat("\nListo. Modelos de dispersión en",
    dir_salidas %>% file.path("modelos_dispersion.rds"), "\n")
cat("Métricas en", dir_salidas %>% file.path("metricas_dispersion.csv"), "\n")

