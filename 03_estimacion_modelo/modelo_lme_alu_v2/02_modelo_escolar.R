# =============================================================
# 02_modelo_escolar.R
# -------------------------------------------------------------
# Modelo del NIVEL del colegio: predice su promedio SIMCE.
#
#   promedio_simce ~ mean_logro_enc + nivel_hist_colegio
#
# Una regresión lineal por combinación grado x área.
#
# Dos términos, y ésa es la idea. La fórmula es mínima ACÁ porque acá hay
# entre 150 y 220 colegios por grupo, y cada grado de libertad se paga
# caro. La estructura vive en 01, donde hay miles de colegios y cuatro
# años para estimarla: ninguno de los dos términos es una variable cruda.
#
#   mean_logro_enc      el ensayo de este año. Empaqueta la calibración
#                       IRT (00), el puntaje verdadero sobre el banco
#                       completo y el encogimiento del promedio escolar
#                       según su confiabilidad (01, secc. 5b-bis). Por ahí
#                       entra el número de ensayos rendidos: no como algo
#                       que suba o baje el SIMCE, sino como precisión de
#                       la medida.
#   nivel_hist_colegio  el colegio de antes. Empaqueta un modelo mixto
#                       sobre el universo NACIONAL con prior contextual de
#                       GSE, dependencia y ruralidad (01, secc. 6). Un
#                       colegio sin historia recibe lo esperado para su
#                       contexto; uno con historia larga queda dominado
#                       por su propia evidencia.
#
# Cualquier término nuevo debería seguir la misma regla: estimarlo donde
# están los datos y transferirlo como UN número.
#
# -------------------------------------------------------------
# DOS JUEGOS DE MODELOS
# -------------------------------------------------------------
# `modelos`            excluyen el último año con SIMCE. Son los que
#                      producen las métricas: sin dejar un año fuera no
#                      habría con qué medir.
# `modelos_produccion` usan todos los años con SIMCE. Son los que sirven
#                      para predecir una ronda nueva, donde el año
#                      objetivo no está en los datos de ninguna manera y
#                      desperdiciar el año más reciente no tendría
#                      sentido.
#
# 03 elige entre unos y otros con una sola regla: usar el modelo que NO
# vio el año que se está prediciendo. De los de producción no se reportan
# métricas — medirlas sobre sus propios datos de entrenamiento sería
# optimista.
#
# Este modelo entrega la MEDIA de la distribución de cada colegio; 02b
# entrega su ANCHO. Es el único componente validado contra verdad
# observada directa: el SIMCE promedio publicado.
# =============================================================

library(tidyverse)
library(broom)

# ---- 0. Configuración --------------------------------------------
usuario <- Sys.info()[["user"]]
rutas <- config::get(config = usuario, file = "config.yml")
ruta_outputs <- rutas$ruta_outputs
dir_salidas <- ruta_outputs %>% file.path('modelo_lme_alu_v2')

school_model_data <- dir_salidas %>% file.path('school_model_data.rds') %>% readRDS()

anios     <- sort(unique(school_model_data$agno))
anio_test <- max(anios)
cat("Años con SIMCE observado:", paste(anios, collapse = ", "), "\n")
cat("Año usado como prueba (out-of-time):", anio_test, "\n\n")

formula_modelo <- promedio_simce ~ mean_logro_enc + nivel_hist_colegio

cat("Fórmula:\n  "); print(formula_modelo); cat("\n")

grupos <- school_model_data %>% distinct(grado, area)

# ---- 1. Modelos de validación y sus métricas -------------------------
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
  # Referencia mínima: darle a todos los colegios el promedio del
  # entrenamiento. Sin ella un MAE de 10 no significa nada.
  mae_baseline <- mean(abs(mean(train$promedio_simce) - obs))
  r2_test      <- 1 - sum((obs - pred)^2) / sum((obs - mean(obs))^2)

  resultados[[clave]] <- tibble(
    grado = g, area = a,
    n_train = nrow(train), n_test = nrow(test),
    media_pred_test = mean(pred), media_obs_test = mean(obs),
    mae = mae_modelo,
    mae_baseline = mae_baseline,
    mejora_vs_baseline_pct = 100 * (mae_baseline - mae_modelo) / mae_baseline,
    r2_test = r2_test,
    sesgo_test = mean(pred - obs)
  )

  cat("---", clave, "---\n")
  print(tidy(modelo))
  cat(sprintf(
    "MAE: %.1f | baseline (promedio del entrenamiento): %.1f | mejora: %.0f%% | R2 (test %s): %.2f\n",
    mae_modelo, mae_baseline, resultados[[clave]]$mejora_vs_baseline_pct,
    anio_test, r2_test
  ))
  cat(sprintf("  Sesgo medio (predicho - observado): %+.1f puntos\n\n",
              resultados[[clave]]$sesgo_test))
}

tabla_resultados <- bind_rows(resultados)
cat("Resumen de validación (todos los grupos):\n")
print(tabla_resultados)

# El sesgo importa mirarlo en conjunto: si sale del mismo signo en TODOS
# los grupos no es ruido muestral, es que el nivel de logro de los ensayos
# se movió entre años sin que el SIMCE de los mismos colegios se moviera, y
# el modelo —que usa el logro en niveles— lo lee como mejora real. Es un
# problema abierto; ver el registro de pruebas, sección 2.5.
if (nrow(tabla_resultados) > 0) {
  cat(sprintf(
    "\nSesgo medio out-of-time en todos los grupos: %+.1f puntos (rango %+.1f a %+.1f)\n",
    mean(tabla_resultados$sesgo_test),
    min(tabla_resultados$sesgo_test), max(tabla_resultados$sesgo_test)
  ))
  if (all(tabla_resultados$sesgo_test > 2)) {
    cat("  AVISO: sesgo positivo en todos los grupos: es la deriva de escala\n",
        " del ensayo entre años. Ver el registro, sección 2.5.\n")
  }
}

# ---- 2. Modelos de producción ----------------------------------------
modelos_produccion <- list()

for (i in seq_len(nrow(grupos))) {

  g <- grupos$grado[i]; a <- grupos$area[i]
  clave <- paste(g, a, sep = "_")

  datos_grupo <- school_model_data %>%
    filter(grado == g, area == a, !is.na(promedio_simce))

  if (nrow(datos_grupo) < 20) {
    cat("Grupo", clave, ": muy pocos datos para el modelo de producción, se omite.\n")
    next
  }

  modelos_produccion[[clave]] <- lm(formula_modelo, data = datos_grupo)
}

cat("\nModelos de producción ajustados con", paste(anios, collapse = ", "),
    ":", length(modelos_produccion), "grupos\n")

# Un coeficiente que salte al agregar el último año es el síntoma de la
# misma deriva entre años del chequeo de arriba, así que conviene tener las
# dos versiones a la vista.
cat("Coeficientes (producción vs. validación):\n")
print(
  map_dfr(names(modelos_produccion), function(clave) {
    cp <- coef(modelos_produccion[[clave]])
    cv <- if (clave %in% names(modelos)) coef(modelos[[clave]])
          else setNames(rep(NA, length(cp)), names(cp))
    tibble(grupo = clave, termino = names(cp),
           produccion = round(unname(cp), 3),
           validacion = round(unname(cv[names(cp)]), 3))
  }) %>%
    filter(termino != "(Intercept)") %>%
    as.data.frame()
)

# ---- 3. Diagnóstico: observado vs. predicho en el año de prueba -------
# Se guardan también el rbd y el contexto de cada colegio: la presentación
# los usa para los tooltips del gráfico interactivo.
diag_plot_data <- map_dfr(names(modelos), function(clave) {
  partes <- str_split(clave, "_", n = 2)[[1]]
  g <- partes[1]; a <- partes[2]
  datos_grupo <- school_model_data %>% filter(grado == g, area == a, agno == anio_test)
  tibble(
    grado = g, area = a,
    rbd_revisado  = datos_grupo$rbd_revisado,
    gse_etiqueta  = datos_grupo$gse_etiqueta,
    depe2_etiqueta = datos_grupo$depe2_etiqueta,
    n_anios_nivel_hist  = datos_grupo$n_anios_nivel_hist,
    n_estudiantes = datos_grupo$n_estudiantes,
    mean_logro    = datos_grupo$mean_logro,
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

ggsave(dir_salidas %>% file.path("diagnostico_observado_vs_predicho.png"),
       p, width = 8, height = 6)

# ---- 4. Guardar -------------------------------------------------------
saveRDS(modelos, dir_salidas %>% file.path("modelos_escolares.rds"))
saveRDS(modelos_produccion,
        dir_salidas %>% file.path("modelos_escolares_produccion.rds"))
# Años con SIMCE observado. 03 lo usa para saber si el año que va a
# predecir es uno cerrado (y entonces corresponde el modelo de validación)
# o una ronda nueva (y corresponde el de producción).
saveRDS(anios, dir_salidas %>% file.path("anios_cerrados.rds"))
saveRDS(diag_plot_data, dir_salidas %>% file.path("diag_nivel.rds"))
saveRDS(anio_test, dir_salidas %>% file.path("anio_test.rds"))
write_csv(tabla_resultados, dir_salidas %>% file.path("metricas_validacion.csv"))

cat("\nListo. Modelos de NIVEL en",
    dir_salidas %>% file.path("modelos_escolares.rds"), "\n")
cat("Siguiente paso: 02b_modelo_dispersion.R (modelo del ANCHO de la distribución)\n")
