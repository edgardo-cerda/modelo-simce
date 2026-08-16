# =============================================================
# 02b_modelo_dispersion.R   (v9)
# -------------------------------------------------------------
# Segundo modelo del colegio: en vez del PROMEDIO del SIMCE
# (02_modelo_escolar.R), predice el ANCHO de su distribución
# interna, es decir la desviación estándar de los puntajes
# individuales de sus estudiantes:
#
#   sd_simce ~ sd_entre_estud + iqr_logro_ensayo + sd_hist_colegio +
#              mean_logro
#
# Cuatro términos lineales: un coeficiente por predictor, todos legibles.
#
# -------------------------------------------------------------
# CAMBIO v9: SE VA EL SPLINE Y SE VA EL SEGUNDO TÉRMINO DE NIVEL
# -------------------------------------------------------------
# Hasta la v8 la parte de nivel era
# `ns(mean_logro, 3) + ns(nivel_hist_colegio, 2)`: cinco grados de
# libertad y dos medidas correlacionadas de la misma cosa, de las que
# este mismo encabezado admitía que sus coeficientes no eran
# interpretables por separado.
#
# El spline estaba ahí por una razón de fondo correcta: la relación entre
# nivel y dispersión interna NO es monótona. Por deciles de promedio del
# colegio, en el universo NACIONAL 2025:
#
#   2m matemática:  40 46 49 52 55 57 58 59 60 53
#   4b lenguaje:    51 52 53 53 51 51 50 49 47 44
#   4b matemática:  42 42 44 43 43 43 42 41 40 37
#
# Ese efecto piso/techo es real. Lo que no se sostuvo es que ESTE modelo
# pueda aprovecharlo. Comparadas out-of-time las cinco especificaciones:
#
#   dos splines (v8)      5 df   MAE 4.513
#   un spline             3 df   MAE 4.582
#   spline + lineal       4 df   MAE 4.580
#   lineal, dos términos  2 df   MAE 4.482
#   lineal, un término    1 df   MAE 4.463   <- ésta
#
# La lineal simple gana en los cuatro grupos. Todas las diferencias caben
# en 0.12 puntos sobre una base de 4.5, así que a esta resolución son
# empates — pero cuando hay empate gana la simple.
#
# La explicación reconcilia las dos cosas: la curva de arriba está medida
# sobre ~6.400 colegios de TODO el rango nacional, y este modelo se
# entrena sobre 135-215 colegios concentrados en GSE alto y particular
# pagado. Dentro de ese rango restringido la relación es efectivamente
# monótona, y el spline estaba ajustando una curvatura que los datos de
# entrenamiento no pueden fijar. Si la base Santillana llegara a cubrir el
# rango completo, habría que volver a mirarlo.
#
# `sd_hist_colegio` NO es lo mismo que `nivel_hist_colegio` y se queda: es
# el histórico de la DISPERSIÓN de este colegio, no el de su NIVEL.
# Confundirlos es fácil por el nombre.
#
# Otra reacción instintiva que conviene dejar contestada: la DISTRIBUCIÓN
# de la respuesta está bien como está. `sd_simce` es casi simétrica
# (asimetría entre -0.03 y -0.17) y log(sd) queda sesgada a la izquierda
# (-0.56 a -0.73), así que un GLM gamma o un log-link empeorarían el
# ajuste, no lo mejorarían (probado: MAE prácticamente idéntico).
#
# ¿Por qué hace falta? Porque era la limitación central de la v2:
# las predicciones individuales heredaban casi nada de la
# variabilidad real entre estudiantes de un mismo colegio (el
# modelo nunca la había visto, porque sólo se entrenaba con
# promedios). El resultado eran predicciones individuales todas
# apretadas alrededor del promedio del colegio: rankeaban bien
# pero subestimaban gravemente cuán distintos son en realidad los
# estudiantes entre sí. Con el archivo de alumnos ese target
# ahora existe y se puede modelar directamente.
#
# Los predictores, en orden de importancia observada:
#   - sd_hist_colegio  : qué tan heterogéneo ha sido este colegio en
#                        años anteriores (ventana expansiva, sin
#                        mirar el año objetivo). El más informativo:
#                        la heterogeneidad de un colegio es bastante
#                        persistente en el tiempo. Desde la v4 incluye
#                        prior contextual (GSE, dependencia, ruralidad):
#                        un colegio sin historia ya no recibe la
#                        constante nacional sino la dispersión esperada
#                        para su estrato. Ojo: el aporte del contexto
#                        acá es chico (R² ~0.01 en el universo
#                        nacional), sirve de valor por defecto más que
#                        de señal — ver 01, sección 6b.
#   - iqr_logro_ensayo / sd_entre_estud : cuán dispersos están los
#                        resultados de sus estudiantes en los ensayos
#                        de ESTE año. Es la señal "fresca", la que
#                        detecta cambios que la historia no ve.
#   - mean_logro       : nivel del colegio. Entra porque la dispersión
#                        depende del nivel (efecto piso/techo de la
#                        prueba: los colegios muy arriba o muy abajo
#                        tienen menos recorrido).
#
# Ponderación: la sd muestral de un colegio con 20 alumnos es mucho
# más ruidosa que la de uno con 200 (su error relativo es
# ~1/sqrt(2(n-1))). Se pondera cada colegio por su número de
# alumnos para no dejar que los colegios chicos, que son ruido casi
# puro, dominen el ajuste.
#
# Validación: misma lógica out-of-time que 02 (último año como
# prueba), contra dos baselines: predecir siempre la dispersión
# promedio del grupo, y predecir la dispersión histórica del propio
# colegio.
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

# SALIÓ EN v9: el gráfico de la curva nivel -> dispersión. Tenía sentido
# cuando el término era un spline y su forma no se podía leer de los
# coeficientes. Con el modelo lineal la relación ES el coeficiente de
# `mean_logro` que imprime `tidy()` más arriba, y un gráfico de una recta
# no agrega nada. (Deja de escribirse `curva_nivel_dispersion.rds`; no lo
# consume ningún otro script.)

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
write_csv(tabla_sd, dir_salidas %>% file.path("metricas_dispersion.csv"))

cat("\nListo. Modelos de dispersión en",
    dir_salidas %>% file.path("modelos_dispersion.rds"), "\n")
cat("Métricas en", dir_salidas %>% file.path("metricas_dispersion.csv"), "\n")

