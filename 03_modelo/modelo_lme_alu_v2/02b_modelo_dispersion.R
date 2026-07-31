# =============================================================
# 02b_modelo_dispersion.R   (v5)
# -------------------------------------------------------------
# Segundo modelo del colegio: en vez del PROMEDIO del SIMCE
# (02_modelo_escolar.R), predice el ANCHO de su distribución
# interna, es decir la desviación estándar de los puntajes
# individuales de sus estudiantes:
#
#   sd_simce ~ sd_entre_estud + iqr_logro_ensayo + sd_hist_colegio +
#              ns(mean_logro, 3) + ns(nivel_hist_colegio, 2)
#
# CAMBIO v5 (punto 4): los dos términos de NIVEL entran ahora como
# spline natural en vez de lineales. Motivo: la relación entre el nivel
# del colegio y su dispersión interna NO es monótona. Por deciles de
# promedio del colegio, en el universo nacional 2025:
#
#   2m matemática:  40 46 49 52 55 57 58 59 60 53
#   4b lenguaje:    51 52 53 53 51 51 50 49 47 44
#   4b matemática:  42 42 44 43 43 43 42 41 40 37
#
# Es el efecto piso/techo que este script ya mencionaba, pero un
# coeficiente lineal no puede representarlo: aplasta la subida inicial
# contra la bajada final. En 2m matemática la sd sube 20 puntos y
# después baja 7; un solo coeficiente no dice nada útil ahí.
#
# Se usan POCOS grados de libertad a propósito (3 y 2), porque hay entre
# 135 y 215 filas de entrenamiento por grupo. Medido out-of-time contra
# la versión lineal, la ganancia es clara en 2m y levemente negativa en
# 4b:
#     2m lenguaje   5.35 -> 5.08     4b lenguaje   3.84 -> 3.89
#     2m matemática 6.12 -> 5.50     4b matemática 3.19 -> 3.30
# El script imprime la comparación en cada corrida para poder revisarlo.
#
# Lo que NO cambió, y conviene dejar dicho porque es la reacción
# instintiva: la DISTRIBUCIÓN de la respuesta está bien como está.
# `sd_simce` es casi simétrica (asimetría entre -0.03 y -0.17) y
# log(sd) queda sesgada a la izquierda (-0.56 a -0.73), así que un
# GLM gamma o un log-link empeorarían el ajuste, no lo mejorarían
# (probado: MAE prácticamente idéntico). El problema era la linealidad
# en los PREDICTORES, no la familia de la respuesta.
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
#   - mean_logro / nivel_hist_colegio : nivel del colegio.
#                        Entra porque la dispersión depende del nivel
#                        (efecto piso/techo de la prueba: los colegios
#                        muy arriba o muy abajo tienen menos recorrido).
#                        OJO con la distinción, que antes de la v5 el
#                        nombre invitaba a confundir: `sd_hist_colegio`
#                        es el histórico de la DISPERSIÓN de este
#                        colegio, y `nivel_hist_colegio` es el histórico
#                        de su NIVEL (viene de la sección 6 de 01, cuyo
#                        target es promedio_simce). Los dos están en la
#                        fórmula a propósito y miden cosas distintas.
#                        Como el nivel entra además por `mean_logro`,
#                        los coeficientes individuales de esos dos
#                        términos NO son interpretables por separado
#                        (están correlacionados); la predicción conjunta
#                        sí lo es.
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
library(splines)

# ---- Parámetros del spline (punto 4) ------------------------------
# Pocos grados de libertad a propósito: 135-215 filas de entrenamiento
# por grupo. df = 3 en ns() son 2 nudos interiores; df = 2, uno.
# Fuera del rango de entrenamiento ns() extrapola LINEALMENTE, que es el
# comportamiento seguro (y además `limites_sd` acota el resultado).
USAR_SPLINE      <- TRUE
DF_MEAN_LOGRO    <- 3
DF_NIVEL_HIST    <- 2
# Bajo este número de filas de entrenamiento no se intenta el spline:
# se cae a la fórmula lineal.
MIN_TRAIN_SPLINE <- 80

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

formula_sd_lineal <- sd_simce ~ sd_entre_estud + iqr_logro_ensayo +
  sd_hist_colegio + mean_logro + nivel_hist_colegio

formula_sd_spline <- as.formula(sprintf(
  paste("sd_simce ~ sd_entre_estud + iqr_logro_ensayo + sd_hist_colegio +",
        "ns(mean_logro, df = %d) + ns(nivel_hist_colegio, df = %d)"),
  DF_MEAN_LOGRO, DF_NIVEL_HIST
))

cat("Fórmula base (spline =", USAR_SPLINE, "):\n  ")
print(if (USAR_SPLINE) formula_sd_spline else formula_sd_lineal)
cat("\n")

# Ajusta con spline y, si algo falla (pocos valores distintos, matriz
# singular), cae a la fórmula lineal en vez de abortar el grupo entero.
ajustar_sd <- function(train) {
  if (USAR_SPLINE && nrow(train) >= MIN_TRAIN_SPLINE) {
    m <- tryCatch(
      lm(formula_sd_spline, data = train, weights = n_alu_simce),
      error = function(e) NULL
    )
    if (!is.null(m) && !any(is.na(coef(m)))) {
      return(list(modelo = m, tipo = "spline"))
    }
    warning("Spline no ajustable en un grupo: se usa la fórmula lineal.")
  }
  list(modelo = lm(formula_sd_lineal, data = train, weights = n_alu_simce),
       tipo   = "lineal")
}

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

  ajuste <- ajustar_sd(train)
  modelo <- ajuste$modelo
  modelos_sd[[clave]] <- modelo

  # Rango plausible: una sd predicha nunca debería salirse mucho del
  # rango observado en entrenamiento (protege contra extrapolaciones
  # absurdas en colegios con features atípicas). Con spline esto importa
  # más, no menos: es la red que atrapa cualquier curvatura rara en los
  # bordes del rango de entrenamiento.
  lim <- quantile(train$sd_simce, c(0.02, 0.98), names = FALSE)
  limites_sd[[clave]] <- lim

  pred <- pmin(pmax(predict(modelo, newdata = test), lim[1]), lim[2])
  obs  <- test$sd_simce

  # Auditoría: la misma fórmula pero lineal en el nivel. No se usa para
  # elegir; se imprime para poder ver en cada corrida si el spline se
  # está pagando en este grupo o no.
  mae_lineal <- tryCatch({
    m0 <- lm(formula_sd_lineal, data = train, weights = n_alu_simce)
    p0 <- pmin(pmax(predict(m0, newdata = test), lim[1]), lim[2])
    mean(abs(p0 - obs))
  }, error = function(e) NA_real_)

  mae_modelo    <- mean(abs(pred - obs))
  mae_baseline  <- mean(abs(mean(train$sd_simce) - obs))          # "todos igual de dispersos"
  mae_historico <- mean(abs(test$sd_hist_colegio - obs))          # "igual que su propia historia"
  r2_test       <- 1 - sum((obs - pred)^2) / sum((obs - mean(obs))^2)

  resultados[[clave]] <- tibble(
    grado = g, area = a, tipo_ajuste = ajuste$tipo,
    n_train = nrow(train), n_test = nrow(test),
    sd_media_observada = mean(obs),
    sd_media_predicha  = mean(pred),
    mae = mae_modelo,
    mae_lineal_auditoria   = mae_lineal,
    mae_baseline_constante = mae_baseline,
    mae_baseline_historico = mae_historico,
    mejora_vs_constante_pct = 100 * (mae_baseline - mae_modelo) / mae_baseline,
    mejora_vs_historico_pct = 100 * (mae_historico - mae_modelo) / mae_historico,
    mejora_vs_lineal_pct    = 100 * (mae_lineal - mae_modelo) / mae_lineal,
    r2_test = r2_test
  )

  cat("---", clave, "(", ajuste$tipo, ") ---\n")
  print(tidy(modelo))
  cat(sprintf(
    "MAE: %.2f | lineal (auditoría): %.2f | baseline constante: %.2f | baseline histórico: %.2f | R2 (test %s): %.2f\n\n",
    mae_modelo, mae_lineal, mae_baseline, mae_historico, anio_test, r2_test
  ))
}

tabla_sd <- bind_rows(resultados)
cat("Resumen de validación del modelo de dispersión:\n")
print(tabla_sd)

if ("mejora_vs_lineal_pct" %in% names(tabla_sd)) {
  cat(sprintf(
    "\nGanancia media del spline sobre la versión lineal: %+.1f%% de MAE\n",
    mean(tabla_sd$mejora_vs_lineal_pct, na.rm = TRUE)
  ))
  cat("  (Si sale negativo de forma consistente en varias corridas, bajar\n",
      "  DF_MEAN_LOGRO a 2 o poner USAR_SPLINE en FALSE.)\n")
}

# ---- Forma estimada de la curva nivel -> dispersión -------------------
# El punto del spline es que la relación no es monótona, así que conviene
# poder VERLA. Se evalúa el modelo variando mean_logro y dejando el resto
# de los predictores en su mediana.
curva_nivel <- map_dfr(names(modelos_sd), function(clave) {
  partes <- str_split(clave, "_", n = 2)[[1]]
  g <- partes[1]; a <- partes[2]
  dg <- datos_sd %>% filter(grado == g, area == a, agno != anio_test)
  if (nrow(dg) == 0) return(NULL)
  rejilla <- tibble(
    mean_logro = seq(quantile(dg$mean_logro, 0.02, names = FALSE),
                     quantile(dg$mean_logro, 0.98, names = FALSE),
                     length.out = 60),
    sd_entre_estud     = median(dg$sd_entre_estud, na.rm = TRUE),
    iqr_logro_ensayo   = median(dg$iqr_logro_ensayo, na.rm = TRUE),
    sd_hist_colegio    = median(dg$sd_hist_colegio, na.rm = TRUE),
    nivel_hist_colegio = median(dg$nivel_hist_colegio, na.rm = TRUE)
  )
  rejilla %>%
    mutate(grado = g, area = a,
           sd_predicha = predict(modelos_sd[[clave]], newdata = rejilla))
})

if (nrow(curva_nivel) > 0) {
  p_curva <- ggplot(curva_nivel, aes(mean_logro, sd_predicha)) +
    geom_line(linewidth = 0.8) +
    facet_wrap(~ grado + area, scales = "free") +
    labs(title = "Relación estimada entre nivel del colegio y dispersión interna",
         subtitle = "Resto de los predictores fijados en su mediana",
         x = "mean_logro (ensayo)", y = "sd SIMCE predicha") +
    theme_minimal()
  ggsave(dir_salidas %>% file.path("diagnostico_curva_nivel_dispersion.png"),
         p_curva, width = 8, height = 6)
  saveRDS(curva_nivel, dir_salidas %>% file.path("curva_nivel_dispersion.rds"))
}

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

cat("\nListo. Modelos de dispersión en output/modelo_lme/modelos_dispersion.rds\n")
cat("Métricas en output/modelo_lme/metricas_dispersion.csv\n")
