# =============================================================
# 03_prediccion_nueva_ronda.R  (v3 - dos versiones de predicción individual)
# -------------------------------------------------------------
# Aplica los modelos de 02 (nivel) y 02b (dispersión) para predecir
# la "próxima ronda" a nivel de colegio y de estudiante.
#
# La lógica de la predicción individual cambió por completo respecto
# a la v2. Antes se le aplicaban los coeficientes del modelo de
# colegio a las features de cada estudiante. Eso ordenaba bien a los
# estudiantes, pero producía una distribución demasiado angosta: los
# coeficientes fueron estimados sobre PROMEDIOS de colegio, cuya
# variabilidad es mucho menor que la de los estudiantes individuales
# (los promedios promedian el ruido). El resultado eran predicciones
# individuales apiñadas en torno al promedio del colegio, cuando en
# los datos reales dos estudiantes del mismo colegio difieren en
# ~40-55 puntos de desviación estándar.
#
# Ahora la predicción individual se construye en dos piezas
# separadas, cada una estimada donde sí hay evidencia:
#
#     puntaje_estudiante = MEDIA del colegio (modelo 02)
#                        + ANCHO del colegio (modelo 02b)
#                          x POSICIÓN del estudiante dentro de su colegio
#
# Las dos versiones se diferencian sólo en cómo traducen "posición
# dentro del colegio" a puntos SIMCE:
#
# --- VERSIÓN A: modelo de media + dispersión -----------------------
#   pred_A = mu_hat + rho * sd_hat * z_ensayo
#
#   donde z_ensayo es cuántas desviaciones estándar por sobre/bajo el
#   promedio de su colegio está el estudiante en los ensayos, y rho es
#   la correlación supuesta entre ensayo y SIMCE a nivel individual.
#
#   El rho es el punto importante: al no ser 1, la predicción se
#   ENCOGE hacia el promedio del colegio (regresión a la media), que
#   es lo correcto para un pronóstico puntual —predecir el extremo
#   para un estudiante extremo maximiza el error esperado— y a cambio
#   deja una incertidumbre residual explícita,
#   sd_residual = sd_hat * sqrt(1 - rho^2), con la que se construye
#   el rango de la predicción. Esta versión es la apropiada cuando la
#   pregunta es "¿cuánto se espera que saque ESTE estudiante, y con
#   cuánta incertidumbre?".
#
# --- VERSIÓN B: matching por percentil ------------------------------
#   pred_B = mu_hat + sd_hat * Q(percentil del estudiante)
#
#   donde Q es la forma empírica de la distribución interna de puntajes
#   observada en miles de colegios reales (calculada en 01, por grado,
#   área y tercil de nivel; no asume normalidad). A cada estudiante se
#   le asigna el puntaje que corresponde a su percentil dentro del
#   colegio.
#
#   Esta versión NO encoge: por construcción, el conjunto de
#   predicciones de un colegio reproduce la distribución que ese
#   colegio debería tener (media, ancho y forma, incluidas las colas).
#   Es la apropiada cuando la pregunta es "¿cómo se va a ver la
#   distribución de mi curso?", "¿cuántos estudiantes quedarían en
#   nivel insuficiente?", "¿quiénes están en riesgo?". A cambio, sus
#   predicciones individuales son puntualmente menos precisas que las
#   de A: asume ranking perfecto entre ensayo y SIMCE.
#
# Ninguna de las dos puede validarse estudiante por estudiante (no
# existe un vínculo entre el id del ensayo Santillana y el id del
# alumno SIMCE), pero ambas SÍ se validan a nivel de distribución
# contra los puntajes individuales reales: ver 04_validacion_individual.R.
# =============================================================

library(tidyverse)

# ---- 0. Configuración --------------------------------------------
usuario <- Sys.info()[["user"]]
rutas <- config::get(config = usuario, file = "config.yml")
ruta_outputs <- rutas$ruta_outputs
dir_salidas <- ruta_outputs %>% file.path('modelo_lme_alu')

# Correlación supuesta entre el ensayo y el SIMCE a nivel individual,
# DENTRO de un colegio. No es estimable con los datos disponibles
# (haría falta un vínculo alumno-alumno entre ambas fuentes), pero sí
# está acotada: la correlación entre dos mediciones nunca supera la
# raíz del producto de sus confiabilidades. 01 estima esas
# confiabilidades y guarda el techo resultante (rho_maximo, del orden
# de 0.71-0.85 según grado y área). 0.70 es un valor conservador y
# defendible; conviene revisarlo si alguna vez se consigue un cruce
# real, y mirar la sensibilidad que reporta 04.
RHO_ENSAYO_SIMCE <- 0.70

# Nivel de confianza del rango que se reporta en la versión A.
NIVEL_RANGO <- 0.80   # -> intervalo p10-p90

# ---- 1. Insumos ---------------------------------------------------
ind_features    <- dir_salidas %>% file.path("ind_features.rds") %>% readRDS()
school_features <- dir_salidas %>% file.path("school_features.rds") %>% readRDS()
modelos         <- dir_salidas %>% file.path("modelos_escolares.rds") %>% readRDS()
modelos_sd      <- dir_salidas %>% file.path("modelos_dispersion.rds") %>% readRDS()
limites_sd      <- dir_salidas %>% file.path("limites_dispersion.rds") %>% readRDS()
forma_z         <- dir_salidas %>% file.path("forma_z.rds") %>% readRDS()
cortes_tercil   <- dir_salidas %>% file.path("cortes_tercil.rds") %>% readRDS()
limites_simce   <- dir_salidas %>% file.path("limites_simce.rds") %>% readRDS()
confiabilidades <- dir_salidas %>% file.path("confiabilidades.rds") %>% readRDS()

anio_prediccion <- max(school_features$agno)
cat("Prediciendo ronda:", anio_prediccion, "\n")
cat("rho (ensayo -> SIMCE individual) usado en la versión A:", RHO_ENSAYO_SIMCE, "\n\n")

cat("Techo de rho según confiabilidad de cada instrumento:\n")
print(confiabilidades %>%
        select(grado, area, conf_ensayo, conf_simce, rho_maximo) %>%
        mutate(across(where(is.numeric), ~round(.x, 2))))
if (any(RHO_ENSAYO_SIMCE > confiabilidades$rho_maximo)) {
  warning("RHO_ENSAYO_SIMCE supera el techo de confiabilidad en al menos un grupo: ",
          "las predicciones de la versión A serán demasiado agresivas en los extremos.")
}
cat("\n")

# ---- 2. Predicción a nivel de colegio: media y ancho ---------------
# mu_hat viene del modelo de 02; sd_hat del modelo de 02b. Si para un
# grupo no hay modelo de dispersión (pocos datos), se usa como respaldo
# la dispersión histórica del propio colegio, que ya viene calculada
# con ventana expansiva en 01.
predecir_colegio <- function(datos, clave) {

  datos$pred_simce_colegio <- if (clave %in% names(modelos)) {
    predict(modelos[[clave]], newdata = datos)
  } else NA_real_

  if (clave %in% names(modelos_sd)) {
    lim <- limites_sd[[clave]]
    sd_hat <- predict(modelos_sd[[clave]], newdata = datos)
    datos$sd_simce_pred <- pmin(pmax(sd_hat, lim[1]), lim[2])
    datos$origen_sd <- "modelo"
  } else {
    datos$sd_simce_pred <- datos$sd_hist_colegio
    datos$origen_sd <- "historica"
  }
  # Respaldo final para colegios sin historia ni modelo: la dispersión
  # esperada para su contexto (GSE, dependencia, ruralidad), calculada
  # en 01. Antes acá iba una constante nacional.
  datos$sd_simce_pred <- coalesce(datos$sd_simce_pred, datos$contexto_sd)
  datos
}

pred_colegio <- school_features %>%
  filter(agno == anio_prediccion) %>%
  group_split(grado, area) %>%
  map_dfr(~ predecir_colegio(.x, paste(.x$grado[1], .x$area[1], sep = "_"))) %>%
  # Tercil de nivel, para elegir la plantilla de forma en la versión B.
  # Se asigna con la media PREDICHA (no la observada): en producción no
  # se conoce la observada.
  left_join(cortes_tercil, by = c("agno", "grado", "area")) %>%
  mutate(
    tercil = case_when(
      is.na(corte_1) | is.na(pred_simce_colegio) ~ "todos",
      pred_simce_colegio <= corte_1              ~ "bajo",
      pred_simce_colegio <= corte_2              ~ "medio",
      TRUE                                       ~ "alto"
    )
  )

cat("Colegios con predicción:", sum(!is.na(pred_colegio$pred_simce_colegio)), "\n")
print(pred_colegio %>%
        group_by(grado, area) %>%
        summarise(n = n(),
                  media_pred = mean(pred_simce_colegio, na.rm = TRUE),
                  sd_pred_media = mean(sd_simce_pred, na.rm = TRUE),
                  .groups = "drop"))

write_csv(
  pred_colegio %>%
    select(agno, grado, area, rbd_revisado, n_estudiantes,
           gse_etiqueta, depe2_etiqueta, rural_etiqueta, nom_com_rbd,
           agno_contexto, n_anios_hist,
           mean_logro, pred_final_logro,
           contexto_nivel, desvio_colegio, colegio_efecto_historico,
           contexto_sd, sd_hist_colegio,
           pred_simce_colegio, sd_simce_pred, origen_sd, tercil),
  dir_salidas %>% file.path("predicciones_colegio.csv")
)

# Comparación de cada colegio contra su propio estrato: útil para
# reportar ("su colegio está X puntos sobre lo esperado para un
# particular subvencionado GSE medio alto") y para detectar predicciones
# raras antes de que salgan a un cliente.
resumen_estrato <- pred_colegio %>%
  group_by(grado, area, gse_etiqueta, depe2_etiqueta) %>%
  summarise(n = n(),
            pred_media = mean(pred_simce_colegio, na.rm = TRUE),
            sd_media   = mean(sd_simce_pred, na.rm = TRUE),
            .groups = "drop") %>%
  filter(n >= 5)
cat("\nPredicción media por estrato (contexto rezagado):\n")
print(resumen_estrato %>% mutate(across(where(is.numeric), ~round(.x, 1))))

# ---- 3. Base individual: estudiante + su colegio -------------------
ind_pred <- ind_features %>%
  filter(agno == anio_prediccion) %>%
  rename(n_evals_prom = n_evals) %>%   # nombre que espera el modelo de 02
  # Las columnas de contexto (gse_etiqueta, depe2_etiqueta, ...) ya vienen
  # dentro de ind_features desde 01: no se vuelven a pegar acá para no
  # generar sufijos .x/.y.
  inner_join(
    pred_colegio %>%
      select(agno, grado, area, rbd_revisado,
             pred_simce_colegio, sd_simce_pred, tercil),
    by = c("agno", "grado", "area", "rbd_revisado")
  ) %>%
  left_join(limites_simce, by = c("grado", "area"))

# ---- 4. VERSIÓN A: media + dispersión ------------------------------
z_rango <- qnorm(1 - (1 - NIVEL_RANGO) / 2)

pred_A <- ind_pred %>%
  mutate(
    sd_residual   = sd_simce_pred * sqrt(1 - RHO_ENSAYO_SIMCE^2),
    pred_A        = pred_simce_colegio + RHO_ENSAYO_SIMCE * sd_simce_pred * z_ensayo,
    pred_A        = pmin(pmax(pred_A, ptje_min), ptje_max),
    pred_A_inf    = pmax(pred_A - z_rango * sd_residual, ptje_min),
    pred_A_sup    = pmin(pred_A + z_rango * sd_residual, ptje_max)
  )

# ---- 5. VERSIÓN B: matching por percentil ---------------------------
# A cada estudiante se le asigna el cuantil de la plantilla empírica
# que corresponde a su percentil dentro del colegio, y ese cuantil se
# traslada a la escala del colegio con su media y ancho predichos.
aplicar_forma <- function(datos) {
  datos %>%
    group_by(agno, grado, area, tercil) %>%
    group_modify(function(.x, .y) {
      f <- forma_z %>%
        filter(agno == .y$agno, grado == .y$grado, area == .y$area, tercil == .y$tercil)
      if (nrow(f) == 0) {
        f <- forma_z %>%
          filter(agno == .y$agno, grado == .y$grado, area == .y$area, tercil == "todos")
      }
      .x$z_forma <- if (nrow(f) == 0) {
        qnorm(pmin(pmax(.x$pct_ensayo, 0.001), 0.999))   # respaldo: normal
      } else {
        approx(x = f$p, y = f$z, xout = .x$pct_ensayo, rule = 2)$y
      }
      .x
    }) %>%
    ungroup()
}

pred_B <- pred_A %>%
  aplicar_forma() %>%
  mutate(
    pred_B = pred_simce_colegio + sd_simce_pred * z_forma,
    pred_B = pmin(pmax(pred_B, ptje_min), ptje_max)
  )

# ---- 6. Versión v2 (legado), sólo para comparar ---------------------
# Es lo que hacía la versión anterior: aplicarle los coeficientes del
# modelo de colegio a las features del estudiante. Se conserva para que
# 04 pueda mostrar cuánto se gana con el cambio.
predecir_legado <- function(datos, clave) {
  datos$pred_v2_legado <- if (clave %in% names(modelos)) {
    predict(modelos[[clave]], newdata = datos)
  } else NA_real_
  datos
}

pred_individual <- pred_B %>%
  group_split(grado, area) %>%
  map_dfr(~ predecir_legado(.x, paste(.x$grado[1], .x$area[1], sep = "_")))

# ---- 7. Chequeos de coherencia --------------------------------------
# (i) El promedio de las predicciones individuales debe reproducir la
#     predicción del colegio, en ambas versiones (son transformaciones
#     centradas: z_ensayo tiene media 0 dentro del colegio, y la
#     plantilla de forma tiene media ~0).
# (ii) La sd de las predicciones de la versión B debe reproducir el
#     ancho predicho; la de la versión A debe ser rho veces ese ancho
#     (el encogimiento es deliberado, no un error).
chequeo <- pred_individual %>%
  group_by(agno, grado, area, rbd_revisado) %>%
  summarise(
    n_est          = n(),
    media_A        = mean(pred_A, na.rm = TRUE),
    media_B        = mean(pred_B, na.rm = TRUE),
    sd_A           = sd(pred_A, na.rm = TRUE),
    sd_B           = sd(pred_B, na.rm = TRUE),
    sd_v2_legado   = sd(pred_v2_legado, na.rm = TRUE),
    pred_colegio   = first(pred_simce_colegio),
    sd_predicha    = first(sd_simce_pred),
    .groups = "drop"
  ) %>%
  mutate(
    dif_media_A   = media_A - pred_colegio,
    dif_media_B   = media_B - pred_colegio,
    razon_sd_A    = sd_A / sd_predicha,     # debería dar ~rho
    razon_sd_B    = sd_B / sd_predicha,     # debería dar ~1
    razon_sd_v2   = sd_v2_legado / sd_predicha  # la v2 daba ~0.1-0.2
  )

cat("\nChequeos de coherencia (promedios sobre colegios):\n")
print(chequeo %>%
        group_by(grado, area) %>%
        summarise(
          dif_media_A_abs = mean(abs(dif_media_A), na.rm = TRUE),
          dif_media_B_abs = mean(abs(dif_media_B), na.rm = TRUE),
          razon_sd_A      = mean(razon_sd_A, na.rm = TRUE),
          razon_sd_B      = mean(razon_sd_B, na.rm = TRUE),
          razon_sd_legado = mean(razon_sd_v2, na.rm = TRUE),
          .groups = "drop"
        ) %>% mutate(across(where(is.numeric), ~round(.x, 3))))
cat("\n(razon_sd_A debería quedar cerca de rho =", RHO_ENSAYO_SIMCE,
    "; razon_sd_B cerca de 1; razon_sd_legado muestra cuánta dispersión",
    "perdía la versión anterior.)\n\n")

# ---- 8. Guardar ------------------------------------------------------
salida_individual <- pred_individual %>%
  select(agno, grado, area, rbd_revisado, id_usuario_curso,
         gse_etiqueta, depe2_etiqueta, rural_etiqueta,
         n_evals_prom, mean_logro, pred_final_logro,
         pct_ensayo, z_ensayo,
         pred_simce_colegio, sd_simce_pred,
         pred_A, pred_A_inf, pred_A_sup, sd_residual,
         pred_B, pred_v2_legado)

write_csv(salida_individual, dir_salidas %>% file.path("predicciones_individual.csv"))
saveRDS(pred_individual, dir_salidas %>% file.path("predicciones_individual.rds"))
saveRDS(pred_colegio,    dir_salidas %>% file.path("predicciones_colegio.rds"))
write_csv(chequeo, dir_salidas %>% file.path("chequeo_coherencia.csv"))

cat("Listo. Archivos generados en output/modelo_lme/:\n",
    " - predicciones_colegio.csv   (media y ancho predichos por colegio)\n",
    " - predicciones_individual.csv (versión A con rango, versión B, y legado v2)\n",
    " - chequeo_coherencia.csv\n",
    "\nSiguiente paso: 04_validacion_individual.R\n")
