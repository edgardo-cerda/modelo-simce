# =============================================================
# 04_validacion_individual.R   (NUEVO)
# -------------------------------------------------------------
# ¿Cómo se valida una predicción individual si no existe un vínculo
# alumno-a-alumno entre el ensayo Santillana y el SIMCE?
#
# No se puede evaluar estudiante por estudiante. Pero sí se puede
# evaluar algo casi tan útil, y que la v2 no podía evaluar en
# absoluto: si el CONJUNTO de predicciones de un colegio reproduce
# el conjunto de puntajes que ese colegio efectivamente obtuvo.
# Es decir, comparar distribución predicha vs. distribución
# observada, colegio por colegio.
#
# Se hace con dos métricas:
#
#   1) ERROR DE CUANTILES: se comparan los percentiles 5, 10, ..., 95
#      de las predicciones contra los mismos percentiles observados,
#      y se promedia el error absoluto. Si da 10 puntos, significa
#      que la distribución predicha está corrida ~10 puntos respecto
#      de la real en un percentil típico. Es la distancia de
#      Wasserstein-1 discretizada; captura errores de nivel, de ancho
#      y de forma en un solo número.
#
#   2) ERROR INDIVIDUAL BAJO RANKING PERFECTO: se le asigna a cada
#      estudiante el puntaje observado que corresponde a su percentil
#      dentro del colegio, y se mide el error contra su predicción.
#      OJO: esto supone que el ensayo ordena perfectamente a los
#      estudiantes, cosa que es falsa. Por lo tanto es una COTA
#      OPTIMISTA del error individual real, no una estimación de él,
#      y NO sirve para elegir rho (favorece mecánicamente a rho=1).
#      Se reporta porque acota por abajo lo que se le puede prometer
#      a un colegio, y porque permite chequear la cobertura de los
#      rangos de la versión A.
#
# Todo se compara contra dos referencias:
#   - v2 legado: aplicar los coeficientes del colegio al estudiante.
#   - "sólo la media": darle a todos los estudiantes del colegio el
#     mismo puntaje predicho (lo que en la práctica se podía ofrecer
#     antes de tener el archivo de alumnos).
#
# La validación es out-of-time: los modelos de 02 y 02b se entrenaron
# excluyendo este año, y la forma de la distribución y la dispersión
# histórica se calcularon en 01 con ventana expansiva.
# =============================================================

library(tidyverse)

# ---- 0. Configuración --------------------------------------------
usuario <- Sys.info()[["user"]]
rutas <- config::get(config = usuario, file = "config.yml")
ruta_outputs <- rutas$ruta_outputs
dir_salidas <- ruta_outputs %>% file.path('modelo_lme')

MIN_ALU_VALID <- 15
QS <- seq(0.05, 0.95, by = 0.05)
RHOS_SENSIBILIDAD <- c(0.5, 0.6, 0.7, 0.85, 1.0)

pred_individual <- dir_salidas %>% file.path("predicciones_individual.rds") %>% readRDS()
simce_alumno    <- dir_salidas %>% file.path("simce_alumno.rds") %>% readRDS()
simce_dist      <- dir_salidas %>% file.path("simce_dist.rds") %>% readRDS()

anio_val <- max(pred_individual$agno)
cat("Validando predicciones individuales del año:", anio_val, "\n")

obs <- simce_alumno %>% filter(agno == anio_val)
if (nrow(obs) == 0) {
  stop("No hay SIMCE individual observado para ", anio_val,
       ": la validación sólo corre sobre un año ya cerrado.")
}

# ---- 1. Cuantiles observados y predichos por colegio ----------------
cuantiles <- function(x) list(quantile(x, QS, names = FALSE, na.rm = TRUE))

obs_q <- obs %>%
  group_by(grado, area, rbd_revisado) %>%
  filter(n() >= MIN_ALU_VALID) %>%
  summarise(
    n_obs   = n(),
    media_obs = mean(ptje),
    sd_obs  = sd(ptje),
    q_obs   = cuantiles(ptje),
    .groups = "drop"
  )

pred_q <- pred_individual %>%
  group_by(grado, area, rbd_revisado) %>%
  filter(n() >= 10) %>%
  summarise(
    n_pred        = n(),
    pred_colegio  = first(pred_simce_colegio),
    sd_predicha   = first(sd_simce_pred),
    q_A           = cuantiles(pred_A),
    q_B           = cuantiles(pred_B),
    q_v2          = cuantiles(pred_v2_legado),
    sd_A          = sd(pred_A, na.rm = TRUE),
    sd_B          = sd(pred_B, na.rm = TRUE),
    sd_v2         = sd(pred_v2_legado, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!is.na(pred_colegio))

comp <- pred_q %>%
  inner_join(obs_q, by = c("grado", "area", "rbd_revisado")) %>%
  mutate(
    q_solo_media = map(pred_colegio, ~ rep(.x, length(QS))),
    qmae_A          = map2_dbl(q_A, q_obs, ~ mean(abs(.x - .y))),
    qmae_B          = map2_dbl(q_B, q_obs, ~ mean(abs(.x - .y))),
    qmae_v2         = map2_dbl(q_v2, q_obs, ~ mean(abs(.x - .y))),
    qmae_solo_media = map2_dbl(q_solo_media, q_obs, ~ mean(abs(.x - .y))),
    error_sd_A      = sd_A - sd_obs,
    error_sd_B      = sd_B - sd_obs,
    error_sd_v2     = sd_v2 - sd_obs,
    error_media     = pred_colegio - media_obs
  )

cat("\nColegios validados:", nrow(comp), "\n\n")

resumen_dist <- comp %>%
  group_by(grado, area) %>%
  summarise(
    n_colegios       = n(),
    qmae_B           = mean(qmae_B),
    qmae_A           = mean(qmae_A),
    qmae_v2_legado   = mean(qmae_v2),
    qmae_solo_media  = mean(qmae_solo_media),
    sd_observada     = mean(sd_obs),
    sd_predicha      = mean(sd_predicha),
    sd_efectiva_B    = mean(sd_B),
    sd_efectiva_v2   = mean(sd_v2),
    error_abs_media  = mean(abs(error_media)),
    .groups = "drop"
  )

cat("ERROR DE CUANTILES (puntos SIMCE; más bajo es mejor):\n")
print(resumen_dist %>% mutate(across(where(is.numeric), ~round(.x, 1))))
cat("\nLectura: qmae_solo_media es lo que se lograba dándole a todos los\n",
    "estudiantes del colegio el mismo puntaje. La diferencia contra qmae_B\n",
    "es exactamente lo que aporta modelar la dispersión y la forma.\n\n")

# ---- 2. Error individual bajo ranking perfecto (cota optimista) ------
# A cada estudiante se le asigna el puntaje observado de su mismo
# percentil dentro del colegio.
emparejar_por_rango <- function(datos_colegio, puntajes_obs) {
  datos_colegio$obs_emparejado <- quantile(
    puntajes_obs, probs = datos_colegio$pct_ensayo, names = FALSE, na.rm = TRUE
  )
  datos_colegio
}

obs_por_colegio <- obs %>%
  group_by(grado, area, rbd_revisado) %>%
  filter(n() >= MIN_ALU_VALID) %>%
  summarise(puntajes = list(ptje), .groups = "drop")

emparejado <- pred_individual %>%
  inner_join(obs_por_colegio, by = c("grado", "area", "rbd_revisado")) %>%
  group_by(grado, area, rbd_revisado) %>%
  group_modify(function(.x, .y) emparejar_por_rango(.x, .x$puntajes[[1]])) %>%
  ungroup() %>%
  select(-puntajes)

resumen_ind <- emparejado %>%
  group_by(grado, area) %>%
  summarise(
    n_estudiantes   = n(),
    mae_A           = mean(abs(pred_A - obs_emparejado), na.rm = TRUE),
    mae_B           = mean(abs(pred_B - obs_emparejado), na.rm = TRUE),
    mae_v2_legado   = mean(abs(pred_v2_legado - obs_emparejado), na.rm = TRUE),
    mae_solo_media  = mean(abs(pred_simce_colegio - obs_emparejado), na.rm = TRUE),
    cobertura_rango_A = mean(obs_emparejado >= pred_A_inf & obs_emparejado <= pred_A_sup,
                             na.rm = TRUE),
    .groups = "drop"
  )

cat("ERROR INDIVIDUAL BAJO RANKING PERFECTO (cota optimista, ver encabezado):\n")
print(resumen_ind %>% mutate(across(where(is.numeric), ~round(.x, 2))))
cat("\ncobertura_rango_A es la fracción de estudiantes cuyo puntaje emparejado\n",
    "cae dentro del rango p10-p90 de la versión A. Como el emparejamiento\n",
    "supone ranking perfecto, la cobertura real será MENOR que ésta.\n\n")

# ---- 3. Sensibilidad al parámetro rho --------------------------------
# rho no es estimable con estos datos: se muestra qué pasa con cada
# métrica al moverlo. Nótese que las dos métricas empujan en
# direcciones opuestas — el error de cuantiles mejora con rho alto
# (más dispersión) y el error individual emparejado también, pero esa
# segunda métrica está sesgada a favor de rho=1 por construcción. Por
# eso rho debería fijarse por confiabilidad (ver 01), no optimizándolo
# contra esta tabla.
sensibilidad <- map_dfr(RHOS_SENSIBILIDAD, function(rho) {
  tmp <- emparejado %>%
    mutate(
      pred_rho = pred_simce_colegio + rho * sd_simce_pred * z_ensayo,
      sd_res   = sd_simce_pred * sqrt(1 - rho^2),
      inf      = pred_rho - qnorm(0.9) * sd_res,
      sup      = pred_rho + qnorm(0.9) * sd_res
    )
  tmp %>%
    group_by(grado, area) %>%
    summarise(
      rho = rho,
      mae_emparejado = mean(abs(pred_rho - obs_emparejado), na.rm = TRUE),
      cobertura      = mean(obs_emparejado >= inf & obs_emparejado <= sup, na.rm = TRUE),
      ancho_rango    = mean(sup - inf, na.rm = TRUE),
      .groups = "drop"
    )
})

cat("SENSIBILIDAD A rho (versión A):\n")
print(sensibilidad %>%
        mutate(across(where(is.numeric), ~round(.x, 2))) %>%
        arrange(grado, area, rho))

# ---- 3b. Desglose por contexto ---------------------------------------
# ¿El modelo funciona parejo entre estratos, o sólo en los que dominan
# la base Santillana (particular pagado, GSE alto)? Si el error es
# claramente peor en un estrato con pocos colegios, conviene decirlo
# antes de que el reporte llegue a ese colegio.
por_estrato <- comp %>%
  left_join(
    pred_individual %>%
      distinct(grado, area, rbd_revisado, gse_etiqueta, depe2_etiqueta),
    by = c("grado", "area", "rbd_revisado")
  ) %>%
  group_by(grado, gse_etiqueta) %>%
  filter(n() >= 5) %>%
  summarise(n_colegios = n(),
            qmae_B = mean(qmae_B),
            error_abs_media = mean(abs(error_media)),
            sd_obs = mean(sd_obs),
            .groups = "drop")

cat("\nERROR POR ESTRATO GSE (ojo con los estratos de pocos colegios):\n")
print(por_estrato %>% mutate(across(where(is.numeric), ~round(.x, 1))))

# ---- 4. Gráficos ------------------------------------------------------
# (i) Distribución predicha vs. observada, agrupando todos los colegios.
dens_data <- bind_rows(
  emparejado %>% transmute(grado, area, valor = pred_A, fuente = "Versión A (media+dispersión)"),
  emparejado %>% transmute(grado, area, valor = pred_B, fuente = "Versión B (percentil)"),
  emparejado %>% transmute(grado, area, valor = pred_v2_legado, fuente = "v2 legado"),
  obs %>% inner_join(distinct(pred_individual, grado, area, rbd_revisado),
                     by = c("grado", "area", "rbd_revisado")) %>%
    transmute(grado, area, valor = ptje, fuente = "Observado")
)

p1 <- ggplot(dens_data, aes(valor, color = fuente)) +
  geom_density(linewidth = 0.7) +
  facet_wrap(~ grado + area, scales = "free") +
  labs(title = paste("Distribución de puntajes individuales -", anio_val),
       subtitle = "Colegios con ensayo Santillana",
       x = "Puntaje SIMCE", y = "Densidad", color = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(dir_salidas %>% file.path("validacion_distribucion_individual.png"),
       p1, width = 10, height = 7)

# (ii) Dispersión predicha vs. observada por colegio.
p2 <- comp %>%
  select(grado, area, sd_obs, `Versión B` = sd_B, `v2 legado` = sd_v2) %>%
  pivot_longer(c(`Versión B`, `v2 legado`), names_to = "fuente", values_to = "sd_pred") %>%
  ggplot(aes(sd_obs, sd_pred, color = fuente)) +
  geom_point(alpha = 0.4) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  facet_wrap(~ grado + area, scales = "free") +
  labs(title = "Dispersión interna: predicha vs. observada",
       x = "sd observada entre estudiantes", y = "sd de las predicciones", color = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(dir_salidas %>% file.path("validacion_dispersion_individual.png"),
       p2, width = 10, height = 7)

# ---- 5. Guardar ------------------------------------------------------
write_csv(comp %>% select(-starts_with("q_")),
          dir_salidas %>% file.path("validacion_por_colegio.csv"))
write_csv(resumen_dist, dir_salidas %>% file.path("validacion_distribucional.csv"))
write_csv(resumen_ind,  dir_salidas %>% file.path("validacion_individual.csv"))
write_csv(sensibilidad, dir_salidas %>% file.path("validacion_sensibilidad_rho.csv"))
write_csv(por_estrato,  dir_salidas %>% file.path("validacion_por_estrato.csv"))

cat("\nListo. Resultados en output/modelo_lme/:\n",
    " - validacion_distribucional.csv / validacion_individual.csv\n",
    " - validacion_sensibilidad_rho.csv / validacion_por_colegio.csv\n",
    " - validacion_distribucion_individual.png / validacion_dispersion_individual.png\n")
