# =============================================================
# 04_validacion_individual.R
# -------------------------------------------------------------
# ¿Cómo se valida una predicción individual si no existe un vínculo
# alumno-a-alumno entre el ensayo Santillana y el SIMCE?
#
# No se puede evaluar estudiante por estudiante. Pero sí se puede evaluar
# si el CONJUNTO de predicciones de un colegio reproduce el conjunto de
# puntajes que ese colegio efectivamente obtuvo: distribución predicha vs.
# distribución observada, colegio por colegio.
#
# Se hace con dos métricas:
#
#   1) ERROR DE CUANTILES: se comparan los percentiles 5, 10, ..., 95 de
#      las predicciones contra los mismos percentiles observados, y se
#      promedia el error absoluto. Si da 10 puntos, la distribución
#      predicha está corrida ~10 puntos respecto de la real en un
#      percentil típico. Es la distancia de Wasserstein-1 discretizada:
#      captura errores de nivel, de ancho y de forma en un solo número.
#
#   2) ERROR INDIVIDUAL BAJO RANKING PERFECTO: se le asigna a cada
#      estudiante el puntaje observado que corresponde a su percentil
#      dentro del colegio, y se mide el error contra su predicción.
#      OJO: supone que el ensayo ordena perfectamente a los estudiantes,
#      cosa que es falsa. Es una COTA OPTIMISTA del error individual real,
#      no una estimación de él.
#
# Las dos se comparan contra el baseline de "sólo la media": darle a todos
# los estudiantes del colegio el mismo puntaje predicho, que es lo que se
# podía ofrecer antes de tener el archivo de alumnos.
#
# La validación es out-of-time: los modelos de 02 y 02b se entrenaron
# excluyendo este año, y la forma de la distribución y la dispersión
# histórica se calcularon en 01 con ventana expansiva.
#
# CUANDO SE PREDICE UNA RONDA NUEVA no hay SIMCE contra qué validar. Ese
# es el caso normal en producción, así que el script avisa y termina bien
# en vez de fallar.
# =============================================================

library(tidyverse)

# ---- 0. Configuración --------------------------------------------
usuario <- Sys.info()[["user"]]
rutas <- config::get(config = usuario, file = "config.yml")
ruta_outputs <- rutas$ruta_outputs
dir_salidas <- ruta_outputs %>% file.path('modelo_lme_alu_v2')

MIN_ALU_VALID <- 15
QS <- seq(0.05, 0.95, by = 0.05)

pred_individual <- dir_salidas %>% file.path("predicciones_individual.rds") %>% readRDS()
simce_alumno    <- dir_salidas %>% file.path("simce_alumno.rds") %>% readRDS()
simce_dist      <- dir_salidas %>% file.path("simce_dist.rds") %>% readRDS()

anio_val <- max(pred_individual$agno)
cat("Validando predicciones individuales del año:", anio_val, "\n")

obs <- simce_alumno %>% filter(agno == anio_val)

# `quit(status = 0)` sólo fuera de una sesión interactiva, que es como
# corre el pipeline: así el script termina bien y la cadena sigue. En una
# sesión interactiva no se puede cerrar R sin más, así que ahí se levanta
# el error después de haber explicado el motivo.
if (nrow(obs) == 0) {
  cat("\n-------------------------------------------------------------\n")
  cat("No hay SIMCE observado para", anio_val, ": no se puede validar\n")
  cat("esta ronda, y no es un problema — es lo esperable cuando se está\n")
  cat("prediciendo un año que todavía no rinde la prueba.\n\n")
  cat("Las métricas de precisión del modelo salen del último año CERRADO\n")
  cat("y ya están calculadas:\n")
  cat("   metricas_validacion.csv    (nivel, 02)\n")
  cat("   metricas_dispersion.csv    (dispersión, 02b)\n")
  cat("   validacion_*.csv           (individual, de la última corrida\n")
  cat("                               sobre un año cerrado)\n\n")
  cat("Para volver a validar de punta a punta hay que correr el pipeline\n")
  cat("sin los ensayos de la ronda nueva, de modo que el año objetivo\n")
  cat("vuelva a ser uno con SIMCE observado.\n")
  cat("-------------------------------------------------------------\n")
  if (!interactive()) quit(save = "no", status = 0)
  stop("Sin SIMCE observado para ", anio_val, ": ver el mensaje de arriba.")
}

# ---- 1. Cuantiles observados y predichos por colegio ----------------
cuantiles <- function(x) list(quantile(x, QS, names = FALSE, na.rm = TRUE))

obs_q <- obs %>%
  group_by(grado, area, rbd_revisado) %>%
  filter(n() >= MIN_ALU_VALID) %>%
  summarise(
    n_obs     = n(),
    media_obs = mean(ptje),
    sd_obs    = sd(ptje),
    q_obs     = cuantiles(ptje),
    .groups = "drop"
  )

pred_q <- pred_individual %>%
  group_by(grado, area, rbd_revisado) %>%
  filter(n() >= 10) %>%
  summarise(
    n_pred       = n(),
    pred_colegio = first(pred_simce_colegio),
    sd_predicha  = first(sd_simce_pred),
    q_B          = cuantiles(pred_B),
    sd_B         = sd(pred_B, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!is.na(pred_colegio))

comp <- pred_q %>%
  inner_join(obs_q, by = c("grado", "area", "rbd_revisado")) %>%
  mutate(
    q_solo_media    = map(pred_colegio, ~ rep(.x, length(QS))),
    qmae_B          = map2_dbl(q_B, q_obs, ~ mean(abs(.x - .y))),
    qmae_solo_media = map2_dbl(q_solo_media, q_obs, ~ mean(abs(.x - .y))),
    error_sd_B      = sd_B - sd_obs,
    error_media     = pred_colegio - media_obs
  )

cat("\nColegios validados:", nrow(comp), "\n\n")

resumen_dist <- comp %>%
  group_by(grado, area) %>%
  summarise(
    n_colegios      = n(),
    qmae_B          = mean(qmae_B),
    qmae_solo_media = mean(qmae_solo_media),
    sd_observada    = mean(sd_obs),
    sd_predicha     = mean(sd_predicha),
    sd_efectiva_B   = mean(sd_B),
    error_abs_media = mean(abs(error_media)),
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
    n_estudiantes  = n(),
    mae_B          = mean(abs(pred_B - obs_emparejado), na.rm = TRUE),
    mae_solo_media = mean(abs(pred_simce_colegio - obs_emparejado), na.rm = TRUE),
    .groups = "drop"
  )

cat("ERROR INDIVIDUAL BAJO RANKING PERFECTO (cota optimista, ver encabezado):\n")
print(resumen_ind %>% mutate(across(where(is.numeric), ~round(.x, 2))))
cat("\nRecordatorio: el emparejamiento supone que el ensayo ordena perfecto a\n",
    "los estudiantes. El error individual REAL es mayor que éste.\n\n")

# ---- 3. Desglose por contexto ---------------------------------------
# ¿El modelo funciona parejo entre estratos, o sólo en los que dominan la
# base Santillana (particular pagado, GSE alto)? Si el error es claramente
# peor en un estrato con pocos colegios, conviene decirlo antes de que el
# reporte llegue a ese colegio.
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

cat("ERROR POR ESTRATO GSE (ojo con los estratos de pocos colegios):\n")
print(por_estrato %>% mutate(across(where(is.numeric), ~round(.x, 1))))

# ---- 4. Gráficos ------------------------------------------------------
# (i) Distribución predicha vs. observada, agrupando todos los colegios.
dens_data <- bind_rows(
  emparejado %>% transmute(grado, area, valor = pred_B, fuente = "Predicción"),
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
p2 <- ggplot(comp, aes(sd_obs, sd_B)) +
  geom_point(alpha = 0.4) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  facet_wrap(~ grado + area, scales = "free") +
  labs(title = "Dispersión interna: predicha vs. observada",
       x = "sd observada entre estudiantes", y = "sd de las predicciones") +
  theme_minimal()

ggsave(dir_salidas %>% file.path("validacion_dispersion_individual.png"),
       p2, width = 10, height = 7)

# ---- 4b. Densidades resumidas para la presentación -------------------
# El gráfico se arma acá y no en la presentación porque `emparejado` y
# `obs` tienen millones de filas. Se guarda la curva ya evaluada en una
# grilla de 256 puntos por serie: unas pocas miles de filas en total.
dens_validacion <- dens_data %>%
  filter(!is.na(valor)) %>%
  group_by(grado, area, fuente) %>%
  group_modify(~ {
    d <- density(.x$valor, n = 256)
    tibble(x = d$x, y = d$y)
  }) %>%
  ungroup()

# ---- 4c. Distribución por Niveles de Aprendizaje ---------------------
# Traduce puntajes a las categorías que los colegios efectivamente usan
# (Insuficiente / Elemental / Adecuado) y compara la distribución real con
# la que predice el modelo. Es la lectura más directa de si la predicción
# sirve para la pregunta "¿cuántos de mis estudiantes quedarían en cada
# nivel?".
#
# ¡IMPORTANTE! Los puntajes de corte son fijados por la Agencia de Calidad
# de la Educación en los Estándares de Aprendizaje, NO se estiman de los
# datos. Los valores de abajo provienen de los documentos oficiales del
# Mineduc:
#
#   4° básico Lectura     Elemental >= 241, Adecuado >= 284
#   4° básico Matemática  Elemental >= 245, Adecuado >= 295
#   2° medio  Lectura     Elemental >= 250, Adecuado >= 295
#   2° medio  Matemática  Elemental >= 252, Adecuado >= 319
#
# Los tres primeros están verificados contra los Estándares publicados en
# curriculumnacional.cl; el de 2° medio Matemática viene de una copia del
# mismo documento y conviene confirmarlo antes de publicar cifras. Además,
# los Estándares de 2° medio figuran como "no vigentes" en el portal: si la
# Agencia actualizó los cortes para los años de estos datos, hay que
# reemplazarlos acá. Un corte equivocado no rompe nada, simplemente
# clasifica mal y en silencio.
CORTES_NIVEL <- tribble(
  ~grado, ~area,        ~corte_elemental, ~corte_adecuado,
  "4b",   "lenguaje",   241,              284,
  "4b",   "matematica", 245,              295,
  "2m",   "lenguaje",   250,              295,
  "2m",   "matematica", 252,              319
)

clasificar_nivel <- function(datos, columna_puntaje) {
  datos %>%
    left_join(CORTES_NIVEL, by = c("grado", "area")) %>%
    mutate(
      nivel = case_when(
        is.na(corte_elemental) | is.na({{ columna_puntaje }}) ~ NA_character_,
        {{ columna_puntaje }} >= corte_adecuado               ~ "Adecuado",
        {{ columna_puntaje }} >= corte_elemental              ~ "Elemental",
        TRUE                                                 ~ "Insuficiente"
      ),
      nivel = factor(nivel, levels = c("Insuficiente", "Elemental", "Adecuado"))
    )
}

# Se usa el MISMO universo que el resto de la validación: sólo los colegios
# que quedaron en `comp`. Comparar la distribución observada de todos los
# colegios contra la predicha de un subconjunto mezclaría el error del
# modelo con una diferencia de composición.
colegios_validados <- comp %>% distinct(grado, area, rbd_revisado)

niveles_obs <- obs %>%
  inner_join(colegios_validados, by = c("grado", "area", "rbd_revisado")) %>%
  clasificar_nivel(ptje) %>%
  filter(!is.na(nivel)) %>%
  count(grado, area, nivel, name = "n_obs") %>%
  group_by(grado, area) %>%
  mutate(pct_obs = n_obs / sum(n_obs)) %>%
  ungroup()

niveles_pred <- pred_individual %>%
  inner_join(colegios_validados, by = c("grado", "area", "rbd_revisado")) %>%
  clasificar_nivel(pred_B) %>%
  filter(!is.na(nivel)) %>%
  count(grado, area, nivel, name = "n_pred") %>%
  group_by(grado, area) %>%
  mutate(pct_pred = n_pred / sum(n_pred)) %>%
  ungroup()

niveles_logro <- niveles_obs %>%
  full_join(niveles_pred, by = c("grado", "area", "nivel")) %>%
  mutate(across(c(n_obs, n_pred), ~ replace_na(.x, 0L)),
         across(c(pct_obs, pct_pred), ~ replace_na(.x, 0)),
         dif_pp = 100 * (pct_pred - pct_obs)) %>%
  arrange(grado, area, nivel)

cat("\nDISTRIBUCIÓN POR NIVEL DE APRENDIZAJE (observada vs. predicha):\n")
print(niveles_logro %>%
        transmute(grado, area, nivel,
                  `% real` = round(100 * pct_obs, 1),
                  `% modelo` = round(100 * pct_pred, 1),
                  `dif (pp)` = round(dif_pp, 1)))
cat("\nError absoluto medio de la composición, en puntos porcentuales:\n")
print(niveles_logro %>%
        group_by(grado, area) %>%
        summarise(error_pp = mean(abs(dif_pp)), .groups = "drop") %>%
        mutate(error_pp = round(error_pp, 1)))
cat("\nRecordatorio: los cortes son los oficiales de los Estándares de\n",
    "Aprendizaje, no salen de estos datos. Ver el encabezado de esta sección.\n\n")

# ---- 5. Guardar ------------------------------------------------------
saveRDS(niveles_logro,   dir_salidas %>% file.path("niveles_logro.rds"))
write_csv(niveles_logro, dir_salidas %>% file.path("validacion_niveles_logro.csv"))
saveRDS(dens_validacion, dir_salidas %>% file.path("dens_validacion.rds"))
saveRDS(comp %>% select(-starts_with("q_")),
        dir_salidas %>% file.path("validacion_por_colegio.rds"))
write_csv(comp %>% select(-starts_with("q_")),
          dir_salidas %>% file.path("validacion_por_colegio.csv"))
write_csv(resumen_dist, dir_salidas %>% file.path("validacion_distribucional.csv"))
write_csv(resumen_ind,  dir_salidas %>% file.path("validacion_individual.csv"))
write_csv(por_estrato,  dir_salidas %>% file.path("validacion_por_estrato.csv"))

cat("\nListo. Resultados en ", dir_salidas, ":\n",
    " - validacion_distribucional.csv / validacion_individual.csv\n",
    " - validacion_por_colegio.csv / validacion_por_estrato.csv\n",
    " - validacion_distribucion_individual.png / validacion_dispersion_individual.png\n")
