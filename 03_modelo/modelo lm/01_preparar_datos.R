# =============================================================
# 01_preparar_datos.R
# -------------------------------------------------------------
# Carga los ensayos Santillana y los resultados SIMCE a nivel de
# colegio, limpia ambas tablas, y construye dos tablas de
# features listas para modelar:
#
#   - ind_features.rds     : 1 fila por estudiante x año x
#                             grado x área
#   - school_features.rds  : 1 fila por colegio (rbd_revisado) x
#                             año x grado x área, agregando la
#                             tabla individual "de abajo hacia
#                             arriba" (no directamente desde el
#                             csv de ensayos)
#   - school_model_data.rds: school_features + promedio_simce
#                             observado (para entrenar el modelo
#                             en 02_modelo_escolar.R)
#
# Decisiones de datos (detalle en README.md):
#   - El cruce con colegios se hace por rbd_revisado, que Santillana
#     entrega más completo y corregido que 'rbd'.
#   - Sólo hay ensayos para grado 4b y 2m (8b y 6b sólo tienen SIMCE,
#     sin ensayo asociado), por lo que el primer modelo cubre 4b y 2m.
#   - Se agregan por separado 4 modelos (grado x área) porque la
#     escala SIMCE y la dificultad de los ensayos difieren entre
#     4°básico/2°medio y lenguaje/matemática.
# =============================================================

library(tidyverse)

# ---- 0. Configuración --------------------------------------------
# Configurar rutas de archivos: ----
rutas <- config::get(file = "config.yml")
ruta_data_in <- rutas$ruta_data_in
ruta_data_intermedia <- rutas$ruta_data_intermedia
ruta_outputs <- rutas$ruta_outputs

dir_salidas <- ruta_outputs %>% file.path('modelo_lm')
dir_salidas %>% dir.create(showWarnings = FALSE)

# ---- 1. Cargar datos -----------------------------------------------

# Cargar datos ----
## ENSAYOS ----
ensayos_santillana0 <- ruta_data_intermedia %>% 
  file.path('ensayo_simce', 'consolidado_ensayo_simce.parquet') %>% 
  read_parquet()

diccionario_rbd <- ruta_data_intermedia %>% 
  file.path('ensayo_simce', 'diccionario_rbd_ensayo.xlsx') %>%
  read_excel() %>% 
  select(id_colegio, rbd_revisado)

ensayos <- ensayos_santillana0 %>% 
  left_join(diccionario_rbd, by = 'id_colegio') %>% 
  mutate(
    nivel = case_when(str_detect(tolower(curso), '2..m') ~ '2m',
                      str_detect(tolower(curso), '4..b') ~ '4b'),
    area = case_when(str_detect(tolower(area), 'lenguaje') ~ 'lenguaje', 
                     str_detect(tolower(area), 'mate') ~ 'matematica'),
    agno = as.numeric(agno))

## SIMCES ----
simce0_rbd <- ruta_data_intermedia %>% 
  file.path('simce', 'consolidado_datos_simce_rbd.parquet') %>% 
  read_parquet()

simce <- simce0_rbd %>% 
  select(agno, grado, rbd, starts_with('prom')) %>% 
  pivot_longer(cols = starts_with('prom'),
               names_to = 'area',
               values_to = 'promedio_simce') %>% 
  mutate(area = ifelse(str_detect(area, 'lect'), 'lenguaje', 'matematica'),
         agno = as.numeric(agno))


# ---- 2. Limpieza de ensayos -----------------------------------------
ensayos_limpio <- ensayos %>%
  mutate(
    # El número de ensayo (1 a 6) va dentro del texto de "evaluacion",
    # p.ej. "Preparación Simce 3 (2025)" -> 3. Es más confiable que
    # usar id_evaluacion, que no sigue un orden temporal claro.
    ensayo_num   = str_match(evaluacion, "Simce\\s+(\\d+)")[, 2] %>% as.integer(),
    rbd_revisado = as.integer(rbd_revisado),
    # ~0.01% de los registros supera 100% de logro (probablemente
    # puntaje extra/errores de origen); se acota a 100 para no
    # distorsionar los promedios.
    porcentaje_logro = pmin(porcentaje_logro, 100)
  ) %>%
  rename(grado = nivel) %>%
  filter(!is.na(rbd_revisado), !is.na(ensayo_num))

# Un mismo estudiante puede tener 2 registros para el mismo número de
# ensayo (p.ej. versión "basal" y "extenso" del ensayo 1 en matemática
# 2023). Se colapsan promediando antes de seguir.
ensayos_dedup <- ensayos_limpio %>%
  group_by(id_usuario_curso, agno, grado, area, ensayo_num, rbd_revisado) %>%
  summarise(porcentaje_logro = mean(porcentaje_logro), .groups = "drop")

# ---- 3. Features a nivel de estudiante -------------------------------
# Pendiente (slope) de logro a través de los ensayos del año, usando la
# fórmula cerrada de una regresión simple (más rápido que lm() fila a
# fila sobre ~95 mil grupos).
calc_slope <- function(x, y) {
  if (length(x) < 2 || var(x) == 0) return(NA_real_)
  cov(x, y) / var(x)
}

ind_features <- ensayos_dedup %>%
  group_by(id_usuario_curso, agno, grado, area, rbd_revisado) %>%
  summarise(
    n_evals     = n(),
    mean_logro  = mean(porcentaje_logro),
    last_logro  = porcentaje_logro[which.max(ensayo_num)],
    slope_logro = calc_slope(ensayo_num, porcentaje_logro),
    .groups = "drop"
  ) %>%
  # ~27% de los estudiantes rindió sólo 1 ensayo ese año -> slope
  # no calculable. Se imputa 0 ("sin tendencia observable") en vez de
  # descartar al estudiante o dejar NA (que dejaría sin predicción a
  # más de un cuarto de los alumnos en el paso 3).
  mutate(slope_logro = if_else(is.na(slope_logro), 0, slope_logro))

# ---- 4. Features a nivel de colegio (agregando lo individual) --------
school_features <- ind_features %>%
  group_by(agno, grado, area, rbd_revisado) %>%
  summarise(
    n_estudiantes  = n(),
    mean_logro     = mean(mean_logro, na.rm = TRUE),
    last_logro     = mean(last_logro, na.rm = TRUE),
    slope_logro    = mean(slope_logro, na.rm = TRUE),
    n_evals_prom   = mean(n_evals),
    sd_entre_estud = sd(mean_logro, na.rm = TRUE),   # dispersión interna (sólo referencia, no se usa en el 1er modelo)
    .groups = "drop"
  )

# ---- 5. Limpieza SIMCE y cruce con ensayos ----------------------------
simce_limpio <- simce %>%
  distinct() %>%                       # el archivo trae filas duplicadas exactas en 2024
  filter(promedio_simce > 0) %>%       # 0 = colegio sin resultado publicado ese año/área
  mutate(rbd = as.integer(rbd)) %>%
  rename(rbd_revisado = rbd)

school_model_data <- school_features %>%
  inner_join(
    simce_limpio %>% select(agno, grado, area, rbd_revisado, promedio_simce),
    by = c("agno", "grado", "area", "rbd_revisado")
  )

cat("Filas ind_features:      ", nrow(ind_features), "\n")
cat("Filas school_features:   ", nrow(school_features), "\n")
cat("Filas school_model_data: ", nrow(school_model_data), " (colegios con ensayo Y simce cruzados)\n\n")
cat("Cobertura por año/grado/área en school_model_data:\n")
print(school_model_data %>% count(agno, grado, area))

# ---- 6. Guardar --------------------------------------------------------
saveRDS(ind_features,      dir_salidas %>% file.path("ind_features.rds"))
saveRDS(school_features,   dir_salidas %>% file.path("school_features.rds"))
saveRDS(school_model_data, dir_salidas %>% file.path("school_model_data.rds"))

cat("\nListo. Objetos guardados en output/*.rds\n")
